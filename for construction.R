#Open the data
data<- read_excel("R/magistras/construction.xlsx")
View(data)
data_miau <- data[, colSums(is.na(data)) == 0]
construction<- setdiff(names(data_miau), c("Year", "Month", "timetogether"))
start_year<-2013

#Checking initial data for Seasonality
initial_construction <- test_seasonality_to_latex(data = data, variables = construction, start_year = start_year)

#Seasonally adjusting 
results_construction <- adjust_seasonality(data, construction, start_year)

start_year

Construction_Confidence <- c("Confidence", "Demand_assessment_balance", "Demand_assessment_too_high", "Demand_assessment_too_low", "Employees_increase", "Employees_decrease", "Employee_balance")
existing_vars <- intersect(Construction_Confidence, names(results_construction))
existing_vars

confi_construction <- results_construction[existing_vars]

cons_order <- c(
  "Confidence",
  "Demand_assessment_balance", "Employee_balance",
  "Demand_assessment_too_high", "Demand_assessment_too_low",
  "Employees_increase", "Employees_decrease"
)

confi_construction <- confi_construction[cons_order]

#Aggregation matrix for Construction Confidence
agg_mat <- matrix(c(
  0.5, -0.5, 0.5, -0.5,  # Confidence = (Balance1 + Balance2)/2
  1, -1, 0, 0,    # Balance1 = Better1 + Worse1
  0, 0, 1, -1    # Balance2 = Better2 + Worse2
), nrow = 3, byrow = TRUE)

rownames(agg_mat) <- c("Confidence", "Demand_assessment_balance","Employee_balance")
colnames(agg_mat) <- c("Demand_assessment_too_high","Demand_assessment_too_low","Employees_increase","Employees_decrease")

agg_mat

sa_methods  <- c("x13", "str")
reco_methods <- c("none", "ols", "wls", "mint_shr", "oasd")

get_sa_matrix <- function(data, vars, method) {
  do.call(cbind, lapply(data[vars], function(x) {
    x[[paste0("adjusted_", method)]]
  }))
}

get_res_matrix <- function(data, vars, method) {
  do.call(cbind, lapply(data[vars], function(x) {
    x[[paste0("res_", method)]]
  }))
}

reconcile_forecasts <- function(base, agg_mat, res, method) {
  if (method == "none") return(base)
  
  csrec(
    base = base,
    agg_mat = agg_mat,
    res = res,
    comb = method
  )
}

results_all      <- list()   # accuracy metrics
reco_series_all  <- list()   # reconciled SA series
reco_resid_all   <- list()   # reconciled residuals


for (sa in sa_methods) {
  base_sa <- get_sa_matrix(confi_construction, cons_order, sa)
  base_sa <- as.matrix(base_sa)
  residuals_mat <- get_res_matrix(confi_construction, cons_order, sa)
  residuals_mat <- as.matrix(residuals_mat)
  residuals_mat <- window(residuals_mat, start = c(2013, 1))
  
  sa_actuals <- base_sa + residuals_mat
  
  for (reco in reco_methods) {
    
    key <- paste(sa, reco, sep = "_")
    
    if (reco == "mint_shr") {
      reco_fc <- csrec(
        base     = base_sa,    
        agg_mat  = agg_mat,
        res      = residuals_mat,
        method   = "mint",
        covariance = "shr"
      )
    } else {
      reco_fc <- reconcile_forecasts(
        base    = base_sa,
        agg_mat = agg_mat,
        res     = residuals_mat,
        method  = reco
      )
    }
    reco_fc <- as.matrix(reco_fc)
    
    reco_resid <- sa_actuals - reco_fc
    
    metrics <- lapply(cons_order, function(v) {
      actual   <- as.numeric(results_construction[[v]]$original)
      pred     <- reco_fc[, v]
      rmse_val <- sqrt(mean((actual - pred)^2, na.rm = TRUE))
      mae_val  <- mean(abs(actual - pred), na.rm = TRUE)
      bias_val <- mean(actual - pred, na.rm = TRUE)
      c(RMSE = rmse_val, MAE = mae_val, Bias = bias_val)
    })
    
    df <- do.call(rbind, metrics)
    df <- data.frame(
      Series        = cons_order,
      SA            = sa,
      Reconciliation = reco,
      df
    )
    
    results_all[[key]]     <- df
    reco_series_all[[key]] <- reco_fc
    reco_resid_all[[key]]  <- reco_resid
  }
}

final_results <- do.call(rbind, results_all)
reconsiled_series <- data.frame()
reconsiled_series <- do.call(rbind, reco_series_all)


test_seasonality_to_latex(
  data = reconsiled_series,
  variables = names(reconsiled_series),
  start_year = 2004
)

#Plotting reconciled series
target_key <- "x13_ols"

all_plots_list <- lapply(cons_order, function(v) {
  
  reconciled_matrix <- reco_series_all[[target_key]]
  
  data.frame(
 
    Time = as.vector(time(results_construction[[v]]$original)),
    
    Actual = as.numeric(results_construction[[v]]$original),
    
    SA_adjusted = as.numeric(results_construction[[v]]$adjusted_x13),
    
    Reconciled = as.numeric(reconciled_matrix[, v]),
    
    Series = v
  )
})

all_plots_df <- do.call(rbind, all_plots_list)

all_plots_long <- all_plots_df %>%
  pivot_longer(cols = c(Actual, SA_adjusted, Reconciled), 
               names_to = "Type", 
               values_to = "Value")

after_construction <- test_seasonality(data = reco_mint, variables = cons_order, start_year = 2013)


ggplot(all_plots_long, aes(x = Time, y = Value, color = Type)) +
  geom_line(aes(linetype = Type), linewidth = 0.7) +
  scale_color_manual(values = c("Actual" = "black", 
                                "SA_adjusted" = "#2C7BB6", 
                                "Reconciled" = "#D7191C")) +
  scale_linetype_manual(values = c("Actual" = "solid", 
                                   "SA_adjusted" = "solid", 
                                   "Reconciled" = "solid")) +
  facet_wrap(~ Series, ncol=2, scales = "free_y") + 
  theme_bw(base_size = 15) +
  labs(title = "Comparison: Original vs. x13-Arima- SEATS SA vs. Reconciled (OLS)") +
  theme(legend.position = "bottom")

