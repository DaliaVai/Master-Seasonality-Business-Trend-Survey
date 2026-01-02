data3<- read_excel("R/magistras/retail.xlsx")
View(data3)

retail<- setdiff(names(data3), c("Year", "Month", "Laikotarpis"))
start_year<-2004

initial_retail <- test_seasonality(data = data3, variables = retail, start_year = 2004)

results_retail <- adjust_seasonality(data3, retail, start_year)

Retail_Confidence<- c("Confidence", "Business_Situation_Past_balance", "Business_Situation_Past_better", "Business_Situation_Past_Worse", "Stock_balance", "Stock_too_high", "Stock_too_small", "Business_Situation_Future_balance", 
                      "Business_Situation_Future_better", "Business_Situation_Future_Worse")

existing_vars <- intersect(Retail_Confidence, names(results_retail))

existing_vars

confi_retail <- results_retail[existing_vars]

retail_order <- c(
  "Confidence",
  "Business_Situation_Past_balance", "Stock_balance", "Business_Situation_Future_balance",
  "Business_Situation_Past_better", "Business_Situation_Past_Worse",
  "Stock_too_high", "Stock_too_small",
  "Business_Situation_Future_better", "Business_Situation_Future_Worse"
)

confi_retail <- confi_retail[retail_order]

#Aggregation matrix for Retail Confidence
agg_mat <- matrix(c(
  1/3, -1/3, -1/3, 1/3, 1/3, -1/3, # Confidence = (Balance1 + Balance2 + Balance3)/3
  1, -1, 0, 0, 0, 0,    # Balance1 = Better1 + Worse1
  0, 0, 1, -1, 0, 0,    # Balance2 = Better2 + Worse2
  0, 0, 0, 0, 1, -1    # Balance3 = Better3 + Worse3
), nrow = 4, byrow = TRUE)

rownames(agg_mat) <- c("Confidence", "Business_Situation_Past_balance","Stock_balance", "Business_Situation_Future_balance")
colnames(agg_mat) <- c("Business_Situation_Past_better","Business_Situation_Past_Worse","Stock_too_high","Stock_too_small","Business_Situation_Future_better","Business_Situation_Future_Worse")

agg_mat

sa_methods  <- c("x13", "str")
reco_methods <- c("none", "ols", "shr", "oasd", "wls")

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
  base_sa <- get_sa_matrix(confi_retail, retail_order, sa)
  base_sa <- as.matrix(base_sa)
  residuals_mat <- get_res_matrix(confi_retail, retail_order, sa)
  residuals_mat <-as.matrix(residuals_mat)
  residuals_mat <- window(residuals_mat, start = c(2004, 1))
  
  sa_actuals <- base_sa + residuals_mat
  
  
  for (reco in reco_methods) {
    
    key <- paste(sa, reco, sep = "_")
    
    reco_fc <- reconcile_forecasts(
      base   = base_sa,
      agg_mat = agg_mat,
      res    = residuals_mat,
      method = reco
    )
    reco_fc <- as.matrix(reco_fc)  # ensure it's a matrix
    
    reco_resid <- reconcile_forecasts(
      base    = residuals_mat,
      agg_mat = agg_mat,
      res     = residuals_mat,
      method  = reco
    )
    
    reco_resid <- sa_actuals - reco_fc
    
    metrics <- lapply(retail_order, function(v) {
      actual <- as.numeric(results_retail[[v]]$original)
      pred   <- reco_fc[, v]
      rmse_val <- sqrt(mean((actual - pred)^2, na.rm = TRUE))
      mae_val  <- mean(abs(actual - pred), na.rm = TRUE)
      bias_val <- mean(actual - pred, na.rm = TRUE) 
      c(RMSE = rmse_val, MAE = mae_val, Bias = bias_val)
    })
    
    df <- do.call(rbind, metrics)
    df <- data.frame(
      Series = retail_order,
      SA = sa,
      Reconciliation = reco,
      df
    )
    
    results_all[[key]]     <- df
    reco_series_all[[key]] <- reco_fc
    reco_resid_all[[key]]  <- reco_resid
  }
}

final_results <- do.call(rbind, results_all)
final_results 


target_key <- "str_wls"

all_plots_list <- lapply(retail_order, function(v) {
  reconciled_matrix <- reco_series_all[[target_key]]
  
  data.frame(
    Time = as.vector(time(results_retail[[v]]$original)),
    
    Actual = as.numeric(results_retail[[v]]$original),
    
    SA_adjusted = as.numeric(results_retail[[v]]$adjusted_str),
    
    Reconciled = as.numeric(reconciled_matrix[, v]),
    
    Series = v
  )
})




all_plots_df <- do.call(rbind, all_plots_list)

all_plots_long <- all_plots_df %>%
  pivot_longer(cols = c(Actual, SA_adjusted, Reconciled), 
               names_to = "Type", 
               values_to = "Value")



ggplot(all_plots_long, aes(x = Time, y = Value, color = Type)) +
  geom_line(aes(linetype = Type), linewidth = 0.7) +
  scale_color_manual(values = c("Actual" = "black", 
                                "SA_adjusted" = "#2C7BB6", 
                                "Reconciled" = "#D7191C")) +
  scale_linetype_manual(values = c("Actual" = "solid", 
                                   "SA_adjusted" = "solid", 
                                   "Reconciled" = "solid")) +
  facet_wrap(~ Series, scales = "free_y", ncol = 3) + 
  theme_bw(base_size = 15) +
  labs(title = "Comparison: Original vs. x13-Arima-SEATS-SA vs. Reconciled (OLS)") +
  theme(legend.position = "bottom")

