
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

Industry_Confidence<- c("Confidence", "Production_Future_Balance", "Production_Future_Higher", "Production_Future_Lower", "Demand_assessment_balance", "Demand_assessment_too_high", "Demand_assessment_too_low", "Stock_balance", 
                        "Stock_too_high", "Stock_too_small")
existing_vars <- intersect(Industry_Confidence, names(results_industry))
existing_vars
industry_order <- c(
  "Confidence",
  "Production_Future_Balance", "Demand_assessment_balance","Stock_balance",
  "Production_Future_Higher", "Production_Future_Lower",
  "Demand_assessment_too_high", "Demand_assessment_too_low",
  "Stock_too_high","Stock_too_small"
)
confi_industry <- results_industry[existing_vars]
confi_industry <- confi_industry[industry_order]

agg_mat <- matrix(c(
  1/3, -1/3, 1/3, -1/3, -1/3, 1/3, # Confidence = (Balance1 + Balance2 + Balance3)/3
  1, -1, 0, 0, 0, 0,    # Balance1 = Better1 + Worse1
  0, 0, 1, -1, 0, 0,    # Balance2 = Better2 + Worse2
  0, 0, 0, 0, 1, -1    # Balance3 = Better3 + Worse3
), nrow = 4, byrow = TRUE)

rownames(agg_mat) <- c("Confidence", "Production_Future_Balance","Demand_assessment_balance", "Stock_balance")
colnames(agg_mat) <- c("Production_Future_Higher","Production_Future_Lower","Demand_assessment_too_high","Production_Future_Lower","Stock_too_high","Stock_too_small")

agg_mat

start_year<-2004

results_all      <- list()   # accuracy metrics
reco_series_all  <- list()   # reconciled SA series
reco_resid_all   <- list()   # reconciled residuals

colnames(reco_fc)

setdiff(industry_order, colnames(reco_fc))
industry_order
(agg_mat)

sa<-"x13"
for (sa in sa_methods) {
  base_sa <- get_sa_matrix(confi_industry, industry_order, sa)
  length(base_sa)
  
  residuals_mat <- get_res_matrix(confi_industry, industry_order, sa)
  length(residuals_mat)
  
  if (length(residuals_mat)!=length(base_sa)){
  residuals_mat <- window(residuals_mat, start = c(2004, 1))
    residuals_mat <- ts(
    c(0, as.numeric(residuals_mat)),
    start = c(2004, 1),
    frequency = 12)
    }
    
  base_sa <- as.matrix(base_sa)
  residuals_mat <- as.matrix(residuals_mat)
  sa_actuals <- base_sa + residuals_mat
  head(sa_actuals)
  names(base_sa)
  for (reco in reco_methods) {
    
    key <- paste(sa, reco, sep = "_")
    
    # --- CHANGED PART: use csrec() for MinT-shr, old function otherwise ---
    if (reco == "mint_shr") {
      reco_fc <- csrec(
        base     = base_sa,    # forecasts to reconcile
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
    
    # residuals after reconciliation
    reco_resid <- sa_actuals - reco_fc
    
    # accuracy metrics
    metrics <- lapply(industry_order, function(v) {
      actual   <- as.numeric(results_industry[[v]]$original)
      pred     <- reco_fc[, v]
      rmse_val <- sqrt(mean((actual - pred)^2, na.rm = TRUE))
      mae_val  <- mean(abs(actual - pred), na.rm = TRUE)
      bias_val <- mean(actual - pred, na.rm = TRUE)
      c(RMSE = rmse_val, MAE = mae_val, Bias = bias_val)
    })
    
    df <- do.call(rbind, metrics)
    df <- data.frame(
      Series        = industry_order,
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

reconsiled_series <- do.call(rbind, reco_series_all)

reconsiled_series <- data.frame()

tidy_list <- list()

for (key in names(reco_series_all)) {
  df <- as.data.frame(reco_series_all[[key]])    # convert matrix to data frame
  colnames(df) <- paste(colnames(df), key, sep = "_")  # add SA+Reco info
  tidy_list[[key]] <- df
}

# Combine all columns by time (assuming same number of rows)
reconsiled_series <- do.call(cbind, tidy_list)

resid_list <- lapply(names(reco_resid_all), function(key) {
  df <- as.data.frame(reco_resid_all[[key]])
  colnames(df) <- paste(colnames(df), key, sep = "_")
  return(df)
})

# Combine all columns by time
residuals_df <- do.call(cbind, resid_list)

residuals_df<-data.frame()

library(ggplot2)
# Example for one series
series_name <- colnames(residuals_df)[1]

ggplot(residuals_df, aes(x = 1:nrow(residuals_df), y = residuals_df[[series_name]])) +
  geom_line() +
  labs(title = paste("Residuals over time:", series_name),
       x = "Time", y = "Residuals")

ggplot(residuals_df, aes(x = residuals_df[[series_name]])) +
  geom_histogram(bins = 30, fill="skyblue", color="black") +
  labs(title = paste("Residual distribution:", series_name), x="Residuals", y="Count")

mean(residuals_df[[series_name]], na.rm = TRUE)

acf(residuals_df[[series_name]], main=paste("ACF of residuals:", series_name))

Box.test(residuals_df[[series_name]], lag = 12, type = "Ljung-Box")

shapiro.test(residuals_df[[series_name]])



is_zero_series <- function(x) {
  all(is.na(x) | abs(x) < 1e-10)
}


for (col in setdiff(colnames(residuals_df), c("Year","Month"))) {
  cat("\n====================\nResiduals:", col, "\n")
  res <- residuals_df[[col]]
  
  # Bias
  cat("Mean (bias):", mean(res, na.rm = TRUE), "\n")
  
  # Ljung-Box
  lb <- Box.test(res, lag = 12, type = "Ljung-Box")
  print(lb)
  
  # Shapiro-Wilk
  if(length(res) >= 3 & length(res) <= 5000) { # Shapiro-Wilk limit
    sw <- shapiro.test(res)
    print(sw)
  }
  
  # Optional: you can also plot residuals or ACF if desired
}

clean_reco_resid_all <- list()

for (key in names(reco_resid_all)) {
  
  R <- reco_resid_all[[key]]        # matrix
  good_cols <- !apply(R, 2, is_zero_series)
  
  # keep only real residual series
  clean_reco_resid_all[[key]] <- R[, good_cols, drop = FALSE]
}

for (key in names(clean_reco_resid_all)) {
  
  cat("\n====================\nMETHOD:", key, "\n")
  
  R <- clean_reco_resid_all[[key]]
  
  for (v in colnames(R)) {
    cat("\nResiduals:", v, "\n")
    
    res <- R[, v]
    
    cat("Mean:", mean(res, na.rm=TRUE), "\n")
    print(Box.test(res, lag=12, type="Ljung-Box"))
    print(shapiro.test(res))
    cat("Skewness:", skewness(res, na.rm = TRUE), "\n")
    cat("Kurtosis:", kurtosis(res, na.rm = TRUE), "\n")
    dw <- dwtest(res ~ 1)
    cat("Durbin-Watson statistic:", dw$statistic, "p-value:", dw$p.value, "\n")
    
  }
}




dh_result <- doornik_hansen(clean_reco_resid_all[key])

dh_results <- list()

names(clean_reco_resid_all)

for (key in names(clean_reco_resid_all)) {
  
  resid_mat <- clean_reco_resid_all[[key]]
  
  dh_result <- doornik_hansen(resid_mat)
  
  dh_results[[key]] <- list(
    test_statistic = dh_result$Statistic,
    p_value = dh_result$p.value
  )
}

# View results
dh_results

names(dh_result)




resid_metrics <- data.frame(
  Method = character(),
  Residuals = character(),
  # Mean = numeric(),
  #  Skewness = numeric(),
  #  Kurtosis = numeric(),
  Shapiro_p = numeric(),
  LjungBox_p = numeric(),
  DurbinWatson_stat = numeric(),
  DurbinWatson_p = numeric(),
  QS_p = numeric(),
  Friedman_p = numeric(),
  KruskalWallis_p = numeric(),
  stringsAsFactors = FALSE
)

for (key in names(clean_reco_resid_all)) {
  R <- clean_reco_resid_all[[key]]
  
  for (v in colnames(R)) {
    res <- R[, v]
    
    # Compute metrics
    # mean_val <- mean(res, na.rm=TRUE)
    #  skew_val <- skewness(res, na.rm=TRUE)
    #  kurt_val <- kurtosis(res, na.rm=TRUE)
    shapiro_p <- shapiro.test(res)$p.value
    lb_p <- Box.test(res, lag=12, type="Ljung-Box")$p.value
    qs_p <- qs(res)$Pval
    fried_result <- fried(res, freq = 12) 
    friedman_p <- fried_result$Pval
    kw_test <- kw(res, freq = 12)
    kw_p <- kw_test$Pval
    dw <- dwtest(res ~ 1)
    
    # Append to the data frame
    resid_metrics <- rbind(resid_metrics, data.frame(
      Method = key,
      Residuals = v,
      #  Mean = mean_val,
      #  Skewness = skew_val,
      #  Kurtosis = kurt_val,
      Shapiro_p = shapiro_p,
      LjungBox_p = lb_p,
      DurbinWatson_stat = dw$statistic,
      DurbinWatson_p = dw$p.value,
      QS_p = qs_p,
      Friedman_p = friedman_p,
      KruskalWallis_p = kw_p,
      stringsAsFactors = FALSE
    ))
  }
}




resid_metrics[, 3:9] <- round(resid_metrics[, 3:9], 4)

# Convert to LaTeX using xtable
latex_table <- xtable(resid_metrics, 
                      caption = "Residual Diagnostics for Different Methods",
                      label = "tab:resid_metrics")
print(latex_table, include.rownames = FALSE, booktabs = TRUE)

dh_res<- as.data.frame(dh_results)

xtable(dh_res, caption="Doornik-Hansen Multivariate Normality Test", label="tab:resid_dh")


class(resid_clean)

resid_clean <- data.frame()

resid_clean <- resid_metrics

resid_clean$Method <- gsub("^x13_", "", resid_clean$Method)
resid_clean$Method <- gsub("^stl_", "", resid_clean$Method)
resid_clean$Method <- gsub("^str_", "", resid_clean$Method)

# Optional: make uppercase nicer labels
resid_clean$Method <- toupper(resid_clean$Method)

# Remove prefixes in Residuals column
resid_clean$Residuals <- gsub("^sa_actuals\\.base_sa\\.", "", resid_clean$Residuals)
resid_clean$Residuals <- gsub("^base_sa\\.", "", resid_clean$Residuals)




keep_cols <- c(
  "Method",
  "Residuals",
  # "Skewness",
  #  "Kurtosis",
  "Shapiro_p",
  "LjungBox_p",
  "DurbinWatson_stat",
  "DurbinWatson_p",
  "QS_p",
  "Friedman_p"
)

# 2. Split by method family
resid_x13 <- subset(resid_clean, grepl("^X13", resid_metrics$Method, ignore.case = TRUE))[ , keep_cols]
resid_stl <- subset(resid_clean, grepl("^STL", resid_metrics$Method, ignore.case = TRUE))[ , keep_cols]
resid_str <- subset(resid_clean, grepl("^STR", resid_metrics$Method, ignore.case = TRUE))[ , keep_cols]
# 3. Create LaTeX tables

# X-13
print(
  xtable(
    resid_x13,
    caption = "Residual diagnostics for X-13-based methods",
    label   = "tab:resid_x13"
  ),
  include.rownames = FALSE,
  booktabs = TRUE
)

# STL
print(
  xtable(
    resid_stl,
    caption = "Residual diagnostics for STL-based methods",
    label   = "tab:resid_stl"
  ),
  include.rownames = FALSE,
  booktabs = TRUE
)

# STR
print(
  xtable(
    resid_str,
    caption = "Residual diagnostics for STR-based methods",
    label   = "tab:resid_str"
  ),
  include.rownames = FALSE,
  booktabs = TRUE
)









test_seasonality_to_latex(
  data = reconsiled_series,
  variables = names(reconsiled_series),
  start_year = 2004
)



final_results

summary_best <- final_results %>%
  group_by(Series) %>%
  slice_min(order_by = RMSE, n = 1) %>%
  select(Series, SA, Reconciliation, RMSE, Bias)

print(summary_best)


improvement_df <- final_results %>%
  group_by(Series, SA) %>%
  mutate(Improvement = (RMSE[Reconciliation == "none"] - RMSE) / RMSE[Reconciliation == "none"] * 100) %>%
  filter(Reconciliation != "none")

improvement_df

check_series <- "Confidence"
best_reco <- reco_fc[, check_series]
actuals   <- as.numeric(results_industry[[check_series]]$original)
residuals <- actuals - best_reco

# Plot ACF
acf(residuals, main = paste("Residuals ACF for", check_series))

lapply(confi_industry, function(x) head(x$original))
head(confi_industry$Confidence$original)

# Choose the series you want to visualize
target_series <- "Confidence"



# rmse_table <- results_all %>%
#   select(Series, SA, Reconciliation, RMSE) %>%
#   unite("SA_Reco", SA, Reconciliation, sep = "_") %>%  # combine SA + reconciliation for column names
#   pivot_wider(names_from = SA_Reco, values_from = RMSE)
# 
# mae_table <- results_all %>%
#   select(Series, SA, Reconciliation, MAE) %>%
#   unite("SA_Reco", SA, Reconciliation, sep = "_") %>%
#   pivot_wider(names_from = SA_Reco, values_from = MAE)
# 
# # Print tables
# print(rmse_table)
# print(mae_table)
results_df <-data.frame()

library(dplyr)
library(tidyr)

# Combine list of data frames into one
results_df <- do.call(rbind, results_all)

# Then pivot for RMSE
rmse_table <- results_df %>%
  select(Series, SA, Reconciliation, RMSE) %>%
  unite("SA_Reco", SA, Reconciliation, sep = "_") %>%
  pivot_wider(names_from = SA_Reco, values_from = RMSE)

rmse_table <- results_df %>%
  select(Series, SA, Reconciliation, RMSE) %>%
  mutate(Row = paste(Series, SA, sep = " + ")) %>%
  select(Row, Reconciliation, RMSE) %>%
  pivot_wider(names_from = Reconciliation, values_from = RMSE)

rmse_table <- results_df %>%
  select(Series, SA, Reconciliation, RMSE) %>%
  pivot_wider(
    names_from  = c(SA, Reconciliation),
    values_from = RMSE
  )


# Pivot for MAE
mae_table <- results_df %>%
  select(Series, SA, Reconciliation, MAE) %>%
  unite("SA_Reco", SA, Reconciliation, sep = "_") %>%
  pivot_wider(names_from = SA_Reco, values_from = MAE)



# Print
print(rmse_table)
print(mae_table)

library(xtable)

rmse_xtable <- xtable(rmse_table, 
                      caption = "RMSE for different seasonal adjustment and reconciliation methods",
                      label = "tab:rmse_table")
print(rmse_xtable, include.rownames = FALSE, booktabs = TRUE,sanitize.colnames.function = identity)

rmse_table <- results_df %>%
  select(SA, Series, Reconciliation, RMSE) %>%
  pivot_wider(
    names_from  = Reconciliation,
    values_from = RMSE
  ) %>%
  mutate(Row = paste(SA, Series)) %>%  # X13 Confidence, X13 Demand, etc.
  select(Row, everything(), -SA, -Series)  # Keep Row as first column

mae_table <- results_df %>%
  select(SA, Series, Reconciliation, MAE) %>%
  pivot_wider(
    names_from  = Reconciliation,
    values_from = MAE
  ) %>%
  mutate(Row = paste(SA, Series)) %>%  # X13 Confidence, X13 Demand, etc.
  select(Row, everything(), -SA, -Series) 




mae_table <- xtable(mae_table, 
                    caption = "MAE for different seasonal adjustment and reconciliation methods",
                    label = "tab:rmse_table")
print(mae_table, include.rownames = FALSE, booktabs = TRUE)


# Count how many times each method was the best (lowest RMSE)
best_methods <- apply(rmse_table[, -1], 1, function(x) names(x)[which.min(x)])
table(best_methods)

# Calculate Percentage Improvement of Shrinkage over Base
rmse_table$improvement <- (rmse_table$x13_none - rmse_table$x13_shr) / rmse_table$x13_none * 100

summary_best <- final_results %>%
  group_by(Series) %>%
  slice_min(order_by = RMSE, n = 1) %>%
  select(Series, SA, Reconciliation, RMSE, Bias)

print(summary_best)


all_plots_list <- lapply(industry_order, function(v) {
  data.frame(
    Time = as.vector(time(results_industry[[v]]$original)),
    Actual = as.numeric(results_industry[[v]]$original),
    Reconciled = as.numeric(reco_series_all[, v]$str_ols),
    Series = v
  )
})

head(reco_series_all)




all_plots_list <- lapply(industry_order, function(v) {
  data.frame(
    Time = as.vector(time(results_industry[[v]]$original)),
    Actual = as.numeric(results_industry[[v]]$original),
    Reconciled = as.numeric(results_industry[[v]]$adjusted_str),
    Series = v
  )
})

target_key <- "x13_ols"

all_plots_list <- lapply(industry_order, function(v) {
  
  reconciled_matrix <- reco_series_all[[target_key]]
  
  data.frame(
    
    Time = as.vector(time(results_industry[[v]]$original)),
    
    Actual = as.numeric(results_industry[[v]]$original),
    
    SA_adjusted = as.numeric(results_industry[[v]]$adjusted_x13),
    
    # Extract the column 'v' from the reconciled matrix
    Reconciled = as.numeric(reconciled_matrix[, v]),
    
    Series = v
  )
})



all_plots_df <- do.call(rbind, all_plots_list)

all_plots_long <- all_plots_df %>%
  pivot_longer(cols = c(Actual, SA_adjusted, Reconciled), 
               names_to = "Type", 
               values_to = "Value")

after_industry <- test_seasonality(data = reco_mint, variables = industry_order, start_year = 2004)


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




reco_mint<- csrec(
  base = base_sa,    # Forecasts to reconcile
  agg_mat = agg_mat,  
  res = residuals_mat,         # Residuals
  method = "mint",
  covariance = "shr"
)

print(head(reco_mint))


reco_mint <- as.data.frame(reco_mint)


all_plots_list <- lapply(industry_order, function(v) {
  
  data.frame(
    # Time remains the same
    Time = as.vector(time(results_industry[[v]]$original)),
    
    # Original data
    Actual = as.numeric(results_industry[[v]]$original),
    
    SA_adjusted = as.numeric(results_industry[[v]]$adjusted_str),
    
    # Extract the column 'v' from the reconciled matrix
    Reconciled = as.numeric(reco_mint[, v]),
    
    Series = v
  )
})


ggplot(all_plots_long, aes(x = Time, y = Value, color = Type)) +
  geom_line(linewidth = 0.7) +
  facet_wrap(~ Series, scales = "free_y") + 
  theme_bw() +
  labs(
    title = "Actual vs. STR-OLS Reconciled Forecasts",
    subtitle = paste("Method:", target_key),
    x = "Year",
    y = "Value"
  ) +
  theme(legend.position = "bottom")





library(tidyr)
all_plots_long <- all_plots_df %>%
  pivot_longer(cols = c(Actual, Reconciled), names_to = "Type", values_to = "Value")

# Plot with Facets
ggplot(all_plots_long, aes(x = Time, y = Value, color = Type)) +
  geom_line() +
  facet_wrap(~ Series, scales = "free_y") + # 'free_y' allows each plot to have its own scale
  theme_bw() +
  labs(title = "Overview of Seasonal Adjustment & Reconciliation Across All Series") +
  theme(legend.position = "bottom")



# Extract bottom level series (series that aren't sums of others)
# This depends on your agg_mat structure
bottom_series <- colnames(agg_mat) 
bottom_series

# Reconstruct the totals from the bottom up
reconstructed_totals <- reco_fc[, bottom_series] %*% t(agg_mat)

# Calculate the difference between Reconciled Totals and Reconstructed Totals
diff_matrix <- reco_fc[, rownames(agg_mat)] - reconstructed_totals

# If this is working, the max difference should be near 0 (e.g., 1e-12)
max(abs(diff_matrix))