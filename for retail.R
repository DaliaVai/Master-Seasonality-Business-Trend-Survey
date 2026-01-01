
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

reco_names <- colnames(reco_fc)
reco_names

str(base_sa)
str(reco_fc)
reco_fc[, v]
summary(confi_retail)


residuals_mat <- get_res_matrix(confi_retail, retail_order, "x13")
residuals_mat
residuals_mat <- window(residuals_mat, start = c(2004, 1))

start_year<-2004

results_all      <- list()   # accuracy metrics
reco_series_all  <- list()   # reconciled SA series
reco_resid_all   <- list()   # reconciled residuals

colnames(reco_fc)
reco_fc
setdiff(retail_order, colnames(reco_fc))
retail_order

retail_order
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
actuals   <- as.numeric(results_retail[[check_series]]$original)
residuals <- actuals - best_reco

# Plot ACF
acf(residuals, main = paste("Residuals ACF for", check_series))

lapply(confi_retail, function(x) head(x$original))
head(confi_retail$Confidence$original)

# Choose the series you want to visualize
target_series <- "Confidence"

# 1. Get Actual Data
actual_df <- data.frame(
  Time  = as.vector(time(results_retail[[target_series]]$original)),
  Value = as.numeric(results_retail[[target_series]]$original),
  Type  = "Original (Raw)"
)

# 2. Get Reconciled Forecast (from your last loop)
viz_reco <- data.frame(
  Time  = as.vector(time(results_retail[[target_series]]$original)),
  Value = as.numeric(reco_fc[, target_series]),
  Type  = "SA + Reconciled"
)

# 3. Combine safely
viz_df <- rbind(actual_df, viz_reco)

# 4. Plot
library(ggplot2)
ggplot(viz_df, aes(x = Time, y = Value, color = Type)) +
  geom_line(linewidth = 1) + 
  theme_minimal() +
  labs(
    title = paste("Visual Evaluation:", target_series),
    subtitle = "Comparing Raw Data to Seasonally Adjusted & Reconciled Results",
    x = "Year",
    y = "Index Value"
  ) +
  scale_color_manual(values = c("black", "#0072B2"))


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



# Combine list of data frames into one
results_df <- do.call(rbind, results_all)

# Then pivot for RMSE
rmse_table <- results_df %>%
  select(Series, SA, Reconciliation, RMSE) %>%
  unite("SA_Reco", SA, Reconciliation, sep = "_") %>%
  pivot_wider(names_from = SA_Reco, values_from = RMSE)

# Pivot for MAE
mae_table <- results_df %>%
  select(Series, SA, Reconciliation, MAE) %>%
  unite("SA_Reco", SA, Reconciliation, sep = "_") %>%
  pivot_wider(names_from = SA_Reco, values_from = MAE)

# Print
print(rmse_table)
print(mae_table)


rmse_xtable <- xtable(rmse_table, 
                      caption = "RMSE for different seasonal adjustment and reconciliation methods",
                      label = "tab:rmse_table")
print(rmse_xtable, include.rownames = FALSE, booktabs = TRUE)

mae_xtable <- xtable(mae_table, 
                      caption = "RMSE for different seasonal adjustment and reconciliation methods",
                      label = "tab:rmse_table")
print(mae_xtable, include.rownames = FALSE, booktabs = TRUE)


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


all_plots_list <- lapply(retail_order, function(v) {
  data.frame(
    Time = as.vector(time(results_retail[[v]]$original)),
    Actual = as.numeric(results_retail[[v]]$original),
    Reconciled = as.numeric(reco_series_all[, v]$str_ols),
    Series = v
  )
})

head(reco_series_all)

all_plots_list <- make_plot_data("X13_MinT")
plot_data <- do.call(rbind, all_plots_list)

make_plot_data <- function(key) {
  reco_mat <- reco_series_all[[key]]
  n_time <- nrow(reco_mat)
  
  plot_data <- do.call(rbind, lapply(seq_along(retail_order), function(i) {
    v <- retail_order[i]
    actual_vec <- as.numeric(results_retail[[v]]$original)
    
    # truncate to min length
    min_len <- min(length(actual_vec), n_time)
    
    data.frame(
      Time       = 1:min_len,
      Actual     = actual_vec[1:min_len],
      Reconciled = reco_mat[1:min_len, i],  # <- use position
      Series     = v,
      Method     = key
    )
  }))
  
  return(plot_data)
}

make_plot_data <- function(key) {
  reco_mat <- reco_series_all[[key]]
  
  # Ensure reco_mat has same rownames/length as actuals
  n_time <- nrow(reco_mat)
  
  plot_data <- do.call(rbind, lapply(retail_order, function(v) {
    
    actual_vec <- as.numeric(results_retail[[v]]$original)
    
    # truncate or pad if lengths differ
    min_len <- min(length(actual_vec), n_time)
    
    data.frame(
      Time       = 1:min_len,                   # simple sequential index
      Actual     = actual_vec[1:min_len],
      Reconciled = reco_mat[1:min_len, v],      # by column name if exists
      Series     = v,
      Method     = key
    )
  }))
  
  return(plot_data)
}


all_plots_list <- lapply(retail_order, function(v) {
  data.frame(
    Time = as.vector(time(results_retail[[v]]$original)),
    Actual = as.numeric(results_retail[[v]]$original),
    Reconciled = as.numeric(results_retail[[v]]$adjusted_str),
    Series = v
  )
})

target_key <- "str_wls"

all_plots_list <- lapply(retail_order, function(v) {
  # Get the reconciled matrix for STR_OLS
  reconciled_matrix <- reco_series_all[[target_key]]
  
  data.frame(
    # Time remains the same
    Time = as.vector(time(results_retail[[v]]$original)),
    
    # Original data
    Actual = as.numeric(results_retail[[v]]$original),
    
    SA_adjusted = as.numeric(results_retail[[v]]$adjusted_str),
    
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




agg_mat




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

agg_mat


 
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