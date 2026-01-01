
Services_Confidence<- c("Confidence", "Business_Situation_Past_balance", "Business_Situation_Past_better", "Business_Situation_Past_Worse", "Demand_Past_Balance", "Demand_Past_Better", "Demand_Past_Worse", "Demand_Future_Balance", 
                        "Demand_Future_Better", "Demand_Future_Worse")
existing_vars <- intersect(Services_Confidence, names(results_services))
existing_vars
services_order <- c(
  "Confidence",
  "Business_Situation_Past_balance", "Demand_Past_Balance","Demand_Future_Balance",
  "Business_Situation_Past_better", "Business_Situation_Past_Worse",
  "Demand_Past_Better", "Demand_Past_Worse",
  "Demand_Future_Better","Demand_Future_Worse"
)
confi_services <- results_services[existing_vars]
confi_services <- confi_services[services_order]

#Aggregation matrix for Retail Confidence
agg_mat <- matrix(c(
  1/3, -1/3, 1/3,- 1/3, 1/3, -1/3, # Confidence = (Balance1 + Balance2 + Balance3)/3
  1, -1, 0, 0, 0, 0,    # Balance1 = Better1 + Worse1
  0, 0, 1, -1, 0, 0,    # Balance2 = Better2 + Worse2
  0, 0, 0, 0, 1, -1    # Balance3 = Better3 + Worse3
), nrow = 4, byrow = TRUE)

rownames(agg_mat) <- c("Confidence", "Business_Situation_Past_balance","Demand_Past_Balance", "Demand_Future_Balance")
colnames(agg_mat) <- c("Business_Situation_Past_better","Business_Situation_Past_Worse","Demand_Past_Better","Demand_Past_Worse","Demand_Future_Better","Demand_Future_Worse")

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

reco_names <- colnames(reco_fc)
reco_names

str(base_sa)
str(reco_fc)
reco_fc[, v]
summary(confi_services)







residuals_mat <- get_res_matrix(confi_services, services_order, "x13")
head(residuals_mat)
residuals_mat <- window(residuals_mat, start = c(2004, 1))

start_year<-2004

results_all      <- list()   # accuracy metrics
reco_series_all  <- list()   # reconciled SA series
reco_resid_all   <- list()   # reconciled residuals

colnames(reco_fc)
reco_fc
setdiff(services_order, colnames(confi_services))
summary(confi_services)

services_order
for (sa in sa_methods) {
  base_sa <- get_sa_matrix(confi_services, services_order, sa)
  base_sa <- as.matrix(base_sa)
  residuals_mat <- get_res_matrix(confi_services, services_order, sa)
  residuals_mat <-as.matrix(residuals_mat)
  residuals_mat <- window(residuals_mat, start = c(2004, 1))
  
  sa_actuals <- base_sa + residuals_mat
  
  
  for (reco in reco_methods) {
    
    key <- paste(sa, reco, sep = "_")
    
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
    reco_fc <- as.matrix(reco_fc)  # ensure it's a matrix
    
    reco_resid <- sa_actuals - reco_fc
    
    metrics <- lapply(services_order, function(v) {
      actual <- as.numeric(results_services[[v]]$original)
      pred   <- reco_fc[, v]
      rmse_val <- sqrt(mean((actual - pred)^2, na.rm = TRUE))
      mae_val  <- mean(abs(actual - pred), na.rm = TRUE)
      bias_val <- mean(actual - pred, na.rm = TRUE) 
      c(RMSE = rmse_val, MAE = mae_val, Bias = bias_val)
    })
    
    df <- do.call(rbind, metrics)
    df <- data.frame(
      Series = services_order,
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

agg_mat

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
actuals   <- as.numeric(results_services[[check_series]]$original)
residuals <- actuals - best_reco

# Plot ACF
acf(residuals, main = paste("Residuals ACF for", check_series))

lapply(confi_services, function(x) head(x$original))
head(confi_services$Confidence$original)

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


library(dplyr)
library(tidyr)

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

library(xtable)

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


all_plots_list <- lapply(services_order, function(v) {
  data.frame(
    Time = as.vector(time(results_services[[v]]$original)),
    Actual = as.numeric(results_services[[v]]$original),
    Reconciled = as.numeric(reco_series_all[, v]$str_ols),
    Series = v
  )
})

head(reco_series_all)




all_plots_list <- lapply(services_order, function(v) {
  data.frame(
    Time = as.vector(time(results_services[[v]]$original)),
    Actual = as.numeric(results_services[[v]]$original),
    Reconciled = as.numeric(results_services[[v]]$adjusted_str),
    Series = v
  )
})

target_key <- "x13_ols"

all_plots_list <- lapply(services_order, function(v) {
  # Get the reconciled matrix for X13_OLS
  reconciled_matrix <- reco_series_all[[target_key]]
  
  data.frame(
    # Time remains the same
    Time = as.vector(time(results_services[[v]]$original)),
    
    # Original data
    Actual = as.numeric(results_services[[v]]$original),
    
    SA_adjusted = as.numeric(results_services[[v]]$adjusted_x13),
    
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
  facet_wrap(~ Series, scales = "free_y") + 
  theme_bw() +
  labs(title = "Comparison: Original vs. STR-SA vs. Reconciled (OLS)",
       subtitle = "Visualizing the impact of reconciliation on seasonally adjusted data") +
  theme(legend.position = "bottom")




reco_mint<- csrec(
  base = base_sa,    # Forecasts to reconcile
  agg_mat = agg_mat,  
  res = residuals_mat,         # Residuals
  method = "mint",
  covariance = "shr"
)

print(head(reco_mint))


after_services <- test_seasonality(data = reco_mint, variables = services_order, start_year = 2004)


reco_mint <- as.data.frame(reco_mint)


all_plots_list <- lapply(services_order, function(v) {
  
  data.frame(
    # Time remains the same
    Time = as.vector(time(results_services[[v]]$original)),
    
    # Original data
    Actual = as.numeric(results_services[[v]]$original),
    
    SA_adjusted = as.numeric(results_services[[v]]$adjusted_str),
    
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