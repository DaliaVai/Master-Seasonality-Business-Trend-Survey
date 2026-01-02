library(Metrics)
library(stR)
library(fmsb)
library(seasonal)
library(forecast)  
library(readxl)
library(reshape2)
library(dplyr)
library(zoo)
library(lubridate)
library(ggplot2)
library(seastests)
library(seasonalview)
library(lmtest)      
library(stats)  
library(FoReco)  
library(xtable)
library(tidyr)
library(readxl)


sa_methods  <- c("x13", "str")
reco_methods <- c("none", "ols", "wls", "mint_shr", "oasd")

#Function for Seasonality test

test_seasonality<- function(data, variables, start_year) {
  
  for (var in variables) {
    ts_data <- ts(data[[var]], start = c(start_year, 1), frequency = 12)
    
    cat( "\n-----------------\n", "\nVariable:", var)
    
    # Ollech & Webel combined test
    wo_test_result <- combined_test(y = ts_data)
    print(wo_test_result)
    
    # isSeasonal test
    print(isSeasonal(ts_data, test = "combined", freq = 12))
    
    # Kruskal-Wallis test
    kw_test <- kw(ts_data, freq = 12)
    print(kw_test)
    
    # Friedman's test
    fried_result <- fried(ts_data, freq = 12) 
    print(fried_result)
    
  }
}

#Seasonal Adjustment Function

adjust_seasonality <- function(data, variables, start_year) {
  results <- list()
  
  for (var in variables) {
    ts_data <- ts(data[[var]], start = c(start_year, 1), frequency = 12)
    
    # If series are not seasonal skip the adjustment
    seasonal_flag <- isSeasonal(ts_data, test = "combined", freq = 12)
    
    if (seasonal_flag) {
      # X-13ARIMA-SEATS
      model_x13 <- seas(ts_data)
      res_x13 <- residuals(model_x13)
      adjusted_x13 <- final(model_x13)
      
      # STR decomposition
      str_decomp <- AutoSTR(ts_data)
      trend_component <- str_decomp$output$predictors[[1]]$data
      random_component <- str_decomp$output$random$data
      adjusted_str <- trend_component + random_component
      adjusted_str <- ts(adjusted_str, start = c(start_year, 1), frequency = 12)
      res_str <- str_decomp$output$random$data
    } else {
      adjusted_x13 <- ts_data
      adjusted_stl <- ts_data
      adjusted_str <- ts_data
      
      res_x13 <- ts_data * 0  # zero residuals if no adustment needed
      res_stl <- ts_data * 0
      res_str <- ts_data * 0
    }
    
    results[[var]] <- list(
      original = ts_data,
      adjusted_x13 = adjusted_x13,
      adjusted_stl = adjusted_stl,
      adjusted_str = adjusted_str,
      res_x13 = res_x13,
      res_stl = res_stl,
      res_str = res_str
    )
  }
  
  return(results)
}

#Reconciliation methods -> here more for information easier to follow or apply in the files in accordance to sectors
#Example on construction sector, variables are defined in for construction r file
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

results_all      <- list()   # For RMSE and MAE
reco_series_all  <- list()   # reconciled SA series
reco_resid_all   <- list()   # reconciled residuals


for (sa in sa_methods) {
  base_sa <- get_sa_matrix(confi_construction, cons_order, sa)
  base_sa <- as.matrix(base_sa)
  residuals_mat <- get_res_matrix(confi_construction, cons_order, sa)
  residuals_mat <- as.matrix(residuals_mat)
  residuals_mat <- window(residuals_mat, start = c(start_year, 1))
  
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
    
    # residuals after reconciliation
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

reconsiled_series <- do.call(rbind, reco_series_all)



#Radar plot for Original and Adjusted Series
# Prepare radar chart data
ts_data_by_year <- function(series, start_year) {
  num_months <- length(series)
  num_years <- num_months %/% 12
  series <- series[1:(num_years * 12)]  # Ensure only full years are used
  ts_matrix <- matrix(series, ncol = 12, byrow = TRUE)  # Monthly data in rows
  rownames(ts_matrix) <- paste0("Year_", start_year:(start_year + nrow(ts_matrix) - 1))
  colnames(ts_matrix) <- month.abb  # Month abbreviations as column names
  return(as.data.frame(ts_matrix))
}

# Plot radar charts for original, X-13ARIMA-SEATS, STL, and STR
start_year<-2004
for (var in names(results_retail)) {
  
  radar_data_original <- ts_data_by_year(results_retail[[var]]$original, start_year)
  radar_data_x13 <- ts_data_by_year(results_retail[[var]]$adjusted_x13, start_year)
  radar_data_str <- ts_data_by_year(results_retail[[var]]$adjusted_str, start_year)
  
  # Max and min values for scaling
  max_val <- max(c(unlist(radar_data_original), unlist(radar_data_x13), 
                   unlist(radar_data_stl), unlist(radar_data_str)), na.rm = TRUE)
  min_val <- min(c(unlist(radar_data_original), unlist(radar_data_x13), 
                   unlist(radar_data_stl), unlist(radar_data_str)), na.rm = TRUE)
  
  radar_data_original <- rbind(rep(max_val, 12), rep(min_val, 12), radar_data_original)
  radar_data_x13 <- rbind(rep(max_val, 12), rep(min_val, 12), radar_data_x13)
  radar_data_str <- rbind(rep(max_val, 12), rep(min_val, 12), radar_data_str)
  
  dev.new() 
  par(mfrow = c(1, 4), mar = c(2, 2, 2, 2))
  
  radarchart(radar_data_original, axistype = 0, pcol = rainbow(nrow(radar_data_original) - 2), plwd = 2)
  title(paste("Original Data for", var))  
  
  radarchart(radar_data_x13, axistype = 0, pcol = rainbow(nrow(radar_data_x13) - 2), plwd = 2)
  title(paste("X-13ARIMA-SEATS for", var))  
  
  radarchart(radar_data_str, axistype = 0, pcol = rainbow(nrow(radar_data_str) - 2), plwd = 2)
  title(paste("STR for", var))  
}


#Simple plots
par(mfrow = c(1, 1), mar = c(4, 4, 3, 2))

plot(results_retail$Business_Situation_Future_better$original,
     type = "l",
     col = "grey30",
     lwd = 2,
     xlab = "Time",
     ylab = "Index value",
     main = "Business Situation – Original and Seasonally Adjusted Series")

lines(results_retail$Business_Situation_Future_better$adjusted_x13,
      col = "#D55E00", lwd = 2)

lines(results_retail$Business_Situation_Future_better$adjusted_str,
      col = "#009E73", lwd = 2, lty = 3)

lines(grafikui,
      col = "#0072B2", lwd = 2, lty = 3)

legend("bottomleft",
       legend = c("Original", "X-13ARIMA-SEATS", "STR"),
       col = c("grey30", "#D55E00", "#0072B2", "#009E73"),
       lty = c(1, 1, 2, 3),
       lwd = 2,
       bty = "n")


head(grafikui)
grafikui <- ts(reconsiled_series$x13_none.Business_Situation_Future_better_x13_none, start=(2004), frequency = 12)

##0072B2" nice blue collor mėlyna spalva

#Functions for evaluations 
# 1. Coherence
check_coherence <- function(reco_fc, agg_mat, tol = 1e-8) {
  
  reco_fc <- as.matrix(reco_fc)
  
  all_names    <- rownames(agg_mat)
  bottom_names <- colnames(agg_mat)
  
  # Bottom-level reconciled series 
  b_hat <- reco_fc[, bottom_names, drop = FALSE]
  head(b_hat)
  
  # Calculated all-series from bottom using S:
  y_hat_implied <- b_hat %*% t(agg_mat) 
  head(y_hat_implied)
  
  # Compare to reconciled "all-series" columns (aligned to agg_mat row order)
  y_hat_all <- reco_fc[, all_names, drop = FALSE]
  head(y_hat_all)
  
  E <- y_hat_all - y_hat_implied
  head(E)
  
  time_max <- apply(abs(E), 1, max, na.rm = TRUE)
  node_max <- apply(abs(E), 2, max, na.rm = TRUE)
  
  list(
    max_abs_incoherence = max(abs(E), na.rm = TRUE),
    pct_time_incoherent = round(100 * mean(time_max > tol, na.rm = TRUE), 2),
    worst_time_max      = max(time_max, na.rm = TRUE),
    worst_node          = names(which.max(node_max)),
    worst_node_max      = max(node_max, na.rm = TRUE),
    residuals_matrix    = E
  )
}

coherence_summary <- list()

for (key in names(reco_series_all)) {
  
  reco_fc <- reco_series_all[[key]]
  
  ch <- check_coherence(reco_fc = reco_series_all[[key]], agg_mat = agg_mat, tol = 1e-8)
  
  parts <- strsplit(key, "_")[[1]]
  sa_part   <- parts[1]
  reco_part <- paste(parts[-1], collapse = "_")  
  
  coherence_summary[[key]] <- data.frame(
    Key = key,
    SA  = sa_part,
    Reconciliation = reco_part,
    max_abs_incoherence = ch$max_abs_incoherence,
    pct_time_incoherent = ch$pct_time_incoherent,
    worst_node          = ch$worst_node,
    worst_node_max      = ch$worst_node_max,
    stringsAsFactors = FALSE
  )
  
}

coherence_summary_df <- do.call(rbind, coherence_summary)
coherence_summary_df<-coherence_summary_df[4:5]
coherence_summary_df


# 2. Residual diagnostics

is_zero_series <- function(x) {
  all(is.na(x) | abs(x) < 1e-10)
}

tidy_list <- list()

#To have only real residuals, those series, which were not adjusted are dropped
for (key in names(reco_resid_all)) {
  
  R <- reco_resid_all[[key]]        # matrix
  good_cols <- !apply(R, 2, is_zero_series)
  
  clean_reco_resid_all[[key]] <- R[, good_cols, drop = FALSE]
}

resid_metrics <- data.frame(
  Method = character(),
  Residuals = character(),
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
    
    shapiro_p <- shapiro.test(res)$p.value
    lb_p <- Box.test(res, lag=12, type="Ljung-Box")$p.value
    qs_p <- qs(res)$Pval
    fried_result <- fried(res, freq = 12) 
    friedman_p <- fried_result$Pval
    kw_test <- kw(res, freq = 12)
    kw_p <- kw_test$Pval
    dw <- dwtest(res ~ 1)
    
    resid_metrics <- rbind(resid_metrics, data.frame(
      Method = key,
      Residuals = v,
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


# 3. Spectral Density

spectral_seasonality <- function(x, freq = 12, plot = FALSE) {
  x <- na.omit(as.numeric(x))
  
  sp <- spectrum(x, plot = plot)  
  
  f  <- sp$freq #frequencies
  S  <- sp$spec #spectral density
  
  # seasonal frequencies for monthly: 
  seasonal_freqs <- (1:(freq/2)) / freq
  
  # find nearest spectral frequency indices
  idx <- sapply(seasonal_freqs, function(sf) which.min(abs(f - sf)))
  
  seasonal_power <- sum(S[idx])
  total_power    <- sum(S)
  ratio <- seasonal_power / total_power
  
  list(
    seasonal_power_ratio = ratio,
    seasonal_power = seasonal_power,
    total_power = total_power,
    seasonal_indices = idx,
    seasonal_freqs = seasonal_freqs
  )
}

for (key in names(clean_reco_resid_all)) {
  R <- clean_reco_resid_all[[key]]
  
  for (v in colnames(R)) {
    res <- R[, v]
    
    spectral_seasonality(res, freq = 12, plot = TRUE)
    
  }
}


spec_metrics <- data.frame(
  Method = character(),
  Residuals = character(),
  Seasonal_power_ratio = numeric(),
  Seasonal_power = numeric(),
  Total_power = numeric(),
  stringsAsFactors = FALSE
)

for (key in names(clean_reco_resid_all)) {
  R <- clean_reco_resid_all[[key]]
  
  for (v in colnames(R)) {
    res <- R[, v]
    
    sp_res <- spectral_seasonality(res, freq = 12, plot = FALSE)
    
    spec_metrics <- rbind(
      spec_metrics,
      data.frame(
        Method = key,
        Residuals = v,
        Seasonal_power_ratio = sp_res$seasonal_power_ratio,
        Seasonal_power = sp_res$seasonal_power,
        Total_power = sp_res$total_power,
        stringsAsFactors = FALSE
      )
    )
  }
}

spec_metrics$Method    <- gsub("^x13_", "X13_", spec_metrics$Method)
spec_metrics$Method    <- gsub("^str_", "STR_", spec_metrics$Method)
spec_metrics$Residuals <- gsub("^sa_actuals\\.base_sa\\.|^base_sa\\.", "", spec_metrics$Residuals)


spec_summary <- spec_metrics |>
  filter(grepl("Confidence$", Residuals)) |>
  group_by(Method) |>
  summarise(
    mean_ratio = mean(Seasonal_power_ratio, na.rm = TRUE),
    median_ratio = median(Seasonal_power_ratio, na.rm = TRUE)
  ) |>
  arrange(mean_ratio)

print(spec_summary, n=24)



nice_spectrum_plot <- function(x, freq = 12, main = "Spectral Density") {
  x <- na.omit(as.numeric(x))
  
  sp <- spectrum(x, plot = FALSE)
  
  f <- sp$freq
  S <- sp$spec
  
  plot(f, S,
       type = "l",
       lwd = 2,
       xlab = "Frequency",
       ylab = "Spectral density",
       main = main)
  
  # add seasonal frequencies
  seasonal_freqs <- (1:(freq/2)) / freq
  abline(v = seasonal_freqs, col = "red", lty = 2)
  
  legend("topright",
         legend = c("Spectrum", "Seasonal frequencies"),
         col = c("black", "red"),
         lty = c(1, 2),
         bty = "n")
}

#Example of usage

nice_spectrum_plot(clean_reco_resid_all[["x13_none"]][, "base_sa.Confidence"], main = "X-13 residual spectrum – Confidence")

clean_name <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  
  x <- gsub("^sa_actuals\\.base_sa\\.", "", x)
  x <- gsub("^base_sa\\.", "", x)
  x <- gsub("^sa_", "", x)
  
  return(x)
}

par(mfrow = c(4, 2))   # 2 rows, 3 columns
par(mar = c(4, 4, 3, 1))  # nicer margins



for (v in colnames(clean_reco_resid_all[["x13_ols"]])) {
  
  res <- clean_reco_resid_all[["x13_ols"]][, v]
  
  pretty_v <- clean_name(v)
  
  title <- paste("X13 OLS residual spectrum –", pretty_v)
  
  nice_spectrum_plot(res, main = title)
}



#For Latex outputs

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
  mutate(Row = paste(SA, Series)) %>%  
  select(Row, everything(), -SA, -Series) 

#RMSE and MAE
rmse_xtable <- xtable(rmse_table, 
                      caption = "RMSE for different seasonal adjustment and reconciliation methods",
                      label = "tab:rmse_table")
print(rmse_xtable, include.rownames = FALSE, booktabs = TRUE,sanitize.colnames.function = identity)

mae_table <- xtable(mae_table, 
                    caption = "MAE for different seasonal adjustment and reconciliation methods",
                    label = "tab:rmse_table")
print(mae_table, include.rownames = FALSE, booktabs = TRUE)


#Coherence to Latex

coherence_to_latex(coherence_summary_df)
coherence_to_latex <- function(coherence_df,
                               caption = "Coherence of the Reconciled Variables",
                               label   = "tab:coherence",
                               threshold = 0.001,   # max_abs < threshold -> print "<threshold"
                               thres     = 0.01) {  # pct < thres -> print "<thres"
  
  df <- coherence_df
  
  max_abs_num  <- as.numeric(as.character(df$max_abs_incoherence))
  pct_time_num <- as.numeric(as.character(df$pct_time_incoherent))  
  
  df$Method <- toupper(gsub("_", " ", rownames(df)))
  df <- df[, c("Method", setdiff(names(df), "Method"))]
  
  df$max_abs_incoherence <- ifelse(
    abs(max_abs_num) < threshold,
    paste0("<", format(threshold, digits = 3, nsmall = 3)),
    format(round(max_abs_num, 3), nsmall = 3)
  )
  
  df$pct_time_incoherent <- ifelse(
    abs(pct_time_num) < thres,
    paste0("<", format(thres, digits = 2, nsmall = 2)),
    format(round(pct_time_num, 2), nsmall = 2)
  )
  
  xt <- xtable::xtable(df, caption = caption, label = label)
  
  print(
    xt,
    include.rownames = FALSE,
    booktabs = TRUE,
    sanitize.text.function = identity
  )
  
  invisible(xt)
}

#Test Seasonality to Latex


test_seasonality_to_latex <- function(data, variables, start_year) {
  
  results_list <- list()
  
  for (var in variables) {
    if(!is.numeric(data[[var]])) next
    
    ts_data <- ts(data[[var]], start = c(start_year, 1), frequency = 12)
    
    
    is_seasonal <- isSeasonal(ts_data, test = "combined", freq = 12)
    kw_p <- as.numeric(kw(ts_data, freq = 12)[2])
    fried_p <- as.numeric(fried(ts_data, freq = 12)[2])
    
    mixed <- (!is_seasonal) && (kw_p < 0.05 || fried_p < 0.05)
    
    results_list[[var]] <- data.frame(
      Variable = gsub("_", " ", var), 
      WO_Test = ifelse(is_seasonal, "Seasonal", "Non-Seasonal"),
      KW_p = format.pval(kw_p, digits = 3, eps = 0.001),
      Fried_p = format.pval(fried_p, digits = 3, eps = 0.001),
      Status = case_when(
        is_seasonal ~ "\\checkmark",
        mixed ~ "\\textit{Mixed*}", # Mark mixed results
        TRUE ~ "---"
      ),
      stringsAsFactors = FALSE
    )
  }
  
  final_table <- do.call(rbind, results_list)
  
  
  print(xtable(final_table, 
               caption = "Retail Sector. Initial Seasonality Test Results",
               align = "llcccc"), 
        include.rownames = FALSE, 
        sanitize.text.function = function(x) x,
        booktabs = TRUE) 
}

#Residuals to Latex

resid_clean <- data.frame()

resid_clean <- resid_metrics

resid_clean$Method <- gsub("^x13_", "", resid_clean$Method)
resid_clean$Method <- gsub("^str_", "", resid_clean$Method)

# Uppercase 
resid_clean$Method <- toupper(resid_clean$Method)

# Remove prefixes 
resid_clean$Residuals <- gsub("^sa_actuals\\.base_sa\\.", "", resid_clean$Residuals)
resid_clean$Residuals <- gsub("^base_sa\\.", "", resid_clean$Residuals)


keep_cols <- c(
  "Method",
  "Residuals",
  "Shapiro_p",
  "LjungBox_p",
  "DurbinWatson_stat",
  "DurbinWatson_p",
  "QS_p",
  "Friedman_p"
)

# 2. Split by method family
resid_x13 <- subset(resid_clean, grepl("^X13", resid_metrics$Method, ignore.case = TRUE))[ , keep_cols]
resid_str <- subset(resid_clean, grepl("^STR", resid_metrics$Method, ignore.case = TRUE))[ , keep_cols]

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


