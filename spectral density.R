
spectral_seasonality <- function(x, freq = 12, plot = FALSE) {
  x <- na.omit(as.numeric(x))
  
  sp <- spectrum(x, plot = plot)  # base R spectral density
  
  # frequencies and spectral density
  f  <- sp$freq
  S  <- sp$spec
  
  # seasonal frequencies for monthly: 1/12, 2/12, ..., 6/12
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
#spec_metrics$Method    <- gsub("^stl_", "STL_", spec_metrics$Method)
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


res_conf_x13 <- clean_reco_resid_all[["x13_none"]][, "sa_actuals.base_sa.Confidence"]
spectral_seasonality(res_conf_x13, freq = 12, plot = TRUE)



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

names(clean_reco_resid_all)

par(mfrow = c(1, 1))  

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



for (v in colnames(clean_reco_resid_all[["x13_mint_shr"]])) {
  
  res <- clean_reco_resid_all[["x13_mint_shr"]][, v]
  
  pretty_v <- clean_name(v)
  
  title <- paste("X-13 MINT Shr residual spectrum –", pretty_v)
  
  nice_spectrum_plot(res, main = title)
}
