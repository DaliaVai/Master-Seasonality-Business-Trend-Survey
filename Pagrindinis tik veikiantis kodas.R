install.packages("Metrics")
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
library(MVN)
library(lmtest)       # For Durbin-Watson test
library(moments)      # For skewness and kurtosis
library(stats)  
library(FoReco)  
library(xtable)
library(fBasics)   # Doornik–Hansen
library(tidyr)

# Load data
library(readxl)
data<- read_excel("R/magistras/data.xlsx")
View(data)
data2<- read_excel("R/magistras/services.xlsx")
View(data2)
data2 <- data2[data2$Year > 2003 | (data2$Year == 2004 & data2$Month >= 1), ] #data sets starts in the middle of 2003 but has NA in the beginning

data3<- read_excel("R/magistras/retail.xlsx")
View(data3)

data4<- read_excel("R/magistras/industry.xlsx")
View(data4)


data_miau <- data[, colSums(is.na(data)) == 0]
construction<- setdiff(names(data_miau), c("Year", "Month", "timetogether"))
construction
start_year<-2013

data_miau2 <- data2[, colSums(is.na(data2)) == 0]
services<- setdiff(names(data_miau2), c("Year", "Month", "Laikotarpis"))
services
start_year_services<-2004

data_miau6 <- data[, colSums(is.na(data4)) == 0]
industry<- setdiff(names(data4), c("Year", "Month", "Laikotarpis"))
industry
start_year<-2004


retail<- setdiff(names(data3), c("Year", "Month", "Laikotarpis"))
start_year_retail<-2004

initial_construction <- test_seasonality_to_latex(data = data, variables = construction, start_year = 2013)

initial_services <- test_seasonality(data = data2, variables = services, start_year = 2004)

initial_retail <- test_seasonality(data = data3, variables = retail, start_year = 2004)

initial_industry <- test_seasonality_to_latex(data = data4, variables = industry, start_year = 2004)

results_retail <- adjust_seasonality(data3, retail, start_year_retail)
results_services <- adjust_seasonality(data2, services, start_year_services)
results_construction <- adjust_seasonality(data, construction, 2013)
results_industry <- adjust_seasonality(data4, Industry_Confidence, 2004)


#checking for NA
na_columns <- data[, colSums(is.na(data)) > 0]

print(na_columns)

#Our main variables
komp_Demand <- c("Demand_will_increase", "Demand_will_stay_the_same", "Demand_will_decrease", "Demand_balance")

komp_Finance <- c("Finances_better", "Finances_unchanged", "Finances_worse", "Finance_balance")

komp_Prices <- c("Prices_will_increase", "Prices_will_stay_the_same", "Prices_will_decrease", "Price_balance")

komp_Evaluation <- c("Demand_assessment_too_high", "Demand_assessment_sufficient", "Demand_assessment_too_low", "Demand_assessment_balance")

komp_Volume <- c("Construction_volume_increased", "Construction_volume_unchanged", "Construction_volume_decreased", "Construction_volume_balance")

Construction_Confidence <- c("Confidence", "Demand_assessment_balance", "Demand_assessment_too_high", "Demand_assessment_too_low", "Employees_will_increase", "Employees_will_decrease", "Employee_balance")



Retail_Confidence<- c("Confidence", "Business_Situation_Past_balance", "Business_Situation_Past_better", "Business_Situation_Past_Worse", "Stock_balance", "Stock_too_high", "Stock_too_small", "Business_Situation_Future_better", 
"Business_Situation_Future_worse", "Business_Situation_Future_better")



var<- "Per daug_paklausos_vertinimas"
ts_data <- ts(data[[var]], start = c(start_year, 1), frequency = 12)
ts_data


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
      
      # STL
      stl_decomp <- stl(ts_data, s.window = "periodic")
      adjusted_stl <- ts_data - stl_decomp$time.series[, "seasonal"]
      res_stl <- stl_decomp$time.series[, "remainder"]
      
      # STR decomposition
      str_decomp <- AutoSTR(ts_data)
      trend_component <- str_decomp$output$predictors[[1]]$data
      random_component <- str_decomp$output$random$data
      adjusted_str <- trend_component + random_component
      adjusted_str <- ts(adjusted_str, start = c(start_year, 1), frequency = 12)
      res_str <- str_decomp$output$random$data
    } else {
      # Skip seasonal adjustment, just return original series as "adjusted"
      adjusted_x13 <- ts_data
      adjusted_stl <- ts_data
      adjusted_str <- ts_data
      
      res_x13 <- ts_data * 0  # zero residuals
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




res2 <- adjust_seasonality(data, komp_Confidence, start_year)

summary(res1)
summary(res2)

names(res1) <- c("Better","Not changed" , "Worse", "Balance")
names(res2) <- c("Confidence","Balance1" , "Better1", "Worse1", "Better2", "Worse2", "Balance2")


#Reconcilation Funcion

#Forecasts
base_forecasts <- cbind(
  "Balance" = res1$Balance$adjusted_x13,
  "Better" = res1$Better$adjusted_x13,
  "Worse" = res1$Worse$adjusted_x13
)

base_forecasts2 <- cbind(
  "Confidence" = res2$Confidence$adjusted_x13,
  "Balance1" =res2$Balance1$adjusted_x13,
  "Balance2" =res2$Balance2$adjusted_x13,
  "Better1" = ,
  "Worse1" = res2$Worse1$adjusted_x13,
  "Better2" = res2$Better2$adjusted_x13,
  "Worse2" = res2$Worse2$adjusted_x13
)
print(head(base_forecasts))


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


Retail_Confidence<- c("Confidence", "Business_Situation_Past_balance", "Business_Situation_Past_better", "Business_Situation_Past_Worse", "Stock_balance", "Stock_too_high", "Stock_too_small", "Business_Situation_Future_balance", 
                      "Business_Situation_Future_Worse", "Business_Situation_Future_better")

existing_vars <- intersect(Retail_Confidence, names(results_retail))

Construction_Confidence <- c("Confidence", "Demand_assessment_balance", "Demand_assessment_too_high", "Demand_assessment_too_low", "Employees_increase", "Employees_decrease", "Employee_balance")
existing_vars <- intersect(Construction_Confidence, names(results_construction))
existing_vars

confi_construction <- results_construction[existing_vars]
summary(confi_construction)
names(confi_construction) <- c("Confidence","Balance1" , "Better1", "Worse1", "Better2", "Worse2", "Balance2" )

cons_order <- c(
  "Confidence",
  "Demand_assessment_balance", "Employee_balance",
  "Demand_assessment_too_high", "Demand_assessment_too_low",
  "Employees_increase", "Employees_decrease"
)

confi_construction <- confi_construction[cons_order]

confi_retail <- results_retail[existing_vars]
summary(confi_retail)
names(confidencee_retail_filtered) <- c("Confidence","Balance1" , "Better1", "Worse1", "Balance2", "Better2", "Worse2", "Balance3", "Better3", "Worse3" )
retail_order <- c(
  "Confidence",
  "Business_Situation_Past_balance", "Stock_balance", "Business_Situation_Future_balance",
  "Business_Situation_Past_better", "Business_Situation_Past_Worse",
  "Stock_too_high", "Stock_too_small",
  "Business_Situation_Future_Worse", "Business_Situation_Future_better"
)

confi_retail <- confi_retail[retail_order]

initial_retail <- test_seasonality(data = results_retail, variables = results_retail, start_year = 2004)

method <- "x13"

# Forecasts matrix for Retail
forecasts <- do.call(cbind, lapply(confi_construction, function(x) x[[paste0("adjusted_", method)]]))
head(forecasts)

# Residuals matrix for Retail
residuals_mat <- do.call(cbind, lapply(confi_construction, function(x) x[[paste0("res_", method)]]))
residuals_mat <- window(residuals_mat, start = c(2013, 1))
head(residuals_mat)

names(results_retail)

#Residual matrix 
res_matrix <- cbind(
  "Balance" = res1$Balance$res_x13,
  "Better" = res1$Better$res_x13,
  "Worse" = res1$Worse$res_x13
)

res_matrix2 <- cbind(
  "Confidence" = res2$Confidence$res_x13,
  "Balance1" =res2$Balance1$res_x13,
  "Balance2" =res2$Balance2$res_x13,
  "Better1" = res2$Better1$res_x13,
  "Worse1" = res2$Worse1$res_x13,
  "Better2" = res2$Better2$res_x13,
  "Worse2" = res2$Worse2$res_x13
)

res_matrix2 <- window(res_matrix2, start = c(2013, 1))

head(res_matrix2)

#Aggregation matrix for Balance
#Balance = Better - Worse
agg_mat <- matrix(c( 1, -1), nrow = 1)
colnames(agg_mat) <- c("Better", "Worse")
agg_mat

#Aggregation matrix for Construction Confidence
agg_mat <- matrix(c(
  0.5, -0.5, 0.5, -0.5,  # Confidence = (Balance1 + Balance2)/2
  1, -1, 0, 0,    # Balance1 = Better1 + Worse1
  0, 0, 1, -1    # Balance2 = Better2 + Worse2
), nrow = 3, byrow = TRUE)

rownames(agg_mat) <- c("Confidence", "Demand_assessment_balance","Employee_balance")
colnames(agg_mat) <- c("Demand_assessment_too_high","Demand_assessment_too_low","Employees_increase","Employees_decrease")

agg_mat

#Aggregation matrix for Retail Confidence
agg_mat <- matrix(c(
  1/3, -1/3, -1/3, 1/3, 1/3, -1/3, # Confidence = (Balance1 + Balance2 + Balance3)/3
  1, -1, 0, 0, 0, 0,    # Balance1 = Better1 + Worse1
  0, 0, 1, -1, 0, 0,    # Balance2 = Better2 + Worse2
  0, 0, 0, 0, 1, -1    # Balance3 = Better3 + Worse3
), nrow = 4, byrow = TRUE)

rownames(agg_mat) <- c("Confidence", "Business_Situation_Past_balance","Stock_balance", "Business_Situation_Future_balance")
colnames(agg_mat) <- c("Business_Situation_Past_better","Business_Situation_Past_Worse","Stock_too_high","Stock_too_small","Business_Situation_Future_better","Business_Situation_Future_Worse")


#Bootom-Up veikia all good

base <- cbind(
  "Better" = res1$Better$adjusted_x13,
  "Worse" = res1$Worse$adjusted_x13
)
print(head(base))

reco_bu <- csbu(
  base = base,     
  agg_mat = agg_mat 
)

print(head(reco_bu))

#OLS Reconcilation

reco_ols <- csrec(
  base = forecasts,    
  agg_mat = agg_mat,  
  res = residuals_mat,         
  comb = "ols"              
)

print(head(reco_ols))

#MINT Reconcilation
reco_shr <- csrec(
  base = forecasts,   
  agg_mat = agg_mat,  
  res = residuals_mat ,         
  comb = "shr"              
)

print(head(reco_shr))

vars <- colnames(reco_ols)
colnames(reco_ols)
all(vars %in% colnames(reco_ols))

class(reco_shr)
str(reco_shr)
reco_ols <- as.data.frame(reco_ols)
head(reco_shr)

test_seasonality(data = reco_ols,
                 variables = vars,
                 start_year = start_year)

# Make SA actuals from your residuals + adjusted values
sa_actuals <- forecasts + residuals_mat
head(sa_actuals)

# Reconciled residuals (SA scale)
reco_ols <- as.matrix(reco_ols)
reco_residuals <- sa_actuals - reco_ols 
head(reco_residuals)

spectrum(reco_residuals[, "retail_forecasts.Confidence"])


Box.test(reco_residuals[, "retail_forecasts.Confidence"], type = "Ljung-Box")
cov(reco_residuals)

#Oracle shrunk covariance (Ando and Xiao, 2023).

reco_mint<- csrec(
  base = base_sa,    # Forecasts to reconcile
  agg_mat = agg_mat,  
  res = residuals_mat,         # Residuals
  method = "mint",
  covariance = "shr"
)

print(head(reco_mint))



#Radar data for Original and Adjusted Series
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
  radar_data_stl <- ts_data_by_year(results_retail[[var]]$adjusted_stl, start_year)
  radar_data_str <- ts_data_by_year(results_retail[[var]]$adjusted_str, start_year)
  
  # Max and min values for scaling
  max_val <- max(c(unlist(radar_data_original), unlist(radar_data_x13), 
                   unlist(radar_data_stl), unlist(radar_data_str)), na.rm = TRUE)
  min_val <- min(c(unlist(radar_data_original), unlist(radar_data_x13), 
                   unlist(radar_data_stl), unlist(radar_data_str)), na.rm = TRUE)
  
  radar_data_original <- rbind(rep(max_val, 12), rep(min_val, 12), radar_data_original)
  radar_data_x13 <- rbind(rep(max_val, 12), rep(min_val, 12), radar_data_x13)
  radar_data_stl <- rbind(rep(max_val, 12), rep(min_val, 12), radar_data_stl)
  radar_data_str <- rbind(rep(max_val, 12), rep(min_val, 12), radar_data_str)
  
  dev.new() 
  par(mfrow = c(1, 4), mar = c(2, 2, 2, 2))
  
  radarchart(radar_data_original, axistype = 0, pcol = rainbow(nrow(radar_data_original) - 2), plwd = 2)
  title(paste("Original Data for", var))  
  
  radarchart(radar_data_x13, axistype = 0, pcol = rainbow(nrow(radar_data_x13) - 2), plwd = 2)
  title(paste("X-13ARIMA-SEATS for", var))  
  
  radarchart(radar_data_stl, axistype = 0, pcol = rainbow(nrow(radar_data_stl) - 2), plwd = 2)
  title(paste("STL for", var))
  
  radarchart(radar_data_str, axistype = 0, pcol = rainbow(nrow(radar_data_str) - 2), plwd = 2)
  title(paste("STR for", var))  
}


#Outliers

#Boxplot by month
boxplot(results_retail$Business_Situation_Future_better$adjusted_stl ~ cycle(results_retail$Business_Situation_Future_better$adjusted_x13),
        xlab = "Month", ylab = "Value",
        main = "Monthly boxplots")

z <- scale(results_retail$Business_Situation_Future_better$original)
z <-which(abs(z) > 3)
decimal_to_month(time(results_retail$Business_Situation_Future_better$adjusted_x13)[z])

decimal_to_month <- function(x) {
  year <- floor(x)
  month <- round(12 * (x - year) + 1)
  paste0(year, "-", sprintf("%02d", month))
}



results_retail$Business_Situation_Future_better$adjusted_x13

#Simple plots
par(mfrow = c(1, 1))
plot(results_retail$Business_Situation_Future_better$original, col = "black", lwd = 2)
lines(results_retail$Business_Situation_Future_better$adjusted_x13, col = "red", lwd = 2)
lines(results_retail$Business_Situation_Future_better$adjusted_stl, col = "blue", lwd = 2)
lines(results_retail$Business_Situation_Future_better$adjusted_str, col = "green", lwd = 2)
legend("topright", legend = c("Original", "STL", "X-13 Arima-SEATS", "STR"), col = c("black", "blue", "red", "green"), lwd = 2)


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

lines(results_retail$Business_Situation_Future_better$adjusted_stl,
      col = "#0072B2", lwd = 2, lty = 2)

lines(results_retail$Business_Situation_Future_better$adjusted_str,
      col = "#009E73", lwd = 2, lty = 3)

legend("bottomleft",
       legend = c("Original", "X-13ARIMA-SEATS", "STL", "STR"),
       col = c("grey30", "#D55E00", "#0072B2", "#009E73"),
       lty = c(1, 1, 2, 3),
       lwd = 2,
       bty = "n")


plot(results_retail$Business_Situation_Future_better$original,
     type = "l", col = "grey70", lwd = 1,
     xlab = "Time", ylab = "Index value",
     main = "Comparison of Seasonal Adjustment Methods")

lines(results_retail$Business_Situation_Future_better$adjusted_x13,
      col = "red", lwd = 2)

lines(results_retail$Business_Situation_Future_better$adjusted_stl,
      col = "blue", lwd = 2, lty = 2)

lines(results_retail$Business_Situation_Future_better$adjusted_str,
      col = "darkgreen", lwd = 2, lty = 3)



diff_sa <- results_retail$Business_Situation_Future_better$adjusted_stl - results_retail$Business_Situation_Future_better$adjusted_str
max(diff_sa)
min(diff_sa)
plot(diff_sa, main = "Difference: STL – STR")
abline(h = 0, col = "red")

# Evaluation Functions

#Seasonality

reco_shr <- as.data.frame(reco_shr)
colnames(reco_shr) <- c("Balance", "Better", "Worse")

testams <- ts(reco_shr$Balance, start = c(start_year, 1), frequency = 12)

wo_test_result <- combined_test(y = testams) #Ollech and Webel's combined seasonality test
names(wo_test_result)
isSeasonal (ts_data, test = "combined", freq =12) # tas pats tik duoda atsakyma true ar false

kw_test<-(kw(testams, freq = 12)) 
kw_test

names(kw_test)
fried_result <- fried(testams, freq = 12) # Friedman's test for seasonality
print(fried_result)

qs(testams, freq = 12)


teststat <-ocsb(
  testams,
  method = "OLS",
  augmentations = c(3, 0),
  freq = 12,
  nrun = 200
)

check_residuals(teststat)

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

res1 <- test_seasonality(data, komp_Confidence, start_year)

#Residual Seasonality
sd(reco_residuals[, "retail_forecasts.Confidence"])
plot(reco_residuals[, "retail_forecasts.Confidence"], type = "l", main = "Residuals of Confidence after Reconciliation")
abline(h = 0, col = "red")

#Accuracy
name_map <- c(
  "Business_Situation_Past_balance" = "Balance1",
  "Business_Situation_Past_better" = "Better1",
  "Business_Situation_Past_Worse" = "Worse1",
  "Business_Situation_Future_balance" = "Balance2",
  "Business_Situation_Future_better" = "Better2",
  "Business_Situation_Future_Worse" = "Worse2",
  "Stock_balance" = "Balance3",
  "Stock_too_high" = "Better3",
  "Stock_too_small" = "Worse3",
  "Confidence" = "Confidence"
)

name_map <- c(
  "Demand_assessment_balance" = "Balance1",
  "Demand_assessment_too_high" = "Better1",
  "Demand_assessment_too_low" = "Worse1",
  "Employee_balance" = "Balance2",
  "Employees_increase" = "Better2",
  "Employees_decrease" = "Worse2",
  "Construction_Confidence" = "Confidence"
)


confi_construction <- c("Confidence", "Demand_assessment_balance", "Demand_assessment_too_high", "Demand_assessment_too_low", "Employee_balance", "Employees_increase", "Employees_decrease")

inv_map <- names(name_map)
names(inv_map) <- name_map

summary(confi_construction)
confi_construction

colnames(confi_construction) <- inv_map[colnames(confi_construction)]

head(confi_construction)

results <- data.frame(
  Series = colnames(reco_ols),
  RMSE = NA,
  MAE  = NA,
  MAPE = NA
)
rownames(results) <- NULL


rmse(results_retail$Business_Situation_Past_balance$original, reco_shr[, "Business_Situation_Past_balance"])

results<- data.frame(
  Series = colnames(reco_ols),
  RMSE_SA = NA,
  MAE_SA  = NA,
  MAPE_SA = NA,
  RMSE_Reco = NA,
  MAE_Reco  = NA,
  MAPE_Reco = NA
)
rownames(results) <- NULL

for (i in seq_along(colnames(reco_ols))) {
  var <- colnames(reco_ols)[i]
  
  # Extract numeric vectors
  actuals   <- as.numeric(results_construction[[var]]$original)
  adjusted  <- as.numeric(results_construction[[var]]$adjusted_stl)  # or adjusted_stl / adjusted_str
  forecasts <- as.numeric(reco_ols[, var])
  
  # Make lengths equal
  min_len <- min(length(actuals), length(adjusted), length(forecasts))
  actuals   <- actuals[1:min_len]
  adjusted  <- adjusted[1:min_len]
  forecasts <- forecasts[1:min_len]
  
  # Remove NAs
  valid_idx <- complete.cases(actuals, adjusted, forecasts)
  actuals   <- actuals[valid_idx]
  adjusted  <- adjusted[valid_idx]
  forecasts <- forecasts[valid_idx]
  
  # Compute metrics
  results$RMSE_SA[i]    <- rmse(actuals, adjusted)
  results$MAE_SA[i]     <- mae(actuals, adjusted)
  results$MAPE_SA[i]    <- mape(actuals, adjusted)
  
  results$RMSE_Reco[i]  <- rmse(actuals, forecasts)
  results$MAE_Reco[i]   <- mae(actuals, forecasts)
  results$MAPE_Reco[i]  <- mape(actuals, forecasts)
}

print(results)


reco_shr <- as.matrix(reco_shr)


#Sliding Spans??

#Revision History??

#Spectral Density




#Rougness




#Autocorrelation

adjustments <- sa_after_rec - sa_before_rec

# 2. Statistika apie korekcijas
summary_stats <- data.frame(
  Mean = mean(adjustments, na.rm = TRUE),
  SD = sd(adjustments, na.rm = TRUE),
  Min = min(adjustments, na.rm = TRUE),
  Max = max(adjustments, na.rm = TRUE)
)
print(summary_stats)

# 3. Histogram korekcijų pasiskirstymui
hist(adjustments,
     main = "Histogram of Reconciliation Adjustments",
     xlab = "Adjustment",
     col = "lightblue",
     breaks = 20)

# 4. Laiko eilutės grafikas (prieš / po reconciliation)
library(ggplot2)

df_plot <- data.frame(
  Time = time,
  Before = sa_before_rec,
  After = sa_after_rec,
  Adjustment = adjustments
)

# a) Prieš ir po reconciliation
ggplot(df_plot, aes(x = Time)) +
  geom_line(aes(y = Before, color = "Before Rec")) +
  geom_line(aes(y = After, color = "After Rec")) +
  labs(title = "Seasonally Adjusted Series: Before vs After Reconciliation",
       y = "Value", color = "Series") +
  theme_minimal()

# b) Korekcijų laiko eilutė
ggplot(df_plot, aes(x = Time, y = Adjustment)) +
  geom_line(color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Reconciliation Adjustments Over Time",
       y = "Adjustment") +
  theme_minimal()




results <- data.frame(
  Series = colnames(reco_shr),
  RMSE = NA,
  MAE  = NA,
  MAPE = NA
)

rm(rmse)

summary(res2)

for (var in names(res2)) {
  original_series[[var]] <- res2[[var]]$original
}




for (var in names(res2)) {
  results$RMSE[var] <- rmse(res2[[var]]$original, reco_shr[var])
  results$MAE[var]  <- mae(res2[[var]]$original, reco_shr[var])
  results$MAPE[var] <- mape(res2[[var]]$original, reco_shr[var])
}

# View results
print(results)


for (i in seq_along(res2)) {
  var <- names(res2)[i]
  
  # Ensure reco_shr[[var]] is numeric
  fcast <- as.numeric(reco_shr[[var]])
  obs   <- as.numeric(res2[[var]]$original)
  
  results$RMSE[i] <- rmse(obs, fcast)
  results$MAE[i]  <- mae(obs, fcast)
  results$MAPE[i] <- mape(obs, fcast)
}

print(results)
length(res2[[var]]$original)
length(reco_shr[[var]])



accuracy_base <- accuracy(
  base_fc,
  data_sa,
  measures = list(
    RMSE = RMSE,
    MAE  = MAE,
    MAPE = MAPE
  )
)

accuracy_recon <- accuracy(
  recon_fc,
  data_sa,
  measures = list(
    RMSE = RMSE,
    MAE  = MAE,
    MAPE = MAPE
  )
)

accuracy_comparison <- accuracy_base %>%
  select(.model, RMSE, MAE, MAPE) %>%
  rename_with(~ paste0(., "_base"), - .model) %>%
  left_join(
    accuracy_recon %>%
      select(.model, RMSE, MAE, MAPE) %>%
      rename_with(~ paste0(., "_recon"), - .model),
    by = ".model"
  )

accuracy_comparison


e_base  <- residuals(base_fc)
e_recon <- residuals(recon_fc)

dm.test(e_base, e_recon, alternative = "two.sided", h = 1, power = 2)






for (i in seq_along(res2)) {
  var <- names(res2)[i]
  
  obs <- as.numeric(res2[[var]]$original)
  
  # Column from matrix/data frame
  if (!var %in% colnames(reco_shr)) {
    warning(paste("Forecast for", var, "not found. Skipping."))
    next
  }
  fcast <- as.numeric(reco_shr[, var])
  
  # Trim to shortest length
  n <- min(length(obs), length(fcast))
  obs <- obs[1:n]
  fcast <- fcast[1:n]
  
  # Compute metrics
  results$RMSE[i] <- rmse(obs, fcast)
  results$MAE[i]  <- mae(obs, fcast)
  results$MAPE[i] <- mape(obs, fcast)
}

print(results)


#For Latex

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
test_seasonality_to_latex(data3, retail, start_year = 2004)
test_seasonality_to_latex(data2, services, start_year = start_year_services)

