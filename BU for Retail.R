compute_BU <- function(data, vars, sa_method, agg_mat) {
  
  # bottom-level variables
  bottom_vars <- colnames(agg_mat)
  
  base_bottom <- do.call(cbind, lapply(data[bottom_vars], function(x) {
    x[[paste0("adjusted_", sa_method)]]
  }))
  
  csbu(
    base    = base_bottom,
    agg_mat = agg_mat
  )
}


reco_mint<- csrec(
  base = base_sa,    # Forecasts to reconcile
  agg_mat = agg_mat,  
  res = residuals_mat,         # Residuals
  method = "mint",
  covariance = "shr"
)

print(head(reco_mint))


bu_fc <- compute_BU(confi_retail, retail_order, sa, agg_mat)
agg_mat



S_BU <- rbind(
  Confidence = c( 1/3, -1/3,  1/3, -1/3,  1/3, -1/3),
  
  Business_Situation_Past_balance =
    c( 1,   -1,    0,    0,    0,    0),
  
  Stock_balance =
    c( 0,    0,    1,   -1,    0,    0),
  
  Business_Situation_Future_balance =
    c( 0,    0,    0,    0,    1,   -1),
  
  Business_Situation_Past_better  = c(1,0,0,0,0,0),
  Business_Situation_Past_Worse   = c(0,1,0,0,0,0),
  Stock_too_high                  = c(0,0,1,0,0,0),
  Stock_too_small                 = c(0,0,0,1,0,0),
  Business_Situation_Future_better= c(0,0,0,0,1,0),
  Business_Situation_Future_Worse = c(0,0,0,0,0,1)
)

colnames(S_BU) <- c(
  "Business_Situation_Past_better",
  "Business_Situation_Past_Worse",
  "Stock_too_high",
  "Stock_too_small",
  "Business_Situation_Future_better",
  "Business_Situation_Future_Worse"
)


compute_BU <- function(data, sa_method, S_BU) {
  
  bottom <- do.call(cbind, lapply(colnames(S_BU), function(v)
    data[[v]][[paste0("adjusted_", sa_method)]]
  ))
  
  bottom %*% t(S_BU)
}

bu_fc <- compute_BU(confi_retail, sa, S_BU)
head(bu_fc)


bu_fc <- ts(
  compute_BU(confi_retail, sa, S_BU),
  start = start(results_retail[[1]]$original),
  frequency = frequency(results_retail[[1]]$original)
)
reco_mint <- as.data.frame(reco_mint)



bu_fc <- as.matrix(bu_fc)

target_key <- "str_wls"

all_plots_list <- lapply(retail_order, function(v) {
  
  data.frame(
    # Time remains the same
    Time = as.vector(time(results_retail[[v]]$original)),
    
    # Original data
    Actual = as.numeric(results_retail[[v]]$original),
    
    SA_adjusted = as.numeric(results_retail[[v]]$adjusted_str),
    
    # Extract the column 'v' from the reconciled matrix
    Reconciled = as.numeric(reco_mint[, v]),
    
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

initial_retail <- test_seasonality(data = bu_fc, variables = retail_order, start_year = 2004)




weights <-rep(1,length(retail_order))
balance_ind <-grep("balance", retail_order)
balance_ind

weights[balance_ind] <- 100
components<-grep("better|Worse|too_high|too_small", retail_order)
components

weights[components] <- 0.01

weights

reco_wls <- recf(
  base = base_sa,
  agg_mat = agg_mat,
  W = diag(weights),
  type = "wls"
)


