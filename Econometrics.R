library(dplyr)
library(ggplot2)
library(quantmod)
library(tidyverse)
library(slider)

#####################################

#We want to have not all the ticker but only two of them.
#We also wanto to cut the S&P 500 in order to get more significant results 
data <- all_indices_data
unique(data$ticker)
data_filtrato <- data[data$ticker %in% c("^GSPC", "^STOXX50E"), ]
data_usa <- data_filtrato[data_filtrato$ticker == "^GSPC",]
data_europe <- data_filtrato[data_filtrato$ticker == "^STOXX50E",]
data_usa<- data_usa |>
  filter(date>="2000-01-01")

#Now we compute the log values
europe_filtered <- data_europe %>%
  mutate(log_return = log(close / lag(close))) %>%
  filter(!is.na(log_return))

usa_filtered <- data_usa %>%
  mutate(log_return = log(close / lag(close))) %>%
  filter(!is.na(log_return))

#Values for Europe
mean_europe <- mean(europe_filtered$log_return)
var_europe <- var(europe_filtered$log_return)
sd_europe <- sd(europe_filtered$log_return)
skew_europe <- (sum((europe_filtered$log_return - mean_europe)^3) / 
           length(europe_filtered$log_return)) / (sd_europe^3)
kurt_europe <-(sum((europe_filtered$log_return - mean_europe)^4) / 
    length(europe_filtered$log_return)) / (sd_europe^4) - 3

 #Values for USA 
mean_usa <- mean(usa_filtered$log_return)
var_usa <- var(usa_filtered$log_return)
sd_usa <- sd(usa_filtered$log_return)
skew_usa <- (sum((usa_filtered$log_return - mean_usa)^3) / 
                 length(usa_filtered$log_return)) / (sd_usa^3)
kurt_usa <-(sum((usa_filtered$log_return - mean_usa)^4) / 
                 length(usa_filtered$log_return)) / (sd_usa^4) - 3

acf_europe <- acf(europe_filtered$log_return, 
                  lag.max = 5, 
                  plot = TRUE,
                  main="Sample Autocorrelation Function – EUROSTOXX 50"
                  )
acf_usa <- acf(usa_filtered$log_return, 
               lag.max = 5, 
               plot = TRUE,
               main="Sample Autocorrelation Function – S&P 500"
               )

#####################################

#We now calculate the VR values for different lags for usa 
usa_sum <- usa_filtered %>%
  arrange(date) %>%
  mutate(
    logret_2d  = slide_dbl(log_return, sum, .before = 1,  .complete = TRUE), 
    logret_5d  = slide_dbl(log_return, sum, .before = 4,  .complete = TRUE), 
    logret_10d = slide_dbl(log_return, sum, .before = 9,  .complete = TRUE),
    logret_20d = slide_dbl(log_return, sum, .before = 19, .complete = TRUE)
  )

VR2  <- var(usa_sum$logret_2d,  na.rm = TRUE) / (2  * var_usa)
VR5  <- var(usa_sum$logret_5d,  na.rm = TRUE) / (5  * var_usa)
VR10 <- var(usa_sum$logret_10d, na.rm = TRUE) / (10 * var_usa)
VR20 <- var(usa_sum$logret_20d, na.rm = TRUE) / (20 * var_usa)
#The value are less than 1, then we have mean reversion 

#Now we calculate if the variance ratios are significantly different than 0
T_usa <- nrow(usa_filtered)
T_europe <- nrow(europe_filtered)

z_test_vr <- function(VR, q, T) {
  se <- sqrt(2 * (2*q - 1) * (q - 1) / (3 * q * T))
  z <- (VR - 1) / se
  p <- 2 * pnorm(-abs(z))
  return(c(z = z, p = p))
}

# USA
z_test_vr(VR2,  2,  T_usa)
z_test_vr(VR5,  5,  T_usa)
z_test_vr(VR10, 10, T_usa)
z_test_vr(VR20, 20, T_usa)

# EUROPE
z_test_vr(Vr2,  2,  T_europe)
z_test_vr(Vr5,  5,  T_europe)
z_test_vr(Vr10, 10, T_europe)
z_test_vr(Vr20, 20, T_europe)


#Then we calculate the other q
usa_sum2 <- usa_filtered %>%
  arrange(date) %>%
  mutate(
    logret_50d  = slide_dbl(log_return, sum, .before = 49,  .complete = TRUE), 
    logret_100d  = slide_dbl(log_return, sum, .before = 99,  .complete = TRUE), 
    logret_200d = slide_dbl(log_return, sum, .before = 199,  .complete = TRUE),
    #PARTE IN PIù
    logret_300d = slide_dbl(log_return, sum, .before = 299,  .complete = TRUE),
    logret_500d = slide_dbl(log_return, sum, .before = 499,  .complete = TRUE),
    
  )

VR50  <- var(usa_sum2$logret_50d,  na.rm = TRUE) / (50  * var_usa)
VR100 <- var(usa_sum2$logret_100d, na.rm = TRUE) / (100 * var_usa)
VR200 <- var(usa_sum2$logret_200d, na.rm = TRUE) / (200 * var_usa)
VR300 <- var(usa_sum2$logret_300d, na.rm = TRUE) / (300 * var_usa)
VR500 <- var(usa_sum2$logret_500d, na.rm = TRUE) / (500 * var_usa)



#We create a dataframe to then plot the results 
VR_plot <- data.frame(
  q = c(2, 5, 10, 20, 50, 100, 200, 300, 500),
  vr = c(VR2, VR5, VR10, VR20, VR50, VR100, VR200, VR300, VR500)
)

ggplot(VR_plot, aes(x = q, y = vr)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 3) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") + 
  labs(
    title = "Variance Ratio for USA",
    x = "Lag (q)",
    y = "Variance Ratio VR(q)"
  ) +
  theme_minimal()

#####################################

#We do the same for Europe now 

europe_sum <- europe_filtered %>%
  arrange(date) %>%
  mutate(
    E_logret_2d  = slide_dbl(log_return, sum, .before = 1,  .complete = TRUE), 
    E_logret_5d  = slide_dbl(log_return, sum, .before = 4,  .complete = TRUE), 
    E_logret_10d = slide_dbl(log_return, sum, .before = 9,  .complete = TRUE),
    E_logret_20d = slide_dbl(log_return, sum, .before = 19, .complete = TRUE)
  )

Vr2  <- var(europe_sum$E_logret_2d,  na.rm = TRUE) / (2  * var_europe)
Vr5  <- var(europe_sum$E_logret_5d,  na.rm = TRUE) / (5  * var_europe)
Vr10 <- var(europe_sum$E_logret_10d, na.rm = TRUE) / (10 * var_europe)
Vr20 <- var(europe_sum$E_logret_20d, na.rm = TRUE) / (20 * var_europe)

europe_sum2 <- europe_filtered %>%
  arrange(date) %>%
  mutate(
    E_logret_50d  = slide_dbl(log_return, sum, .before = 49,  .complete = TRUE), 
    E_logret_100d  = slide_dbl(log_return, sum, .before = 99,  .complete = TRUE), 
    E_logret_200d = slide_dbl(log_return, sum, .before = 199,  .complete = TRUE),
  )

Vr50  <- var(europe_sum2$E_logret_50d,  na.rm = TRUE) / (50  * var_europe)
Vr100 <- var(europe_sum2$E_logret_100d, na.rm = TRUE) / (100 * var_europe)
Vr200 <- var(europe_sum2$E_logret_200d, na.rm = TRUE) / (200 * var_europe)

Vr_plot <- data.frame(
  q = c(2, 5, 10, 20, 50, 100, 200),
  vr = c(Vr2, Vr5, Vr10, Vr20, Vr50, Vr100, Vr200)
)

ggplot(Vr_plot, aes(x = q, y = vr)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 3) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") + 
    labs(
      title = "Variance Ratio for Europe",
      x = "Lag (q)",
      y = "Variance Ratio VR(q)"
    ) +
      theme_minimal()

#####################################

#We now need to merge the data frames for USA and Europe in order to do the OLS

merged_datas <- usa_filtered %>%
  select(date, log_return_usa = log_return) %>%
  inner_join(
    europe_filtered %>%
      select(date, log_return_eu = log_return),
    by = "date"
  ) %>%
  arrange(date)

OLS_model <- lm(log_return_eu ~ log_return_usa, data = merged_datas)

# Now we need to test if alfa=0
alpha_est  <- -0.0001367
se_alpha   <- 0.0001724
df         <- 4221
t_manuale <- alpha_est / se_alpha
p_manuale <- 2 * pt(-abs(t_manuale), df = df)

critical_t_5 <- qt(0.975, df = 4221)  
t_osservato <- abs(-0.793)

if (t_osservato > critical_t_5) {
  print("We refute H0: alpha different from 0")
} else {
  print("We don't refute H0: alpha could be 0")
}

# We need then to calculate the residual variance

residual_variance <- summary(OLS_model)$sigma ^ 2

# Then we compute an equally weighted portfolio

merged_datas <- merged_datas %>%
  mutate(
    rp_ew = 0.5 * log_return_usa + 0.5 * log_return_eu
  )

var_portfolio <- var(merged_datas$rp_ew, na.rm = TRUE)
var_mean_without_correlation <- 0.5 * (var_usa + var_europe)
#We also checked the vlaues with and without correlation 


######

#We now want to do a robustness check. We use only the value after the 2008 crisis 

class(data_usa$date)

data_usa_robustness <- data_usa %>%
  filter(as.Date(date) > as.Date("2010-01-01"))

usa_filtered_robustness <- data_usa_robustness %>%
  mutate(log_return = log(close / lag(close))) %>%
  filter(!is.na(log_return))

mean_usa_r <- mean(usa_filtered_robustness$log_return)
var_usa_r <- var(usa_filtered_robustness$log_return)
sd_usa_r <- sd(usa_filtered_robustness$log_return)
skew_usa_r <- (sum((usa_filtered_robustness$log_return - mean_usa_r)^3) / 
               length(usa_filtered_robustness$log_return)) / (sd_usa_r^3)
kurt_usa_r <-(sum((usa_filtered_robustness$log_return - mean_usa_r)^4) / 
              length(usa_filtered_robustness$log_return)) / (sd_usa_r^4) - 3


usa_sum_robustness <- usa_filtered_robustness %>%
  arrange(date) %>%
  mutate(
    logret_2d  = slide_dbl(log_return, sum, .before = 1,  .complete = TRUE), 
    logret_5d  = slide_dbl(log_return, sum, .before = 4,  .complete = TRUE), 
    logret_10d = slide_dbl(log_return, sum, .before = 9,  .complete = TRUE),
    logret_20d = slide_dbl(log_return, sum, .before = 19, .complete = TRUE))

VR2_r  <- var(usa_sum_robustness$logret_2d,  na.rm = TRUE) / (2  * var_usa_r)
VR5_r  <- var(usa_sum_robustness$logret_5d,  na.rm = TRUE) / (5  * var_usa_r)
VR10_r <- var(usa_sum_robustness$logret_10d, na.rm = TRUE) / (10 * var_usa_r)
VR20_r <- var(usa_sum_robustness$logret_20d, na.rm = TRUE) / (20 * var_usa_r)

merged_datas <- usa_filtered_robustness %>%
  select(date, log_return_usa = log_return) %>%
  inner_join(
    europe_filtered %>%
      select(date, log_return_eu = log_return),
    by = "date"
  ) %>%
  arrange(date)

OLS_robustness <- lm(log_return_eu ~ log_return_usa, data = merged_datas)
summary(OLS_robustness)







