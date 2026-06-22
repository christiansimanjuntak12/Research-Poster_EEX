##load necessary library##
library(tidyr) #to drop na
library(lubridate)#package for identifying the years
library(ggplot2) #visualize
#install.packages("strucchange") #check the structural break
library(strucchange)
library(moments)#skewness and kurtosis
library(tseries)#stationary test
library(dplyr)#regime
library(rugarch)#GARCH model
library(forecast) #auto ARIMA
#install.packages("FinTS") #checking the ARCH effect
library(FinTS)
library(gridExtra)#combine figure





#########################################Italy Power futures###############################################


#1. Obtain the data
data_italy<-read.csv(file.choose())
head(data_italy)
colnames(data_italy)

#remove the NA value
data_italy_clean<-data_italy %>% drop_na()
str(data_italy_clean)
sum(is.na(data_italy_clean))  


#convert the column Not.Signed.In to date format
data_italy_clean$X<-as.Date(data_italy_clean$X, format = "%d/%m/%Y")
colnames(data_italy_clean)[1] <-"Date"
min(data_italy_clean$Date)
max(data_italy_clean$Date)
sort(unique(year(data_italy_clean$Date)))
length(unique(year(data_italy_clean$Date)))
colnames(data_italy_clean)
head(data_italy_clean)
str(data_italy_clean)
mean(data_italy_clean$EEX.ITALIAN.BL.M.CONT...SETT..PRICE)
max(data_italy_clean$EEX.ITALIAN.BL.M.CONT...SETT..PRICE)
mean_set_price_italy<-mean(data_italy_clean$EEX.ITALIAN.BL.M.CONT...SETT..PRICE, na.rm = TRUE)



#2. Visualize the data
f_italy<-ggplot(data_italy_clean, aes(x = Date, y = EEX.ITALIAN.BL.M.CONT...SETT..PRICE)) +
  geom_rect(aes(xmin=as.Date("2021-10-01"),
                xmax=as.Date("2022-12-31"),
                ymin=-Inf,
                ymax=Inf),
            fill="yellow",
            alpha=0.003)+
  geom_line(color = "black", size = 1) +
  geom_hline(yintercept = mean_set_price_italy,
             color="red",
             linetype="dashed",
             size=1.5)+
  geom_smooth(method = "loess", color = "red", size=1, se = FALSE)+
  labs(title = "Daily settlement price for \n continuous monthly Italy Power Futures",
       x="", y = "EUR/MWh") +
  scale_x_date(expand = c(0, 0),  
               date_breaks = "1 years", date_labels = "%Y") +
  scale_y_continuous(breaks = seq(0, 1000, by = 50))+
  theme_bw() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x  = element_text(size = 12, angle = 45, vjust =1, hjust = 1),
    axis.text.y  = element_text(size = 12)
  )
f_italy




#3.Calculate the daily log returns and check the structural break
data_italy_clean$log_return<-c(NA,diff(log(data_italy_clean$EEX.ITALIAN.BL.M.CONT...SETT..PRICE)))
head(data_italy_clean)
data_italy_clean_nona <- data_italy_clean[-1, ]
head(data_italy_clean_nona)
sum(is.na(data_italy_clean_nona))#checking if there is NA value in data frame


#structural change
bp_mean_italy <- breakpoints(data_italy_clean_nona$log_return ~ 1)

summary(bp_mean_italy)
plot(bp_mean_italy)
#Since BIC increase continuously,
#there is no strong evidence that the mean of the daily log returns changed over time


bp_vol_italy <- breakpoints(I(data_italy_clean_nona$log_return^2) ~ 1)
summary(bp_vol_italy)
plot(bp_vol_italy)
#it is identified that the preferred model has 2 structural breaks in volatility

bp2_italy <- breakpoints(bp_vol_italy, breaks = 2)
bp2_italy$breakpoints


breaks_italy <- bp2_italy$breakpoints
break_dates_italy <- data_italy_clean_nona$Date[breaks_italy]
break_dates_italy



#Visualization
library(scales) # 1 digit decimal
italy_return<-ggplot(data_italy_clean_nona, aes(x = Date, y = log_return)) +
  geom_line(color = "black", size = 1, na.rm = TRUE) +
  geom_vline(xintercept = break_dates_italy,
             colour = "red",
             linetype = "dashed",
             size = 2)+
  labs(title = "The log return of Italy Power Futures",
       x="", y = "") +
  scale_x_date(expand = c(0, 0), limits = as.Date(c("2014-04-08", NA)),
               date_breaks = "1 years", date_labels = "%Y") +
  scale_y_continuous(breaks = seq(-0.5, 0.5, by = 0.1), labels = number_format(accuracy = 0.1))+
  theme_bw() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x  = element_text(size = 12, angle = 45, vjust =1, hjust = 1),
    axis.text.y  = element_text(size = 12)
  )

italy_return


#4.Descriptive statistics
summary(data_italy_clean_nona$log_return)
mean(data_italy_clean_nona$log_return)
sd(data_italy_clean_nona$log_return)
var(data_italy_clean_nona$log_return)
min(data_italy_clean_nona$log_return)
max(data_italy_clean_nona$log_return)

library(moments)
skewness(data_italy_clean_nona$log_return)
kurtosis(data_italy_clean_nona$log_return)

#5.Test stationary
library(tseries)
jarque.bera.test(data_italy_clean_nona$log_return)
adf.test(data_italy_clean_nona$log_return)
kpss.test(data_italy_clean_nona$log_return)


#6.Test for autocorrelation Ljung-Box on return
lags <- c(10, 20, 25, 30)

lb_results_italy <- do.call(rbind, lapply(lags, function(l) {
  test <- Box.test(
    data_italy_clean_nona$log_return,
    lag = l,
    type = "Ljung-Box"
  )
  
  data.frame(
    Lag = l,
    Statistic = as.numeric(test$statistic),
    DF = as.numeric(test$parameter),
    P_Value = test$p.value
  )
}))

lb_results_italy


#7. check acf and pacf
acf(data_italy_clean_nona$log_return)
pacf(data_italy_clean_nona$log_return)



#8. Checking the ARCH effect
#install.packages("FinTS")
library(FinTS)


arch_results_italy <- do.call(rbind, lapply(lags, function(l) {
  test <- ArchTest(data_italy_clean_nona$log_return, lags = l)
  
  data.frame(
    Lags = l,
    Statistic = as.numeric(test$statistic),
    P_Value = test$p.value
  )
}))

arch_results_italy




#9. Perform GARCH model
library(rugarch)

# Choose mean model
library(forecast)
auto.arima(data_italy_clean_nona$log_return)

spec_italy <- ugarchspec(
  mean.model = list(
    armaOrder = c(0,1),
    include.mean = TRUE
  ),
  variance.model = list(
    model = "sGARCH",
    garchOrder = c(1,1)
  ),
  distribution.model = "std"
)

fit_garch_italy <- ugarchfit(spec_italy, data_italy_clean_nona$log_return)
show(fit_garch_italy)


#10. Diagnostic test
res_italy<-residuals(fit_garch_italy, standardize=TRUE)

#Diagnostic arch effect 
arch_residuals_italy <- do.call(rbind, lapply(lags, function(l) {
  test <- ArchTest(res_italy, lags = l)
  
  data.frame(
    Lag = l,
    Statistic = as.numeric(test$statistic),
    P_Value = test$p.value
  )
}))
# Display without scientific notation
arch_residuals_italy$P_Value <- format(
  arch_residuals_italy$P_Value,
  scientific = FALSE,
  digits = 10
)

arch_residuals_italy



#Diagnostic test for Residual autocorrelation
lb_residuals_italy <- do.call(rbind, lapply(lags, function(l) {
  test <- Box.test(
    res_italy,
    lag = l,
    type = "Ljung-Box"
  )
  
  data.frame(
    Lag = l,
    Statistic = as.numeric(test$statistic),
    DF = as.numeric(test$parameter),
    P_Value = test$p.value
  )
}))
# Display without scientific notation
lb_residuals_italy$P_Value <- format(
  lb_residuals_italy$P_Value,
  scientific = FALSE,
  digits = 10
)

lb_residuals_italy



#Diagnostic test for squared Residual autocorrelation 
lb_squared_residuals_italy <- do.call(rbind, lapply(lags, function(l) {
  test <- Box.test(
    res_italy^2,
    lag = l,
    type = "Ljung-Box"
  )
  
  data.frame(
    Lag = l,
    Statistic = as.numeric(test$statistic),
    DF = as.numeric(test$parameter),
    P_Value = test$p.value
  )
}))
# Display without scientific notation
lb_squared_residuals_italy$P_Value <- format(
  lb_squared_residuals_italy$P_Value,
  scientific = FALSE,
  digits = 10
)

lb_squared_residuals_italy




#Visualization
vol_italy <- sigma(fit_garch_italy)
data_italy_clean_nona$garch_vol_italy <- vol_italy
head(data_italy_clean_nona)

#quantile to define the threshold of volatility
p25_italy <- quantile(data_italy_clean_nona$garch_vol_italy, 0.25, na.rm = TRUE)
p90_italy <- quantile(data_italy_clean_nona$garch_vol_italy, 0.90, na.rm = TRUE)



vol_garch_italy<-ggplot(data_italy_clean_nona, aes(x = Date, y = garch_vol_italy)) +
  geom_line(color = "black", size = 1, na.rm = TRUE) +
  geom_ribbon(
    aes(ymin = p90_italy, ymax = ifelse(garch_vol_italy>p90_italy,garch_vol_italy, p90_italy)),
    fill = "darkorange2",
    alpha = 0.3,
    na.rm = TRUE
  ) +
  
  # LOW volatility shading (below p25)
  geom_ribbon(
    aes(ymin = ifelse(garch_vol_italy<p25_italy,garch_vol_italy, p25_italy), ymax = p25_italy),
    fill = "green",
    alpha = 0.3,
    na.rm = TRUE
  ) +
  theme_bw() +
  labs(
    title = "GARCH (1,1) Conditional Volatility \n Italy Power Futures",
    x = "Time",
    y = "Volatility"
  )+
  scale_x_date(
    limits = c(as.Date("2014-04-08"), max(data_italy_clean_nona$Date, na.rm = TRUE)),
    expand = c(0, 0),
    date_breaks = "1 year",
    date_labels = "%Y"
  )+
  scale_y_continuous(limits = c(0, 0.15), breaks = seq(0, 0.15 , by = 0.01))+
  theme_bw() + 
  geom_hline(yintercept = p25_italy,
             color = "green4",
             size = 1,
             linetype = "dashed") +
  geom_hline(yintercept = p90_italy,
             color = "darkorange2",
             size = 1,
             linetype = "dashed") +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x  = element_text(size = 12,angle = 45, vjust =1, hjust = 1),
    axis.text.y  = element_text(size = 12)
  )

vol_garch_italy



#Combined Figures
f_italy
italy_return
vol_garch_italy
library(gridExtra)

grid.arrange(f_italy, italy_return, vol_garch_italy,  ncol = 1, nrow=3)

