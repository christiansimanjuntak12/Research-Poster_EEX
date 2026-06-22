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





#########################################German Power futures###############################################


#1. Obtain the data
data_german<-read.csv(file.choose())
head(data_german)
colnames(data_german)

#remove the NA value
data_german_clean<-data_german %>% drop_na()
str(data_german_clean)

#convert the column X to date format
data_german_clean$X<-as.Date(data_german_clean$X, format = "%d/%m/%Y")
colnames(data_german_clean)[1] <-"Date"
min(data_german_clean$Date)
max(data_german_clean$Date)
sort(unique(year(data_german_clean$Date)))
length(unique(year(data_german_clean$Date)))
colnames(data_german_clean)
mean_set_price_german<-mean(data_german_clean$EEX.PHELIX.BASELOAD.M.CONTINUOUS...SETT..PRICE, na.rm = TRUE)


#2. Visualize the data
f_german<-ggplot(data_german_clean, aes(x = Date, y = EEX.PHELIX.BASELOAD.M.CONTINUOUS...SETT..PRICE)) +
  geom_rect(aes(xmin=as.Date("2018-03-01"),
                xmax=as.Date("2019-03-01"),
                ymin=-Inf,
                ymax=Inf),
            fill="yellow",
            alpha=0.003)+
  geom_rect(aes(xmin=as.Date("2014-01-01"),
                xmax=as.Date("2016-03-30"),
                ymin=-Inf,
                ymax=Inf),
            fill="yellow",
            alpha=0.003)+
  geom_rect(aes(xmin=as.Date("2008-01-01"),
                xmax=as.Date("2008-12-30"),
                ymin=-Inf,
                ymax=Inf),
            fill="yellow",
            alpha=0.003)+
  geom_rect(aes(xmin=as.Date("2007-01-01"),
                xmax=as.Date("2007-10-30"),
                ymin=-Inf,
                ymax=Inf),
            fill="yellow",
            alpha=0.003)+
  geom_rect(aes(xmin=as.Date("2005-11-01"),
                xmax=as.Date("2006-03-30"),
                ymin=-Inf,
                ymax=Inf),
            fill="yellow",
            alpha=0.003)+
  geom_line(color = "blue", size = 1) +
  geom_hline(yintercept = mean_set_price_german,
             color="red",
             linetype="dashed",
             size=1.5)+
  geom_smooth(method = "loess", color = "red", size=1, se = FALSE)+
  labs(title = "Daily settlement price for \n continuous monthly German Power Futures",
       x="", y = "EUR/MWh") +
  scale_x_date(expand = c(0, 0), limits = as.Date(c("2002-07-01", NA)),
               date_breaks = "1 years", date_labels = "%Y") +
  scale_y_continuous(breaks = seq(0, 100, by = 10))+
  theme_bw() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x  = element_text(size = 12, angle = 45, vjust =1, hjust = 1),
    axis.text.y  = element_text(size = 12)
  )
f_german




#3.Calculate the daily log returns and check the structural break
data_german_clean$log_return<-c(NA,diff(log(data_german_clean$EEX.PHELIX.BASELOAD.M.CONTINUOUS...SETT..PRICE)))
head(data_german_clean)
data_german_clean_nona <- data_german_clean[-1, ]
head(data_german_clean_nona)
sum(is.na(data_german_clean_nona))#checking if there is NA value in data frame



#structural change
bp_mean_german <- breakpoints(data_german_clean_nona$log_return ~ 1)
summary(bp_mean_german)
plot(bp_mean_german)
#there is no strong evidence that the mean of the daily log returns changed over time


bp_vol_german <- breakpoints(I(data_german_clean_nona$log_return^2) ~ 1)
summary(bp_vol_german)
plot(bp_vol_german)
#it is identified that the preferred model has 2 structural breaks in volatility

bp2_german <- breakpoints(bp_vol_german, breaks = 2)
bp2_german$breakpoints


breaks_german <- bp2_german$breakpoints
break_dates_german <- data_german_clean_nona$Date[breaks_german]
break_dates_german



#Visualization
german_return<-ggplot(data_german_clean_nona, aes(x = Date, y = log_return)) +
  geom_line(color = "blue", size = 1, na.rm = TRUE) +
  geom_vline(xintercept = break_dates_german,
             colour = "red",
             linetype = "dashed",
             size = 2)+
  labs(title = "The log return of German Power Futures",
       x="", y = "") +
  scale_x_date(expand = c(0, 0), limits = as.Date(c("2002-07-01", NA)),
               date_breaks = "1 years", date_labels = "%Y") +
  scale_y_continuous(breaks = seq(-0.5, 0.5, by = 0.1))+
  theme_bw() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x  = element_text(size = 12, angle = 45, vjust =1, hjust = 1),
    axis.text.y  = element_text(size = 12)
  )

german_return


#4.Descriptive statistics
summary(data_german_clean_nona$log_return)
mean(data_german_clean_nona$log_return)
sd(data_german_clean_nona$log_return)
var(data_german_clean_nona$log_return)
min(data_german_clean_nona$log_return)
max(data_german_clean_nona$log_return)

library(moments)
skewness(data_german_clean_nona$log_return)
kurtosis(data_german_clean_nona$log_return)


#5.Test stationary
library(tseries)
jarque.bera.test(data_german_clean_nona$log_return)
adf.test(data_german_clean_nona$log_return)
kpss.test(data_german_clean_nona$log_return)


#6.Test for autocorrelation Ljung-Box on return
lags <- c(10, 20, 25, 30)

lb_results_german <- do.call(rbind, lapply(lags, function(l) {
  test <- Box.test(
    data_german_clean_nona$log_return,
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

lb_results_german


#7. check acf and pacf
acf(data_german_clean_nona$log_return)
pacf(data_german_clean_nona$log_return)



#8. Checking the ARCH effect
#install.packages("FinTS")
library(FinTS)

arch_results_german <- do.call(rbind, lapply(lags, function(l) {
  test <- ArchTest(data_german_clean_nona$log_return, lags = l)
  
  data.frame(
    Lags = l,
    Statistic = as.numeric(test$statistic),
    P_Value = test$p.value
  )
}))

arch_results_german






#9. Perform GARCH model
library(rugarch)

# Choose mean model
library(forecast)
auto.arima(data_german_clean_nona$log_return)


spec_german <- ugarchspec(
  mean.model = list(
    armaOrder = c(0,2),
    include.mean = TRUE
  ),
  variance.model = list(
    model = "sGARCH",
    garchOrder = c(1,1)
  ),
  distribution.model = "std"
)

fit_garch_german <- ugarchfit(spec_german, data_german_clean_nona$log_return)
show(fit_garch_german)



#10. Diagnostic test
res_german<-residuals(fit_garch_german, standardize=TRUE)

#Diagnostic arch effect 
arch_residuals_german <- do.call(rbind, lapply(lags, function(l) {
  test <- ArchTest(res_german, lags = l)
  
  data.frame(
    Lag = l,
    Statistic = as.numeric(test$statistic),
    P_Value = test$p.value
  )
}))
# Display without scientific notation
arch_residuals_german$P_Value <- format(
  arch_residuals_german$P_Value,
  scientific = FALSE,
  digits = 10
)

arch_residuals_german



#Diagnostic test for Residual autocorrelation
lb_residuals_german <- do.call(rbind, lapply(lags, function(l) {
  test <- Box.test(
    res_german,
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
lb_residuals_german$P_Value <- format(
  lb_residuals_german$P_Value,
  scientific = FALSE,
  digits = 10
)

lb_residuals_german



#Diagnostic test for squared Residual autocorrelation 
lb_squared_residuals_german <- do.call(rbind, lapply(lags, function(l) {
  test <- Box.test(
    res_german^2,
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
lb_squared_residuals_german$P_Value <- format(
  lb_squared_residuals_german$P_Value,
  scientific = FALSE,
  digits = 10
)

lb_squared_residuals_german





#Visualization
vol_german <- sigma(fit_garch_german)
data_german_clean_nona$garch_vol_german <- vol_german


#quantile to define the threshold of volatility
p25_german <- quantile(data_german_clean_nona$garch_vol_german, 0.25, na.rm = TRUE)
p90_german <- quantile(data_german_clean_nona$garch_vol_german, 0.90, na.rm = TRUE)



vol_garch_german<-ggplot(data_german_clean_nona, aes(x = Date, y = garch_vol_german)) +
  geom_line(color = "blue", size = 1, na.rm = TRUE) +
  geom_ribbon(
    aes(ymin = p90_german, ymax = ifelse(garch_vol_german>p90_german,garch_vol_german, p90_german)),
    fill = "darkorange2",
    alpha = 0.3,
    na.rm = TRUE
  ) +
  
  # LOW volatility shading (below p25)
  geom_ribbon(
    aes(ymin = ifelse(garch_vol_german<p25_german,garch_vol_german, p25_german), ymax = p25_german),
    fill = "green",
    alpha = 0.3,
    na.rm = TRUE
  ) +
  theme_bw() +
  labs(
    title = "GARCH (1,1) Conditional Volatility \n German Power Futures",
    x = "Time",
    y = "Volatility"
  )+
  scale_x_date(expand = c(0, 0), limits = as.Date(c("2002-07-01", NA)),
               date_breaks = "1 years", date_labels = "%Y") +
  scale_y_continuous(limits = c(0.02, 0.09), breaks = seq(-0.1, 0.1 , by = 0.01))+
  theme_bw() + 
  geom_hline(yintercept = p25_german,
             color = "green4",
             size = 1,
             linetype = "dashed") +
  geom_hline(yintercept = p90_german,
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

vol_garch_german


#Combined Figures
f_german
german_return
vol_garch_german
library(gridExtra)

grid.arrange(f_german, german_return, vol_garch_german, ncol = 1, nrow=3)

