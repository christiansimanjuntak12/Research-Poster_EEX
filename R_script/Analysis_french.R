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





#########################################French Power futures###############################################


#1. Obtain the data
data_french<-read.csv(file.choose())
head(data_french)
colnames(data_french)

#remove the NA value
data_french <- data_french[, -(3:7)]
head(data_french)
#checking if there is NA value in data frame
data_french_clean<-data_french %>% drop_na()
str(data_french_clean)
sum(is.na(data_french_clean))


#convert the column Not.Signed.In to date format
data_french_clean$Name<-as.Date(data_french_clean$Name, format = "%d/%m/%Y")
colnames(data_french_clean)[1] <-"Date"
min(data_french_clean$Date)
max(data_french_clean$Date)
sort(unique(year(data_french_clean$Date)))
length(unique(year(data_french_clean$Date)))
colnames(data_french_clean)
head(data_french_clean)
mean_set_price_french<-mean(data_french_clean$EEX.FRENCH.BASELOAD.MONTHLY.CONT....SETT..PRICE, na.rm = TRUE)



#remove row with 0.01 value
data_french_clean <- data_french_clean[-c(1:90), ]
head(data_french_clean)


#2. Visualize the data
f_french<-ggplot(data_french_clean, aes(x = Date, y = EEX.FRENCH.BASELOAD.MONTHLY.CONT....SETT..PRICE)) +
  geom_rect(aes(xmin=as.Date("2016-06-30"),
                xmax=as.Date("2017-03-30"),
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
  geom_rect(aes(xmin=as.Date("2013-01-01"),
                xmax=as.Date("2013-06-30"),
                ymin=-Inf,
                ymax=Inf),
            fill="yellow",
            alpha=0.003)+
  geom_rect(aes(xmin=as.Date("2008-06-01"),
                xmax=as.Date("2008-12-30"),
                ymin=-Inf,
                ymax=Inf),
            fill="yellow",
            alpha=0.003)+
  geom_rect(aes(xmin=as.Date("2007-01-01"),
                xmax=as.Date("2007-12-30"),
                ymin=-Inf,
                ymax=Inf),
            fill="yellow",
            alpha=0.003)+
  geom_line(color = "purple", size = 1) +
  geom_hline(yintercept = mean_set_price_french,
             color="red",
             linetype="dashed",
             size=1.5)+
  geom_smooth(method = "loess", color = "red", size=1, se = FALSE)+
  labs(title = "Daily settlement price for \n continuous monthly French Power Futures",
       x="", y = "EUR/MWh") +
  scale_x_date(expand = c(0, 0),
               date_breaks = "1 years", date_labels = "%Y") +
  scale_y_continuous(breaks = seq(0, 150, by = 10))+
  theme_bw() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x  = element_text(size = 12, angle = 45, vjust =1, hjust = 1),
    axis.text.y  = element_text(size = 12)
  )
f_french




#3.Calculate the daily log returns and check the structural break
data_french_clean$log_return<-c(NA,diff(log(data_french_clean$EEX.FRENCH.BASELOAD.MONTHLY.CONT....SETT..PRICE)))
head(data_french_clean)
data_french_clean_nona <- data_french_clean[-1, ]
head(data_french_clean_nona)
sum(is.na(data_french_clean_nona))#checking if there is NA value in data frame


#structural change
bp_mean_french <- breakpoints(data_french_clean_nona$log_return ~ 1)

summary(bp_mean_french)
plot(bp_mean_french)
#Since BIC increase continuously,
#there is no strong evidence that the mean of the daily log returns changed over time


bp_vol_french <- breakpoints(I(data_french_clean_nona$log_return^2) ~ 1)
summary(bp_vol_french)
plot(bp_vol_french)
#it is identified that the preferred model has 4 structural breaks in volatility

bp2_french <- breakpoints(bp_vol_french, breaks = 4)
bp2_french$breakpoints


breaks_french <- bp2_french$breakpoints
break_dates_french <- data_french_clean_nona$Date[breaks_french]
break_dates_french



#Visualization
library(scales) # 1 digit decimal
french_return<-ggplot(data_french_clean_nona, aes(x = Date, y = log_return)) +
  geom_line(color = "purple", size = 1, na.rm = TRUE) +
  geom_vline(xintercept = break_dates_french,
             colour = "red",
             linetype = "dashed",
             size = 2)+
  labs(title = "The log return of French Power Futures",
       x="", y = "") +
  scale_x_date(expand = c(0, 0), limits = as.Date(c("2005-12-29", NA)),
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

french_return


#4.Descriptive statistics
summary(data_french_clean_nona$log_return)
mean(data_french_clean_nona$log_return)
sd(data_french_clean_nona$log_return)
var(data_french_clean_nona$log_return)
min(data_french_clean_nona$log_return)
max(data_french_clean_nona$log_return)

library(moments)
skewness(data_french_clean_nona$log_return)
kurtosis(data_french_clean_nona$log_return)

#5.Test stationary
library(tseries)
jarque.bera.test(data_french_clean_nona$log_return)
adf.test(data_french_clean_nona$log_return)
kpss.test(data_french_clean_nona$log_return)


#6.Test for autocorrelation Ljung-Box on return
lags <- c(10, 20, 25, 30)

lb_results_french <- do.call(rbind, lapply(lags, function(l) {
  test <- Box.test(
    data_french_clean_nona$log_return,
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

lb_results_french



#7. check acf and pacf
acf(data_french_clean_nona$log_return)
pacf(data_french_clean_nona$log_return)


#8. Checking the ARCH effect
#install.packages("FinTS")
library(FinTS)

arch_results_french <- do.call(rbind, lapply(lags, function(l) {
  test <- ArchTest(data_french_clean_nona$log_return, lags = l)
  
  data.frame(
    Lags = l,
    Statistic = as.numeric(test$statistic),
    P_Value = test$p.value
  )
}))

arch_results_french
# lag 10 and 20 are not significant
# ARCH effect present, the lags is 25 means volatility dependency on the long term





#9. Perform GARCH model
library(rugarch)

# Choose mean model
library(forecast)
auto.arima(data_french_clean_nona$log_return)


spec_french <- ugarchspec(
  mean.model = list(
    armaOrder = c(1,1),
    include.mean = TRUE
  ),
  variance.model = list(
    model = "sGARCH",
    garchOrder = c(1,1)
  ),
  distribution.model = "std"
)

fit_garch_french <- ugarchfit(spec_french, data_french_clean_nona$log_return)
show(fit_garch_french)


#10. Diagnostic test
res_french<-residuals(fit_garch_french, standardize=TRUE)

#Diagnostic arch effect 
arch_residuals_french <- do.call(rbind, lapply(lags, function(l) {
  test <- ArchTest(res_french, lags = l)
  
  data.frame(
    Lag = l,
    Statistic = as.numeric(test$statistic),
    P_Value = test$p.value
  )
}))

arch_residuals_french



#Diagnostic test for Residual autocorrelation
lb_residuals_french <- do.call(rbind, lapply(lags, function(l) {
  test <- Box.test(
    res_french,
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

lb_residuals_french



#Diagnostic test for squared Residual autocorrelation 
lb_squared_residuals_french <- do.call(rbind, lapply(lags, function(l) {
  test <- Box.test(
    res_french^2,
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

lb_squared_residuals_french




#Visualization
vol_french <- sigma(fit_garch_french)
data_french_clean_nona$garch_vol_french <- vol_french
head(data_french_clean_nona)

#quantile to define the threshold of volatility
p25_french <- quantile(data_french_clean_nona$garch_vol_french, 0.25, na.rm = TRUE)
p90_french <- quantile(data_french_clean_nona$garch_vol_french, 0.90, na.rm = TRUE)



vol_garch_french<-ggplot(data_french_clean_nona, aes(x = Date, y = garch_vol_french)) +
  geom_line(color = "purple", size = 1, na.rm = TRUE) +
  geom_ribbon(
    aes(ymin = p90_french, ymax = ifelse(garch_vol_french>p90_french,garch_vol_french, p90_french)),
    fill = "darkorange2",
    alpha = 0.3,
    na.rm = TRUE
  ) +
  
  # LOW volatility shading (below p25)
  geom_ribbon(
    aes(ymin = ifelse(garch_vol_french<p25_french,garch_vol_french, p25_french), ymax = p25_french),
    fill = "green",
    alpha = 0.3,
    na.rm = TRUE
  ) +
  theme_bw() +
  labs(
    title = "GARCH (1,1) Conditional Volatility \n French Power Futures",
    x = "Time",
    y = "Volatility"
  )+
  scale_x_date(
    limits = c(as.Date("2006-01-03"), max(data_french_clean_nona$Date, na.rm = TRUE)),
    expand = c(0, 0),
    date_breaks = "1 year",
    date_labels = "%Y"
  )+
  scale_y_continuous(limits = c(0.04, 0.11), breaks = seq(-0.1, 0.11 , by = 0.01))+
  theme_bw() + 
  geom_hline(yintercept = p25_french,
             color = "green4",
             size = 1,
             linetype = "dashed") +
  geom_hline(yintercept = p90_french,
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

vol_garch_french



#Combined Figures
f_french
french_return
vol_garch_french
library(gridExtra)

grid.arrange(f_german, f_french, f_italy,
             german_return, french_return, italy_return,
             vol_garch_german, vol_garch_french, vol_garch_italy, ncol = 3, nrow=3)
 

