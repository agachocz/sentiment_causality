library(dplyr)
library(tidyr)
library(ggplot2)
library(tseries)
library(vars)

# read data
dax_m <- read.csv("data/dax_m.csv", sep =",", header=T, dec = ".")[,c(1,5,6)] %>%
  rename(Date = Data, DAX = Zamkniecie, Vol_GER = Wolumen) %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))

dax_d <- read.csv("data/dax_d.csv", sep =",", header=T, dec = ".")[,c(1,5,6)] %>%
  rename(Date = Data, DAX = Zamkniecie, Vol_GER = Wolumen) %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))

wig_m <- read.csv("data/wig_m.csv", sep =",", header=T, dec = ".")[,c(1,5,6)] %>% 
  rename(Date = Data, WIG = Zamkniecie, Vol_PL = Wolumen) %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))

wig_d <- read.csv("data/wig_d.csv", sep =",", header=T, dec = ".")[,c(1,5,6)] %>% 
  rename(Date = Data, WIG = Zamkniecie, Vol_PL = Wolumen) %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))
  #mutate(Month = format(Date, "%m-%Y"))

plneur_m <- read.csv("data/plneur_m.csv", sep = ",")[,c(1,5)] %>%
  rename(Date = Data, PLNEUR = Zamkniecie) %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))

plneur_d <- read.csv("data/plneur_d.csv", sep = ",")[,c(1,5)] %>%
  rename(Date = Data, PLNEUR = Zamkniecie) %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))

daily_stocks <- dax_d %>% full_join(wig_d, by = "Date") %>%
  full_join(plneur_d, by = "Date") %>%
  filter(across(everything(), complete.cases)) %>%
  mutate(WIG = WIG*PLNEUR)

monthly_stocks <- dax_m %>% full_join(wig_m, by = "Date") %>%
  full_join(plneur_m, by = "Date") %>%
  filter(across(everything(), complete.cases)) %>%
  mutate(WIG = WIG*PLNEUR)

zew <- read.csv("data/ZEW_eurozone.csv", header = T, sep = ";", dec = ",")
colnames(zew) <- c("Date", "EconomicGrowth", "Inflation", "CurrentSituation", "STOXX50", "InterestRate")
zew$Date <- as.Date(zew$Date, format = "%d.%m.%Y")
zew$months = format(zew$Date, "%m-%Y")

# transform ZEW to daily values

dates <- data.frame(days = daily_stocks$Date) %>% 
  mutate(months = format(days, "%m-%Y"))

zew_daily <- zew %>%
  merge(dates, all = TRUE) %>% 
  dplyr::select(Date = days, EconomicGrowth, CurrentSituation) %>% 
  unique() %>%
  arrange(Date)


daily_data <- full_join(daily_stocks, zew_daily, by = "Date") %>% 
  dplyr::select(Date, DAX, WIG, Vol_GER, Vol_PL, EconomicGrowth, CurrentSituation) %>%
  mutate(DAX = log(DAX/lag(DAX)), WIG = log(WIG/lag(WIG)),
         Vol_GER = (Vol_GER/lag(Vol_GER)), Vol_PL = (Vol_PL/lag(Vol_PL))) %>%
  slice(-1) %>% filter(if_all(everything(), complete.cases))

sd_GER <- sd(daily_data$Vol_GER)
sd_PL <- sd(daily_data$Vol_PL)

daily_data <- daily_data %>% mutate(Vol_GER = if_else(Vol_GER > 6*sd_GER, 6*sd_GER, Vol_GER),
                                    Vol_PL = if_else(Vol_PL > 6*sd_PL, 6*sd_PL, Vol_PL)) %>%
  mutate(Vol_GER = if_else(Vol_GER < -6*sd_GER, -6*sd_GER, Vol_GER),
         Vol_PL = if_else(Vol_PL < -6*sd_PL, -6*sd_PL, Vol_PL))


monthly_data <- full_join(monthly_stocks, zew, by = "Date") %>% 
  dplyr::select(Date, DAX, WIG, Vol_GER, Vol_PL, EconomicGrowth, CurrentSituation,
                Inflation, STOXX50, InterestRate) %>%
  mutate(DAX = log(DAX/lag(DAX)), WIG = log(WIG/lag(WIG)),
         Vol_GER = (Vol_GER-lag(Vol_GER))/10000000, Vol_PL = (Vol_PL-lag(Vol_PL))/10000000,
         EG = EconomicGrowth-lag(EconomicGrowth),
         CS = CurrentSituation - lag(CurrentSituation),
         Infl = Inflation - lag(Inflation),
         STOXX50 = STOXX50 - lag(STOXX50),
         Rate = InterestRate - lag(InterestRate)) %>%
  slice(-1) %>% filter(across(everything(), complete.cases)) %>%
  mutate(Crisis = if_else(between(Date, as.Date("2008-08-01"), as.Date("2014-01-01")) |
                            Date > as.Date("2020-03-01"), 1, 0))

adf.test(monthly_data$CS) # stationary
adf.test(monthly_data$Vol_GER)
adf.test(monthly_data$Vol_PL)
adf.test(monthly_data$CurrentSituation)


# VAR for sentiment indicators

VARselect(monthly_data[,c(2:5,9,11:12)], lag.max = 15, type="const")
model <- VAR(monthly_data[,c(2:5,9,11:12)], p = 2, type = "const")
summary(model)

coeffs <- coefficients(model)
coeffs_table <- cbind(DAX = coeffs$DAX[,1], WIG = coeffs$WIG[,1], 
                      Vol_GER = coeffs$Vol_GER[,1], Vol_PL = coeffs$Vol_PL[,1],
                      STOXX50 = coeffs$STOXX50[,1], EG = coeffs$EG[,1], CS = coeffs$CS[,1])
write.csv(round(coeffs_table, 5), "coeff_table.csv")

2*pnorm(coeffs$DAX[,1]/coeffs$DAX[,2], lower.tail = F)

library(lmtest)
nw <- NeweyWest(model, lag = 2)
cftest <- coeftest(model, vcov = nw)

causality(model, cause = "STOXX50", vcov. = nw)

granger_causality(model, vcov. = nw)

(2*(1-pnorm(abs(cftest[,1]/cftest[,2])))<=0.1)
2*(1-pt(0.0012708/0.00060369, df = 212))

res <- residuals(model)

plot(x = monthly_data$Date[-c(1:2)], y = res[,7], type = "l")

plot(model)
normality.test(model, multivariate.only = T) # nie przechodzi
arch.test(model) # heteroskedastyczność
serial.test(model) # przechodzi



# Granger

causality(model, cause = "CS")



# ZEW plot
monthly_data %>% dplyr::select(Date, EconomicGrowth, CurrentSituation, Inflation,
                        STOXX50, InterestRate) %>% 
  pivot_longer(-Date) %>% ggplot(aes(x = Date, y = value, col = name)) + geom_line()




# check if volume impacts DAX with daily data
VARselect(daily_data[,c(2:5)], lag.max = 20, type="const")
model_d <- VAR(daily_data[,c(2:5)], p = 4, type = "const")

summary(model_d)

arch.test(model_d)
serial.test(model_d, lags.bg = 4)
Box.test(residuals(model)[,4])
causality(model, cause = "Vol_GER")
plot(model_d)

nw <- vcovHC(model_d)
cftest <- coeftest(model_d, vcov = nw)
causality(model_d, cause = "Vol_PL", vcov. = nw)

# ważona MNK


library(bruceR)
granger <- granger_causality(cftest)
granger$result

plot(daily_data$Vol_PL, type = "l")
# impact of volume on DAX and WIG

cor(monthly_data$Vol_GER, monthly_data$STOXX50)
