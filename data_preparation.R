library(dplyr)
library(tidyr)
library(ggplot2)
library(tseries)
library(vars)

# read data
dax_m <- read.csv("data/dax_m.csv", sep =",", header=T, dec = ".")[,c(1,5,6)] %>%
  rename(Date = Data, Close = Zamkniecie, Volume = Wolumen) %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))

dax_d <- read.csv("data/dax_d.csv", sep =",", header=T, dec = ".")[,c(1,5,6)] %>%
  rename(Date = Data, Close = Zamkniecie, Volume = Wolumen) %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))

wig_m <- read.csv("data/wig_m.csv", sep =",", header=T, dec = ".")[,c(1,5,6)] %>% 
  rename(Date = Data, Close = Zamkniecie, Volume = Wolumen) %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))

wig_d <- read.csv("data/wig_d.csv", sep =",", header=T, dec = ".")[,c(1,5,6)] %>% 
  rename(Date = Data, Close = Zamkniecie, Volume = Wolumen) %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))
  #mutate(Month = format(Date, "%m-%Y"))

daily_stocks <- dax_d %>% full_join(wig_d, by = "Date") %>%
  rename(DAX = Close.x, Vol_GER = Volume.x, WIG = Close.y, Vol_PL = Volume.y) %>%
  filter(across(everything(), complete.cases))

monthly_stocks <- dax_m %>% full_join(wig_m, by = "Date") %>%
  rename(DAX = Close.x, Vol_GER = Volume.x, WIG = Close.y, Vol_PL = Volume.y) %>%
  filter(across(everything(), complete.cases))

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
         Vol_GER = (Vol_GER-lag(Vol_GER))/10000000, Vol_PL = (Vol_PL-lag(Vol_PL))/1000000) %>%
  slice(-1) %>% filter(across(everything(), complete.cases))

monthly_data <- full_join(monthly_stocks, zew, by = "Date") %>% 
  dplyr::select(Date, DAX, WIG, Vol_GER, Vol_PL, EconomicGrowth, CurrentSituation,
                Inflation, STOXX50, InterestRate) %>%
  mutate(DAX = log(DAX/lag(DAX)), WIG = log(WIG/lag(WIG)),
         Vol_GER = (Vol_GER-lag(Vol_GER))/10000000, Vol_PL = (Vol_PL-lag(Vol_PL))/1000000) %>%
  slice(-1) %>% filter(across(everything(), complete.cases))

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
  slice(-1) %>% filter(across(everything(), complete.cases))

adf.test(monthly_data$CS) # stationary
adf.test(monthly_data$Vol_GER)
adf.test(monthly_data$Vol_PL)
adf.test(monthly_data$CurrentSituation)


# PL
VARselect(monthly_data[,-c(1,2,3,4)], lag.max = 15, type="const")
model_PL <- VAR(monthly_data[,-c(1,2,3,4)], p = 2, type = "const")
summary(model_PL)

# GER
VARselect(monthly_data[,-c(1,2,3,5)], lag.max = 15, type="const")
model_GER <- VAR(monthly_data[,-c(1,2,3,5)], p = 2, type = "const")
summary(model_GER)

# both
VARselect(monthly_data[,-c(1,2,3)], lag.max = 15, type="const")
model <- VAR(monthly_data[,-c(1,2,3)], p = 2, type = "const")
summary(model)


feir <- irf(model_GER,
            n.ahead = 5, ortho = T, runs = 1000)

plot(feir)

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
coeftest(model, vcov = vcovHC)

2*(1-pnorm(0.0012708/0.00060369))

plot(model)
normality.test(model, multivariate.only = T) # nie przechodzi
arch.test(model) # heteroskedastyczność
serial.test(model) # przechodzi

feir <- irf(model, n.ahead = 10, ortho = F, runs = 10000)

plot(feir)

# Granger

causality(model, cause = "Vol_PL")



# ZEW plot
monthly_data %>% dplyr::select(Date, EconomicGrowth, CurrentSituation, Inflation,
                        STOXX50, InterestRate) %>% 
  pivot_longer(-Date) %>% ggplot(aes(x = Date, y = value, col = name)) + geom_line()




# check if volume impacts DAX with daily data
VARselect(daily_data[,c(2,4)], lag.max = 15, type="const")
model <- VAR(monthly_data[,c(2,4)], p = 15, type = "const")
summary(model)
