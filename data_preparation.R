library(dplyr)
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
  dplyr::select(Date, DAX, WIG, Vol_GER, Vol_PL, EconomicGrowth, CurrentSituation) %>%
  mutate(DAX = log(DAX/lag(DAX)), WIG = log(WIG/lag(WIG)),
         Vol_GER = (Vol_GER-lag(Vol_GER))/10000000, Vol_PL = (Vol_PL-lag(Vol_PL))/1000000) %>%
  slice(-1) %>% filter(across(everything(), complete.cases))

adf.test(monthly_data$EconomicGrowth) # stationary
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
