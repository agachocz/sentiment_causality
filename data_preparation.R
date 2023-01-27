library(dplyr)
library(ggplot2)
library(tseries)
library(vars)

dax <- read.csv("data/dax_m.csv", sep =",", header=T, dec = ".")[,c(1,5,6)] %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
  mutate(Month = format(Date, "%m-%Y"))

wig <- read.csv("data/wig_d.csv", sep =",", header=T, dec = ".")[,c(1,5,6)] %>% 
  rename(Date = Data, Close = Zamkniecie, Volume = Wolumen) %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
  mutate(Month = format(Date, "%m-%Y"))

zew <- read.csv("data/ZEW_eurozone.csv", header = T, sep = ";", dec = ",")
colnames(zew) <- c("Date", "EconomicGrowth", "Inflation", "CurrentSituation", "STOXX50", "InterestRate")
zew$Date <- as.Date(zew$Date, format = "%d.%m.%Y")

# transform ZEW to daily values
dates <- data.frame(days = wig$Date) %>% 
  mutate(months = format(days, "%m-%Y"))

zew_daily <- zew %>% #mutate(Inflation = if_else(Inflation == 0, 1, Inflation)) %>%
  mutate(months = format(Date, "%m-%Y")) %>%
  merge(dates, all = TRUE) %>% 
  dplyr::select(Date = days, EconomicGrowth, CurrentSituation) %>% 
  unique() %>%
  arrange(Date)


data <- full_join(wig, zew, by = Date) %>% 
  dplyr::select(Date, Close, Volume, EconomicGrowth, CurrentSituation) %>%
  mutate(Close = log(Close/lag(Close)), EconomicGrowth = (EconomicGrowth-lag(EconomicGrowth)), 
          Volume = (Volume-lag(Volume))/1000000, CurrentSituation = (CurrentSituation-lag(CurrentSituation))) %>%
  slice(-1) %>% filter(across(everything(), complete.cases))

data <- full_join(wig, zew_daily, by = "Date") %>% 
  dplyr::select(Date, Close, Volume, EconomicGrowth, CurrentSituation) %>%
  #mutate(Volume = (Volume-lag(Volume))/100000000) %>%
  mutate(Close = log(Close/lag(Close)), Volume = (Volume/lag(Volume))) %>%
  slice(-1) %>% filter(across(everything(), complete.cases))

adf.test(data$Close) # stationary
adf.test(data$EconomicGrowth) # stationary
adf.test(data$Volume)
adf.test(data$CurrentSituation)

cor(data[,-1])

VARselect(data[,-c(1,2)], lag.max = 15, type="const")
model <- VAR(data[,-c(1,2)], p = 4, type = "const")
summary(model)
coefficients(model)

plot(data$Volume, type = "l")
plot(dax$Date, dax$Volume, type = "l")
plot(data$Close, type = "l")
plot(data$EconomicGrowth, type = "l")
plot(zew$CurrentSituation, type = "l")
