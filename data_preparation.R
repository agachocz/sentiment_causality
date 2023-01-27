library(dplyr)
library(ggplot2)
library(tseries)
library(vars)

dax <- read.csv("data/dax_m.csv", sep =",", header=T, dec = ".")[,c(1,5,6)] %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
  mutate(Month = format(Date, "%m-%Y"))

wig <- read.csv("data/wig_m.csv", sep =",", header=T, dec = ".")[,c(1,5,6)] %>% 
  rename(Date = Data, Close = Zamkniecie, Volume = Wolumen) %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
  mutate(Month = format(Date, "%m-%Y"))

zew <- read.csv("data/ZEW_eurozone.csv", header = T, sep = ";", dec = ",")
colnames(zew) <- c("Date", "EconomicGrowth", "Inflation", "CurrentSituation", "STOXX50", "InterestRate")
zew <- zew %>% mutate(Date = as.Date(Date, format = "%d.%m.%Y")) %>%
  mutate(Month = format(Date, "%m-%Y"))

data <- full_join(wig, zew, by = "Month") %>% 
  dplyr::select(Month, Close, Volume, EconomicGrowth, CurrentSituation) %>%
  mutate(Close = log(Close/lag(Close)), EconomicGrowth = (EconomicGrowth-lag(EconomicGrowth)), 
          Volume = (Volume-lag(Volume))/1000000, CurrentSituation = (CurrentSituation-lag(CurrentSituation))) %>%
  slice(-1) %>% filter(across(everything(), complete.cases))

adf.test(data$Close) # stationary
adf.test(data$EconomicGrowth) # stationary
adf.test(data$Volume)

cor(data[,-1])

VARselect(data[,-c(1,2)], lag.max = 15, type="const")
model <- VAR(data[,-c(1,2)], p = 2, type = "const")
summary(model)
coefficients(model)

plot(data$Volume, type = "l")
plot(dax$Date, dax$Volume, type = "l")
plot(data$Close, type = "l")
plot(zew$EconomicGrowth, type = "l")
plot(zew$CurrentSituation, type = "l")
