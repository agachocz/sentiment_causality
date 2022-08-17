library(dplyr)
library(ggplot2)
library(tseries)
library(vars)

dax <- read.csv("data/dax_m.csv", sep =",", header=T, dec = ".")[,c(1,5)] %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
  mutate(Month = format(Date, "%m-%Y"))
zew <- read.csv("data/ZEW_eurozone.csv", header = T, sep = ";", dec = ",")
colnames(zew) <- c("Date", "EconomicGrowth", "Inflation", "CurrentSituation", "STOXX50", "InterestRate")
zew <- zew %>% mutate(Date = as.Date(Date, format = "%d.%m.%Y")) %>%
  mutate(Month = format(Date, "%m-%Y"))

data <- full_join(dax, zew, by = "Month") %>% dplyr::select(Month, Close, EconomicGrowth) %>%
  mutate(Close = Close - lag(Close), EconomicGrowth = EconomicGrowth - lag(EconomicGrowth)) %>%
  slice(-1) %>% filter(across(everything(), complete.cases))

adf.test(data$Close) # stationary
adf.test(data$EconomicGrowth) # stationary

VARselect(data[,-1], lag.max = 15, type="const")
model <- VAR(data[,-1], p = 1, type = "none")
summary(model)
coefficients(model)
