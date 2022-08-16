library(dplyr)
library(ggplot2)
library(tseries)

dax <- read.csv("data/dax_m.csv", sep =",", header=T, dec = ".")[,c(1,5)]
dax$Date <- as.Date(dax$Date, format = "%Y-%m-%d")
zew <- read.csv("data/ZEW_eurozone.csv", header = T, sep = ";", dec = ",")
colnames(zew) <- c("Date", "EconomicGrowth", "Inflation", "CurrentSituation", "STOXX50", "InterestRate")
zew$Date <- as.Date(zew$Date, format = "%d.%m.%Y")
