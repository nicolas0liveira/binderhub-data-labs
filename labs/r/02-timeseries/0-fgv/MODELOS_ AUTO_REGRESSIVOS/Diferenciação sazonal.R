#install.packages(c("fpp2", "forecast", "tidyverse","readxl"))
library(fpp2)
library(forecast)
library(tsibble)
library(readxl)
library(gridExtra)

a10
head(a10)
tail(a10)

cbind(
      "Vendas mensais em Milhões" = a10, 
      "Vendas mensais log" = log(a10),
      "Variação de vendas em log" = diff(log(a10),12)) %>%
  autoplot(facets = TRUE) +
  xlab("Ano")+ylab("")+
  ggtitle("vendas de Remédios para Diabetes")


cbind("Sales ($million)" = a10,
      "Monthly log sales" = log(a10),
      "Annual change in log sales" = diff(log(a10),12)) %>%
  autoplot(facets=TRUE) +
  xlab("Year") + ylab("") +
  ggtitle("Antidiabetic drug sales")

Acf(a10)
Acf(diff(a10))
Acf(diff(log(a10),12))



