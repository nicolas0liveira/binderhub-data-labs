# install.packages("fpp2")
# install.packages("forecast")
# install.packages("tidyverse")
# installl.packages("ggplot2")
# install.packages("readxl")
# install.packages("tseries")
# install.packages("gridExtra")
# install.packages("seasonal")

library(fpp2)
library(forecast)
library(tidyverse)
library(ggplot2)
library(readxl)
library(tseries)
library(gridExtra)
library(seasonal)

h02

h1 <- autoplot(h02)+ 
  ggtitle("Venda mensal de Corticoides (em milhões)")

h2 <- ggseasonplot(h02, year.labels = TRUE, year.label.left = FALSE)+
  ylab("Vendas de Corticoides em milhões")+ xlab("Meses")+
  ggtitle("Venda mensal de corticoides (em milhões)")

grid.arrange(h1, h2, nrow = 2)


fit <- auto.arima(h02, seasonal=TRUE,
                   stepwise = FALSE, approximation = FALSE)
fit

checkresiduals(fit)

g1 <- autoplot(h02, series = "Data" )+
  autolayer(fitted(fit), series = "Fitted", lwd=1)+
  xlab("Ano")+
  ylab("Vendas de Corticoides em milhões")+
  ggtitle("Venda mensal de corticoides (em milhões)ARIMA(3,0,1((0,1,2)[4]")

g2 <- autoplot(forecast((fit), h=60))

grid.arrange(g1, g2, nrow=2)

Box.test(fit$residuals, lag= 4, type = "Lj")
