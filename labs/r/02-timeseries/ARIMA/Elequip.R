#install.packages("fpp2")
# install.packages("forecast")
# install.packages("tidyverse")
# installl.packages("ggplot2")
# install.packages("readxl")
# install.packages("tseries")
# install.packages("gridExtra")
# install.packages("urca")
# install.packages("seasonal")

library(fpp2)
library(forecast)
library(tidyverse)
library(ggplot2)
library(readxl)
library(tseries)
library(gridExtra)
library(urca)
library(seasonal)

elecequip

e1 <- autoplot(elecequip)+ggtitle("Sem ajuste sazonal")

# Efetuando o ajuste sazonal

elecequip%>%stl(s.window='periodic') %>% seasadj() -> eeadj

e2 <- autoplot(eeadj)

grid.arrange(e1, e2, ncol =2)

# teste de raiz unitária

ur.kpss(eeadj)

# The value of the test statistic is: 0.7017
# rejeita a Hipótese nula de estacionariedade.

# Efetuando as diferenças
eeadj %>% diff() %>% ggtsdisplay(main="")

(fit <- auto.arima(eeadj, seasonal=FALSE,
                   stepwise = FALSE, approximation = FALSE))

checkresiduals(fit)

# Plotando a raiz unitária
autoplot(fit)

# A função auto.arima() é estrita e não aceitará um modelo
# com raizes próximas ao círculo

g1 <- autoplot(eeadj, series = "Data" )+
  autolayer(fitted(fit), series = "Fitted", lwd=1)+
  xlab("Ano")+
  ylab("Variação % no Consumo - US")+
  ggtitle("Variação% no Consumo - US- Data x Modelo Arima(3,1,1)")

g2 <- autoplot(forecast(fit))

fit$residuals

grid.arrange(g1, g2, nrow = 2)

Box.test(fit$residuals, lag= 3, type = "Lj")
