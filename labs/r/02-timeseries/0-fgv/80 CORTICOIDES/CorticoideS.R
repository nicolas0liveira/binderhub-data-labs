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

data(h02)

g1 <- autoplot(h02)+ 
         ggtitle("Venda mensal de Corticoides (em milhões)")


g2 <- ggseasonplot(h02, year.labels = TRUE, year.label.left = FALSE)+
  ylab("Vendas de Corticoides em milhões")+ xlab("Meses")+
  ggtitle("Venda mensal de corticoides (em milhões)")

grid.arrange(g1, g2, nrow = 2)

ggAcf(h02)
ggPacf(h02)

h02.diff <- diff(h02)

ggAcf(h02.diff)
ggPacf(h02.diff)
 
 h02.d2 <- diff(h02.diff)

 ggAcf(h02.d2)
ggPacf(h02.d2)

# Usando a função auto.arima()

fit.1 <- auto.arima(h02, seasonal=TRUE,
                   stepwise = FALSE, approximation = FALSE)

fit.1

# Series: h02 
# ARIMA(3,1,1)(0,1,1)[12] 

# Coefficients:
#         ar1     ar2     ar3      ma1     sma1
#       0.0639  0.3355  0.3035  -0.9733  -0.5362
# s.e.  0.0785  0.0751  0.0797   0.0342   0.0703

# sigma^2 estimated as 0.00286:  log likelihood=287.67
# AIC=-563.33   AICc=-562.88   BIC=-543.82

###################################################
# Testando a estacionariedade dos resíduos
###################################################

#install.packages("urca")
library(urca)

# teste de Kwiatkowski-Phillips-Schmidt-Shin (KPSS)

# Ho ´série é estacionária
# H1  série não é estacionária

resid(fit) %>% ur.kpss() %>% summary()

#Value of test-statistic is: 0.0481 < 0.463 a 5% 

# Rejeita Ho: a série é estacionária.

#install.packages("tseries")
library(tseries)

Box.test(fit$residuals, lag= 24, type = "Lj")
#lag 24 refere-se a 12 meses x 2

# Ho: residuos são distiguinveis de uma serie de ruídos brancos
# H1: resíduos não são distinguíveis de uma série de ruído branco.

#  p-value = 0,08159 rejeita Ho.

# Se o valor de p for maior que 0,05, # os resíduos 
# serão independentes ( ou seja a série se parece com um ruído branco)
#, e  queremos que o modelo esteja correto.

# Tudo Ok com o modelo Arima

g1 <- autoplot(h02, series = "Data" )+
  autolayer(fitted(fit), series = "Fitted", lwd=1)+
  xlab("Ano")+
  ylab("Vendas de Corticoides em milhões")+
  ggtitle("Venda mensal de corticoides (em milhões)ARIMA(3,1,1)(0,1,1)[12]")

g2 <- autoplot(forecast((fit), h=60))

grid.arrange(g1, g2, nrow=2)

AIC(fit)

fit <- arima(h02, order=c(3,2,1), seasonal=c(1,2,1))
fit

checkresiduals(fit)

