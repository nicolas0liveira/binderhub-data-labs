library(readxl)
library(tseries)
library(forecast)
library(tidyverse)
library(ggplot2)
library(fpp2)
library(xlsx)
# Dados obddos na ANDA - Associação Nacional para Difusão de Adubos
# http://anda.org.br/estatisticas/


df <- read_excel("Fertilizantes.xlsx")
head(df)
tail(df)
class(df$consumo)

# Transformando em TS

ts.total <- ts(df$consumo, frequency = 12, start = c(1998,1))

autoplot(ts.total) + ggtitle("Fertilizantes Entregues ao mercado")+
  ylab("mil toneladas") # plot da série completa

ggseasonplot(ts.total, year.labels = TRUE, year.label.left = TRUE)+
  ggtitle("Entrega ao Mercado")+
  ylab("Entrega ao mercado em mil toneladas") + xlab("Mês")

boxplot(ts.total~cycle(ts.total),xlab="meses", ylab = "Mil Toneladas" ,
        col="orange", main ="Boxplot-Entrega de Fertilizantes-BR-1998-2019")

# Train & Test

ts.train <- window(ts.total,end = c(2017,12))

ts.test <- window(ts.total, start = c(2018, 1))

tail(ts.train)
head(ts.test)

# Começando por HW Aditivo projetando 21 períodos

fit.HWA <- hw(ts.train, seasonal = "additive", h = 21)

autoplot(ts.train) +
  autolayer(fit.HWA, series = "Previsão HW aditiva", PI = FALSE) +
  autolayer(ts.test, series = "Realizado") + # A projeção entra aqui
  xlab("Data") +
  ylab("Consumo") +
  ggtitle("Fertilizantes") +
  guides(colour = guide_legend(title = "Série"))

# Agora ETS multiplicativa

fit.HWM <- hw(ts.train, seasonal = "multiplicative", h = 21)

autoplot(ts.train) +
  autolayer(fit.HWM, series = "Previsão HW multiplicativa", PI = FALSE) +
  autolayer(ts.test, series = "Realizado") +
  xlab("Data") +
  ylab("Consumo") +
  ggtitle("Fertilizantes") +
  guides(colour = guide_legend(title = "Série"))
# parece muito melhor!!

write.xlsx(fit.HWM, "previsões_HWM_1ano.xlsx")
write.xlsx(fit.HWA, "previsões_HWA_1ano.xlsx")

##################################################
# modelo Arima 

arima.ts <- auto.arima(ts.train)
arima.ts

# Series: ts.train 
# ARIMA(0,0,3)(0,1,2)[12] with drift 
# 
# Coefficients:
#   ma1     ma2     ma3     sma1     sma2   drift
# 0.7751  0.5347  0.3254  -0.6427  -0.1657  7.1279
# s.e.  0.0641  0.0738  0.0611   0.0775   0.0758  0.8615
# 
# sigma^2 estimated as 49661:  log likelihood=-1559.33
# AIC=3132.65   AICc=3133.16   BIC=3156.66

ts.train %>%
  Arima(order=c(0,0,3), seasonal=c(0,1,2), include.drift = TRUE) %>%
  residuals() %>% ggtsdisplay()

fit.Arima <- ts.train %>%
  Arima(order=c(0,0,3), seasonal=c(0,1,2), include.drift = TRUE)

checkresiduals(fit.Arima)

# Ljung-Box test
# 
# data:  Residuals from ARIMA(0,0,3)(0,1,2)[12] with drift
# Q* = 16.705, df = 18, p-value = 0.5435  ==> valor-p alto  ==> pronto para a previsão
# 
# Model df: 6.   Total lags used: 24

prev.Arima <- fit.Arima %>% forecast(h = 21)
prev.Arima
prev.Arima %>% autoplot()

write.xlsx(prev.Arima, "previsões_Arima_1ano.xlsx")
