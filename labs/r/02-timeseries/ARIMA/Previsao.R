library(readxl)
library(tseries)
library(forecast)
library(tidyverse)
library(ggplo2)
library(fpp2)

# Dados obddos na ANDA - Associação Nacional de Defensivos Agricolas

df <- read_excel("Fertilizantes.xlsx")
head(df)
tail(df)

ts.total <- ts(df$consumo, frequency = 12, start = c(1998,1))

ts.train <- window(ts.total,end = c(2014,12)) # segie para treinamento até 2014

ts.test <- window(ts.total, start = c(2015, 1)) # serie para teste a partir de 2015

autoplot(ts.total) # plot da série completa

ggseasonplot(ts.total, year.labels = TRUE, year.label.left = TRUE)+
  ylab("Entrega ao mercado em mil toneladas")+ xlab("Mês")
  ggtitle("Entrega ao Mercado")

# Comecando com Holt-Winters aditivo

fit1 <- hw(ts.train, seasonal = "additive", h = 57)

autoplot(ts.train) +
  autolayer(fit1, series = "Previsão HW aditiva", PI = FALSE) +
  autolayer(ts.test, series = "Dados", PI = FALSE) +
  xlab("Data") +
  ylab("Consumo") +
  ggtitle("Fertilizantes") +
  guides(colour = guide_legend(title = "Forecast"))

library(xlsx)
write.xlsx(fit1, "previsões_HWA.xlsx")

# Continuando com Holt-Winters multiplicativo

fit2 <- hw(ts.train,seasonal = "multiplicative", h = 57)
autoplot(ts.train) +
  autolayer(fit2, series = "Previsão HW Multiplicativa") +
  autolayer(ts.test, series = "Dados") +
  xlab("Data") +
  ylab("Consumo") +
  ggtitle("Fertilizantes") +
  guides(colour = guide_legend(title = "Forecast"))

library(xlsx)
write.xlsx(fit1, "previsões_HWA.xlsx")
write.xlsx(fit2, "previsões_HWM.xlsx")

#########################################################
# Creio ser um pouco exagerado o periodo de previsao - que tal prever apenas dois anos adiante?

ts.train2 <- window(ts.total,end = c(2017,12))

ts.test2 <- window(ts.total, start = c(2018, 1))

fit.HWA <- hw(ts.train2, seasonal = "additive", h = 21)

boxplot(ts.train~cycle(ts.train),xlab="meses", ylab = "Mil Toneladas" ,col="blue", main ="Boxplot-Consumo de Fertilizantes-BR-1998-2017")

autoplot(ts.train2) +
  autolayer(fit.HWA, series = "Previsão HW aditiva", PI = FALSE) +
  autolayer(ts.test2, series = "Realizado") +
  xlab("Data") +
  ylab("Consumo") +
  ggtitle("Fertilizantes") +
  guides(colour = guide_legend(title = "Série"))

# Agora ETS muktiplicativa
fit.HWM <- hw(ts.train2, seasonal = "multiplicative", h = 21)
autoplot(ts.train2) +
  autolayer(fit.HWM, series = "Previsão HW multiplicativa", PI = FALSE) +
  autolayer(ts.test2, series = "Realizado") +
  xlab("Data") +
  ylab("Consumo") +
  ggtitle("Fertilizantes") +
  guides(colour = guide_legend(title = "Série"))
# parece muito melhor!!

write.xlsx(fit.HWM, "previsões_HWM_1ano.xlsx")
write.xlsx(fit.HWA, "previsões_HWA_1ano.xlsx")

##################################################
# modelo Arima 
arima.ts <- auto.arima(ts.train2)
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

ts.train2 %>%
  Arima(order=c(0,0,3), seasonal=c(0,1,2), include.drift = TRUE) %>%
  residuals() %>% ggtsdisplay()

fit.Arima <- ts.train2 %>%
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
