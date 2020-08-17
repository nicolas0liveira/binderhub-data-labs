#install.packages(c("BETS", "urca", "TSA", "forecast", "tmtest", "normtest",
#                   "FinTS", "xlsx", "fpp2", "TSstudio"))

library(BETS)
library(urca)
library(TSA)
library(forecast)
library(tmtest)
library(normtest)
library(xlsx)
library(fpp2)
library(TSstudio) 

fert <- Fertilizantes

anyNA(fert)
head(fert)
tail(fert)
class(fert)
fert

#dado que interessa
consumo <- fert$consumo
consumo

# Criando a TS:
  par(mfrow = c(1,1))
ts_consumo <- ts(consumo, start = c(1998,01), frequency = 12)
plot(ts_consumo, main = "TS") 

#decompondo
decomp <- decompose(ts_consumo, type = "multiplicative");
plot(decomp)

#Comportanmento Sazonal da demanda
par(mfrow = c(1,2))
seasonplot(ts_consumo, 12, col=rainbow(12), year.labels=TRUE, main="Seasonplot Consumo de Fertilizante/mês",
           xlab="Mês", ylab="Consumo")

boxplot(ts_consumo ~ cycle(ts_consumo),col="orange",xlab="Mês", 
        ylab="Consumo")
title("Boxplot Consumo de Fertilizante/mês")

#criando a janela
fert.ts <-window(ts_consumo, frequency = 12, start= c(2007,01), end = c(2017,12) )
fert.ts



# HOLT_WINTERS

ts.mult<- hw(fert.ts, h = 24, seasonal = "multiplicative")

autoplot(fert.ts, main = "Holt Winters Multiplicative" ) + 
  autolayer(ts.mult)

ts.add <- hw(fert.ts, h = 24, seasonal = "additive")

autoplot(fert.ts, main = "Holt Winters Additive") + 
  autolayer(ts.add)

summary(ts.mult)
summary(ts.add)

accuracy(ts.mult) #Holt-Winters Multiplicativo
accuracy(ts.add) #Holt-Winters Aditivo 

library(urca)

# teste de Kwiatkowski-Phillips-Schmidt-Shin (KPSS)

# Ho série é estacionária
# H1 série não é estacionária

fert.ts %>% ur.kpss() %>% summary()

####################### 
# KPSS Unit Root Test # 
####################### 

#Test is of type: mu with 4 lags. 
#
#Value of test-statistic is: 0.8973 
#
#Critical value for a significance level of: 
#                10pct  5pct 2.5pct  1pct
#critical values 0.347 0.463  0.574 0.739

### Rejeita H0, pois o valor é mais alto que o valor crítico

############################################################
#  ADF
############################################################

#install.packages("tseries")
library(tseries)

# Ho série não é estacionária
# H1 série é estacionária
# Queremos REJEITAR a hipótese nula para este teste, 
# portanto, queremos um valor p menor que 0,05 (ou menor).

adf.test(fert.ts)

#In adf.test(retorno_ts) : p-value smaller than printed p-value (p-value < 0.01)
# Rejeitam-se Ho, a série é estacionária,

############################################################
#Phillips-Perron
############################################################

# Ho série não é estacionária
# H1 série é estacionária 
# Queremos REJEITAR a hipótese nula para este teste, 
# portanto, queremos um valor p menor que 0,05 (ou menor).


pp.test(fert.ts)
#In pp.test(retorno_ts) : p-value smaller than printed p-value. Rejeita Ho.


#ACF e PACF
acf <- ggAcf(fert.ts, lag.max=24)
acf
acf$data

pacf <- ggPacf(fert.ts, lag.max =24)
pacf
pacf$data


diff <- diff(fert.ts)
diff

acf.diff <- ggAcf(diff)
acf.diff

acf.diff <- ggPacf(diff)
acf.diff

par(mfrow = c(1,2))
Acf(fert.ts, lag.max = 24, main = "ACF Plot for Close")
Acf(diff(fert.ts,1), lag.max = 24, main = "ACF Plot for Differenced Series")

par(mfrow = c(1,2))
Pacf(fert.ts, lag.max = 24, main = "PACF Plot for Close")
Pacf(diff(fert.ts,1), lag.max = 24, main = "PACF Plot for Differenced Series")


library("forecast")

#PRIMEIRO MODELO SARIMA(1,1,1)(1,1,1)12

fit.air.1 <- Arima(fert.ts, order = c(1,1,1), seasonal = c(1,1,1),
                   method = "ML", lambda = 0)
fit.air.1


# teste de significância para o modelo SARIMA(1,1,1)(1,1,1)12

t_test(fit.air.1)

# SEGUNDO MODELO SARIMA (1,1,1)(0,1,1)12

fit.air.2 <- Arima(fert.ts, order = c(1,1,1), seasonal = c(0,1,1),
                   method = "ML", lambda=0)
fit.air.2

t_test(fit.air.2)

#utizando a função auto.arima()pacote fpp2

autoARIMA <- auto.arima(fert.ts, seasonal= TRUE, stepwise = FALSE, approximation = FALSE)
autoARIMA
autoplot(autoARIMA)
#ARIMA(1,0,0)(2,1,0)[12] with drift 

fit.air.3 <- Arima(fert.ts, order = c(1,0,0), seasonal = c(2,1,0),
                   method = "ML", lambda=0)
fit.air.3

t_test(fit.air.3)

AICC(fit.air.1)
AIC(fit.air.2)
AIC(fit.air.3)

#Forecast
forecast <- forecast(fit.air.3, h = 24) # SE QUISER FIXAR IC level = 95)
autoplot(forecast)
forecast$fitted
forecast


checkresiduals(fit.air.3)
