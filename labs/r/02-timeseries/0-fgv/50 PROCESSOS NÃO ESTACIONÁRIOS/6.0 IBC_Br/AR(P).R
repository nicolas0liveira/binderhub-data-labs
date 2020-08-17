install.packages(c("fpp", "fpp2", "readxl", "urca", "xlsx"))
library(fpp)
library(fpp2)
library(readxl)
library(urca)
library(xlsx) # para modelos autoregressivos

df <- read_excel("ibc_br2.xlsx")
head(df)
df <- df[,-1]
head(df)
tail(df)

ts.ibc <- ts(df, frequency=12, start=c(2005,1))
ts.ibc

autoplot(ts.ibc, xlab = "Mês", ylab="IBC-BR", main = "IBC-BR")

# Correlograma

acf <- ggAcf(ts.ibc)
acf

pacf <- ggPacf(ts.ibc)
pacf

# A ´serie é claramente não estacionária.
# Pelo Pccf vemos claramente que a série fica estacionária no lag = 3

#Teste ADF - No nível

adf.test(ts.ibc)

adf_ibc<- ur.df(ts.ibc, type = "trend", lags = 12, selectlags = "AIC")

summary(adf_ibc)@teststat

summary(adf_ibc)@cval

# Não rejeita Ho. 

# KPSS

kpss_ibc<- ur.kpss(ts.ibc, type = "tau", lags = "short")
summary(kpss_ibc)@teststat
summary(kpss_ibc)@cval

# Rejeita ho

# Efetuando a diferença

diff <- diff(ts.ibc)
autoplot(diff)

# Efetuando os testes de autocorrelação serial

acf.diff <- ggAcf(diff, lag.max = 24)
acf.diff

pacf.diff <- ggPacf(diff, lag.max=24)
pacf.diff

# Teste ADF - No nível

adf.test(diff)

adf.diff<- ur.df(diff, type = "trend", lags = 12, selectlags = "AIC")

summary(adf.diff)@teststat

summary(adf.diff)@cval

# Rejeita Ho ao nivel de 5% 

# KPSS

kpss.diff<- ur.kpss(diff, type = "tau", lags = "short")
summary(kpss.diff)@teststat
summary(kpss.diff)@cval

# Não rejeita ho. A série se comporta como um ruido branco

ndiffs(ts.ibc)

# AR(1)

ar_1 <- arima(ts.ibc, order = c(3,0,0))
ar_1

autoplot(ar_1)


#Efetuando as previsões

forecast <- forecast(ar_1, h = 12) # SE QUISER FIXAR IC level = 95)
autoplot(forecast)
forecast$fitted
forecast


autoplot(ts.ibc, series = "IBC_BR")+
          autolayer(forecast$fitted, series = "Modelo Ar(1)")+
          autolayer(forecast, series = "Previsão com IC 95%", PI =TRUE)

 
# Vamos brincar com train & test

ts.train <- window(ts.ibc, start=c(2005,1), end=c(2019,9))

ts.test <- window(ts.ibc, start = c(2019,10), end=c(2020, 03))

tail(ts.train)

head(ts.test)

# AR(1)

ar_1.train <- arima(ts.train, order = c(1,0,0))
ar_1.train


#Efetuando as previsões

forecast <- forecast(ar_1.train, h = 6) # SE QUISER FIXAR IC level = 95)
autoplot(forecast)
forecast$fitted
forecast

autoplot(ts.train, series = "Train")+
          autolayer(ts.test, series = "Test")+
            autolayer(forecast$fitted, series = "Previsão AR(1)")+
            autolayer(forecast)+
            xlab("mes")+
            ylab("IBC_BR")+
            ggtitle("Indice de Atividade Econômica do Banco Central do Brasil")

# dados reais         dados projetados
# jan-20  134,06      135,9354
# fev-20  134,28      135,8958
# mar-20  136,44      135,6952

((134.06/135.8954)-1)*100
((134.28/135.8958)-1)*100
((136.44/135.6952)-1)*100

AIC(ar_1.train)
