#install.packages(c("Quandl", "fpp","fpp2", "xlsx", "readxl"))

library(Quandl) #  data scraping
library(fpp)
library(fpp2)
library(xlsx)
library(readxl)
library(gridExtra)

# https://fred.stlouisfed.org/series/NATURALGAS  (note que a série está em mes. 
# Vou transformá-la em trimestre)



# robusta<-Quandl(code = "ODA/PCOFFROB_USD",
            #collapse="quarterly", # tranformando a série em trimestres.
#             type = "ts",
#             end_date = "2020-05-01")


#write.xlsx(robusta, "robusta.xlsx")

robusta <- read_xlsx("robusta.xlsx")

robusta <- robusta$x

class(robusta)

ts.robusta <- ts(robusta, frequency=12, start = c(1980,1))

class(ts.robusta)

ts.robusta

autoplot(ts.robusta)

# EFETUANDO UM CORTE DE JAN-2000 EM DIANTE

ts.robusta <- window(ts.robusta, start= c(2000,1))

decomp <- decompose(ts.robusta, type = "multiplicative")
decomp
autoplot(decomp)

acf.robusta <- ggAcf(ts.robusta, lag.max=24)
acf.robusta

pacf.robusta <- ggPacf(ts.robusta)
pacf.robusta

#Teste ADF - No nível

adf.test(ts.robusta)

adf_robusta<- ur.df(ts.robusta, type = "trend", lags = 60, selectlags = "AIC")

summary(adf_robusta)@teststat

summary(adf_robusta)@cval

# Não rejeita Ho. 

# KPSS

kpss_robusta<- ur.kpss(ts.ibc, type = "tau", lags = "short")
summary(kpss_robusta)@teststat
summary(kpss_robusta)@cval

# Rejeita ho

# Efetuando a diferença

diff <- diff(ts.robusta)
autoplot(diff)

# Efetuando os testes de autocorrelação serial

acf.diff <- ggAcf(diff, lag.max = 60)
acf.diff

pacf.diff <- ggPacf(diff, lag.max=60)
pacf.diff

# MA(1)

ma_1 <- arima(ts.robusta, order = c(0,0,1))
ma_1

autoplot(ma_1)

#Efetuando as previsões

forecast <- forecast(ma_1) # SE QUISER FIXAR IC level = 95)
autoplot(forecast)
forecast$fitted
forecast


autoplot(ts.robusta, series = "CAFÉ ROBUSTA")+
  autolayer(forecast$fitted, series = "Modelo MA(1)")+
  autolayer(forecast, series = "Previsão ")


# Vamos particionar com train & test

ts.train <- window(ts.robusta, start=c(2000,1), end=c(2019,9))

ts.test <- window(ts.robusta, start = c(2019,10), end=c(2020, 04))
ts.test
tail(ts.train)

head(ts.test)

# MA(1)

ma_1.train <- arima(ts.train, order = c(0,0,1))
ma_1.train


#Efetuando as previsões

forecast <- forecast(ma_1.train, h = 7) # SE QUISER FIXAR IC level = 95)
autoplot(forecast)
forecast$fitted
forecast

autoplot(ts.train, series = "Train")+
  autolayer(ts.test, series = "Test")+
  autolayer(forecast$fitted, series = "Previsão MA(1)", PI=TRUE)+
  autolayer(forecast)+
  xlab("mes")+
  ylab("IBC_BR")+
  ggtitle("COTAÇÃO MENSAL CAFÉ ROBUSTA EM US$")

cbind <- data.frame(ts.test, forecast)
cbind$erro <- ((cbind$ts.test/cbind$Point.Forecast)-1)*100
cbind
