# install.packages("readxl") 
# install.packages("fpp2")
# install.packages("forecast")

library(readxl)
library(fpp2)
library(forecast)

#Ler arquivo data

df <- read_excel("novo.xlsx",sheet="DATA")

data <- df$Data
data

#transformando em ts

ts.data <- ts(data, frequency = 1)
class(ts.data)

autoplot(ts.data)

# conforme exemplo no Excel

acf <- ggAcf(ts.data, lag.max=)
acf
acf$data

pacf <- ggPacf(ts.data, lag.max =12)
pacf
pacf$data

diff <- (diff(log(diff(ts.data))), lag.max=24)

acf.diff <- Acf(diff)
acf.diff


