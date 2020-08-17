#instalando os pacotes necessários
#install.packages(c("fpp","fpp2", "xlsx", "readxl","forecast","tseries"))
#install.packages(c("lubridate",  "dplyr", "ggplot2","lubridate","stringr","plumber"))
#install.packages(c("xts")) #bom para converter em formato de ts, caso precise
#install.packages(c("plumber"))

#install.packages(c("devtools"))
#library(devtools)
#install_github("cran/PerformanceAnalytics")
#install.packages("PerformanceAnalytics", repos="http://R-Forge.R-project.org")

library(fpp)
library(fpp2)
library(xlsx)
library(readxl)
library(gridExtra)
library(forecast)
library(tseries)
library(stringr)
library(gridExtra)
#library(plumber)

set.seed(1986)

# df = read_excel("Fertilizantes.xlsx")
df <- read_excel("Trabalho/Fertilizantes.xlsx")



### análise inicial dos dados
head(df)
summary(df)
min(df$date);max(df$date)
boxplot(df$consumo)
max(df$consumo)
anyNA(df)
var(df$consumo,na.rm=FALSE)
sd(df$consumo,na.rm=FALSE)

#Criando a TS
?ts
ts.consumo = ts(df$consumo, frequency = 12, start = c(1998, 1))
ts.consumo
plot(ts.consumo)
summary(ts.consumo)

### construindo janela: jan/2007 ~ dez/2017
ts.consumo= window(ts.consumo, frequency = 12, start = c(2007, 1), end = c(2017, 12))
ts.consumo
plot(ts.consumo)
summary(ts.consumo)

### observações iniciais e os componentes da série temporal (Sobretudo a Sazonalidade)
ggseasonplot(ts.consumo, year.labels = TRUE, year.label.left = TRUE,season.labels = TRUE)+
  ylab("Consumo (Tonelada)")+
  xlab("Mês")+
  ggtitle("Consumo de fertilizantes (em toneladas)")

?cycle
boxplot(ts.consumo~cycle(ts.consumo),xlab="Mês",
        ylab = "Consumo (Tonelada)" ,
        col="steelblue", main ="Consumo de fertilizantes (em toneladas)")

#decompondo a série
#decompose = decompose(ts.consumo, type = "additive")
decompose = decompose(ts.consumo, type = "mult")
autoplot(decompose, colour ="darkblue") 
#autoplot(decompose$seasonal)

###############################################################################################
## Verificar a estacionariedade da série. 
###############################################################################################

# ACF = autocorrelação  diz se ela se comporta ou não como um ruido branco
?ggAcf
acf = ggAcf(ts.consumo, lag.max=24);acf 

# PACF = autocorrelaçao parcial (para saber com qual defazagem vou trabalhar)
?ggPacf
pacf = ggPacf(ts.consumo);pacf

######################################################
## Testes formais para verificar a estacionaridade
######################################################

## Augmented Dickey–Fuller Test
?adf.test
adf.test(ts.consumo, alternative="stationary")
        # Ho: Não stationary | Ha: stationary
  # Dickey-Fuller = -8.3097, Lag order = 5, p-value = 0.01
  # alternative hypothesis: stationary
  #![Atenção] Ha setado por meio do paramétro "alternative" ("stationary"/"explosive") [Atenção]!
  #! Análise: baixo valor-p (<0,05), portanto rejeitamos o Ho, logo ela é *estacionária*

## Phillips-Perron Unit Root Test
?pp
pp.test(ts.consumo, alternative="stationary")
    # Ho: Não Estacionária | Ha: Estacionária
    # Dickey-Fuller Z(alpha) = -44.863, Truncation lag parameter = 4, p-value = 0.01
  # alternative hypothesis: stationary
  #![Atenção] Ha setado por meio do paramétro "alternative" (stationary/explosive) [Atenção]!
    #! Análise: baixo valor-p (<0,05), portanto rejeitamos Ho, logo ela é *estacionária*

## KPSS Test for Level Stationarity
# library(urca) (TODO!Verificar diferença entre o kpss dos pacotes tseries e urca)
?tseries::kpss.test
tseries::kpss.test(ts.consumo, null="Level")
  # Ho: Estacionária | Ha: Não Estacionária
    # KPSS Level = 0.89733, Truncation lag parameter = 4, p-value = 0.01
  #![Atenção] Ho setado por meio do paramétro "null" (Level/Trend) [Atenção]!
  #! Análise: baixo valor-p (< .05), portanto rejeitamos Ho, logo ela é *não estacionária*

  # TESTE PELA 1a. DIFERENÇA
diff.ts.consumo = diff(ts.consumo)
ggAcf(diff.ts.consumo)
ggPacf(diff.ts.consumo)
adf.test(diff.ts.consumo, alternative="stationary") #Ha: stationary (rejeita Ho) (p-value)
tseries::kpss.test(diff.ts.consumo, null="Level") #é estacionária


# Não é necessária correção, pois a série é estacionária 
# # CORRIGINDO A HETEROCEDASTICIDADE 
# diff.log.ts.consumo = (diff(log(ts.consumo),lag = 24))
# diff.log.ts.consumo
# 
# checkresiduals(ts.consumo)
# checkresiduals(diff.ts.consumo)
# checkresiduals(diff.log.ts.consumo)
# 
# g1 = ggAcf(diff.log.ts.consumo, lag.max = 24)+
#   ylab("diff(log(ts.consumo")+
#   ggtitle("ACF diff_log")
# 
# g2 = ggPacf(diff.log.ts.consumo,lag.max=24)+
#   ggtitle("PACF diff_log")
# 
# grid.arrange(g1, g2, nrow =2)


###############################################################################################
## Selecionar os modelos de estudo: Holt-Winters Aditivo e o Multiplicativo, bem como o Modelo SARIMA.
###############################################################################################

## MODELO HW ADITIVO

hw.add = hw(ts.consumo, seasonal = "additive", h = 2*12, level = 95);hw.add
fit.add = hw.add$fitted

autoplot(ts.consumo, series = "original")+
  autolayer(fit.add,series = "previsão")+
  autolayer(hw.add, showgap = FALSE)+
  ggtitle("HW - Modelo Aditivo")+
  ylab("Consumo (Tonelada)")+
  xlab("Tempo")


## MODELO HW MULTIPLICATIVO

hw.mult = hw(ts.consumo, seasonal = "multiplicative", h = 2*12, level = 95);hw.mult
fit.mult = hw.mult$fitted

autoplot(ts.consumo, series = "original")+
  autolayer(fit.mult, series = "previsão")+
  autolayer(hw.mult, showgap = FALSE)+
  ggtitle("HW - Modelo Multiplicativo")+
  ylab("Consumo (Tonelada)")+
  xlab("Tempo")


#comparando os modelos HW
autoplot(ts.consumo, series = "original")+
  autolayer(fit.mult, series = "multiplicativo")+
  autolayer(fit.add, series = "aditivo")+
  ggtitle("HW - Comparativo entre modelo Aditivo e Multiplicativo")+
  ylab("Consumo (Tonelada)")+
  xlab("Tempo")

?accuracy
accuracy(hw.add)
  #                    ME     RMSE      MAE        MPE     MAPE      MASE       ACF1
  # Training set 4.989507 249.5421 192.9105 -0.4357936 9.009581 0.5861314 0.01854717
accuracy(hw.mult)
  #                     ME     RMSE      MAE        MPE     MAPE      MASE      ACF1
  # Training set 0.6448575 269.6934 206.0947 -0.9724518 9.606356 0.6261898 0.1271872

checkresiduals(hw.add)
checkresiduals(hw.mult)

# Testando a Heterocedasticidade
library(FinTS)
?ArchTest
ArchTest(hw.add$residuals, lags = 24)
  # ARCH LM-test; Null hypothesis: no ARCH effects
  # Chi-squared = 22.779, df = 24, p-value = 0.5329
    #!Análise: alto valor-p (>= .05), aceitamos Ho, não há efeitos da heterocedasticidade (tudo indica homoscedasticidade)

ArchTest(hw.mult$residuals, lags = 24)
  # ARCH LM-test; Null hypothesis: no ARCH effects
  # Chi-squared = 33.822, df = 24, p-value = 0.08788
      #!Análise: alto valor-p (>= .05), aceitamos Ho, não há efeitos da heterocedasticidade (tudo indica homoscedasticidade)


## MODELO SARIMA 1 [SARIMA(1,1,1)(1,1,1)]
?Arima
sarima.1 <- Arima(ts.consumo, order = c(1,1,1), seasonal = c(1,1,1),method = "ML", lambda = 0)
sarima.1


# MODELO SARIMA 2 [SARIMA(1,0,1)(2,1,1)]
sarima.2 <- Arima(ts.consumo, order = c(1,0,1), seasonal = c(1,1,1),method = "ML", lambda = 0)
sarima.2

## MODELO (S)ARIMA - usando auto.arima()
auto.arima(ts.consumo, seasonal = TRUE, stepwise = FALSE, approximation = FALSE) #ARIMA(1,0,0)(2,1,0)[12] with drift 
#auto.arima(diff(log(ts.consumo)), seasonal = TRUE, stepwise = FALSE, approximation = FALSE) #ARIMA(0,0,2)(2,1,0)[12] 

sarima.auto = Arima(ts.consumo, order = c(1,0,0), seasonal = c(2,1,0), method = "ML", lambda = 0);sarima.auto
autoplot(sarima.auto)

library(graphics)
dev.new()
dev.off()
?tsdiag
diag = tsdiag(sarima.auto, gof.lag = 24,)

# testanto a ausencia de autocorrelação (portmanteau)
Box.test(x = sarima.auto$residuals, lag= 24, type = "Ljung-Box", fitdf = 2)
#checkresiduals(sarima.auto)

#Testando a Heterocedasticidade
library(FinTS)
?ArchTest
ArchTest(sarima.auto$residuals, lags = 24) 
  # ARCH LM-test; Null hypothesis: no ARCH effects
  # Chi-squared = 15.064, df = 24, p-value = 0.9189
    #!Análise: alto valor-p (>= .5), aceitamos Ho, não há heterocedasticidade (homoscedasticidade)

#Normalidade
#require(normtest)
library(normtest)
jb.norm.test(sarima.auto$residuals, nrepl = 2000)
summary(sarima.auto$residuals)

#verificando normalidade
hist(sarima.auto$residuals)

moda = function(v) {
  uniqv = unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

result = moda(as.numeric(sarima.auto$residuals));result




###############################################################################################
## Verificando o melhor modelo e resumindo...
###############################################################################################

AIC(hw.add$model)   #HW-Aditivo               #AIC: 2135.712
AIC(hw.mult$model)  #HW-Multiplicativo        #AIC: 2181.164
AIC(sarima.1)       #ARIMA(1,1,1)(1,1,1)[12]  #AIC: -144.6717
AIC(sarima.2)       #ARIMA(1,0,1)(1,1,1)[12]  #AIC: -144.9788
AIC(sarima.auto)    #ARIMA(1,0,0)(2,1,0)[12]  #AIC: -152.0371 (Menor AIC)

hw.add$model  #AICc: 2141.080
hw.mult$model #AICc:2186.532
sarima.1      #AICc=-144.14
sarima.2      #AICc=-144.45
sarima.auto   #menor AICc(-151.69), logo melhor modelo

accuracy(hw.add)      #RMSE:249.5421
accuracy(hw.mult)     #RMSE:269.6934
accuracy(sarima.1)    #RMSE:247.7314
accuracy(sarima.2)    #RMSE:255.2942
accuracy(sarima.auto) #RMSE:250.2959

# testanto a ausencia de autocorrelação (portmanteau)
?checkresiduals
checkresiduals(hw.add)      # Ljung-Box test | Q* = 19.428, df = 8, p-value = 0.01273   (não há evidências de ausência de autocorrelação)
checkresiduals(hw.mult)     # Ljung-Box test | Q* = 34.369, df = 8, p-value = 3.482e-05 (não há evidências de ausência autocorrelação)
checkresiduals(sarima.1)    # Ljung-Box test | Q* = 21.03, df = 20, p-value = 0.3954    (há evidências de ausência autocorrelação)
checkresiduals(sarima.2)   
# Ljung-Box test | Q* = 27.312, df = 20, p-value = 0.1267   (há evidências de ausência autocorrelação)
checkresiduals(sarima.auto) # Ljung-Box test | Q* = 23.678, df = 21, p-value = 0.3089   (há evidências de ausência autocorrelação)


#Testando a Heterocedasticidade
library(FinTS)
?ArchTest                                   #Ho: no ARCH effects
ArchTest(hw.add$residuals, lags = 24)       #OK
ArchTest(hw.mult$residuals, lags = 24)      #OK
ArchTest(sarima.1$residuals, lags = 24)     #OK
ArchTest(sarima.2$residuals, lags = 24)     #OK
ArchTest(sarima.auto$residuals, lags = 24)  #OK
#OK: evidencias de homocedasticidade  (p-value >=.05)

#apenas analisando os resíduos
dev.new()
par(mfrow=c(5,1))
plot(hw.add$residuals,  ylab = 'hw-add', xlab='') ;abline(h=0, col="red")
plot(hw.mult$residuals,  ylab = 'hw-mult', xlab='') ;abline(h=0, col="red")
plot(sarima.1$residuals,  ylab = 'sarima1', xlab='') ;abline(h=0, col="red")
plot(sarima.2$residuals,  ylab = 'sarima2', xlab='') ;abline(h=0, col="red")
plot(sarima.auto$residuals,  ylab = 'autosarima', xlab='') ;abline(h=0, col="red")

#Diagnosticos
library(graphics)
dev.new()
dev.off()
?tsdiag
diag = tsdiag(sarima.2, gof.lag = 24)
diag = tsdiag(sarima.auto, gof.lag = 24)


###############################################################################################
## Previsão até Dezembro/2020 (usando o modelo de menor AIC)
###############################################################################################
library(forecast)

forecast.sarima.auto = forecast(object = sarima.auto, h = 2*12, level = 95) #95% de confiança
forecast.sarima.auto
forecast.sarima.auto$lower
forecast.sarima.auto$fitted
forecast.sarima.auto$upper



dev.new()
autoplot(ts.consumo, series = "Original")+
  autolayer(forecast.sarima.auto$fitted, series = "Modelo")+
  autolayer(forecast.sarima.auto, series = "Previsão", showgap = FALSE)+
  xlab("Mês")+
  ylab("Consumo")+
  ggtitle("Fertilizantes - Previsão de consumo")


#visualizando cenários
boxplot(forecast.sarima.auto$lower, forecast.sarima.auto$upper, 
        names = c("Pessimista","Otimista")
        , col="lightblue")

# ts.consumo$tt = forecast.sarima.auto$fitted[2,] / ts.consumo[1,]
# ts.consumo$tt

## Testar o modelo
#preparar matriz de confusão
  

