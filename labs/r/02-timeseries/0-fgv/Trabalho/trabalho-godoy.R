#                Trabalho Individual 
#  MBA Executivo em Business Analytics e Big Data 
#           BRASILIA00/MBS019A9-TBABD-T08
#            Análise de Séries Temporais 
#               Ricardo Wanner de Godoy


# O trabalho individual deverá ser entregue sob a forma de documento em Word (ou pdf) 
# juntamente com o script em R e deverá constar: 



# Instalação dos pacotes necessários no Script.

install.packages("fpp")
install.packages("fpp2")
install.packages("Quandl")
install.packages("xlsx")
install.packages("gridExtra")
install.packages("tidyverse")
install.packages("readr")
install.packages("dplyr")
install.packages("gapminder")
install.packages("Lahman")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("psych")
install.packages("lubridate")
install.packages("BETS")
install.packages("FinTS")
install.packages("normtest")
install.packages("forecast")
install.packages("tseries")
install.packages("urca")
install.packages("mFilter")
install.packages("gmodels")


library(Quandl)
library(fpp)
library(fpp2)
library(xlsx)
library(gridExtra)
library(tidyverse)
library(readr)
library(dplyr)
library(gapminder)
library(Lahman)
library(ggplot2)
library(tidyr)
library(psych)
library(lubridate)
library(BETS)
library(FinTS)
library(normtest)
library(forecast)
library(tseries)
library(urca)
library(mFilter)
library(gmodels)
library(stats)




# Ocorreu a transforma da coluna "consumo" para Type = Texto no arquivo Fertilizantes1.csv. 

# Carregando os dados no R.
ANDA_Fertilizante <- read.csv2("C:/Users/Godoy/Desktop/Ex_Final_Analise_Serie/Fertilizantes1.csv", dec = ",")
str(ANDA_Fertilizante)
View(ANDA_Fertilizante)


########## a.	Uma breve comentário inicial relacionado a análise exploratória dos dados, incluindo 
# a visualização, identificação de padrões não usuais (se houver), decomposição e o entendimento 
# do padrão da série.


# Corrigir o valor da data 20123-09 por 2012-09 
ANDA_Fertilizante$date[ANDA_Fertilizante$date == "20123-09"] <- "2012-09"

# Corrigir o valor da data 2017-08 por 2018-08 no consumo = 4824
ANDA_Fertilizante[248,1] <- "2018-08" 

# verificando as correções realizadas na base ANDA_Fertilizante.
ANDA_Fertilizante_Corrigido <- c(ANDA_Fertilizante[177, ],ANDA_Fertilizante[248, ]) 
str(ANDA_Fertilizante_Corrigido)
View(ANDA_Fertilizante_Corrigido)

# Correções realizadas na Base:
# List of 4
# $ date   : chr "2012-09"
# $ consumo: chr "3422"
# $ date   : chr "2018-08"
# $ consumo: chr "4824"


########## b.	Considerar o intervalo de janeiro/2007 até dezembro/2017 para modelagem da série temporal.
ANDA_Fertilizante_2007_2017 <- ANDA_Fertilizante%>%filter(date >= "2007-01" & date <= "2017-12")
str(ANDA_Fertilizante_2007_2017)
View(ANDA_Fertilizante_2007_2017)

# oBSERVAÇÃO: com o filtro de data a base passou de 261 para 132 objetos.

summary(ANDA_Fertilizante_2007_2017)
# Resposta do summary

#       date              consumo    
#   Length:132          Min.   : 977  
# Class :character      1st Qu.:1730  
# Mode  :character      Median :2232  
#                       Mean   :2371  
#                       3rd Qu.:2940  
#                       Max.   :4234   

head(ANDA_Fertilizante_2007_2017)
# Resposta do head
#   date    consumo
#1 2007-01   1571
#2 2007-02   1442
#3 2007-03   1587
#4 2007-04   1307
#5 2007-05   1654
#6 2007-06   1829

tail(ANDA_Fertilizante_2007_2017)
# Resposta do tail
#     date    consumo
#127 2017-07   3369
#128 2017-08   4058
#129 2017-09   4234
#130 2017-10   3398
#131 2017-11   3288
#132 2017-12   2358

########## c.	Selecionar os modelos de estudo: HoltWinters Aditivo e o Multiplicativo, bem como o Modelo SARIMA.

# Construindo o Modelo HoltWinters Aditivo e o Multiplicativo

# Separando o vetor que nos interessa
Consumo_Fertilizante <- ANDA_Fertilizante_2007_2017$consumo
Consumo_Fertilizante

# Criando a TS do Vetor Consumo:
ts_Vetor_Consumo <- ts(Consumo_Fertilizante, start = c(2007,01), frequency = 12)
ts_Vetor_Consumo

class(ts_Vetor_Consumo)
# Resposta do class
# [1] "ts"

# Criando o Plot (gráfico) do ts_Vetor_Consumo.
autoplot(ts_Vetor_Consumo, xlab='Ano', ylab = 'Qtd Consumo', main ='TS Vetor Consumo _ Ano') +
  geom_line() + geom_point() + geom_smooth(se = FALSE, method = "lm")


# Resposta do autoplot: a principio apresenta uma plotagem "sazonal". [Ver Plots]

# função HoltWinters - pararametros de suavização - HoltWinters Aditivo
Vtr_Consumo_aj_sazonal <- HoltWinters (ts_Vetor_Consumo, seasonal = "additive")
Vtr_Consumo_aj_sazonal

# Resposta do HoltWinters Aditivo:
# Holt-Winters exponential smoothing with trend and additive seasonal component.
# Call: HoltWinters(x = ts_Vetor_Consumo, seasonal = "additive")
# Smoothing parameters:
# alpha: 0.6016047
# beta : 0
# gamma: 0.7349632

# Coefficients:
#  [,1]

#a   2980.04239
#b    -10.93255
#s1  -566.04667
#s2  -717.03621
#s3  -847.62465
#s4  -908.18911
#s5    53.11856
#s6   455.31369
#s7   809.80126
#s8  1231.26143
#s9  1213.99436
#s10  704.31641
#s11  217.11180
#s12 -617.09939

# Criando o Plot (gráfico) do Ajuste HoltWinters Aditivo.
plot(ts_Vetor_Consumo, xlab = 'Ano', ylab = 'Qtd Consumo', main = 'Ajuste Sazonal HoltWinters Aditivo')
abline(h=2500, col="red", lwd = 2)
lines(fitted(Vtr_Consumo_aj_sazonal)[,1], lwd = 2, col = 'blue')
legend("topleft",c('Consumo de Fertilizantes','Ajuste HW Aditivo'), lwd = c(2,2), 
       col = c('black', 'blue'), bty ='n', cex = 0.6) 

############### Checando o modelo SARIMA.1 nos gráficos:
#################################checkresiduals(Vtr_Consumo_sarima.1)


  
# Criando o Plot (gráfico) do Ajuste HoltWinters Multiplicativo.
Vtr_Consumo_aj_sazonal_mult <- HoltWinters(ts_Vetor_Consumo, seasonal = "multiplicative")
Vtr_Consumo_aj_sazonal_mult

# Resposta do HoltWinters Multiplicativo:
# Holt-Winters exponential smoothing with trend and multiplicative seasonal component.
# Call: HoltWinters(x = ts_Vetor_Consumo, seasonal = "multiplicative")
# Smoothing parameters:
# alpha: 0.5768696
# beta : 0.009130493
# gamma: 0.6329541

# Coefficients:
#  [,1]
#a   3016.4300422
#b      2.2925103
#s1     0.7658913
#s2     0.7022312
#s3     0.6403397
#s4     0.5978572
#s5     0.9768776
#s6     1.1482983
#s7     1.3048672
#s8     1.4801487
#s9     1.4827824
#s10    1.3083666
#s11    1.0906563
#s12    0.7765333


# Criando o Plot (gráfico) do Ajuste Sazonal HoltWinters Multiplicativo.
plot(ts_Vetor_Consumo, xlab = 'Ano', ylab = 'Qtd Consumo', main = 'Ajuste Sazonal HoltWinters Multiplicativo')
abline(h=2500, col="red", lwd = 2)
lines(fitted(Vtr_Consumo_aj_sazonal_mult)[,1], lwd = 3, col = 'yellow')
legend("topleft",c('Consumo de Fertilizantes','Ajuste HW Multiplicativo'), lwd = c(2,2), 
       col = c('black', 'yellow'),bty ='n', cex = 0.6)
       
############### Checando o modelo SARIMA.1 nos gráficos:
#################################checkresiduals(Vtr_Consumo_sarima.1)



# Criando o Plot (gráfico) da HoltWinters Aditivo e Multiplicativo.
plot(ts_Vetor_Consumo, xlab = 'Ano', ylab = 'Qtd Consumo', main = 'HoltWinters Aditivo e Multiplicativo')
abline(h=2500, col="red", lwd = 2)
  lines(fitted(Vtr_Consumo_aj_sazonal)[,1], lwd = 2, col='blue')
  lines(fitted(Vtr_Consumo_aj_sazonal_mult)[,1], lwd = 2, col ='yellow')
  legend("topleft",c("Consumo de Fertilizantes","Ajuste HW Aditivo", "Ajuste HW Multiplicativo"),
       lwd = c(2,2,2), col = c('black', 'blue', 'yellow'), bty ='n', cex = 0.6)
    
  


# Resposta do plot: HoltWinters Aditivo e Multiplicativo. [Ver Plots]



# Construindo o Modelo SARIMA.1.

# Quando lambda = 0, o comando transforma os valores em log.

Vtr_Consumo_sarima.1 <- Arima(ts_Vetor_Consumo, order = c(2,1,2), seasonal = c(2,1,2),
                  method = "ML", lambda = 0)

Vtr_Consumo_sarima.1

# Resposta do teste de significância para o modelo SARIMA.1 (2,1,2)(2,1,2)[12]:

# Series: ts_Vetor_Consumo 
# ARIMA(2,1,2)(2,1,2)[12] 
# Box Cox transformation: lambda= 0 

#Coefficients:
#         ar1    ar2      ma1        ma2       sar1      sar2     sma1      sma2
#       0.0585  0.4134   -0.3258   -0.6742   -0.9780   -0.3030   0.3752   -0.6247
#s.e.   0.3835  0.2858    0.3655    0.3637    0.1284    0.1339   0.3505    0.2492

# sigma^2 estimated as 0.01222:  log likelihood = 82.72
# AIC = -147.44   AICc = -145.79   BIC = -122.43


# Testando o modelo SARIMA.1 com t_test:
t_test (Vtr_Consumo_sarima.1)


# Resposta do teste no modelo:

#           Coeffs      Std.Errors  t Crit.Values    Rej.H0
#   ar1   0.05853844    0.3835415     0.1526261      1.9796  FALSE
#   ar2   0.41344271    0.2857701     1.4467667      1.9796  FALSE
#   ma1  -0.32584578    0.3655314     0.8914303      1.9796  FALSE
#   ma2  -0.67415126    0.3637124     1.8535282      1.9796  FALSE
#   sar1 -0.97803968    0.1283510     7.6200386      1.9796   TRUE
#   sar2 -0.30304501    0.1339282     2.2627420      1.9796   TRUE
#   sma1  0.37522003    0.3504798     1.0705895      1.9796  FALSE
#   sma2 -0.62472002    0.2491800     2.5071033      1.9796   TRUE


# Checando o modelo SARIMA.1 nos gráficos:
checkresiduals(Vtr_Consumo_sarima.1)

# Resposta do plot: SARIMA.1. [Ver Plots]

require(FinTS)
# Checando a Heterocedasticidade do Modelo SARIMA.1.
ArchTest(Vtr_Consumo_sarima.1$residuals, lags = 36)

# Resposta da checagem do modelo SARIMA.1.

#ARCH LM-test; Null hypothesis: no ARCH effects
# data:  Vtr_Consumo_sarima.1$residuals
# Chi-squared = 19.351, df = 36, p-value = 0.9894


require(normtest)
# Checando a Normalidade do Modelo SARIMA.1 - Jarque-Bera
jb.norm.test(Vtr_Consumo_sarima.1$residuals, nrepl = 2000)

# Resposta da checagem do modelo SARIMA.1.

# Jarque-Bera test for normality
# data:  Vtr_Consumo_sarima.1$residuals
# JB = 121.49, p-value < 2.2e-16

# Checando a Normalidade do Modelo SARIMA.1 - Shapiro-Wilk
shapiro.test(Vtr_Consumo_sarima.1$residuals)

# Resposta da checagem do modelo SARIMA.1.

# Shapiro-Wilk normality test
# data:  Vtr_Consumo_sarima.2$residuals
# W = 0.93978, p-value = 1.75e-05

# Checando a Normalidade do Modelo SARIMA.1 - histrograma
hist(Vtr_Consumo_sarima.1$residuals)

# Resposta do Histrograma: SARIMA.1. [Ver Plots]



# Checando o summary do Modelo SARIMA.1.
round(summary(Vtr_Consumo_sarima.1$residuals), digits = 3)

# Resposta do o summary do Modelo SARIMA.2.
#     Min.  1st Qu.  Median    Mean    3rd Qu.    Max. 
#   -0.302  -0.045    0.001    0.003    0.054    0.514 


# Criando a função MODA:
moda <- function(v) {uniqv <- unique(v)
uniqv[which.max(tabulate(match(v, uniqv)))]}

# calculando a MODA do Modelo SARIMA.1.
Vtr_Consumo_sarima.1_result <- moda(as.numeric(Vtr_Consumo_sarima.1$residuals))
print(Vtr_Consumo_sarima.1_result)

# Resultado da MODA do Modelo SARIMA.1.
# 0.004248989


# Construindo o Modelo SARIMA.2.

Vtr_Consumo_sarima.2 <- Arima(ts_Vetor_Consumo, order = c(1,1,1), seasonal = c(0,1,1),
                  method = "ML", lambda = 0)

Vtr_Consumo_sarima.2

# Resposta do teste de significância para o modelo SARIMA.2 (1,1,1)(0,1,1)[12]:

# Series: ts_Vetor_Consumo 
# ARIMA(1,1,1)(0,1,1)[12] 
# Box Cox transformation: lambda= 0 

# Coefficients:
#         ar1      ma1     sma1
#       0.6924  -1.0000  -0.7782
#s.e.   0.0684   0.0477   0.0989

#sigma^2 estimated as 0.0143:  log likelihood = 77.24
#AIC = -146.48   AICc = -146.13   BIC =- 135.36


# Testando o modelo SARIMA.2 com t_test:
t_test (Vtr_Consumo_sarima.2)

# Resposta do teste no modelo SARIMA.2:
#          Coeffs     Std.Errors  t Crit.Values    Rej.H0
#ar1     0.6924465    0.06841791    10.120836     1.97882   TRUE
#ma1    -0.9999995    0.04766018    20.981864     1.97882   TRUE
#sma1   -0.7782037    0.09885779     7.871951     1.97882   TRUE


# Checando o modelo SARIMA.2 nos gráficos:
checkresiduals(Vtr_Consumo_sarima.2)

# Resposta do plot: SARIMA.2. [Ver Plots]


# Checando a Heterocedasticidade do Modelo SARIMA.2.
ArchTest(Vtr_Consumo_sarima.2$residuals, lags = 36)

# Resposta da checagem do modelo SARIMA.2.

#ARCH LM-test; Null hypothesis: no ARCH effects
# data:  Vtr_Consumo_sarima.2$residuals
# Chi-squared = 20.108, df = 36, p-value = 0.985

require(normtest)
# Checando a Normalidade do Modelo SARIMA.2 - Jarque-Bera
jb.norm.test(Vtr_Consumo_sarima.2$residuals, nrepl = 2000)

# Resposta da checagem do modelo SARIMA.2.

# Jarque-Bera test for normality
# data:  Vtr_Consumo_sarima.2$residuals
# JB = 158, p-value < 2.2e-16

# Checando a Normalidade do Modelo SARIMA.2 - Shapiro-Wilk
shapiro.test(Vtr_Consumo_sarima.2$residuals)

# Resposta da checagem do modelo SARIMA.2.

# Shapiro-Wilk normality test
# data:  Vtr_Consumo_sarima.2$residuals
# W = 0.93052, p-value = 4.089e-06

# Checando a Normalidade do Modelo SARIMA.2 - histrograma
hist(Vtr_Consumo_sarima.2$residuals)

# Resposta do Histrograma: SARIMA.2. [Ver Plots]


# Checando o summary do Modelo SARIMA.2.
round(summary(Vtr_Consumo_sarima.2$residuals), digits = 3)

# Resposta do o summary do Modelo SARIMA.2.
#     Min.  1st Qu.  Median    Mean    3rd Qu.    Max. 
#   -0.348  -0.047    0.001    0.004    0.060    0.581 


# Criando a função MODA:
moda <- function(v) {uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]}

# calculando a MODA do Modelo SARIMA.2.
Vtr_Consumo_sarima.2_result <- moda(as.numeric(Vtr_Consumo_sarima.2$residuals))
print(Vtr_Consumo_sarima.2_result)

# Resultado da MODA do Modelo SARIMA.2.
# 0.004248989


# Construindo automaticamente o Modelo SARIMA.3 - auto.ARIMA.

auto.arima(diff(log(ts_Vetor_Consumo)), seasonal = TRUE, stepwise = FALSE, approximation = FALSE)

# Resposta do teste de significância para o modelo SARIMA.3 (0,0,2)(2,1,0)[12]:

# Series: diff(log(ts_Vetor_Consumo)) 
# ARIMA(0,0,2)(2,1,0)[12] 

#Coefficients:
#         ma1      ma2      sar1     sar2
#       -0.2710  -0.2771  -0.7384  -0.5101
#s.e.    0.0932   0.1089   0.0833   0.0828

# sigma^2 estimated as 0.01518:  log likelihood=76.94
# AIC = -143.88   AICc = -143.35   BIC = -129.98


# Construindo o Modelo SARIMA.3.

Vtr_Consumo_sarima.3 <- Arima(ts_Vetor_Consumo, order = c(0,0,2), seasonal = c(2,1,0),
                  method = "ML", lambda = 0)
Vtr_Consumo_sarima.3

# Resposta do teste de significância para o modelo SARIMA.3 (0,0,2)(2,1,0)[12]:

# Series: ts_Vetor_Consumo 
# ARIMA(0,0,2)(2,1,0)[12] 
# Box Cox transformation: lambda= 0 

# Coefficients:
#         ma1     ma2     sar1     sar2
#       0.6554  0.3437  -0.6285  -0.4350
# s.e.  0.0995  0.1065   0.0878   0.0858

# sigma^2 estimated as 0.01749:  log likelihood=70.5
# AIC = -130.99   AICc = -130.46   BIC = -117.05

# Testando o modelo SARIMA.3 com t_test:
t_test (Vtr_Consumo_sarima.3)

# Resposta do teste no modelo SARIMA.3:
#          Coeffs     Std.Errors  t Crit.Values    Rej.H0
#ma1      0.6553541   0.09948725    6.587318      1.97882   TRUE
#ma2      0.3436525   0.10652913    3.225902      1.97882   TRUE
#sar1    -0.6284501   0.08784714    7.153905      1.97882   TRUE
#sar2    -0.4349532   0.08578822    5.070081      1.97882   TRUE


# Checando o modelo SARIMA.3 nos gráficos:
checkresiduals(Vtr_Consumo_sarima.3)

# Resposta do plot: SARIMA.3. [Ver Plots]

# Checando a Heterocedasticidade do Modelo SARIMA.3.
ArchTest(Vtr_Consumo_sarima.3$residuals, lags = 36)

# Resposta da checagem do modelo SARIMA.3.

#ARCH LM-test; Null hypothesis: no ARCH effects
# data:  Vtr_Consumo_sarima.3$residuals
# Chi-squared = 32.79, df = 36, p-value = 0.6221

require(normtest)
# Checando a Normalidade do Modelo SARIMA.3 - Jarque-Bera
jb.norm.test(Vtr_Consumo_sarima.3$residuals, nrepl = 2000)

# Resposta da checagem do modelo SARIMA.3.

# Jarque-Bera test for normality
# data:  Vtr_Consumo_sarima.3$residuals
# JB = 62.434, p-value = 5e-04

# Checando a Normalidade do Modelo SARIMA.3 - Shapiro-Wilk
shapiro.test(Vtr_Consumo_sarima.3$residuals)

# Resposta da checagem do modelo SARIMA.3.

# Shapiro-Wilk normality test
# data:  Vtr_Consumo_sarima.3$residuals
# W = 0.94982, p-value = 9.662e-05

# Checando a Normalidade do Modelo SARIMA.3 - histrograma
hist(Vtr_Consumo_sarima.3$residuals)

# Resposta do Histrograma: SARIMA.3. [Ver Plots]

# Checando o summary do Modelo SARIMA.3.
round(summary(Vtr_Consumo_sarima.3$residuals), digits = 3)

# Resposta do o summary do Modelo SARIMA.3.
#     Min.  1st Qu.  Median    Mean    3rd Qu.    Max. 
#   -0.396  -0.028   0.020     0.030    0.101    0.558 


# Criando a função MODA:
moda <- function(v) {uniqv <- unique(v)
uniqv[which.max(tabulate(match(v, uniqv)))]}

# calculando a MODA do Modelo SARIMA.3.
Vtr_Consumo_sarima.3_result <- moda(as.numeric(Vtr_Consumo_sarima.3$residuals))
print(Vtr_Consumo_sarima.3_result)

# Resultado da MODA do Modelo SARIMA.3.
# 0.007359459



########## d.	Verificar a estacionariedade da série. 

#TESTE DE DICKEY-FULLER AUMENTADO: O teste de Dickey-Fuller Aumentado é conhecido na literatura 
#como teste ADF(Augmented Dickey-Fuller) e requer o estudo sobre a seguinte regressão:

adf.test(ts_Vetor_Consumo)
# Resultado do teste de Dickey-Fuller Aumentado.


# Augmented Dickey-Fuller Test
# data:  ts_Vetor_Consumo
# Dickey-Fuller = -8.3097, Lag order = 5, p-value = 0.01
# alternative hypothesis: stationary

adf_ts_Vetor_Consumo <- ur.df(ts_Vetor_Consumo, type = "trend", lags = 36, selectlags = "AIC")

summary(adf_ts_Vetor_Consumo)@teststat
# Resultado do teste de Dickey-Fuller Aumentado.
#               tau3        phi2        phi3
# statistic   -2.253576   2.638518    2.940624

summary(adf_ts_Vetor_Consumo)@cval
# Resultado do teste de Dickey-Fuller Aumentado.
#        1pct    5pct    10pct
#tau3   -3.99   -3.43    -3.13
#phi2    6.22    4.75     4.07
#phi3    8.43    6.49     5.47


#TESTE DE PHILLIPS - PERRON: O teste de Phillips - Perron, conhecido na literatura como teste PP 
#é uma generalização do teste de Dickley - Fuller para os casos em que os erros são correlacionados 
#e, possivelmente,heterocedásticos. 

pp.test(ts_Vetor_Consumo)

# Phillips-Perron Unit Root Test
# data:  ts_Vetor_Consumo
# Dickey-Fuller Z(alpha) = -44.863, Truncation lag parameter = 4, p-value = 0.01
# alternative hypothesis: stationary

pp_ts_Vetor_Consumoc<- ur.pp(ts_Vetor_Consumo, type = "Z-tau", lags = "short")

summary(pp_ts_Vetor_Consumoc)@teststat
# Resultado do teste de Dickley - Fuller.
# -4.452082

summary(pp_ts_Vetor_Consumoc)@cval
# Resultado do teste de Dickley - Fuller.

#                     1pct      5pct     10pct
# critical values -3.480998 -2.883488 -2.578338


#TESTE KPSS: Teste criado por Denis Kwiatkowski , Peter C. B. Phillips, Peter Schmidt e Yongcheol Shin, 
#denominado teste KPSS devido a seus nomes, tem por finalidade determinar estacionariedade em uma série 
#temporal. 

kpss.test(ts_Vetor_Consumo)
# Resultado do teste KPSS.
# KPSS Test for Level Stationarity

# data:  ts_Vetor_Consumo
# KPSS Level = 0.89733, Truncation lag parameter = 4, p-value = 0.01

kpss_ts_Vetor_Consumo <- ur.kpss(ts_Vetor_Consumo, type = "tau", lags = "short")
# Resultado do teste KPSS.

summary(kpss_ts_Vetor_Consumo)@teststat
# Resultado do teste KPSS.
# 0.02101023

summary(kpss_ts_Vetor_Consumo)@cval
# Resultado do teste KPSS.
#                   10pct   5pct   2.5pct   1pct
# critical values   0.119   0.146   0.176   0.216



########## e.	Plotar os correlogramas ACF e PACF.


# Criando o plot o correlograma ACF e PACF. 

acf.ts_Vetor_Consumo <- ggAcf(ts_Vetor_Consumo)
acf.ts_Vetor_Consumo

# Resposta da correlograma ACF. [Ver Plots]


pacf.ts_Vetor_Consumo <- ggPacf(ts_Vetor_Consumo)
pacf.ts_Vetor_Consumo

# Resposta da correlograma PACF. [Ver Plots]


########## f.	Checar os resíduos com os testes de "Portmanteau".

# Testando o modelo SARIMA.1 com Ljung-Box:
Box.test(x = Vtr_Consumo_sarima.1$residuals, lag=36, type = "Ljung-Box", fitdf = 3)

# Resposta do teste no modelo SARIMA.1:
# Box-Ljung test
# data:  Vtr_Consumo_sarima.1$residuals
# X-squared = 25.319, df = 33, p-value = 0.8281


# Testando o modelo SARIMA.2 com Ljung-Box:
Box.test(x = Vtr_Consumo_sarima.2$residuals, lag=36, type = "Ljung-Box", fitdf = 3)

# Resposta do teste no modelo SARIMA.2:
# Box-Ljung test
# data:  Vtr_Consumo_sarima.2$residuals
# X-squared = 36.804, df = 33, p-value = 0.2971


# Testando o modelo SARIMA.3 com Ljung-Box:
Box.test(x = Vtr_Consumo_sarima.3$residuals, lag=36, type = "Ljung-Box", fitdf = 3)

# Resposta do teste no modelo SARIMA.3:
# Box-Ljung test
# data:  Vtr_Consumo_sarima.3$residuals
# X-squared = 60.639, df = 33, p-value = 0.002347


########## g.	Escolher o melhor modelo com base no teste AICc.

# Testando o modelo SARIMA.1:
AIC(Vtr_Consumo_sarima.1)

# Resposta do teste AICc do modelo SARIMA.1.
#-147.4381


# Testando o modelo SARIMA.2:
AIC(Vtr_Consumo_sarima.2)

# Resposta do teste AICc do modelo SARIMA.2.
#-146.4798


# Testando o modelo SARIMA.3:
AIC(Vtr_Consumo_sarima.3) 

# Resposta do teste AICc do modelo SARIMA.3.
#-130.9912


# O melhor resultado do teste AICc é o modelo SARIMA.3



########## h.	Efetuar o forecast para um período até dezembro-2020.

# Forecasting HoltWinters Aditivo
previsao_Vtr_Consumo_aditivo <- forecast(Vtr_Consumo_aj_sazonal, h = 36, level=99)
previsao_Vtr_Consumo_aditivo

# Criando o Plot (gráfico) da previsão HoltWinters Aditivo.
plot(previsao_Vtr_Consumo_aditivo, xlab = 'Ano', ylab = 'Qtd Consumo', 
     main = 'Previsão HoltWinters Aditivo')
abline(h=2500, col="red", lwd = 2)
legend("topleft",c('Consumo de Fertilizantes','Previsão Consumo de Fertilizantes'), lwd = c(2,2), 
       col = c('black', 'blue'),bty ='n', cex = 0.6)


# Resposta do autoplot: previsão HoltWinters Aditivo. [Ver Plots]



# Forecasting HoltWinters Multiplicativo.
previsao_Vtr_Consumo_mult <- forecast(Vtr_Consumo_aj_sazonal_mult, h = 36, level=99)
previsao_Vtr_Consumo_mult


# Criando o Plot (gráfico) da previsão HoltWinters Multiplicativo.
plot(previsao_Vtr_Consumo_mult, xlab = 'Ano', ylab = 'Qtd Consumo', 
     main = 'Previsão HoltWinters Multiplicativo')
abline(h=2500, col="red", lwd = 2)
legend("topleft",c('Consumo de Fertilizantes','Previsão Consumo de Fertilizantes'), lwd = c(2,2), 
       col = c('black', 'blue'),bty ='n', cex = 0.6)


# Resposta do autoplot: previsão HoltWinters Multiplicativo. [Ver Plots]



# Forecasting modelo SARIMA.1. 

previsao_Vtr_Consumo_SARIMA.1 <- forecast(object = Vtr_Consumo_sarima.1, h = 36, level = 99)
previsao_Vtr_Consumo_SARIMA.1

# Criando o Plot (gráfico) da previsão modelo SARIMA.2.
plot(previsao_Vtr_Consumo_SARIMA.1, xlab = 'Ano', ylab = 'Qtd Consumo', 
     main = 'Previsão Modelo SARIMA.1')
abline(h=2500, col="red", lwd = 2)
legend("topleft",c('Consumo de Fertilizantes','Previsão Consumo de Fertilizantes'), lwd = c(2,2), 
       col = c('black', 'blue'),bty ='n', cex = 0.6)


# Resposta do plot: previsão modelo SARIMA.2. [Ver Plots]


# Forecasting modelo SARIMA.2.

previsao_Vtr_Consumo_SARIMA.2 <- forecast(object = Vtr_Consumo_sarima.2, h = 36, level = 99)
previsao_Vtr_Consumo_SARIMA.2

# Criando o Plot (gráfico) da previsão modelo SARIMA.2.
plot(previsao_Vtr_Consumo_SARIMA.2, xlab = 'Ano', ylab = 'Qtd Consumo', 
     main = 'Previsão Modelo SARIMA.2')
abline(h=2500, col="red", lwd = 2)
legend("topleft",c('Consumo de Fertilizantes','Previsão Consumo de Fertilizantes'), lwd = c(2,2), 
       col = c('black', 'blue'),bty ='n', cex = 0.6)


# Resposta do plot: previsão modelo SARIMA.2. [Ver Plots]



# Forecasting modelo SARIMA.3.

previsao_Vtr_Consumo_SARIMA.3 <- forecast(object = Vtr_Consumo_sarima.3, h = 36, level = 99)
previsao_Vtr_Consumo_SARIMA.3

# Criando o Plot (gráfico) da previsão modelo SARIMA.2.
plot(previsao_Vtr_Consumo_SARIMA.3, xlab = 'Ano', ylab = 'Qtd Consumo', 
     main = 'Previsão Modelo SARIMA.3')
abline(h=2500, col="red", lwd = 2)
legend("topleft",c('Consumo de Fertilizantes','Previsão Consumo de Fertilizantes'), lwd = c(2,2), 
       col = c('black', 'blue'),bty ='n', cex = 0.6)


# Resposta do plot: previsão modelo SARIMA.3. [Ver Plots]


### Medidas De Precisão Para Um Modelo De Previsão:

# As medidas calculadas são:
# ME:   Erro médio
# RMSE: Erro ao quadrado médio da raiz
# MAE:  Erro absoluto médio
# MPE:  Erro percentual médio
# MAPE: Erro médio percentual absoluto
# MASE: Erro médio absoluto em escala

# Forecasting HoltWinters Aditivo
accuracy(previsao_Vtr_Consumo_aditivo)

# Resposta do Modelo De Previsão.
#                  ME      RMSE    MAE        MPE      MAPE      MASE      ACF1
# Training set  29.28338 303.502 236.5778 -0.4327932 10.96625 0.7188085 0.1633987


# Forecasting HoltWinters Multiplicativo
accuracy(previsao_Vtr_Consumo_mult)

# Resposta do Modelo De Previsão.
#                   ME      RMSE      MAE       MPE      MAPE      MASE      ACF1
# Training set  32.94386 299.7263 232.0505 -0.4041884 10.44852 0.7050528 0.1822696


# Forecasting modelo SARIMA.1
accuracy(previsao_Vtr_Consumo_SARIMA.1)

# Resposta do Modelo De Previsão.
#                   ME      RMSE      MAE       MPE      MAPE      MASE         ACF1
# Training set  13.39642 228.1329 163.0812 -0.1526279 7.045357 0.4954991 -0.009133378


# Forecasting modelo SARIMA.2
accuracy(previsao_Vtr_Consumo_SARIMA.2)

# Resposta do Modelo De Previsão.
#                   ME      RMSE      MAE       MPE      MAPE      MASE       ACF1
# Training set  15.61655 247.0915 174.0745 -0.2088324 7.610672 0.5289009 0.01270399


# Forecasting modelo SARIMA.3
accuracy(previsao_Vtr_Consumo_SARIMA.3)

# Resposta do Modelo De Previsão.
#                   ME      RMSE      MAE      MPE     MAPE      MASE       ACF1
# Training set  84.12312 271.5044 197.0712 2.269741 8.520979 0.5987732 0.05422768



#FAXER ESSES GRAFICOS
#decompM <- decompose(gas,type="multiplicative")
#autoplot(decompM)

#decompA <- decompose(gas,type="additive")
#autoplot(decompA)




# i.	Conclusão sucinta.









#Comportanmento Sazonal da demanda

seasonplot(gas, 12, col=rainbow(12), year.labels=TRUE, main="Consumo")

boxplot(gas ~ cycle(gas),col="orange",xlab="Trimestre", 
        ylab="Consumo")
title("Consumo gas")




ggtsdisplay(ibmclose)





holt_df <- additive_df
holt_df$linear <- 0
holt_df$linear <- holt_linear_df$value
holt_df$ets <- 0
holt_df$ets <- ets_model_df$value
ggplot(holt_df,aes(YearQtr)) + 
  geom_line(aes(y = value, colour = "Holt - Damped")) + 
  geom_line(aes(y = linear, colour = "Holt - Linear")) +
  geom_line(aes(y = ets, colour = "ETS Model"))






