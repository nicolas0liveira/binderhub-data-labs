install.packages(c("fpp", "mFilter", "forecast"))

library(fpp)
library(mFilter)
library(forecast)

# Dados IPEADATA
# Consumo de energia elétrica na Região Sudeste (SE): quantidade
# Frequência: Mensal de 1979.01 até 2020.02
# Fonte: Eletrobras
# Unidade: GWh
# Comentário: Boletim SIESE - Quadro: Consumo total de energia elétrica - Brasi

dados <- read.csv2("consumo_energia.csv")

anyNA(dados)

head(dados)
tail(dados)

consumo <- ts(dados[,2], start=c(1979,1), frequency = 12)
class(consumo)

autoplot(consumo, lwd=2)

decomp <- decompose(consumo, type = "additive")
autoplot(decomp)


autoplot(consumo, xlab='mês', ylab = 'Consumo de Energia Elétrica - Gwh',
     main = "Consumo de Energia Região SE")

ajuste_hw <- HoltWinters(consumo, gamma=FALSE)

ajuste_hw

plot(consumo, xlab='mês', ylab = 'Consumo Energia Elétrica Gwh',
     main = "Região Sudeste")


lines(fitted(ajuste_hw)[,1], lwd=2, col = 'red')

legend("topleft", c("Consumo de Energia Eletrica", "Ajuste SEHolt"),
       lwd=c(1,2), col = c("black", "red"), bty='n', cex = 1)

#Previsão

prev_holt <- forecast(ajuste_hw, h=12, level=99)
rev_holt

# jan-20 : 20.108
# fev-20 : 19.952
# mar-20 : 18.026

(19785.81/20108)-1

(19803.20/19952)-1

(19820.59/18026)-1

(18057.98/18026)-1

autoplot(consumo, xlab = 'tempo', ylab = 'valores observados',
     main = "Consumo de Energia Elétrica GhW - Região Sudeste")


##################################

# Com o fpp2 

holt.consumo <- holt(consumo, h = 12)

holt.consumo$model

holt.consumo$fitted

library(fpp2)

autoplot(consumo, serie="Consumo")+
          autolayer(holt.consumo$fitted, serie= "SE Holt")+
          autolayer(holt.consumo, serie="Previsão h = 12")+
          xlab("Mês")+
          ylab("valores observados/ previstos")+
          ggtitle("Consumo de Energia Elétrica GhW - Região Sudeste")

