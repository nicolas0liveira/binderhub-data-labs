#install.packages(c("fpp2", "tseries", "forecast", "tidyverse", "readxl"))

library(fpp2)
library(tseries)
library(forecast)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(readxl)

uschange

# Utilizando a variavel Consumption

g1 <- autoplot(uschange[,"Consumption"]) +
  xlab("Ano") + ylab("variação% trimestral")+
  ggtitle("Variação% trimestral do Consumo")


g2 <- ggseasonplot(uschange[,"Consumption"], year.labels = TRUE, year.label.left = FALSE)+
  ylab("Comportamento Anual")+ xlab("Trimestres")+
  ggtitle("Variação% trimestral do Consumo por ano")

grid.arrange(g1, g2, ncol = 2)

# Auto.Arima
fit <- auto.arima(uschange[,"Consumption"], seasonal=FALSE)
fit

# Ajustando a projeção

fit%>% 
      forecast(h=10)%>%
        autoplot(include=80)+ # IC 80%
          ylab("Variação% trimestral do Consumo")+
          xlab("Ano")

a1 <- ggAcf(uschange[,"Consumption"],   main=( "ACF"))

a2 <- ggPacf(uschange[,"Consumption"], main =("PACF"))
 
grid.arrange(a1, a2, nrow =2)

# Alternativamente

ggtsdisplay(uschange[,"Consumption"])


# Arima (3,0,0)

#Comparando com Arima()3,0,0) com a anterior Arima(1,0,3) gerada pelo auto.arima()

(fit <- auto.arima(uschange[,"Consumption"], seasonal=FALSE))


(fit2 <- Arima(uschange[,"Consumption"], order=c(3,0,0)))

# Note AICc =  342,08   Arima(1,0,3)
#      AICc =  340,67   Arima(3,0,0)

# Por que o auto.arima() não capturou o arima (3,0,0)

(fit <- auto.arima(uschange[,"Consumption"], seasonal=FALSE,
                   stepwise = FALSE, approximation = FALSE))

# com  stepwise e com approximation encontramos a melhor resposta

# Plot

autoplot(uschange[,"Consumption"],series = "Data" )+
  autolayer(fitted(fit), series = "Fitted", lwd=1)+
  xlab("Ano")+
  ylab("Variação % no Consumo - US")+
  ggtitle("Variação% no Consumo - US (Data x Predição - Arima(3,0,0)")



