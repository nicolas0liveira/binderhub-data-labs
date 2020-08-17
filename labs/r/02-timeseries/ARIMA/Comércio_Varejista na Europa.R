# install.packages("fpp2")
# install.packages("readxl")
# install.packages("tseries")
# install.packages("gridExtra")
# install.packages("seasonal")

library(fpp2)
library(readxl)
library(tseries)
library(gridExtra)
library(seasonal)

euretail

autoplot(euretail)+ 
ylab("Índice de Comércio")+ xlab("Ano")+
ggtitle("Índice de Comércio Varejista - Europa")
                                                      
euretail %>% diff() %>% ggtsdisplay()

euretail %>% diff(lag=4)%>%diff() %>% ggtsdisplay()

fit1 <- auto.arima(euretail, seasonal=TRUE,
                   stepwise = FALSE, approximation = FALSE)

fit1

checkresiduals(fit1)

g1 <- autoplot(euretail, series = "Data" )+
  autolayer(fitted(fit1), series = "Fitted", lwd=1)+
  xlab("Ano")+
  ylab("Índice Comércio Varejista - Europa")+
  ggtitle("Índice de Comércio Varejista - Europa Data x Modelo Arima(0,1,3) (0,1,1)[4]")

g2 <- autoplot(forecast((fit1)))

fit1$residuals

grid.arrange(g1, g2, nrow = 2)

Box.test(fit1$residuals, lag= 4, type = "Lj")
