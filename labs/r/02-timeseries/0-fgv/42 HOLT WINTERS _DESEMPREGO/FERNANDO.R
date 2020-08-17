install.packages(c("fpp", "fpp2"))

library(fpp)
library(fpp2)

# Dados do Ipeadata 
#Taxa de desemprego aberto na RMSP  Mensal de 1984.12 até 2019.06

desemprego <- read.csv2('desemprego.csv', sep=';')

summary(desemprego)

head(desemprego)
tail(desemprego)

# Separando o vetor que nos interessa

desemp <- desemprego$Taxa_desemprego
desemp

# Criando a TS:

ts_desemprego <- ts(desemp, start = c(1984,12), frequency = 12)
ts_desemprego

# Plot

autoplot(ts_desemprego, xlab='mês', ylab = 'taxa_desemprego %', main ='Taxa_ Desemprego: RMSP')

# função HoltWinters()  - parâmetros de suavização - HW Aditivo

ajuste_sazonalidade <- HoltWinters(ts_desemprego, seasonal = "additive")

ajuste_sazonalidade

plot(ts_desemprego, xlab='mês', ylab = 'taxa_desemprego %', main =' Taxa_ Desemprego: RMSP')

lines(fitted(ajuste_sazonalidade)[,1], lwd=1.2, col='red')

legend("topleft",c('Taxa de Desemprego','Ajuste HW Aditivo' ), lwd = c(1,1), col = c('black', 'red'), bty ='n',
       cex=0.8
)

# Forecasting

prev_hw <- forecast(ajuste_sazonalidade, h = 9, level=95)
prev_hw


plot(prev_hw, xlab ='mês', ylab= 'Taxa de Desemprego', 
     main = 'Taxa_ Desemprego: RMSP')

# CONSTRUINDO O MODELO HW MULTIPLICATIVO

ajuste_sazonalidade_mult <- HoltWinters(ts_desemprego, seasonal = "multiplicative")

ajuste_sazonalidade_mult

plot(ts_desemprego, xlab='mês', ylab = 'taxa_desemprego %', 
     main =' Taxa_ Desemprego: RMSP')

lines(fitted(ajuste_sazonalidade_mult)[,1], lwd=2, col='red')

legend("topleft", c('Taxa de Desemprego','Ajuste HW_Mult'), lwd = c(1,1), col = c('black', 'red'),
       cex=0.8)

# Forecasting

prev_hw_mult <- forecast(ajuste_sazonalidade_mult, h = 6, level=95)

plot(prev_hw_mult, xlab ='mês', ylab= 'Taxa de Desemprego', 
     main = 'Taxa_ Desemprego: RMSP', ylim=c(4,20))

# Repete o script

plot(ts_desemprego, xlab='mês', ylab = 'taxa_desemprego %', main =' Taxa_ Desemprego: RMSP')

lines(fitted(ajuste_sazonalidade)[,1], lwd=1.2, col='red')

lines(fitted(ajuste_sazonalidade_mult)[,1], lwd=1.2, col ='blue')

legend("topleft",c("Taxa de Desemprego","Ajuste HW Ativido", "Ajuste HW Multiplicativo"),
       lwd = c(2,1,1), col = c('black', 'red', 'blue'), bty ='n', cex=0.8)

####################
# COm o pacote fpp2
####################

autoplot(ts_desemprego)

#Comportanmento Sazonal da demanda

seasonplot(ts_desemprego, 12, col=rainbow(12), year.labels=TRUE, main="Desemprego")

boxplot(ts_desemprego ~ cycle(ts_desemprego),col="orange",xlab="Mês", 
        ylab="Taxa %")
title("Boxplot Desemprego")

# Decomposição
library(gridExtra)

d1 <- ts_desemprego%>%
  decompose(type= "additive")%>%
  autoplot() + xlab("Ano")+
  ggtitle("Decomposição Aditiva")

d2 <- ts_desemprego%>%
  decompose(type= "multiplicative")%>%
  autoplot() + xlab("Ano")+
  ggtitle("Decomposição Multiplicativa")

grid.arrange(d1,d2,ncol = 2)

# HOT_WINTER

fit.HW_addit <- hw(ts_desemprego,seasonal="additive", h = 24)

autoplot(ts_desemprego) +
  autolayer(fit.HW_addit$fitted, series= "HW_Aditivo h=12", PI = FALSE)+
  autolayer(fit.HW_addit, series="Previsão HW Aditivo h =12",pi = FALSE)+
  ggtitle("Taxa de Desemprego RMSP - HW Aditivo")


fit.HW_mult <- hw(ts_desemprego,seasonal="multiplicative")

autoplot(ts_desemprego) +
  autolayer(fit.HW_mult$fitted, series= "HW_Multiplicativo")+
  autolayer(fit.HW_mult, series="Previsão HW Multiplicativo h =12", PI = FALSE)+
  ggtitle("Taxa de Desemprego RMSP - HW Multiplicativo")

#https://www.rdocumentation.org/packages/forecast/versions/8.12/topics/plot.forecast

autoplot(ts_desemprego) +
  autolayer(fit.HW_addit, series="HW aditivo", PI = FALSE) +
  autolayer(fit.HW_mult, series="HW multiplicativo", PI = FALSE) +
  xlab("Mês") +
  ylab("Taxa de Desemprego)") +
  ggtitle("Taxa de Desemprego -RMSP- Holt-Winters ") +
  guides(colour=guide_legend(title="Forecast"))

accuracy(fit.HW_addit)

accuracy(fit.HW_mult)




