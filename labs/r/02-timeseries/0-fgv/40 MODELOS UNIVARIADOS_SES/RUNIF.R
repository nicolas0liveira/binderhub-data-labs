#install.packages("fpp", "fpp2")
library(fpp)
library(fpp2)

set.seed(12344) #para reproducibilidade

serie <- ts(runif(100, 10, 15), start(1915,1), frequency=1)
autoplot(serie)
#ts com 100 anos, v.a. entre 10 e 15, início em 1915, intervalo ano, frequência anual

mean(serie)
# A série esta em torno de um determinado nível de 12,35443

# A Suavização Exponencial Simples

ajuste <- HoltWinters(serie, beta = FALSE, gamma = FALSE)
ajuste

plot(ajuste, xlab='tempo', ylab= 'valores observados/ ajustados',
     main = 'Série Temporal')

#install.packages("forecast")

library(forecast)

ajuste_pred <- forecast(ajuste, h=10, level=95)

ajuste_pred

autoplot(ajuste_pred, main="Suavização Exponencial Simples", xlab="ano", ylab = 'dado')

