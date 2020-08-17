# Exemplo do livro: Forecasting : Principals & Practice Hyndman & Athanasoupoulos
# Previsão de Produção de petróleo na Arábia Saudita
# Com o uso da Suavização Exposnencial Simples

# Utilizaremos o pacote fpp2

library(fpp2)
data(oil)
oil

help(oil)

# Utilizando um subconjunto a partir de 1996 (função window)

oildata <- window(oil, start= 1996)
autoplot(oildata)+
  xlab("Ano")+
         ylab("Petróleo (mihões de toneladas")

#fpp HoltWinter beta = False, Gammma = False
# fpp2 ses

forecast <- ses(oildata, h = 5, level = 99)
forecast$model
forecast$series
round(accuracy(forecast), 2)

autoplot(forecast)+
  autolayer(fitted(forecast), series = "Fitted")+
  xlab("Ano")+
  ylab("Petróleo (mihões de toneladas")

forecast

oil
