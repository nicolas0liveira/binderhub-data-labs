install.packages(c("Quandl", "fpp","fpp2", "xlsx"))

library(Quandl) #  data scraping
library(fpp)
library(fpp2)
library(xlsx)
library(gridExtra)

# https://fred.stlouisfed.org/series/NATURALGAS  (note que a série está em mes. 
# Vou transformá-la em trimestre)

 gas<-Quandl(code = "FRED/NATURALGAS",
            collapse="quarterly", # tranformando a série em trimestres.
             type = "ts",
             end_date = "2020-03-31")

 
gas
class(gas)
 

# natural_gas <- read_excel("natural_gas_US.xlsx")

head(gas)
tail(gas)

#gas <- natural_gas$Consumo

#class(gas)

library(xlsx) # Conveter a série em xlsx. nem precisava, mas para enviar para a turma
write.xlsx(gas,"natural_gas_US.xlsx")


##################################################
# Transformando em TS
##################################################

ts.gas <- ts(gas, frequency = 4, start = c(2000,01))

autoplot(gas)+
  ggtitle("Gas_Natural_US_Consumo")+
  xlab("Ano")+
  ylab("Bilhões de Pés Cúbicos")

#1 pé cúbico = 25,3147 pés cúbicos
# help(seasonplot)

ggseasonplot(ts.gas, year.labels = TRUE, year.label.left = TRUE)+
  ylab("Consumo em pés cúbicos")+ xlab("Trimestre")+
    ggtitle("Consumo de Gás Natural em pés cúbicos - EUA")


boxplot(ts.gas~cycle(ts.gas),xlab="trimestres",
           ylab = "Consumo em bilhões de pés cúbicos )" ,
              col="blue", main ="Boxplot-Consumo de Gás Natural -2000-2019")


# Trabalho em Grupo
# Decompor a serie
# Suaviazação Exponencial Simples
# Modelo Holt
# Efetuar maodelagem com Holt Winter Aditivo e Mutiliplicativo
# Plotar os gráficos
# Qual é o melhor modelo?
# Previsão para 12 trimestres
