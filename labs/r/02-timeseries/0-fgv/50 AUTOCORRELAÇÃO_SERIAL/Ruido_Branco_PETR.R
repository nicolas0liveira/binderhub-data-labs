# Ruído branco
#install.packages(c("fpp2",))
library(fpp2)
library(gridExtra)

# Simulação

set.seed(1234)

n <- 100

# Geração de 100 variáveis aleatórias N iid (0,1)
# wn = white noise

wn <- rnorm(n)  # white noise
wn
class(wn)

#Transformando wn em ts
wn.ts <- ts(wn)
class(wn.ts)
# Plotando
autoplot(wn.ts)

1.96/(n)^0.5

# Note que para o ruído branco , esperamos que 95% dos picos na ACF 
# fiquem dentro do intervalo mais ou menos 1.96/ raiz de T  , onde T 
# é o comprimento da série temporal.

1.96/(n^(0.5))

# = mais ou menos 0.196. Veja o grafico

Acf(wn.ts)

# Todos os coeficientes de autocorrelação estão dentro desses limites, 
# indicando visualmente que os dados são ruído branco. 

# Vamos a um um caso real

#install.packages("quantmod")

library(quantmod)

getSymbols('PETR4.SA')

# Omitindo as NAs

PETR4.SA_clean <- na.omit(PETR4.SA["2015-01-02/2019-06-03"]) # datas limites
PETR4.SA_clean <- PETR4.SA[index(PETR4.SA_clean)]

nrow(PETR4.SA_clean)
#Temos 1097 observações

petr_close <- as.numeric(PETR4.SA_clean$PETR4.SA.Close)

class(petr_close)

petr_ts <- ts(petr_close)
class(petr_ts)

autoplot(petr_ts)+ ggtitle("PETR4 - Cotações Obtidas no Yahoo_Finance")

# Autocorrelação
Acf(petr_ts)

# A série é estacionaria? ESta tudo muito longo do intervalo
#Ver linha pontilhada em azul

# Altere a data, exemplo: 1980-01-02 e efetue o script

# Vamos aos retornos, onde retorno = [(Preço hoje/ Preço ontem)-1]

retorno <- diff(log(petr_close))
class(retorno)
retorno <- as.data.frame(retorno)
nrow(retorno)
retorno_ts <- ts(retorno)
autoplot(retorno_ts)

1.96/(1064^0.5)

Acf(retorno_ts, main="Correlograma dos Retorno")  

#E agora?
#Uma série de ruídos brancos visualmente é estacionária.

##################################################################
# Teste de Raiz Unitária

#Uma maneira de determinar mais objetivamente se a diferenciação é 
# necessário usar um teste de raiz unitária. 

#Estes são testes de hipótese estatística de estacionariedade, 
#projetados para determinar se a diferenciação é necessária.

#Estão disponíveis vários testes de raiz unitária, baseados em 
#diferentes suposições e que podem levar a respostas conflitantes. 

# Em nossa análise, usamos o teste de 

# Kwiatkowski-Phillips-Schmidt-Shin (KPSS)

#Nesse teste, a hipótese nula é que os dados são estacionários 
#e procuramos evidências de que a hipótese nula é falsa. 

# O teste pode ser calculado usando a função ur.kpss () do pacote urca.

#install.packages("urca")

library(urca)

# teste de Kwiatkowski-Phillips-Schmidt-Shin (KPSS)

# Ho ´série é estacionária
# H1  série não é estacionária

# Ações da PETR4

petr_ts %>% ur.kpss() %>% summary()

# Value of test-statistic is: 10.4244 > 0.463 a 5%. Até mesmo a 1%.
# Rejeita Ho.

#Com relação ao retorno do papel

retorno_ts %>% ur.kpss() %>% summary()

# Value of test-statistic is: 0.0653 < 0.463 ao nível de 5%

# Não rejeita Ho. A série retorno é estacionária.

############################################################
#  ADF
############################################################

#install.packages("tseries")
library(tseries)

# A hipótese nula p é que os dados não são estacionários. 
# Queremos REJEITAR a hipótese nula para este teste, 
# portanto, queremos um valor p menor que 0,05 (ou menor).

#Para as PETR4
adf.test(petr_ts)

# p-value = 0.1655 não rejeitamos a hipótese nula ou seja os dados não 
# saão estacionários.

# Para o retorno
adf.test(retorno_ts)

#In adf.test(retorno_ts) : p-value smaller than printed p-value (p-value < 0.01)

# Rejeitam-se Ho, a série é estacionária,

############################################################
#Phillips-Perron
############################################################

# A hipótese nula p é que os dados não são estacionários. 
# Queremos REJEITAR a hipótese nula para este teste, 
# portanto, queremos um valor p menor que 0,05 (ou menor).

# Para as ações da PETR4
pp.test(petr_ts)
#p-value = 0.2912. Não rejeita Ho. Série Não estacionária
 
#Para os retornos
pp.test(retorno_ts)
#In pp.test(retorno_ts) : p-value smaller than printed p-value. Rejeita Ho. 



