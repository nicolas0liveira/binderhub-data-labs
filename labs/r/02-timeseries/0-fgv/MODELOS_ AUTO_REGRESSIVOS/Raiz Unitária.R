#install.packages(c("fpp2", "forecast", "tidyverse","readxl", "urca"))
library(fpp2)
library(forecast)
library(tsibble)
library(readxl)
library(gridExtra)
library(urca)

goog
autoplot(goog)

# Teste de Raiz Unitária

# Ho ´série é estacionária
# H1  série não é estacionária

# Açoes da Google

goog %>% ur.kpss() %>% summary()

# Valor do teste: 10.7223

# Rejeita Ho

Acf(goog)

# Agora por diferença

diff <- diff(goog)
autoplot(diff)

diff %>% ur.kpss() %>% summary()


# Valor do teste: 0,0324

# Aceita Ho.


# Teste para determinar o número apropriado de primeiras diferenças
# é realizado pela função ndiffs() a fim de determinar a estacionariedade.

ndiffs(goog)

# Que está alinhado com o lag 1.
















