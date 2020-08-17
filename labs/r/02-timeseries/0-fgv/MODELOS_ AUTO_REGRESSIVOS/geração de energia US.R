#install.packages(c("fpp2", "forecast", "tidyverse","readxl"))
library(fpp2)
library(forecast)
library(tsibble)
library(readxl)
library(gridExtra)

usmelec
autoplot(usmelec)

cbind(
      "Bilhões kWh" = usmelec,
      "Log" = log(usmelec),
      "Variação em log" = diff(log(usmelec),12),
      
      "lag 2 log" =
        diff(diff(log(usmelec),12),1)) %>%
  
  autoplot(facets=TRUE) +
  xlab("Ano") + ylab("") +
  ggtitle("Geração de Energia nos US - GwW Bilhões")

Acf(diff(log(usmelec),12))
Acf(diff(diff(log(usmelec),12),1))
