install.packages("TTR", repos="http://R-Forge.R-project.org")
install.packages("xlsx", type = "source", repos = "http://cran.rstudio.com")
install.packages("xts")
install.packages("tidyverse")
install.packages("quantmod")
install.packages("TSstudio")
install.packages("car")


library(rvest)
library(stringr)
library(forcats)
library(lubridate)
library(plotly)
library(dplyr)
library(PerformanceAnalytics)
library(xts)
library(tidyverse)
library(quantmod)
library(car)
library(TTR)
library(TSstudio)

start <- as.Date("2000-02-07")
end <- as.Date("2020-06-01")  

getSymbols('DCOILWTICO', src = 'FRED', from = start, to=end)

DCOILWTICO_clean <- na.omit(DCOILWTICO["2000-02-07/2020-06-01"]) 

oil <- DCOILWTICO_clean

summary(oil)

# Calculando o retorno instantÃ¢neo

oil.ret <- diff(log((oil)))

# Altenativamente

ret <- dailyReturn((DCOILWTICO_clean), type='log')

# Graph oil & ret.oil
par(mfrow=c(2,1))
plot(oil, col="blue")
plot(ret, col="red")
par(mfrow=c(1,1))

#A autocorrelaÃ§Ã£o serial

acf(oil, main="ACF Petroleo")

acf(ret, main="Por Diferença")

#Calculando mÃ©dia e desvio padrÃ£o dos retornos

m=mean(ret);s=sd(ret);

#Histograma do retorno e DistribuiÃ§Ã£o Empirica do retoro

par(mfrow=c(1,2))
hist(ret, nclass=40, freq=FALSE, main='Histogram');curve(dnorm(x,
                                                               mean=m,sd=s), from = -0.3, to = 0.2, add=TRUE, col="red")
plot(density(ret), main='Empirical Distribution');curve(dnorm(x,
                                                              mean=m,sd=s), from = -0.3, to = 0.2, add=TRUE, col="red")
par(mfrow=c(1,1))

mu <-  m # mean of log returns
sigma <- s # sd of log returns 

p0 = 58.84 #MEDIANA

# numero de simulaÃ§Ãµes 

N <- 252
r <- rnorm(N, mu, sigma)
log_price <- log(p0)+cumsum(r)
prices <- exp(log_price)
plot(prices, type="l", col="blue", main="SMC")

####################################################################

#V?rios passeios aleat?rios

start <- p0
m <- mu
sigma <- sigma
n.passos <- 252  #252 dias uteis 
n.random <- 1000    # numero de passeios aleat?rios
# Criando uma matriz vazia
WALK <- matrix(NA, nrow = n.random, ncol = n.passos)

# Come?ando o passeio
WALK[, 1] <- start

# Passeios 
for(i in 2:n.passos){
  WALK[, i] <- exp(log(WALK[, i - 1]) + (rnorm(n.random, m, sigma)))  
  print(i)
}


plot(NA, ylim = c(min(WALK), max(WALK)), xlim = c(0, n.passos), cex.main = 1.0, cex.lab = 1.0,
     xlab = "Tempo", ylab = "Barril WTI ($)", las = 1, font.lab = 2, main = "Random Walk")

for(i in 1:nrow(WALK)){
  lines(WALK[i,], col = rainbow(n.random)[i])
}


par(mfrow=c(1,2))
hist(WALK, nclass=40, freq=FALSE, main='Random Walk Histogram');curve(dnorm(x,
                                                                            mean=m,sd=s), from = -0.3, to = 0.2, add=TRUE, col="red")
plot(density(WALK), main='Random Walk Empirical Distribution');curve(dnorm(x,
                                                                           mean=m,sd=s), from = -0.3, to = 0.2, add=TRUE, col="red")
par(mfrow=c(1,1))

mean(WALK)
sd(WALK)

#install.packages("tigerstats")
library(tigerstats)

pnormGC(12, region="below", mean=mean(WALK),
        sd= sd(WALK) ,graph=TRUE)

qnormGC(0.002105138, region="below", mean=mean(WALK),
        sd= sd(WALK) ,graph=TRUE)

