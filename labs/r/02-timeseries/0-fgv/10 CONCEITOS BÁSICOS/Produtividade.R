#install.packages("pwt8")

library(pwt8)     # Carregando o pacote
help("pwt8.0")    # Descrição do pacote
data("pwt8.0")    # Carregando os dados do pacote
# Variáveis escolhidas são:
# Pib real (rgdpna)
# Média de horas trabalhadas (avg)
# Taxa de cambio (xr)

View(pwt8.0)

br <-subset(pwt8.0, isocode=="BRA", select=c(rgdpna, avh, xr))
br$prod <- br$rgdpna/br$avh
plot(br$prod)

# Criando a ts
#install.packages("fpp2")
library(fpp2)

class(br$prod)

ts.br_prod <- ts(br$prod, frequency = 1, start= 1950)

class(ts.br_prod)

ts.br_prod

autoplot(ts.br_prod)

#Produtivdade nos EUA

us <-subset(pwt8.0, isocode=="USA", select=c(rgdpna, avh, xr))
us$prod <- us$rgdpna/us$avh
ts.us_prod <- ts(us$prod, frequency = 1, start = 1950)
autoplot(ts.us_prod)

# Plotando as comparações (usaremos o tema de gráficos da The Economist)

#install.packages("ggthemes") # Instalar pacote de temas para realçar! 

library(ggthemes) # Load

autoplot(ts.br_prod, series = "Brasil", lwd=2)+
  autolayer(ts.us_prod, series = "USA", lwd=2)+
      xlab("Ano")+
      ylab("Produtividade (PIB/ Média horária trabalhada")+
    theme_economist()+
  ggtitle("Produtividade Brasil vs Estados Unidos")


# Criando o arquivo

produt <- cbind(ts.br_prod, ts.us_prod)

write.table(produt, file='produt.txt')

produt<- read.table('produt.txt')

dados <- ts(produt, start=1950, freq=1)

autoplot(dados, facets = TRUE)+
  xlab("Ano")+
  ylab("Produtividade (PIB/ Média horária trabalhada")+
  theme_economist()+
  ggtitle("Produtividade Brasil vs Estados Unidos")


