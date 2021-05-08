## Arquivo: EXEMPLO_GEOCODING.R

#########################################################
#### Estatística Espacial - turma Brasília T8        ####
#### MBA Business Analytics e Big Data - 2o Sem 2020 ####
#### Eduardo de Rezende Francisco                    ####
#########################################################

###############################################################################
###      Exemplo de uso do R para Geocoding e Mapping - Extensão GGMAP      ###
###############################################################################

# Extensão para plotagem de mapas com ggplot e algumas análises básicas
# (inclui geocode, através de chamada a API do Google Maps)
require(ggmap)

# Exemplo de geocoding
geocode("R. Apotribu 150, Sao Paulo")
x <- geocode("R. Apotribu 150, Sao Paulo, SP, Brasil",output="latlona")

# Geocodifica as 30 primeiras empresas da tabela
tab_empresas <- read.csv("C:/temp/empresas.csv",sep=";",dec=",")
tab <- tab_empresas[1:20,]
tab$lat <- rep(0,20)
tab$long <- rep(0,20)

for (j in 1:20) {

  endereco <- paste("CEP ",tab$CEP[j]," - Brasil",sep='')
  geocodigo <- geocode(endereco,output="latlona")
  
  tab$lat[j] = as.numeric(geocodigo$lat)
  tab$long[j] = as.numeric(geocodigo$lon)
  
}

# Desenha um mapa de SP
sp.map <- get_map(location=c(lon=-46.65,lat=-23.58),zoom=12, maptype = c("hybrid"))
sp.map <- get_map(location=c(lon=-46.65,lat=-23.58),zoom=11, maptype = c("hybrid"))
sp.map <- get_map(location=c(lon=-46.65,lat=-23.58),zoom=10, maptype = c("hybrid"))

ggmap(sp.map)

# Desenha o mapa com as empresas geocodificadas
ggmap(sp.map, base_layer = ggplot(aes(x = long, y = lat), data = tab), extent = "device") +
geom_point(aes(x = long, y = lat, colour = "red"), data = tab, alpha = .5)

ggmap(sp.map, extent = "device") + geom_point(aes(x = long, y = lat, colour = "red"), data = tab, alpha = .5)
