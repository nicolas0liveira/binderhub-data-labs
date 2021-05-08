## Arquivo: EXEMPLO_RGDAL.R

#########################################################
#### Estatística Espacial - turma Brasília T8        ####
#### MBA Business Analytics e Big Data - 2o Sem 2020 ####
#### Eduardo de Rezende Francisco                    ####
#########################################################

########################################################################
###      Exemplo de Exploração de Mapas com a extensão MAPTOOLS    ###
########################################################################

# carrega a extensão RGDAL

#install.packages("maptools")
#require(maptools)
install.packages("rgdal")
require(rgdal)

# carrega o shapefile AREACENS.SHP

# método anterior, baseado no package "mapping"
# ac <- readShapePoly("c:/temp/areacens_sp.shp")

# método baseado no package rgdal
#dsn <- system.file("c:/temp/areacens_sp.shp", package = "rgdal")[1]
ac <- rgdal::readOGR(dsn="c:/temp",layer="areacens_sp")
plot(ac)

# seleciona as Ã¡reas com renda mÃ©dia menor ou igual a R$ 1000
# e desenha de vermelho
ac_ate1000 <- ac[ac$RENDA <= 1000,]
plot(ac_ate1000,col="red",add=T)
