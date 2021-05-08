## Arquivo: EXEMPLO_POINT PATTERN ANALYSIS_ESCOLAS.R

#########################################################
#### Estatística Espacial - turma Brasília T8        ####
#### MBA Business Analytics e Big Data - 2o Sem 2020 ####
#### Eduardo de Rezende Francisco                    ####
#########################################################

########################################################################
###        Exemplo de ESDA - Exploratory Spatial Data Analysis       ###
########################################################################

# install.packages("maptools")
#require(maptools)

install.packages("shapefiles")
require(shapefiles)

install.packages("aspace")
require(aspace)

install.packages("rgdal")
require(rgdal)

# carrega o shapefile AREACENS.SHP
#shape_empresas <- readShapePoints("c:/temp/tema_escolas.shp")
shape_empresas <- rgdal::readOGR(dsn="c:/temp",layer="tema_escolas")


summary(shape_empresas)
plot(shape_empresas)

# separa em um data.frame apenas as coordenadas X e Y
empresas <- cbind(shape_empresas$X,shape_empresas$Y)
empresas

# calcula a Standard Deviational Ellipse das empresas
# e guarda resultado no objeto global r.SDE (sdeloc, sdeatt)
calc_sde(id=1, filename="empresas_Output.txt", centre.xy=NULL, calccentre=TRUE,
         weighted=FALSE, weights=NULL, points=empresas, verbose=FALSE)

# desenha a elipse
plot_sde(plotnew=TRUE, plotSDEaxes=FALSE, plotweightedpts=FALSE,
         plotpoints=TRUE, plotcentre=TRUE,xaxis="Longitude",
         yaxis="Latitude", titletxt="", sde.col="red",
         points.col="light gray",points.pch=18)

plot_sde(plotnew=FALSE, plotSDEaxes=FALSE, plotweightedpts=FALSE,
         titletxt="Elipse de Desvio Padrão das Escolas",
         plotpoints=FALSE, plotcentre=TRUE, sde.col="red")

# exporta a elipse para um shapefile
# Último parâmetro Ã©: 1-point, 2-polyline, 5-polygon
shape_elipse <- convert.to.shapefile(sdeloc, sdeatt, "id", 5)
write.shapefile(shape_elipse,"c:/temp/elipse_escolas",arcgis=T)




