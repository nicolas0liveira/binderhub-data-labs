## Arquivo: MODELOS SAR E GWR_final.R

#########################################################
#### Estatística Espacial - turma Brasília T8        ####
#### MBA Business Analytics e Big Data - 2o Sem 2020 ####
#### Eduardo de Rezende Francisco                    ####
#########################################################

#############################################################################
###      EXPLORAÇÃO DE MODELOS DE REGRESSÃO ESPACIAL (SAR E GWR) NO R     ###
###                                                                       ###
###### Códigos baseados na Tese de Doutorado de                           ###
###### Eduardo de Rezende Francisco - FGV-EAESP - Abril/2010 :            ###
#############################################################################
##         INDICADORES DE RENDA BASEADOS EM CONSUMO DE ENERGIA             ##
##             ELÉTRICA: ABORDAGENS DOMICILIAR E REGIONAL                  ##
##               NA PERSPECTIVA DA ESTATÍSTICA ESPACIAL                    ##
#############################################################################

## Limpa o workspace
## (libera memória de eventuais objetos manipulados antes)
rm(list=ls())

# load MAPTOOLS, SPGWR and SPDEP packages (extensions) in R environment
#library(maptools)
library(rgdal)
library(spgwr)
library(spdep)

# change working directory
setwd("C:/temp")

# Source file: AREAP_SP.CSV (456 Weighted Areas in Sao Paulo City)
# "ap" is the variable that points to the input table
# (ap is the abbreviation of weighted areas in Portuguese)
ap <- read.csv("areacens_sp.csv")

# select some columns (5) in the input table and still point to ap
ap <- as.data.frame(cbind(ap$ID,ap$RENDA,ap$ENERGIA,ap$XCENTR,ap$YCENTR))

# rename these 5 columns
colnames(ap) <- c("ID","Income","Energy","X","Y")

# Map of São Paulo's weighted areas
#poligonos <- readShapePoly("areacens_sp.shp")
poligonos <- rgdal::readOGR(dsn="c:/temp",layer="areacens_sp")
plot(poligonos)

# calculate global residual SST (SQT)
SST <- sum((ap$Income - mean(ap$Income))^2)


############################################################################
## REGRESSÃO LINEAR SIMPLES ################################################
############################################################################

# "lm" is the function used to fit linear models
# "Income ~ Energy" is the way you explicit regression formulas in R
# (Income is the dependent and Energy is one (the only one) independent var)
lm.ap <- lm(Income ~ Energy,data=ap)
lm.ap
summary(lm.ap)

# store the residuals (the response minus fitted values of the model)
OLS_SSE <- sum(lm.ap$residuals^2)

# calculate R2 of the global model and store in "results.ap" variable
r2_OLS <- 1 - (OLS_SSE/SST)
r2_OLS

############################################################################
## GWR - Geographically Weighted Regression ################################
## (AIC com Gaussian Weight) ###############################################
############################################################################

# define coords (X and Y coordinates)
coords <- cbind(ap$X,ap$Y)
colnames(coords) <- c("X","Y")

# Calcula largura de banda (em % de registros)
bwGauss <- gwr.sel(Income~Energy,data=ap,coords=coords,adapt=TRUE,method="aic",
                   gweight=gwr.Gauss,verbose=FALSE)

# Aplica GWR
# "gwr" is the function that implements GWR in R
# Some parameters:
# gweight: defines the geographical weighting function (gauss or bisquare)
# adapt: if NULL the kernel is in "Fixed" type (bandwidth is in distance)
#   if between 0 and 1 is the bandwidth for "Adapt" (k nearest neighbours)
# hatmatrix: if TRUE, return the hatmatrix as a component of the result
gwr.ap <- gwr(Income ~ Energy,data=ap,coords=coords,bandwidth=bwGauss,
              gweight=gwr.Gauss,adapt=bwGauss,hatmatrix=TRUE)
gwr.ap
 
GWR_SSE <- gwr.ap$results$rss
r2_GWR <- 1 - (GWR_SSE/SST)
r2_GWR

############################################################################
# SAR - Spatial Autoregressive lag model ###################################
# (k nearest neighbours [k from AIC Gaussian minimisation]) ################
############################################################################

kGauss <- round(bwGauss * length(ap[,1]))
 
# create spatial weights using k nearest neighbours (knearneigh command)
# and convert to a W matrix style (knn2nb and nb2listw commands)
myknn <- knearneigh(coords,k=kGauss,longlat=FALSE,RANN=FALSE)
mynb <- knn2nb(myknn,sym=TRUE)
mylistw <- nb2listw(mynb,style="W")

# "lagsarlm" is the function that implements SAR Lag model in R
sar.ap <- lagsarlm(Income ~ Energy,data=ap,mylistw,method="Matrix")
  
# store RSS and R2 of the SAR lag model
SARk_SSE <- sar.ap$SSE
r2_SARk <- 1 - (SARk_SSE/SST)
r2_SARk

############################################################################
# SAR - Spatial Autoregressive lag model ###################################
# (polygons - contiguous boundaries' neighbourhood - 1st ORDER) ############
############################################################################

# create spatial weights using adjacency between polygons(regions with
# contiguous boundaries - poly2nb command) of areap_sp shapefile
# and convert to a listw object (nb2listw command)
mynb <- poly2nb(poligonos)
mylistw <- nb2listw(mynb,style="W")

# "lagsarlm" is the function that implements SAR Lag model in R
sar.ap <- lagsarlm(Income ~ Energy,data=ap,mylistw,method="Matrix")
  
# store RSS and R2 of the SAR lag model
SAR_SSE <- sar.ap$SSE
r2_SAR <- 1 - (SAR_SSE/SST)
r2_SAR

############################################################################
# GWR Global com SAR Local #################################################
# (SAR com polygons - contiguous boundaries' neighbourhood - 1st ORDER) ####
############################################################################

residuos <- numeric()
previstos <- numeric()
ParW <- numeric()
ParIntercepto <- numeric()
ParEnergia <- numeric()
r2_local <- numeric()

# considera a vizinhança de 1a ordem para a criação da lista de vizinhos
mynb <- poly2nb(poligonos)

for (i in 1:length(ap[,1]))
{
  if (i == 419) {
    residuos[i] <- 0
    r2_local[i] <- 1
    next
  }

  # seleciona e ordena os índices dos polígonos vizinhos de i
  vizinhos_i <- sort(c(i,mynb[[i]]))

  # seleciona o slot "polygons" (lista de polígonos) dos vizinhos de i
  listapoly_vizinhos_i <- slot(poligonos,"polygons")
  poligonos_i <- subset(listapoly_vizinhos_i, ap[,1] %in% vizinhos_i)

  # converte apenas i e seus vizinhos para o objeto SpatialPolygons
  sp_i <- SpatialPolygons(poligonos_i)

  # cria os objetos nb e listw apenas para i e seus vizinhos
  mynb_i <- poly2nb(sp_i)
  mylistw_i <- nb2listw(mynb_i,style="W",zero.policy=TRUE)
 
  ap_i <- ap[vizinhos_i,]
  
  # calculate global residual SS of local sample
  sum_square_glo_i <- sum((ap_i$Income - mean(ap_i$Income))^2)
  
  sar.ap_i <- lagsarlm(Income ~ Energy,data=ap_i,mylistw_i,
                       method="Matrix",zero.policy=TRUE)
  
  residuos[i] <- sar.ap_i$residuals[ap_i$ID==i]
  previstos[i] <- sar.ap_i$fitted.values[ap_i$ID==i]
  ParW[i] <- sar.ap_i$rho
  ParIntercepto[i] <- sar.ap_i$coefficients[1]
  ParEnergia[i] <- sar.ap_i$coefficients[2]
  r2_local[i] <- 1 - (sar.ap_i$SSE/sum_square_glo_i)
  print(i)
}

# calculate R2 of the global model
GWRSAR_SSE <- sum(residuos^2)
r2_GWRSAR <- 1 - (GWRSAR_SSE/SST)
r2_GWRSAR



## COMPARA DESEMPENHO DOS MODELOS

r2_OLS
r2_GWR
r2_SARk
r2_SAR
r2_GWRSAR
