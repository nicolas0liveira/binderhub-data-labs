## Arquivo: EXEMPLO_COMPLETO.R

#########################################################
#### Estatística Espacial - turma Berrini T2         ####
#### MBA Business Analytics e Big Data - 2o Sem 2020 ####
#### Eduardo de Rezende Francisco                    ####
#########################################################

#############################################################################
###      EXPLORAÇÃO NO R DE FUNÇÕES DE EXPLORAÇÃO E ANÁLISE ESPACIAL      ###
#############################################################################

## Limpa o workspace
## (libera memória de eventuais objetos manipulados antes)

rm(list=ls())

## Instala packages (extensões)
## (melhor utilizar a opção "Instalar pacotes a partir de arquivos zip locais")
## ATENÇÃO: Rodar apenas uma vez os comandos abaixo para instalação das extensões

install.packages("maps")
install.packages("maptools")
install.packages("sp")
install.packages("rgeos")
install.packages("spdep")
install.packages("gstat") 
install.packages("splancs") 
install.packages("spatstat")  
install.packages("lattice")  
install.packages("pgirmess")   
install.packages("RColorBrewer") 
install.packages("classInt")       
install.packages("spgwr")

## Carrega algumas extensões (espaciais e outras não espaciais úteis)

library(maps)         ## Projections
library(maptools)     ## Data management
library(sp)           ## Data management
library(spdep)        ## Spatial autocorrelation
library(gstat)        ## Geostatistics
library(splancs)      ## Kernel Density
library(spatstat)     ## Geostatistics
library(pgirmess)     ## Spatial autocorrelation
library(RColorBrewer) ## Visualization
library(classInt)     ## Class intervals
library(spgwr)        ## GWR

## Define working directory

setwd("C:/Curso Estatística Espacial/Roteiros R/")
load("DadosCurso.RData")
ls()

####################################################
####################################################
####################################################
####################################################
##########                                ##########
##########      Exploração Inicial        ##########
##########                                ##########
####################################################
####################################################
####################################################
####################################################


#############################################
#### Dados Pontuais: Baltimore Crime     ####
#############################################

# Explore os dados
head(crime)
dim(crime)

data <- crime

# Cria matriz de coordenadas
sp_point <- cbind(data$LONG, data$LAT)
colnames(sp_point) <- c("LONG","LAT")
head(sp_point)

# Projeção: UTM Zone 17
proj <- CRS("+proj=utm +zone=17 +datum=WGS84")

# Cria objeto espacial (SpatialPointsDataFrame)
data.sp <- SpatialPointsDataFrame(coords=sp_point,data,proj4string=proj)

# Bounding box dos pontos de dados (menor retângulo que contém os pontos)
bbox(data.sp)


## Plota locais de crimes

par(mar=c(2,2,0.2,0.2))

# desenha o objeto data.sp
plot(data.sp,pch=16, cex=.5, axes=T)

# desenha o scatter plot de data$LONG, data$LAT)
plot(data$LONG,data$LAT,pch=16, cex=.5, axes=T)

dev.off()


###############################################
#### Dados de Polígonos: 2004 Election     ####
###############################################

# Explore os dados
summary(election)
names(election)
data <- election
summary(data)[1:4]


# Plota o mapa de regiões
par(mar=c(0,0,0,0))
plot(data)

dev.off()

# Plota regiões + Locais de crimes em Baltimore
par(mar=rep(0.5,4))
plot(election,xlim=bbox(data.sp)[1,],ylim=bbox(data.sp)[2,],col="beige")
plot(data.sp,pch=1, cex=.5,add=T, col="blue")

dev.off()

######
## Plotting Attributes
######


# Algumas opções de visualização
par(mar=c(0,3,0,0),cex=.6)
display.brewer.all(n=5)

dev.off()

# Cria uma paleta azul - vermelha
br.palette <- colorRampPalette(c("blue", "red"), space = "rgb")
br.palette(5)


# Plota % de votos para Bush
data <- election
var <- data$Bush_pct


# Opção fácil mas inflexível
spplot(data, zcol="Bush_pct", col.regions=br.palette(100),
       main="Percent of County Vote for Bush (2004)")

dev.off()


# Opção mais trabalhosa, mas mais flexível

# Define o número de cores na paleta
pal <- br.palette(n=5)


# Intervalos fixos
classes_fx <- classIntervals(var, n=5, style="fixed",
                             fixedBreaks=c(0, 10, 25, 50, 75, 100), rtimes = 1)
classes_sd <- classIntervals(var, n=5, style = "sd", rtimes = 1)
classes_fi <- classIntervals(var, n=5, style = "fisher", rtimes = 3)
classes_eq <- classIntervals(var, n=5, style = "equal", rtimes = 1)
classes_km <- classIntervals(var, n=5, style = "kmeans", rtimes = 1)
classes_qt <- classIntervals(var, n=5, style = "quantile", rtimes = 1)


# Compare as diversas classificações diferentes
par(mar=c(2,2,2,1)+0.1, mfrow=c(2,3))
plot(classes_fx, pal=pal, main="Fixed Intervals", xlab="", ylab="")
plot(classes_sd, pal=pal, main="Standard Deviation", xlab="", ylab="")
plot(classes_fi, pal=pal, main="Fisher-Jenks", xlab="", ylab="")
plot(classes_km, pal=pal, main="K Means", xlab="", ylab="")
plot(classes_eq, pal=pal, main="Equal Interval", xlab="", ylab="")
plot(classes_qt, pal=pal, main="Quantile", xlab="", ylab="")

dev.off()


# Plota utilizando intervalos fixos
cols <- findColours(classes_fx, pal)

par(mar=rep(0,4))
plot(election,col=cols,border=NA)
legend(x="bottom",cex=.7,fill=attr(cols,"palette"),bty="n",
       legend=names(attr(cols, "table")),
       title="Percent of County Vote for Bush (2004)",ncol=5)


# Plota valores categóricos binários Bush/Kerry (Vermelho/Azul)
cols <- ifelse(data$Bush > data$Kerry,"red","blue")

par(mar=rep(0,4))
plot(election,col=cols,border=NA)
legend(x="bottom",cex=.7,fill=c("red","blue"),bty="n",
       legend=c("Bush","Kerry"),title="Winner of County Vote (2004)",ncol=2)

dev.off()


####################################################
####################################################
####################################################
####################################################
##########                                ##########
##########  Manipulando Dados Espaciais   ##########
##########                                ##########
####################################################
####################################################
####################################################
####################################################


# Abrindo um shapefile de países do mundo
map <- readShapePoly(
       "C:/Curso Estatística Espacial/Bases de Dados/WORLD/world2008",
       IDvar="MAP_CCODE",proj4string=CRS("+proj=eqc +lon_0=90w"))
summary(map)

# Plota a região de estudo
par(mar=rep(0,4))
plot(map)

dev.off()


# Abre Tabela DadosPoliticosPaises (em formato CSV)
polity <- read.csv("DadosPoliticosPaises.csv")
names(polity)


## As duas bases têm um campo em comum: CCODE
## Quatro passos para o processo de merge (join) tabular:
## (1) Extraia p data.frame a partir do mapa (shapefile)
## (2) Use merge() para juntar os dados políticos com o data.frame
## (3) Re-ordene o novo "merged" data.frame para estar conforme o mapa
## (4) Use spCbind para juntar o "merged" data.frame ao mapa
 
m_ccode <- as.data.frame(map)       
merged <- merge(x=m_ccode, y=polity, by.x="CCODE", by.y="ccode",
                all.x=T, all.y=F)
merged <- merged[order(merged$MAP_CCODE),]
rownames(merged) <- map$MAP_CCODE
map2 <- spCbind(map,merged)
names(map2)

# Remova linhas duplicadas
map2$CCODE.1 <- NULL
map2$SP_ID.1 <- NULL
map2$COUNTRY.1 <- NULL
map2$MAP_CCODE.1 <- NULL

# Recodifique a variável polity

map2$polity <- ifelse(map2$polity==-66,NA,map2$polity)
map2$polity <- ifelse(map2$polity==-77,NA,map2$polity)
map2$polity <- ifelse(map2$polity==-88,NA,map2$polity)

# Plote POLITY scores
dem.palette <- colorRampPalette(c("red", "green"), space = "rgb")
spplot(map2,"polity",col.regions=dem.palette(20),
       main="Polity IV Democracy Scores (2008)")    

dev.off()       


####################################################
####################################################
####################################################
####################################################
##########                                ##########
##########    Autocorrelação Espacial     ##########
##########                                ##########
####################################################
####################################################
####################################################
####################################################


#####################################
####   Conversão de Distâncias   ####
#####################################

## Function: Converte km para graus decimais
km2d <- function(km){
out <- (km/1.852)/60
return(out)
}
km2d(500) ## 500 km

## Function: Convert graus decimais para km
d2km <- function(d){
out <- d*60*1.852
return(out)
}
d2km(1) ## 1 grau decimal

##################################
#### Autocorrelação Espacial  ####
##################################

data <- election
names(data)


## Cria matriz de centróides dos polígonos
map_crd <- coordinates(data)

## Contiguity Neighbors
W_cont_el <- poly2nb(data, queen=T)
W_cont_el_mat <- nb2listw(W_cont_el, style="W", zero.policy=TRUE)


## Plota as conexões (links) entre os centróides
par(mar=rep(0,4))
plot(W_cont_el_mat,coords=map_crd,pch=19, cex=0.1, col="gray")

dev.off()

## Global Autocorrelation Tests: Moran's I
moran.test(data$Bush_pct, listw=W_cont_el_mat, zero.policy=T)

## Global Autocorrelation Tests: Geary's C
geary.test(data$Bush_pct, listw=W_cont_el_mat, zero.policy=T)

## Global Autocorrelation Tests: Join Count
#data$BushWin <- as.factor(ifelse(data$Bush > data$Kerry,1,0))
#joincount.multi(data$BushWin, listw=W_cont_el_mat, zero.policy=T)


## Moran Scatterplot
par(mar=c(4,4,1.5,0.5))
moran.plot(data$Bush_pct, listw=W_cont_el_mat, zero.policy=T,
           xlim=c(0,100),ylim=c(0,100), pch=16, col="black",cex=.5, quiet=F,
           labels=as.character(data$NAME),xlab="Percent for Bush",
           ylab="Percent for Bush (Spatial Lag)", main="Moran Scatterplot")


## Local Autocorrelation: Local Moran's I (pressupondo distribuição normal)
lm1 <- localmoran(data$Bush_pct, listw=W_cont_el_mat, zero.policy=T)
data$lm1 <- abs(lm1[,4]) ## Extract z-scores
lm.palette <- colorRampPalette(c("white","orange", "red"), space = "rgb")
spplot(data, zcol="lm1", col.regions=lm.palette(20),
       main="Local Moran's I (|z| scores)", pretty=T)


####################################################
####################################################
####################################################
####################################################
##########                                ##########
##########       Spatial Weights          ##########
##########                                ##########
####################################################
####################################################
####################################################
####################################################

data <- as.data.frame(map2)
merged <- data[order(map2$MAP_CCODE),]
merged <- merged[,c(1,41,42)]
map3 <- spCbind(map2,merged)

data <- map3
IDs <- map3$MAP_CCODE
names(data)

centroides <- (slot(data,"data"))[,c(5,6)]
centroides <- cbind(centroides$X,centroides$Y)

##################
## Contiguidade ##
##################

## Vizinhos por Contiguidade
W_cont <- poly2nb(data, queen=T)
W_cont_mat <- nb2listw(W_cont, style="W", zero.policy=TRUE)

## Vizinhos por Contiguidade (distância de tolerância [snap] = 500km)
W_cont_s <- poly2nb(data, queen=T, snap=km2d(500))
W_cont_s_mat <- nb2listw(W_cont_s, style="W", zero.policy=TRUE)

## Plota as conexões
par(mfrow=c(1,2),mar=c(0,0,1,0))

plot(data,border="grey")
#plot(W_cont_mat,coords=map_crd,pch=19, cex=0.1, col="blue", add=T)
plot(W_cont_mat,coords=centroides,pch=19, cex=0.1, col="blue", add=T)
title("Direct Contiguity")

plot(data,border="grey")
plot(W_cont_s_mat,coords=centroides,pch=19, cex=0.1, col="blue", add=T)
title("Contiguity + 500 km")

dev.off()

#########
## k = 1
#########

## Centróides
W_knn1 <- knn2nb(knearneigh(centroides, k=1), row.names=IDs)
W_knn1_mat <- nb2listw(W_knn1)

## Plota as conexões
plot(data,border="grey")
plot(W_knn1_mat,coords=centroides,pch=19, cex=0.1, col="blue", add=T)
title("k=1 (Centróides)")

dev.off()

#########
## Interpolando os pesos por distância
#########

## Centróides
dist <- unlist(nbdists(W_knn1, centroides))
W_dist1 <- dnearneigh(centroides, d1=0, d2=max(dist), row.names=IDs) 
W_dist1_mat <- nb2listw(W_dist1)

## Plota os links
plot(data,border="grey")
plot(W_dist1_mat,coords=centroides,pch=19, cex=0.1, col="blue", add=T)
title("Minimum Distance (Centroids)")


dev.off()

#########
## k = 4
#########

## Centróides
W_knn4 <- knn2nb(knearneigh(centroides, k=4), row.names=IDs)
W_knn4_mat <- nb2listw(W_knn4)

## Plota as conexões
plot(data,border="grey")
plot(W_knn4_mat,coords=centroides,pch=19, cex=0.1, col="blue", add=T)
title("k=4 (Centroids)")

dev.off()

#########
## Esfera de Influência
#########

## Centróides
W_del <- tri2nb(centroides)
W_soi <- graph2nb(soi.graph(W_del, centroides))
W_soi_mat <- nb2listw(W_soi)

## Plota as conexões
plot(data,border="grey")
plot(W_soi_mat,coords=centroides,pch=19, cex=0.1, col="blue", add=T)
title("Sphere of Influence (Centroids)")

dev.off()



####################################################
####################################################
####################################################
####################################################
##########                                ##########
##########      Regressão Espacial        ##########
##########                                ##########
####################################################
####################################################
####################################################
####################################################

data <- election
names(data)

########
## Linear Model
########

mod.lm <- lm(Bush_pct ~ pcincome, data=data)
summary(mod.lm)


## Plota os resíduos
res <- mod.lm$residuals

res.palette <- colorRampPalette(c("red","orange","white",
                                  "lightgreen","green"), space = "rgb")
pal <- res.palette(5)

classes_fx <- classIntervals(res, n=5, style="fixed",
                             fixedBreaks=c(-50,-25,-5,5,25,50), rtimes = 1)
cols <- findColours(classes_fx,pal)

par(mar=rep(0,4))
plot(data,col=cols, main="Residuals from OLS Model", border="grey")
legend(x="bottom",cex=1,fill=attr(cols,"palette"),bty="n",
       legend=names(attr(cols, "table")),title="Residuals from OLS Model",
       ncol=5)

dev.off()

## Testando a autocorrelação dos resíduos
moran.test(res, listw=W_cont_el_mat, zero.policy=T)


########
## SAR Model
########

mod.sar <- lagsarlm(Bush_pct ~ pcincome, data = data, listw=W_cont_el_mat,
                    zero.policy=T, tol.solve=1e-12)
summary(mod.sar)

res <- mod.sar$residuals

classes_fx <- classIntervals(res, n=5, style="fixed",
                             fixedBreaks=c(-50,-25,-5,5,25,50), rtimes = 1)
cols <- findColours(classes_fx,pal)

par(mar=rep(0,4))
plot(data,col=cols, border="grey")
legend(x="bottom",cex=1,fill=attr(cols,"palette"),bty="n",
       legend=names(attr(cols, "table")),title="Residuals from SAR Model",
       ncol=5)

dev.off()

## Testando a autocorrelação dos resíduos
moran.test(res, listw=W_cont_el_mat, zero.policy=T)



########
## GWR
########

bwG <- gwr.sel(Bush_pct ~ pcincome,data = data,gweight=gwr.Gauss, verbose=F)
mod.gwr <- gwr(Bush_pct ~ pcincome,data = data,bandwidth=bwG,gweight=gwr.Gauss)


## Resíduos
res <- mod.gwr$SDF$gwr.e

classes_fx <- classIntervals(res, n=5, style="fixed",
              fixedBreaks=c(-50,-25,-5,5,25,50), rtimes = 1)
cols <- findColours(classes_fx,pal)

par(mar=rep(0,4))
plot(data,col=cols, border="grey")
legend(x="bottom",cex=1,fill=attr(cols,"palette"),bty="n",
       legend=names(attr(cols, "table")),title="Residuals from GWR Model",
       ncol=5)

## Testando a autocorrelação dos resíduos
moran.test(res, listw=W_cont_el_mat, zero.policy=T)


## Coeficientes
coef <- mod.gwr$SDF$pcincome

classes_fx <- classIntervals(coef, n=5, style="fixed",
              fixedBreaks=c(-.005,-.003,-.001,.001,.003,.005), rtimes = 1)
cols <- findColours(classes_fx,pal)

par(mar=rep(0,4))
plot(data,col=cols, border="grey")
legend(x="bottom",cex=1,fill=attr(cols,"palette"),bty="n",
       legend=names(attr(cols, "table")),
       title="Local Coefficient Estimates (per capita income)",ncol=3)

dev.off()

