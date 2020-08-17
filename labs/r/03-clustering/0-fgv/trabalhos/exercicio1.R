library(readxl)
df <- read_excel("trabalhos/DADOS PAX cluster  2020 06 29.xlsx", sheet = "MOBILE_1");df


################################################
#análise das vars --> outliers, correlação
################################################
par(mfrow=c(1,2))
#par(mfrow=c(1,1))

summary(df)
df = df[complete.cases(df$GROSSINC), ] #removendo MissingValues
df

### Variáveis quantitativas
boxplot(df$IDH ,main ="idh" ,ylab = "índice (0 ~ 1)" ,col="steelblue")
hist(df$IDH ,main ="" ,xlab = "índice (0 ~ 1)"   ,col="steelblue")
#######################################

boxplot(df$IALFAB  ,main ="ialfab"  ,ylab = "índice (0 ~ 1)"  ,col="steelblue")
hist(df$IALFAB     ,main =""   ,xlab = "índice (0 ~ 1)"  ,col="steelblue")
df[df$IALFAB<0.1,]
df = df[df$IALFAB>=0.1,] #removendo outliers
######################################

boxplot(df$POP        ,main ="pop"        ,ylab = "população (mil)"        ,col="steelblue")
hist(df$POP     ,main =""     ,xlab = "população (mil)"     ,col="steelblue")
df[df$POP>1000000,]
#######################################

boxplot(df$IDADEMED        ,main ="idademed"        ,ylab = "idade média (em anos)"        ,col="steelblue")
hist(df$IDADEMED     ,main =""     ,xlab = "idade média (em anos)"     ,col="steelblue")
#######################################

boxplot(df$GROSSINC        ,main =""        ,ylab = "renda bruta (em milhares - U$)"        ,col="steelblue")
hist(df$GROSSINC     ,main =""     ,xlab = "renda bruta (em milhares - U$)"     ,col="steelblue")
#######################################

boxplot(df$MOBPHONE        ,main =""        ,ylab = "consumo de aparelhos (mil)"        ,col="steelblue")
hist(df$MOBPHONE     ,main =""     ,xlab = "consumo de aparelhos (mil)"     ,col="steelblue")
#######################################

boxplot(df$INTERNET        ,main =""        ,ylab = "assinantes internet (mil)"        ,col="steelblue")
hist(df$INTERNET     ,main =""     ,xlab = "assinantes internet (mil)"     ,col="steelblue")

#install_github("cran/PerformanceAnalytics")
#install.packages("PerformanceAnalytics", repos="http://R-Forge.R-project.org")

library(PerformanceAnalytics)
PerformanceAnalytics::chart.Correlation(df[,c(5:11)], histogram=TRUE, pch="+")

#como houve auta correlação com mobfone e pop removi mobfone (depois testar com pop)
drivers = df[,c(5:9,11)];drivers;
summary(drivers)

PerformanceAnalytics::chart.Correlation(drivers, histogram=TRUE, pch="+")
cor(drivers)
par(mfrow=c(1,1))

#padroniza as variáveis
drivers.pad = scale(drivers);

  
#calcular matriz distancias
drivers.dist = dist(drivers.pad);

####################################################
##### algoritmo hierárquico                        #
####################################################
#cluster ward
hcwrd = hclust(drivers.dist, method = "ward.D2")
plot(hcwrd)
abline(h=13, col="red", lwd=2)

?cutree
df$clusterW = cutree(hcwrd, k = 3) #identifica o cluster
drivers$clusterW = cutree(hcwrd, k = 3) #identifica o cluster

## Analisando com as variáveis de acordo com os clusters
par(mfrow=c(2,3))
boxplot(df$IDH~df$clusterW, main="IDH", xlab = "Cluster", col=rainbow(5))
boxplot(df$IALFAB~df$clusterW, main="IALFAB", xlab = "Cluster", col=rainbow(5))
boxplot(df$IDADEMED~df$clusterW, main="IDADEMED", xlab = "Cluster", col=rainbow(5))
boxplot(df$GROSSINC~df$clusterW, main="GROSSINC", xlab = "Cluster", col=rainbow(5))
#boxplot(df$MOBPHONE~df$clusterW, main="MOBPHONE", xlab = "Cluster", col=rainbow(5))
boxplot(df$INTERNET~df$clusterW, main="INTERNET", xlab = "Cluster", col=rainbow(5))
par(mfrow=c(1,1))

?aggregate
?list
aggregate(drivers, by=list(drivers$clusterW), median)#calcular a mediana das variaveis quantitativas

#################################
#  variaveis descritivas (qualitativas)
##################################
install.packages(c("gmodels"))
library(gmodels)

t = table(df$SISGOV,useNA = "ifany"); t
sisgov.prop = prop.table(t); sisgov.prop
b = barplot(sisgov.prop,  lwd = 3, col=2, main="SISGOV", border=F, las=1, ylab = "%")
text(b, sisgov.prop/2,  t ,cex=1)
CrossTable(df$SISGOV,df$clusterW, prop.chisq = F, prop.t = F)
m=table(df$SISGOV, df$clusterW);m
prop.table(m,1)


t = table(df$NIVDES,useNA = "ifany"); t
sisgov.prop = prop.table(t); sisgov.prop
?barplot
b = barplot(sisgov.prop,  lwd = 3, col=2, main="NIVDES", border=F, las=1, ylab = "%")
text(b, sisgov.prop/2,  paste("", t) ,cex=1)
CrossTable(df$NIVDES,df$clusterW, prop.chisq = F, prop.t = F)
m=table(df$NIVDES, df$clusterW);m
prop.table(m,1)


##########################################################
#           vamos rodar o k medoid
##########################################################
#install.packages("fpc")
library(fpc)
set.seed(1000) # recomenda-se que testemos com vários set seeds para modificar partição inicial - melhor = menor soma de quadrados
kmed=pamk(drivers.dist, krange = 2:5, diss = T, critout = T)

#melhor resultado para maior valor de asw (critério default do pamk)--> k=8
kmed$nc #clusters deu igual a 4
kmed$pamobject$medoids

#vamos forçar k=2 (arbitraria): baseado no resultado do dendrograma
set.seed(1000)
kmed=pamk(drivers.dist, krange = 4, diss = T)
kmed$pamobject$medoids
df$kmed=kmed$pamobject$clustering  #esta var no aqruivo tur vai identificar os clusters
drivers$kmed=kmed$pamobject$clustering
table(df$kmed)

################################################################
# analisar os 2 clusters
###############################################################
par(mfrow=c(2,3))
boxplot(df$IDH~df$kmed, main="IDH", col=rainbow(2))
boxplot(df$IALFAB~df$kmed, main="IALFAB", col=rainbow(2))
boxplot(df$POP~df$kmed, main="POP", col=rainbow(2))
boxplot(df$IDADEMED~df$kmed, main="IDADEMED", col=rainbow(2))
boxplot(df$GROSSINC~df$kmed, main="GROSSINC", col=rainbow(2))
boxplot(df$INTERNET~df$kmed, main="INTERNET", col=rainbow(2))

aggregate(drivers, list(drivers$kmed), median)

CrossTable(df$SISGOV,df$kmed, prop.chisq = F, prop.t = F)
CrossTable(df$NIVDES,df$kmed,  prop.chisq = F, prop.t = F)

table(df$clusterW, df$kmed, dnn = c("hier", "medoid")) #cruzando os resultados dos dois métodos

clust_stats = cluster.stats(drivers.dist, df$clusterW, df$kmed) # Corrected Rand index
clust_stats$corrected.rand




####################################################################
#
####################### EXERCÍCIO 2 ################################
#
####################################################################

exec2 <- df[3:8]
summary(exec2)

par(mfrow=c(1,1))
boxplot(exec2$IDH)
boxplot(exec2$IALFAB)
boxplot(exec2$POP)
boxplot(exec2$IDADEMED)

exec2 = subset(exec2, exec2$IALFAB > 0.0)
####################################################################
#calcular a distancia entre as variaveis
####################################################################

#neste caso temos mistura de vars (quant e quali)

library(cluster)

#pacote requer formato factor
exec2$SISGOV=as.factor(exec2$SISGOV)
exec2$NIVDES=as.factor(exec2$NIVDES)
round(cor(exec2[,3:6]),2) 
PerformanceAnalytics::chart.Correlation(exec2[,3:6], histogram=TRUE, pch="+")

exec2dist=daisy(exec2, metric ="gower")
# daisy calcula a distancia utilizando método de Gower
# se só houver vars quant temos que usar a opção metric="gower"


####################################################
##### algoritmo hierárquico                        #
####################################################
hc.tur=hclust(exec2dist, method = "ward.D2")
plot(hc.tur, hang = -1) #vamos considertar 2 clusters
abline(h=1.5, col=2)


exec2$hcqq=cutree(hc.tur, 2) #var que identifica o cluster

par(mfrow=c(2,2))
boxplot(exec2$IDH~exec2$hcqq, main="IDH", col=rainbow(2))
boxplot(exec2$IALFAB~exec2$hcqq, main="IALFAB", col=rainbow(2))
boxplot(exec2$POP~exec2$hcqq, main="POP", col=rainbow(2))
boxplot(exec2$IDADEMED~exec2$hcqq, main="IDADEMED", col=rainbow(2))


aggregate(exec2[,c(3:6)], list(exec2$hcqq), median)

#install.packages("gmodels")
library(gmodels)
CrossTable(exec2$SISGOV,exec2$hcqq, prop.chisq = F, prop.t = F)
CrossTable(exec2$NIVDES,exec2$hcqq,  prop.chisq = F, prop.t = F)

##########################################################
#           vamos rodar o k medoid
##########################################################
#install.packages("fpc")
library(fpc)
set.seed(1000) # recomenda-se que testemos com vários set seeds para modificar partição inicial - melhor = menor soma de quadrados
exec2.kmed=pamk(exec2dist, krange = 2:5, diss = T, critout = T)
#melhor resultado para maior valor de asw (critério default do pamk)--> k=8
exec2.kmed$nc #clusters deu igual a 8
exec2.kmed$pamobject$medoids

#vamos forçar k=2 (arbitraria): baseado no resultado do dendrograma
set.seed(1000)
exec2.kmed=pamk(exec2dist, krange = 3, diss = T)
exec2.kmed$pamobject$medoids
exec2$kmed=exec2.kmed$pamobject$clustering  #esta var no aqruivo tur vai identificar os clusters
table(exec2$kmed)

################################################################
# analisar os 3 clusters
###############################################################
par(mfrow=c(2,2))
boxplot(exec2$IDH~exec2$kmed, main="IDH", col=rainbow(2))
boxplot(exec2$IALFAB~exec2$kmed, main="IAL FAB", col=rainbow(2))
boxplot(exec2$POP~exec2$kmed, main="POP", col=rainbow(2))
boxplot(exec2$IDADEMED~exec2$kmed, main="IDADE MED", col=rainbow(2))


aggregate(exec2[,c(3:6)], list(exec2$kmed), median)

CrossTable(exec2$SISGOV,exec2$kmed, prop.chisq = F, prop.t = F)
CrossTable(exec2$NIVDES,exec2$kmed,  prop.chisq = F, prop.t = F)
table(exec2$hcqq, exec2$kmed) #cruzando os resultados dos dois métodos

######################################################
#   analisar uma descritiva
#####################################################
#boxplot(exec2$GEOG~exec2$kmed, main="GEO", col=rainbow(5))

t = table(exec2$SISGOV,useNA = "ifany"); t
sisgov.prop = prop.table(t); sisgov.prop
b = barplot(sisgov.prop,  lwd = 3, col=2, main="SISGOV", border=F, las=1, ylab = "%")
text(b, sisgov.prop/2,  t ,cex=1)
c=CrossTable(exec2$SISGOV,exec2$kmed, prop.chisq = F, prop.t = F)
m=table(exec2$SISGOV, exec2$kmed);m
prop.table(m,1)


t = table(exec2$NIVDES,useNA = "ifany"); t
sisgov.prop = prop.table(t); sisgov.prop
b = barplot(sisgov.prop,  lwd = 3, col=2, main="NIVDES", border=F, las=1, ylab = "%")
text(b, sisgov.prop/2,  paste("", t) ,cex=1)
CrossTable(exec2$NIVDES,exec2$kmed, prop.chisq = F, prop.t = F)
m=table(exec2$NIVDES, exec2$kmed);m
prop.table(m,1)

barplot(sisgov.prop,  lwd = 3, col=2, main="NIVDES", border=F, las=1, ylab = "%")


######################################################
#   comparar o ward e o pamk
#####################################################

table(exec2$hcqq,exec2$kmed, dnn = c("hier", "medoid"))

clust_stats <- cluster.stats(exec2dist, exec2$hcqq, exec2$kmed) # Corrected Rand index
clust_stats$corrected.rand

######################################################
#   vamos ponderar as vars quali para calcular distancias
#####################################################

ww=c(1,1,1,.5,.5)
tur.dist=daisy(tur.drivers, metric ="gower",weights = ww)

