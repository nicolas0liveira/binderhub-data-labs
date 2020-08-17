library(readxl)
mob <- read_excel(path = "C:/Users/Ana Júlia/Downloads/Análise de cluster/analise_cluster/DADOS.xlsx",
                  sheet = 6)

mob <- read_excel("trabalhos/DADOS PAX cluster  2020 06 29.xlsx", sheet = "MOBILE_1");

mobile <- mob
par(mfrow=c(1,1))
################################################
#análise das vars --> outliers, correlação
###############################################
summary(mobile)

boxplot(mobile$IDH)
boxplot(mobile$IALFAB)
boxplot(mobile$POP)
boxplot(mobile$IDADEMED)
boxplot(mobile$GROSSINC/1000000)
boxplot(mobile$MOBPHONE)
boxplot(mobile$INTERNET)

mobile <- mobile[complete.cases(mobile$GROSSINC), ] #retirando Missing value

#SUBSTITUINDO NA DO mobile$GROSSINC
#mobile$GROSSINC[which(is.na(mobile$GROSSINC))] <- median(mobile$GROSSINC, na.rm = TRUE)
summary(mobile)

#RETIRANDO OUTLIERS
mobile = subset(mobile, mobile$IALFAB > 0.0)
mobile = subset(mobile, mobile$POP < 1000000)
mobile = subset(mobile, mobile$GROSSINC < 15000000)
mobile = subset(mobile, mobile$INTERNET < 35000)

m = table(mobile$SISGOV,useNA = "ifany"); m
mob.gov = prop.table(m); mob.gov
b = barplot(mob.gov,  lwd = 3, col=2, main="SISGOV", border=F, las=1, ylab = "%")
text(b, mob.gov/2,  m ,cex=1)


z = table(mobile$NIVDES,useNA = "ifany"); z
mob.niv = prop.table(z); mob.niv
b = barplot(mob.niv,  lwd = 3, col=2, main="NIVDES", border=F, las=1, ylab = "%")
text(b, mob.niv/2,  z ,cex=1)


################################################
#análise das vars --> outliers, correlação
###############################################

library(PerformanceAnalytics)
PerformanceAnalytics::chart.Correlation(mobile[,5:11], histogram=TRUE, pch="+")

round(cor(mobile[,5:11]),2) # vars quantitativas
#Existe correlação entre INTERNET E GROSSINC; cor > 0.9
#Vou retirar a variável GROSSINC
#mob2 <- mobile[,-9]
#round(cor(mob2[,5:10]),2)

#Existe correlação entre POP E MOBPHONE; cor > 0.9
#Vou retirar a variável MOBPHONE
mob2 <- mobile[,-10]
round(cor(mob2[,5:9]),2)
PerformanceAnalytics::chart.Correlation(mob2[,5:9], histogram=TRUE, pch="+")


mob.scale=scale(mob2[,5:10])
head(mob.scale)
#cor(mob.scale)


#calcular matriz distancias dos dados padronizados
mobdist=dist(mob.scale)

#vamos rodar o cluster utilizando ligação pela média
#hcmed=hclust(mobdist, method = "average")
#plot(hcmed, hang = -1)

#vamos rodar o cluster utilizando o método de Ward
hcwrd=hclust(mobdist, method = "ward.D2")
plot(hcwrd, hang = -1)
abline(h=12, col="red", lwd=2)

#analisar o comportamento dos drivers nos 4 clusters
mob2$hcw=cutree(hcwrd,3)
table(mob2$hcw)

#analisar as descritivas
m=table(mob2$NIVDES, mob2$hcw);m
prop.table(m,1)

library(gmodels)
CrossTable(mob2$SISGOV,mob2$hcw, prop.chisq = F, prop.t = F)
CrossTable(mob2$NIVDES,mob2$hcw,  prop.chisq = F, prop.t = F)

##########################################################
#           vamos rodar o k medoid
##########################################################
#install.packages("fpc")
library(fpc)
set.seed(1000) # recomenda-se que testemos com vários set seeds para modificar partição inicial - melhor = menor soma de quadrados
mob.kmed=pamk(mobdist, krange = 2:5, diss = T, critout = T)

#melhor resultado para maior valor de asw (critério default do pamk)--> k=8
mob.kmed$nc #clusters deu igual a 4
mob.kmed$pamobject$medoids

#vamos forçar k=2 (arbitraria): baseado no resultado do dendrograma
set.seed(1000)
mob.kmed=pamk(mobdist, krange = 4, diss = T)
mob.kmed$pamobject$medoids
mob2$kmed=mob.kmed$pamobject$clustering  #esta var no aqruivo tur vai identificar os clusters
table(mob2$kmed)

################################################################
# analisar os 2 clusters
###############################################################
par(mfrow=c(2,3))
boxplot(mob2$IDH~mob2$kmed, main="IDH", col=rainbow(2))
boxplot(mob2$IALFAB~mob2$kmed, main="IAL FAB", col=rainbow(2))
boxplot(mob2$POP~mob2$kmed, main="POP", col=rainbow(2))
boxplot(mob2$IDADEMED~mob2$kmed, main="IDADE MED", col=rainbow(2))
boxplot(mob2$GROSSINC~mob2$kmed, main="GROSSINC", col=rainbow(2))
boxplot(mob2$INTERNET~mob2$kmed, main="INTERNET", col=rainbow(2))

  aggregate(mob2[,c(5:10)], list(mob2$kmed), median)

CrossTable(mob2$SISGOV,mob2$kmed, prop.chisq = F, prop.t = F)
CrossTable(mob2$NIVDES,mob2$kmed,  prop.chisq = F, prop.t = F)

table(mob2$hcw, mob2$kmed, dnn = c("hier", "medoid")) #cruzando os resultados dos dois métodos

clust_stats <- cluster.stats(mobdist, mob2$hcw, mob2$kmed) # Corrected Rand index
clust_stats$corrected.rand


####################################################################
#
####################### EXERCÍCIO 2 ################################
#
####################################################################

exec2 <- mob[3:8]
summary(exec2)

par(mfrow=c(1,1))
boxplot(exec2$IDH)
boxplot(exec2$IALFAB)
boxplot(exec2$POP)
boxplot(exec2$IDADEMED)


exec2 = subset(exec2, exec2$IALFAB > 0.0)

m = table(exec2$SISGOV,useNA = "ifany"); m
pt = prop.table(m); pt
b = barplot(pt,  lwd = 3, col=2, main="SISGOV", border=F, las=1, ylab = "%")
text(b, pt/2,  m ,cex=1)

z = table(mobile$NIVDES,useNA = "ifany"); z
mob.niv = prop.table(z); mob.niv
b = barplot(mob.niv,  lwd = 3, col=2, main="NIVDES", border=F, las=1, ylab = "%")
text(b, mob.niv/2,  z ,cex=1)

####################################################################
#calcular a distancia entre as filiais
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

tur$PAÍS[tur$kmed==1]
tur$PAÍS[tur$kmed==2]



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

