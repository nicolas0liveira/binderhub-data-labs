library(readxl)
TURISMOATB = read_excel("1-Analise de Clusters/DADOS PAX cluster  2020 06 29.xlsx", 
                        sheet = "TURISMOATB")
  
tur=TURISMOATB
tur
names(tur)

################################################
#análise das vars --> outliers, correlação
###############################################
boxplot(tur$INSTAL)
boxplot(tur$ATEND);grid(col=4)
boxplot(tur$TEMP)
tur=subset(tur, tur$ATEND>4)

#elimina outliers etc..

round(cor(tur[,2:4]),2) # vars quantitativas

#####################################################################
#  caso 1: drivers: apenas as quantitativas INSTAL, ATEND E TEMP
####################################################################

#ecolhe os drivers... 
tur.drivers = tur[,2:4]

#padronizar as variaveis
tur.pad=scale(tur.drivers);

#calcular matriz distancias
tur.dist=dist(tur.pad)

#vamos rodar o cluster utilizando ligacao pela media 
?hclust
hcmed=hclust(tur.dist, method = "average")
plot(hcmed, hand = -1)
#os metodos mais comuns são os de avarage e ward

#vamos rodar o cluster utilizando metodo de ward (Hmedoid é para uso com variaveis qualitativas, não contemplados pelo ward - pelos que utilizam média)
hcwrd=hclust(tur.dist,method = "ward.D2")
plot(hcwrd, hand = -1)
abline(h=4.5, col="red", lwd=2)

#analisar o comportamento dos drivers nos 4 clusters
tur$hcw=cutree(hcwrd,4) # função para cortar o cluster e preencher no arquivo original

boxplot(tur$INSTAL~tur$hcw, col=rainbow(4), main="Nota Instalações")
boxplot(tur$ATEND~tur$hcw, col=rainbow(4), main="Nota Atendimento")
boxplot(tur$TEMP~tur$hcw, col=rainbow(4), main="Nota tempo de espera")

#analizando pelas variáveis descritivas
m=table(tur$LOCAL, tur$hcw)
prop.table(m,1)

#####################################################################
#  caso 1: drivers: todas as vars quali e quanti (exceto AGE)
####################################################################

tur.drivers=tur[,c(2:6)]
names(tur.drivers)


####################################################################
#calcular a distancia entre as filiais
####################################################################

#neste caso temos mistura de variáveis (quali e quanti)

library(cluster)
#pacote requer formato factor
str(tur.drivers)
tur.drivers$FREEPARK=as.factor(tur.drivers$FREEPARK)
tur.drivers$LOCAL=as.factor((tur.drivers$LOCAL))
str(tur.drivers)

tur.dist=daisy(tur.drivers, metric ="gower") # daisy calcula a distancia utilizando método de Gower
# se só houver vars quant temos que usar a opção metric="gower"


####################################################
##### algoritmo hierárquico                        #
####################################################
hc.tur=hclust(tur.dist, method = "ward.D2")
#par(mfrow=c(1,1))
plot(hc.tur, hang = -1)
abline(h=1.5, col="red")

tur$hcqq=cutree(hc.tur, 2) #var que identifica o cluster

par(mfrow=c(1,3))
boxplot(tur$INSTAL~tur$hcqq, main="INSTAL", col=rainbow(5))
boxplot(tur$ATEND~tur$hcqq, main="ATEND", col=rainbow(5))
boxplot(tur$TEMP~tur$hcqq, main="TEMP", col=rainbow(5))

aggregate(tur[,c(2:4,7,8)], list(tur$hcqq), median)#calcular a mediana das variaveis quantitativas

install.packages(c("gmodels"))
library(gmodels)
CrossTable(tur$FREEPARK,tur$hcqq, prop.chisq = F, prop.t = F)
CrossTable(tur$LOCAL,tur$hcqq,  prop.chisq = F, prop.t = F)
 
#metodo hierarquico a dominancia é dos qualitativas, por conta das distancias 0 e 1 das variaáveis dumies

##########################################################
#           vamos rodar o k medoid (melhor que o hierarquico***)
##########################################################
seed=1910

install.packages(c("fpc"))
library(fpc)
set.seed(seed) # recomenda-se que testemos com vários set seeds para modificar partição inicial
tur.kmed=pamk(tur.dist, krange = 2:8, diss = T, critout = T) #o de maior valor de k é o melhor sugerido, mas não sou obrigado a usar a sugestão
# (o range de 2~8 foi um chute)
#melhor resultado para maior valor de asw (critério default do pamk)--> k=8
tur.kmed$nc #clusters deu igual a 8
tur.kmed$pamobject$medoids

#vamos forçar k=5
set.seed(seed)
tur.kmed=pamk(tur.dist, krange = 5, diss = T)
tur.kmed$pamobject$medoids
tur$kmed=tur.kmed$pamobject$clustering  #esta var no aqruivo tur vai identificar os clusters
table(tur$kmed)

################################################################
# analisar os 5 clusters
###############################################################
par(mfrow=c(1,3))
boxplot(tur$INSTAL~tur$kmed, main="INSTAL", col=rainbow(5))
boxplot(tur$ATEND~tur$kmed, main="ATEND", col=rainbow(5))
boxplot(tur$TEMP~tur$kmed, main="TEMP", col=rainbow(5))
aggregate(tur[,c(2:4,7,8)], list(tur$kmed), median)
CrossTable(tur$FREEPARK,tur$kmed, prop.chisq = F, prop.t = F)
CrossTable(tur$LOCAL,tur$kmed,  prop.chisq = F, prop.t = F)
table(tur$hc, tur$kmed) #cruzando os resultados dos dois métodos

######################################################
#   analisar uma descritiva
#####################################################

#se precisar testar algo podemos escolher o medoid de cada cluster, para testar as coisas já que o medoid representa bem o cluster 

boxplot(tur$AGE~tur$kmed, main="TEMP", col=rainbow(5))

clust_stats <- cluster.stats(tur.dist, tur$hc, tur$kmed) # Corrected Rand index
clust_stats$corrected.rand

######################################################
#   comparar o hierarquico (ward) e o medoid (pank)
#####################################################
table(tur$hcqq, tur$kmed, dnn=c("hier","medoid")) #dnn põe nome na tabela

clust_stats <- cluster.stats(tur.dist, tur$hcqq, tur$kmed) # Corrected Rand index (uma especie de correlação entre os clusters)
clust_stats$corrected.rand


######################################################
#   vamos ponderar as variaveis (evitar aquele problema do peso das qualitativas)
#####################################################









