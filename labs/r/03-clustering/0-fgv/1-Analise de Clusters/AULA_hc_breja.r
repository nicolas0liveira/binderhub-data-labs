
zz=CERVEJAS
#transformando chr em factor para calcular distâncias com daisy
zz$origin=as.factor(zz$origin)
zz$light=as.factor(zz$light)

correl=cor(zz[,4:8])
round(correl,digits=3)
zz=zz[,-4]
zz.drivers=zz[,3:8]
colnames(zz.drivers)
#cálculo da matriz de distâncias utilizando função daisy
#não precisamos padronizar. daisy usa a métrica de Gower que já faz isso(*)
dist.cerv=daisy(zz.drivers)

#algorimo pamk da library fpc ajuda a definir número de clusters com asw
set.seed(18) # recomenda-se que testemos com vários set seeds para modificar partição inicial
kk=pamk(dist.cerv, krange = 2:8, diss = T, critout = T )
kk$nc
kk$pamobject$medoids
zz$kpamc=kk$pamobject$clustering 
zz$kpamc

#para a representação só podemos trabalhar com as drivers quantitativas
fviz_cluster(list(data = zz[,4:7], cluster = zz$kpamc), show.clust.cent = F) 


# para fazer o silhouette com library cluster
silk=silhouette(zz$kpamc,dist.cerv)
plot(silk, col = "navyblue", cex=.5)

#marcas em cada cluster
zz$beer[zz$kpamc==1]
zz$beer[zz$kpamc==2]
zz$beer[zz$kpamc==3]


aggregate(zz[,4:7],list(zz$kpamc), median)

#exemplos de análises
par(mfrow=c(1,2))
boxplot(zz$cost~zz$kpamc, col=rainbow(3), main="cost")
boxplot(zz$alcohol~zz$kpamc,col=rainbow(3), main="alcohol")
table(zz$origin, zz$kpamc)
table(zz$light, zz$kpamc)
 