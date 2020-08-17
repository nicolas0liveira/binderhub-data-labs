rec=RECORDES
boxplot(rec$`100m_seg`, main = "100m", col="lightblue")
rec=subset(rec, rec$`100m_seg` < 10.7)

boxplot(rec$`100m_seg`, main = "200m", col="lightblue")
boxplot(rec$`400m_seg`, main = "400m", col="lightblue")
boxplot(rec$`800m_min`, main = "800m", col="lightblue")
boxplot(rec$`1500m_min`, main = "1500m", col="lightblue");grid()
rec=subset(rec,rec$`1500m_min` < 3.9) #removendo outlier
boxplot(rec$`5000m_min`, main = "5000m", col="lightblue")
boxplot(rec$`10000m_min`, main = "10000m", col="lightblue")
boxplot(rec$maratona_min, main = "maratona", col="lightblue")
hist(rec$maratona_min, main="Maratona", col = "lightblue") #curva assimétrica


########## analisar a matriz de correlacoes
#para calcular 
round(cor(rec[,-1]),2)  #tirando a variavel qualitativa, por isso o -1

### padronizar os dados (além das escalas diferentes as unidades tmabém são diferentes)
#só não padroniza quando estiverem na mesma escala
recpad=scale(rec[,-1]) #padroniza as variaveis --> subtrai a media e divide pelo desvio padrao

#achando as cps
cps=prcomp(recpad) # cps: objeto com as informações das componentes principais (CP)
summary(cps);
#standard deviation: desvio padrão de cada CP
#para analisar melhor olhar a variância (Proportion of Variance)
#o campo cumulative proportion indica o quanto vou ter de informação e, consequentemente, quanto vou perder na escolha das variáveis
 
plot(cps, col=11) # no eixo y dá as variancias da CP
#alguns autores dizem para pegar as variáveis onde a variancia > 1
abline(h=1, color="red", )

#alguns autores dizem para pegar as variáveis onde a variancia > 0.75
abline(h=.75, color="blue")

#para este caso analisado, os dois critérios sugerem k=2 (k=qtd clusters)


####### interpretar as CPS (para interpretar as CPS, não se usa o peso!!!!!)
round(cor(rec, cps$x[,1:2]),3)  #calculando a correlãaço entre as variaveis originais e as CP1 e CP2 

# analisando as componentes verificamos a correlação maior entre as componentes para cada variável original
# qto maior a correlação indica a alta correlação com a variável original

plot(cps$x[,1],cps$x[,2], pch=19, col="naveblue", type = "n") #type n é pra nao mostrar nada
text(cps$x[,1],cps$x[,2], labels = cps$pais)

#para ver quem é o pais
witch(cps$x[,1]>4) #lembrando que o cps é o record das CPs... # não deu pra ver nesse caso

##### criando os clusters

dist.cps=dist(cps$x[,1:2]) #calculando a distancia cada pais, caracterizados pelas duas primeiras CPs
hhh=hclust(dist.cps, method="ward.D2")
plot(hhh, hang=-1) #===> o gráfico sugere dois clusters
rec$cluscp=cutree(hhh,2)
rec$pais[rec$clusp==1] # retorna os paises do cluster 1
rec$pais[rec$clusp==2] # retorna os paises do cluster 2




########################## fazendo com as variaveis originais, sem as CPS
recpad=scale(rec[,-1]);
newdist=dist(recpad)
ttt=hclust(newdist,method="ward.D2")
plot(ttt,hang=-1)
#deu dois clusters, mas será que são os mesmos paises
rec$ht=cutree(ttt,2)
rec$pais[rec$ht==1] # retorna os paises do cluster 1
rec$pais[rec$ht==2] # retorna os paises do cluster 2

boxplot(rec$`100m_seg`~rec$ht) #analisando os clustres



