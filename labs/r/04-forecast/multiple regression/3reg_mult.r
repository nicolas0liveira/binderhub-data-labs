hp=HOMEPRICES[,-1]

#adicionando labels
hp$COR=as.factor(hp$COR)
levels(hp$COR)=c("no", "yes")
hp$NE=as.factor(hp$NE)
levels(hp$NE)=c('no', 'yes')

summary(hp)
hp=hp[,-3] #removendo AGE

par(mfrow=c(1,2))
hist(hp$PRICE, main="PRICE", col="yellow")
boxplot(hp$PRICE, main="PRICE", col="yellow")
grid(col=3)
#removendo outliers
hp=subset(hp,hp$PRICE<1900)
#variÃ¡vel resposta muito assimÃ©trica
hp$LPRICE=log(hp$PRICE)
boxplot(hp$LPRICE, main="LPRICE", col="7", lwd=2)


boxplot(hp$SQFT, main='SQFT',col=3, lwd=1.5)
grid(col=3)
hp=subset(hp, hp$SQFT<3000)
par(mfrow=c(1,2))
boxplot(hp$SQFT, main='SQFT',col="green")
hist(hp$SQFT, main='SQFT',col="green")
plot(hp$SQFT,hp$LPRICE, pch=20, col=4)

boxplot(hp$TAX, main="TAX",col="green")
grid(col=3)
#vamos eliminar outliers da var previsora
hp=subset(hp, hp$TAX<1400)
boxplot(hp$TAX, main="TAX",col="green")
plot(hp$TAX,hp$LPRICE, pch=20, col=2)

boxplot(hp$FEATS, col=3, main='FEATS');grid()
table(hp$FEATS)
hp$NFEATS= ifelse(hp$FEATS>5, 5,hp$FEATS)
hp$NFEATS= ifelse(hp$NFEATS<2, 2,hp$NFEATS)
table(hp$NFEATS)

hpu=hp[,-c(1,3)]
reg.mlt=lm(data=hpu, LPRICE~ .)
summary(reg.mlt)

#previsão para indivíduos da amostra
hpu$LPRICE_hat=reg.mlt$fitted.values
hpu$RES=reg.mlt$residuals

#previsao de um novo individuo
novo=data.frame(SQFT=2500, NE="yes", COR="no",TAX=1100, NFEATS=4)
prev.novo=predict(reg.mlt, newdata = novo);prev.novo #prev de LPRICE
#para previsão de price ==> transformação invesa de log ==> exp
exp(prev.novo)

#multicolinearidade
library(rms)
round(vif(reg.mlt),1)

#anomalias
library(car)
influenceIndexPlot(reg.mlt , vars=c("Cook","Studentized","hat"))

#seleÃ§Ã£o de variÃ¡veis
reg.mlt2=step(reg.mlt);summary(reg.mlt2)
influenceIndexPlot(reg.mlt2 , vars=c("Cook","Studentized","hat"))

#install.packages("olsrr")
#ols_step_forward_p(reg.mlt)

#previsao
hp$LPRICE_HAT=fitted.values(reg.mlt2)
hp$RES=residuals(reg.mlt2)
hp$EP=hp$RES/hp$LPRICE*100
plot(hp$EP, pch=20, col=2)
grid(col=4)

#  SQFT=2500, TAX=1200 
novo=data.frame(SQFT=2500, TAX=1200)
lprice.hat=predict(reg.mlt2, novo);lprice.hat
pricenew.hat=exp(lprice.hat);pricenew.hat

