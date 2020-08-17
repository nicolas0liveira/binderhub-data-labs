bb=BETABANK[,2:11]
colnames(bb)
bb$NATUREZA=bb$NATUREZA...5
bb$ESCOLARIDADE=bb$ESCOLARIDADE...3
bb=bb[, -c(2,4),]

#analisar e tratar as vars
bb$ECIV=as.factor(bb$ECIV)
bb$PROFISSAO=as.factor(bb$PROFISSAO)
bb$REGIAO=as.factor(bb$REGIAO)
bb$NATUREZA=as.factor(bb$NATUREZA)
bb$ESCOLARIDADE=as.factor(bb$ESCOLARIDADE)

#por simplicidade (tempo!!!) vou analisar duas vars
boxplot(bb$RENDA~bb$STATUS, main="renda", cor=11); grid(col=1)
quantile(bb$RENDA, probs= seq(0,1, .005))
bb$renda=ifelse(bb$RENDA >250, 250, bb$RENDA)
boxplot(sqrt(bb$renda)~bb$STATUS, main="renda", cor=11); grid(col=1)
bb$Srenda=sqrt(bb$renda)

table(bb$UF)
bb$estado=ifelse(bb$UF!="SP","NOSP","SP")
table(bb$estado)

#limpar arquivo
bb=bb[,-c(6,7,11)]
colnames(bb)

#vamos dividir a amostra em duas partes> lrn e tst
set.seed(1704)
flag=sample(1:35331,20000)
lrn=bb[flag,]; dim(lrn)
tst=bb[-flag,];dim(tst)

#rodar a reg log (lmbrar que status=1 bom)
fit=glm(data = lrn, STATUS~.,family = binomial())
fit2=step(fit, trace=0 )
summary(fit2)

#calcullar pbom para todos da amostra teste
tst$pbom=predict(fit2, newdata = tst, type = "response")
fit2
# magali  ecviv=3 idade=38 profissoa=6 regiao=5, natureza=5, renda=2000 (lembrar do 250)  estado=nosp
magali=data.frame(ECIV="3", IDADE=38, PROFISSAO="6", REGIAO="5", NATUREZA="5",Srenda=sqrt(250),estado="NOSP")
pbom.maga= predict(fit2, newdata = magali, type="response");pbom.maga
#avaliar o ajuste do modelo pelo teorema faraônico de Zoyowski --. NAO É MUITO PRECISO
install.packages("arules")
library(arules)
kpbom=discretize(tst$pbom, method = "frequency", breaks=10)
FF=table(kpbom, tst$STATUS);FF
prop.table(FF,1)


#DETERMINAR UM PONTO DE CORTE
pc=.9
KLASS=ifelse(tst$pbom>pc, "aprova","recusa")
table(KLASS, tst$STATUS) #matriz de classificação
erro=(1071+804)/15331; erro

library (HMeasure)
HMeasure(tst$STATUS, tst$pbom)$metric
