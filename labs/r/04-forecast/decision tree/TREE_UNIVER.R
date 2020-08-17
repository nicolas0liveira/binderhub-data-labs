uu=UNIVER[,-c(1,5)]
uu$Online=as.factor(uu$Online)
uu$CreditCard=as.factor(uu$CreditCard)
uu$cdac=uu$`CD Account`
uu$secur=uu$`Securities Account`
uu=uu[,-c(8,9)]
#verificar se preciso ou nao podar a árvore
printcp(tre)

set.seed(1930) 
ind=sample(1:5000, 2500)
lrn=uu[ind,]
tst=uu[-ind,]

tre=rpart(data=lrn, `Personal Loan`~., method="class" )
tre
printcp(tre)
#como xerror decresce continuamente, nao precisamos podar a árvore

#vamos fazer de conta que o ministro da ML baixa decreto: só podemos ter 3 splits
tre2=prune(tre, cp=.07);tre2
# a título de ilustração.

#vamos continuar utilizando o tre
tre
#pre poda: evitar ter caixinhas com menos de 100 clientes
ad=rpart(data = lrn, `Personal Loan`~.,control = rpart.control(minbucket = 100))
ad

#começar os tetes da arvore ad
#Cacular a prob de yes na amostra tst
prev=predict(ad, newdata=tst, type="prob")
head(prev)
tst$psim=prev[,2]

#como classificar um novo individuo
ad
#mandetta --> income=300, education =3 
mand=data.frame(Age=54, Experience=20,Income=300, Family=4, CCAvg=1.8, Education=3,Mortgage=0, Online="1",CreditCard="0", cdac=1,secur=1)
p.mand=predict(ad,newdata=mand, type="prob");p.mand

#matriz de classificação
pc=.6
Klass=ifelse(tst$psim>pc, "prev.sim", "prev.nao")
table(Klass, tst$`Personal Loan`)

#calcula o AUC
HMeasure(tst$`Personal Loan`, tst$psim)$metric

