library(readxl)
mobile <- read_excel("trabalhos/DADOS PAX cluster  2020 06 29.xlsx", sheet = "MOBILE_1");df
mob=mobile[,2:8]
names(mob)
table(mob$NIVDES, useNA = "ifany")
table(mob$SISGOV, useNA = "ifany")

boxplot(mob$IDH, main="idh", col=11)
summary(mob$IDH)
boxplot(mob$IALFAB, main="alfabetização", col=11)
hist(mob$IALFAB)
summary(mob$IALFAB)

#para descobrir quem é
mob[mob$IALFAB==0,]
#ou
which(mob$IALFAB==0)

#remover os dois pontos
mob = mob[mob$IALFAB>0,]

#reanalizando
boxplot(mob$IALFAB, main="alfabetização", col=11)
hist(mob$IALFAB)

#tentando normalizar e reduzir os outliers, pois a amostra é pequena
hist(log(1-mob$IALFAB))
boxplot(log(1-mob$IALFAB), main="alfabetização", col=11)
mob$IALFAB_T = log(1-mob$IALFAB))








