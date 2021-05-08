################################################################################VAR 2020###############################

# Diret√≥rio
getwd()
setwd("D:/MDA - FGV/Decisıes Mercadologicas/Jogo/")
# Abrindo os dados
#install.packages("readcsv")
library(readxl)

dfv <- read_xlsx("D:/MDA - FGV/Decisıes Mercadologicas/Jogo/Rodada0.xlsx", sheet = 2)

#visualizando os dados
View(dfv)
str(dfv)

#Eliminando variaveis para o modelo
dfv=dfv[,-c(7,16,17,20,27,36)] #eliminadas outras cervejas
#dfv = na.omit(dfv)
dfv = scale(dfv)
dfv=dfv[,-c(23,31,32,33,34)] #eliminadas outras cervejas

cor(dfv)

dfv = data.frame(dfv)
#base Final
#install.packages("writexl")
#library(writexl)
#write.csv(dfv, "D:/MDA - FGV/AplicaÁıes de EstatÌstica Espacial/Regress„o Linear Testes/BaseFinal_Estados_Variacao.csv" )



#dfv=dfv[,-c(1:3)] #retira identificadores
View(dfv)
vif(dfv)

#analisando as vari√°veis quantitativas

#Teste de Normalidade
shapiro.test(dfv$Profit)
#histograma
hist(x=dfv$Profit, ylab = "Freq", xlab = "Taxa", col = c("black"),
     main = "Histograma da varialvel COmpra em dez" , 
     cex.axis = 1, cex.labs=1.5)

#####################################

#######modelo de regress„o
reg.mltv=lm(data=dfv, Profit~.)
summary(reg.mltv)

#multicolinearidade
install.packages("rms")
library(rms)
round(vif(reg.mltv),1)

#anomalias
install.packages("car")
library(car)
influenceIndexPlot(reg.mltv , vars=c("Cook","Studentized","hat"))

#sele√ß√£o de vari√°veis
reg.mltv2=step(reg.mltv);summary(reg.mltv2) #o step analisa as avriaveis e retira as vari·veis e anomalias
influenceIndexPlot(reg.mltv2 , vars=c("Cook","Studentized","hat"))
round(vif(reg.mltv2),1)

#Cria coluna com a previs„o
dfv$Profit_Hat=fitted.values(reg.mltv2)

library(ggplot2)

ggplot(dfv, aes(x = dfv$Profit , y = dfv$Profit_Hat)) +
    labs(x="Profit Original", y="Profit Previsto", title="Profit Cervejas -  Regress„o Linear") +
    theme(axis.text.x = element_text(angle = 55, hjust = 1)) +
    theme(legend.title = element_text(size = 15), legend.text = element_text(size = 8)) +
    geom_point(show.legend = FALSE)+
    geom_smooth(method = lm) 

