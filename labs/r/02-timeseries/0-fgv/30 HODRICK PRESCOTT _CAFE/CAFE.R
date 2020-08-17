install.packages(c("fpp", "fpp2", "mFilter"))
library(fpp)
library(fpp2)
library(mFilter)

data(cafe)
cafe

filtro_HP <-hpfilter(cafe, type = 'lambda')

library(gridExtra)

g1 <- autoplot(cafe, frequency=4, start=c(1982,2), serie="Original")+
        autolayer(filtro_HP$trend, serie="Filtro_HP", lwd = 1)+
      xlab("Trimestres")+
      ylab("Despesas Trimestrais")+
      ggtitle("Série Original Trimestral e Tendência do Filtro_HP")

g2 <- autoplot(filtro_HP$cycle, frequency=12, start=c(1982,4), serie="Ciclo")+
      xlab("Trimestres")+
      ylab("Despesas Trimestrais")+
      ggtitle("Ciclo do Filtro_HP")

 grid.arrange(g1,g2, nrow=1,ncol=2)       
  
  