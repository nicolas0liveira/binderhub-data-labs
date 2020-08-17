library(fpp2)
library(ggthemes)

births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")

ts.births<- ts(births, frequency=12, start=c(1946,1))

ts.births # note o formato da ts

autoplot(ts.births)+
  xlab("Mês")+
  ylab("Nascimento")+
  theme_minimal()+
  ggtitle("Nascimentos em NY")
