qq=SUPERMERCADOS_IMPERIO[,-1]
rl=lm(data=qq, SALARIO_MENSAL~.)
summary(rl)
rl2=step(rl)
summary(rl2)

qq$yhat=rl2$fitted.values
qq$resid=rl2$residuals
qq$ep=qq$resid/qq$SALARIO_MENSAL  *100
plot(qq$ep )
grid(col="red")

install.packages("car")
library(car)
influencePlot(rl2)
influenceIndexPlot(rl2, vars = c("Studentized", "COOK","hat"))
