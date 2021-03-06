library(fpp2)
library(gridExtra)

set.seed(1) 
a <- 1
wn <- rnorm(100)
rw <-a+ cumsum(wn)
t=1:100
rwd <- a+rw + t
tend <- t + wn

# gráficos (figura 3.2)

wn <- ts(wn)
rw <- ts(rw)
rwd <- ts(rwd)
tend <- ts(tend)

a1 <- autoplot(wn, col = "darkblue")
a2 <- autoplot(rw, col = "darkred")
a3 <- autoplot(rwd, col = "orange")
a4 <- autoplot(tend, col="gray50")

a5 <- ggAcf(wn, col = "darkblue")
a6 <- ggAcf(rw, col = "darkred")
a7 <- ggAcf(rwd, col = "orange")
a8 <- ggAcf(tend, col = "gray50")

grid.arrange(a1, a2, a3, a4, a5, a6, a7, a8, nrow=2, ncol = 4)

# Removendo a tendência estacionária

a3 <- autoplot(rwd, col="orange", main = "Antes da remoçao da tendência", lwd=1)


diff_rwd <- diff(rwd)

d3 <- autoplot(diff_rwd, col="orange", main ="Depois da remoção da tendência", lwd =1)

grid.arrange(a3, d3, nrow=1, ncol =2)


# gráficos (figura 3.5)

lmresid_rwd <- lm(rwd ~ t)
det_rwd <- ts(resid(lmresid_rwd))

a3 <- autoplot(rwd, main="Antes da remoção de tendência")
b3 <- autoplot(det_rwd, main = "Depois da remoção de tendência")

grid.arrange(a3,b3, nrow=2, ncol=1)

# gráficos (figura 3.6)
diff_rwd <- ts(diff(rwd))

c3 <- autoplot(TSA::acf(diff_rwd, plot=FALSE), main="Depois da diferenciação")
c4 <- autoplot(TSA::acf(det_rwd, plot=FALSE), main="Depois da remoção de tendência")

grid.arrange(c3, c4, nrow=2, ncol =1)


# gráficos (figura 3.7)
tibc <- 1:133
det_ibcts <- resid(lm(ibcts~tibc))
# dev.off()
# par(mfrow=c(2,2), mar=c(2, 2, 0.8, 0.8))
plot.ts(det_ibcts)
plot.ts(diff(ibcts))
plot(TSA::acf(det_ibcts, plot=FALSE), main="")
plot(TSA::acf(diff(ibcts), plot=FALSE), main="")

##### -------- Seção 3.5 -------- #####

# gráfico (figura 3.8)
ibc_cus <- efp(diff(ibcts)~lag(diff(ibcts)))
bound.ibc_cus <- boundary(ibc_cus, alpha=0.1)
plot(ibc_cus, boundary=T)
sctest(ibc_cus)

# gráfico (figura 3.9)
y<-rw + t
y[50:100]<-y[50:100]+mean(y[0:49])
plot.ts(y)

# gráficos (figura 3.10)
efp_y <- efp(diff(y)~lag(diff(y),1))
plot(efp_y)
plot(breakpoints(diff(y)~lag(diff(y),1), breaks = 6))

##### -------- Seção 3.6 -------- #####

#### Teste ADF ----
adf_ibc<- ur.df(ibcts, type = "trend", lags = 6, selectlags = "AIC")
summary(adf_ibc)@teststat
summary(adf_ibc)@cval

#### Teste KPSS ----
kpss_ibc<- ur.kpss(ibcts, type = "tau", lags = "short")
summary(kpss_ibc)@teststat
summary(kpss_ibc)@cval

#### Teste de Phillips-Perron ----
pp_ibc<- ur.pp(ibcts, type = "Z-tau", lags = "short")
summary(pp_ibc)@teststat
summary(pp_ibc)@cval




lmresid_rwd <- lm(rwd ~ t)
det_rwd <- resid(lmresid_rwd)
plot.ts(rwd)
plot.ts(det_rwd)

# gráficos (figura 3.6)
diff_rwd <- diff(rwd)
plot(TSA::acf(diff_rwd, plot=FALSE), main="")
plot(TSA::acf(det_rwd, plot=FALSE), main="")

# gráficos (figura 3.7)
tibc <- 1:133
det_ibcts <- resid(lm(ibcts~tibc))
# dev.off()
# par(mfrow=c(2,2), mar=c(2, 2, 0.8, 0.8))
plot.ts(det_ibcts)
plot.ts(diff(ibcts))
plot(TSA::acf(det_ibcts, plot=FALSE), main="")
plot(TSA::acf(diff(ibcts), plot=FALSE), main="")
