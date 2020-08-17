library(fpp)
library(fpp2)
library(gridExtra)

ausbeer
class(ausbeer)
head(ausbeer)
tail(ausbeer)

beer <- window(ausbeer, start =1992)

a1 <- autoplot(beer, main = "Série temporal")
a2 <- ggseasonplot(beer, 4, col=rainbow(12), year.labels=TRUE, main="Sazonalidade")
grid.arrange(a1,a2, nrow=2, ncol=1)

gglagplot(beer)

ac <- ggAcf(beer)

ac$data

ggAcf(beer)
