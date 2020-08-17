#install.packages(c("fpp2", "forecast", "tidyverse","readxl"))
library(fpp2)
library(forecast)
library(tsibble)
library(readxl)
library(gridExtra)


g1 <- autoplot(goog200)+ ggtitle("a")
g2 <- autoplot(diff(goog200))+ggtitle("b")
g3 <- autoplot(strikes)+ggtitle("c")
g4 <- autoplot(hsales)+ ggtitle("d")                                
g5 <- autoplot(eggs)+ ggtitle("e")
g6 <- autoplot(pigs)+ ggtitle("f")
g7 <- autoplot(lynx) + ggtitle("g")
g8 <- autoplot(beer)+ ggtitle("h")
g9 <- autoplot(elec)+ ggtitle("i")

grid.arrange(g1,g2,g3,g4, g5,g6,g7,g8,g9, nrow = 3, ncol = 3)


grid.arrange(g1,g2, ncol = 2)

ac1 <- ggAcf(goog200)
ac2 <- ggAcf(diff(goog200))

grid.arrange(ac1, ac2, ncol = 2)

Box.test(diff(goog200), lag=10, type="Ljung-Box")
