library(fpp2)

data("austourists")


austourists

class(austourists)

tourists <- window(austourists, frequency = 4, start = c(2005,01))

tourists

help(decompose)

decomp <- decompose(tourists, type =  "multiplicative")
decomp

autoplot(decomp)

ts.mult <- hw(tourists, h=8, seasonal = "multiplicative")

autoplot(tourists)+
  autolayer(ts.mult)

ts.add <- hw(tourists, h=8, seasonal= "additive")

autoplot(tourists)+
  autolayer(ts.add)

accuracy(ts.mult)
accuracy(ts.add)

checkresiduals(ts.mult)
checkresiduals(ts.add)
