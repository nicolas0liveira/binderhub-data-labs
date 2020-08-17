sim=SIMULA80_reg
reg.lm=lm(data = sim, y~.)
reg.lm

simx=model.matrix(data=sim,~.)
simx=as.data.frame(simx)
X=simx[,1:5]; X=as.matrix(X)
Y=simx[6] # forma mais conveniente; experimentem simx [,6]
Y=as.matrix(Y)
# dos betas
betas=solve(t(X)%*%X)%*%t(X)%*%Y
betas

#gere uma nova coluna x5=x1+x2 e repita. 
#veja o que acontece com multicolinearidade