a=c(1,2,5)   
b=c(4,3,8)

# The c function creates an "atomic" vector:
#atomic vectors, since their components cannot be broken down into smaller components.

#produto escalar
crossprod(a,b)
#ou de forma genérica como produto de matrizes:
t(a) %*% b

#módulo de um vetor
mod_a=sqrt(crossprod(a,a));mod_a

#projeção de b sobre a
q=crossprod(a,b)/crossprod(a);q
q
w=q*a;w

#Produto de matrizes
a1=c(1,5,4)
a2=c(6,4,2)
A=rbind(a1,a2);A
b1=c(2,4)
b2=c(1,3)
b3=c(5,2)
B=rbind(b1,b2,b3);B

t(A)
AB=A%*%B;AB
B%*%A

#traço
diag(AB) # vetor com elementos da diagonal
sum(diag(AB))


A=matrix(c(5,3,4,6), nrow = 2);A
det(A)


x1=c(3,5,12)
x2=c(4,2,9)
x3=c(8,2,3)
X=rbind(x1,x2,x3);X

a1=c(1,-1,1)
a2=c(2,1,3)
a3=c(1,1,1)
A=rbind(a1,a2,a3)
A
det(A)
Ainv=solve(A);Ainv
det(Ainv)
Ainv%*%A

a1=c(1,-1,1)
a2=c(2,1,3)
a3=c(1,1,1)
A=rbind(a1,a2,a3)
b=c(4,0,0)
Ainv=solve(A);Ainv
x=Ainv%*%b;x



print(cor(FREES),digits = 3)
print(cov(FREES),digits = 3)

W=matrix(c(1,2,5,4), nrow = 2)
W
eigen(W)

Q=matrix(c(1,2,-2,4), nrow = 2)
Q
eigen(Q)

a=c(1,0,0)
b=c(0,1,0)
c=c(0,0,3)
D=rbind(a,b,c)
D
eigen(D)

options(scipen = 20)
a=c(1,2,0)
b=c(0,1,0)
c=c(0,0,3)
D=rbind(a,b,c)
D
eigen(D)


C=cov(IRIS)
cov2cor(C)
R=cor(FREES)
round(R,3)
EIG=eigen(R)
EIG$values
AV=EIG$vectors
round(AV,3)
round(AV%*%t(AV),5)
round(t(AV)%*%AV,5)

D=diag(c(2.91081808, 0.92122093, 0.14735328, 0.02060771))
D
AV%*%D%*%t(AV)

mat=cbind(cig$NICOTINA,cig$CO)
mat=as.data.frame(mat)
pad=scale(mat)
pad=as.data.frame(pad)
mat
pad

mat$V1
mat$V2
round(pad$V1,3)
round(pad$V2,3)
R=cor(pad)
R
auto=eigen(R)
auto$values
auto$vectors
plot(cc$CO,cc$NICOTINA, col="black", pch=16, cex=1.5)
grid(col="red")
abline(c(0,1), col="blue",lwd=2)
abline(c(0,-1))

A
x=c(2,3,4)
A%*%x

