#Punto 3
rm(list=ls())
library(pracma)
library(Matrix)
#Matriz A
A = matrix(c(4, -1, -1, -1, -1, 4,
             -1, -1, -1, -1, 4, -1,
             -1, -1, -1, 4), nrow=4, byrow=TRUE)
#Matriz b
b = c(1, 5, 1.5,-2.33)

#Punto a

calPol<-function(a){
  charpoly(a, info = TRUE)
}


#Punto b
toleracia=1e-9
solGS=itersolve(A,b,x0=NULL,nmax = 1000,toleracia,method = "Gauss-Seidel")
print(solGS)
solJacobi=itersolve(A, b, 1:4,nmax = 1000, toleracia, method = c("Jacobi"))
print(solJacobi)
solRichar=itersolve(A, b, x0 = NULL, nmax = 1000, toleracia, method = c("Richardson"))
print(solRichar)
#Punto c

# w grado de relajación 0<w<2
fSOR<-function(a,n,w){
  D=a*eye(n, m = n)
  L<-a
  L[lower.tri(L, diag = FALSE)]<-0
  auxT=(D-(w*L))
  auxT1=inv(auxT)
  U<-a
  U[upper.tri(U, diag = FALSE)]<-0
  auxT2=((1-w)*D+(w*U))
  Tfinal=auxT1*auxT2
}
fSOR(A,4,1)
#Jacobi
jacobi<-function(a,n){
  D=a*eye(n, m = n)
  L<-a
  L[lower.tri(L, diag = FALSE)]<-0
  U<-a
  U[upper.tri(U, diag = FALSE)]<-0
  AA=D+L+U
  print(AA)
}
jacobi(A,4)

#Punto d
solucion<- solve(A,b)
print(solucion)