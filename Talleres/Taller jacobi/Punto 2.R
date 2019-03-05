#Punto 2
rm(list=ls())
library(pracma)
library(Matrix)
n=4#Tam matriz
#Matriz A
A = matrix(c(-8.1, -7, 6.123, -2,
             -1, 4,-3, -1,
             0, -1, -5, 0.6,
             -1, 0.33, 6, 1/2), 
           nrow=n, byrow=TRUE)
#Matriz B
b = matrix(c(1.45,3,5.12,-4), nrow=n, byrow=TRUE)

toleracia=1e-9

#Punto a descomposicion

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
#Punto b
solGS=itersolve(A,b,x0=c(1,2,1,1),toleracia,method = "Gauss-Seidel")
print(solGS)
#Punto c
solJacobi=itersolve(A, b, x0 = c(1,2,1,1), nmax = 5, toleracia, method = c("Jacobi"))
print(solJacobi)
