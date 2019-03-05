#Punto 1
library(pracma)
library(Matrix)

#Parte a
n=4
D1<-eye(n, m = n)#Diagonal de la matriz 
D2<-ones(n, m = n)#Llena la matriz de 1
D3<-zeros(n, m = n)#Llena la matriz de 0

#Matriz A
A = matrix(c(-8.1, -7, 6.123, -2,
             -1, 4,-3, -1,
             0, -1, -5, 0.6,
             -1, 0.33, 6, 1/2), 
           nrow=n, byrow=TRUE)
#Matriz B
b = matrix(c(1.45,3,5.12,-4), nrow=n, byrow=TRUE)

print(D1)
print(D2)
print(D3)

#Parte b
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