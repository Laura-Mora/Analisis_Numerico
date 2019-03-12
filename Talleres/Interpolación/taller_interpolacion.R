#install.packages("Matrix")#instalar paquete
#install.packages("PolynomF")#instalar paquete
library(Matrix)
library(PolynomF)

#Vectores iniciale
x= c(6,8,10,12,14,16,18,20)
y= c(8.4,9.2,12.7,18.4, 21.6, 17.9,11,9)

plot(x,y, pch=19, cex=1, col = "pink", asp=1) 
par(new = TRUE)
# Polinomio de ajuste 7 (polinomio interpolante en este caso)
datx = x[1:8]; daty = y[1:8]
polyAjuste = poly.calc(datx,daty)
polyAjuste
plot(datx,daty, pch=19, cex=1, col = "red", asp=1) 
curve(polyAjuste,add=T,from=0, to=21)
print(polyAjuste)
x7=polyAjuste(x)
y7=polyAjuste(y)
par(new = TRUE)

#Polinomio de ajuste 3
datX3=c(x[1],x[3],x[5], x[7]); datY3=c(y[1],y[3],y[5], y[7])
polyAjuste3 = poly.calc(datX3,datY3)
polyAjuste3
plot(datX3,datY3, pch=19, cex=1, col = "blue", asp=1, axes=FALSE) 
curve(polyAjuste3,add=T,from=0, to=21)
print(polyAjuste3)
x3=polyAjuste3(x)
y3=polyAjuste3(y)
par(new = TRUE)

#Polinomio de ajuste 5
datx5=c(x[1],x[3],x[5], x[6],x[7],x[8]); daty5=c(y[1],y[3],y[5], y[6],y[7],y[8])
polyAjuste5 = poly.calc(datx5,daty5)
polyAjuste5
plot(datx5,daty5, pch=19, cex=1, col = "purple", asp=1, axes=FALSE) 
curve(polyAjuste5,add=T)
print(polyAjuste5)
x5=polyAjuste5(x)
y5=polyAjuste5(y)

tabla=matrix(c(x,y,y7,y3,y5), ncol=8, byrow=TRUE)
rownames= c("x","y","Poly7","Poly3","Poly5")

tabla

  