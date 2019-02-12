Fx<-function(n,a,x){
  return ((1/n)*(x+a/x))
}

raiz<-function(n,a,x0){
  error=100
  i=0
  while(error>0.00000001){
    x=Fx(n,a,x0)
    error=abs(x0-x)/abs(x0)
    cat("X = ",x0,"\t","n=",n,"\t","a=",a,"\t", "error=",error,"\n")
    x0=x
    i=i+1
  }
  cat("La raiz n-esima (n=",n,") de ",a," es ",x," con un total de ",i, " iteraciones.", "\n")
}
raiz(2,81,2)
