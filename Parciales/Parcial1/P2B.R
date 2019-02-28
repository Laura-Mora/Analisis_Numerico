#PUNTO 2B

f=function(x){tan(pi*x)}
g=function(x){cos(pi*x)}
F1=function(x){f(x)-g(x)}
fp= function(x){pi*(sin(pi*x)+(1/(cos(pi*x)^2)))}

Newton<-function(a,b,t0){
  if(dfdt(a)*dfdt(b)<0){
    error=100
    ant=0
    cont=0
    newt=F1(t0)/fp(t0)
    cat("\t","x0","\t","E","\n")
    while(error>0.0001){
      error=abs(t0-ant)/abs(t0)
      cat("\t",t0,"\t",error,"\n")
      ant=t0
      t0=newt
      newt=F1(t0)/fp(t0)
      cont=cont+1
    }
  }else{
    print("Ingrese otro intervalo ya que el ingresado no tiene raiz única para ser calculada.")
  }
  return(t0)
}
min=Newton(4,2,2)
cat("El primer tiempo (positivo) en el cual la distancia es mínima es de:",min,".\nEn este tiempo la distancia de la párticula al punto es de:",f(min),".\n")