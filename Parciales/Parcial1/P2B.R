#PUNTO 2B

f=function(x){tan(pi*x)}
g=function(x){cos(pi*x)}
F1=function(x){f(x)-g(x)}
fp= function(x){pi*(sin(pi*x)+sec^2(pi*x))}

Newton<-function(a,b,t0){
  if(dfdt(a)*dfdt(b)<0){
    error=100
    ant=t0
    cont=0
    newt=F1(t0)/fp(t0)
    cat("\t","x0","\t","x1","\t","E","\n")
    while(error>0.0001){
      error=abs(t0-ant)/abs(t0)
      cat("\t",x0,"\t",x1,"\t",error,"\n")
      newt=F1(x)/fp(x)
      ant=t0
      cont=cont+1
    }
  }else{
    print("Ingrese otro intervalo ya que el ingresado no tiene raiz única para ser calculada.")
  }
  return(t0)
}
min=Newton(60,70,30)
cat("El primer tiempo (positivo) en el cual la distancia es mínima es de:",min,".\nEn este tiempo la distancia de la párticula al punto es de:",f(min),".\n")