f<-function(t){sqrt((2*cos(t)-2)^2+(sin(t)-1)^2)}
dfdt<-function(t){(4*sin(t)-(3*sin(t)+1)*cos(t))/(sqrt((sin(t))^2-2*sin(t)+4*(cos(t))^2-8*cos(t)+5))}
Newton<-function(a,b,t0){
  if(dfdt(a)*dfdt(b)<0){
    error=100
    ant=t0
    cont=0
    d2fdt2=deriv(~(4*sin(t)-(3*sin(t)+1)*cos(t))/(sqrt((sin(t))^2-2*sin(t)+4*(cos(t))^2-8*cos(t)+5)),"t",TRUE)
    if(t0==0.5){
      plot(seq(0.58,0.61,0.0001),dfdt(seq(0.58,0.61,0.0001)),type="l",col="blue")
    }else if(t0==30){
      plot(seq(23,70,0.0001),dfdt(seq(23,70,0.0001)),type="l",col="blue")
    }else{
      plot(seq(a,b,0.0001),dfdt(seq(a,b,0.0001)),type="l",col="blue")
    }
    abline(h=0,col="black")
    while(error>0.0001){
      if(attr(d2fdt2(t0),"gradient")[1]!=0){
        t0=t0-dfdt(t0)/(attr(d2fdt2(t0),"gradient")[1])
      }else{
        print("Método no converge con dicho valor inicial")
        break
      }
      error=abs(t0-ant)/abs(t0)
      ant=t0
      text(t0,0,cont,cex=1.5,col="red")
      cont=cont+1
    }
  }else{
    print("Ingrese otro intervalo ya que el ingresado no tiene raiz única para ser calculada.")
  }
  return(t0)
}
min=Newton(0,1,0.5)
cat("El primer tiempo (positivo) en el cual la distancia es mínima es de:",min,".\nEn este tiempo la distancia de la párticula al punto es de:",f(min),".\n")