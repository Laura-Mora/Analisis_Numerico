#Remueve objetos del entorno y crea función a la que se le hallará las raices
f <- function(x)
{
  return (exp(x)/pi)
}
#f->función a la que hay que encontrarle las raices 
#a->valor sobre el que se efectua la función, b -> valor de la iteración anterior, c-> contador
pf <- function(a,b,c)
{
  #print(paste0("::",a, " | ", b) )
  
  error=abs(a-b)/abs(a)
  cat("X=",a,"\tE=",error,"\n")
  if(error < 0.000000001 || a == -Inf || a == Inf)
  {
    #prom=(a+b)/2
    
    cat("Iteraciones: ",c,"\n")
    return(a)
  }
  else
  {
    pf(f(a),a,c+1)
  }
  
}

puntof <- function(a)
{
  return ( pf(a,a-1,0) )
}

puntof(1)