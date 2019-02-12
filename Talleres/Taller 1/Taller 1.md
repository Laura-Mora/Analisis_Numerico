


# Taller 1

>Realizado por: Rafael Salvador Frieri, Daniel Hamilton-Smith, Laura Juliana Mora

## Punto 1

Evaluar el valor de un polinomio es una tarea que involucra para la maquina realizar un número de operaciones la cual debe ser mínimas. Para cada una de las siguientes polinomios, hallar P(x) en el valor indicado y el número de operaciones mínimo para hacerlo(sugerencia utilizar el algoritmo Horner)

     # Se declara la función para resolver ecuaciones por método de Horner
          Horner<-function(p,n,x0){
	      y=p[1]
	      sumas=0
	      multis=0
	      for(i in c(2:n)){
	        y=x0*y+p[i]
	        sumas=sumas+1
	        multis=multis+1
	      }
	      tot=c(y,sumas,multis)
	      return(tot)
    }
    # Se evaluan los polinomios y se muestra su resultado, número de operaciones óptimas y número de operaciones convencionales
    Px=c(2,0,-3,3,-4)
    n=length(Px)
    x0=-2
    res1=Horner(Px,n,x0)
    res2=Evaluation(Px,n,x0)
    
    cat("El resultado del polinomio 2x^4-3x^2+3x-4 evaluado en x0=-2 es:",res1[1],"\nEl número de operaciones realizadas evaluando convencinalmente fue de:",res2[2]+res2[3],", con ",res2[2]," sumas y ",res2[3]," multiplicaciones.\nEl número de operaciones realizadas con el método de Horner fue de:",res1[2]+res1[3],", que es el número de operaciones mínimo para evaluar el polinomio, con ",res1[2]," sumas y ",res1[3]," multiplicaciones.\n\n")
    
    Px=c(7,6,-6,0,3,-4)
    n=length(Px)
    x0=3
    res1=Horner(Px,n,x0)
    res2=Evaluation(Px,n,x0)
    
    cat("El resultado del polinomio 7x^5+6x^4-6x^3+3x-4 evaluado en x0=3 es:",res1[1],"\nEl número de operaciones realizadas evaluando convencinalmente fue de:",res2[2]+res2[3],", con ",res2[2]," sumas y ",res2[3]," multiplicaciones.\nEl número de operaciones realizadas con el método de Horner fue de:",res1[2]+res1[3],", que es el número de operaciones mínimo para evaluar el polinomio, con ",res1[2]," sumas y ",res1[3]," multiplicaciones.\n\n")
    
    Px=c(-5,0,3,0,2,-4,0)
    n=length(Px)
    x0=-1
    res1=Horner(Px,n,x0)
    res2=Evaluation(Px,n,x0)
    
    cat("El resultado del polinomio -5x^6+3x^4+2x^2-4x evaluado en x0=-1 es:",res1[1],"\nEl número de operaciones realizadas evaluando convencinalmente fue de:",res2[2]+res2[3],", con ",res2[2]," sumas y ",res2[3]," multiplicaciones.\nEl número de operaciones realizadas con el método de Horner fue de:",res1[2]+res1[3],", que es el número de operaciones mínimo para evaluar el polinomio, con ",res1[2]," sumas y ",res1[3]," multiplicaciones.\n\n")

Tras ejecutar este código se obtiene 

    El resultado del polinomio 7x^5+6x^4-6x^3+3x-4 evaluado en x0=3 es: 2030 
    El número de operaciones realizadas evaluando convencinalmente fue de: 20 , con  5  sumas y  15  multiplicaciones.
    El número de operaciones realizadas con el método de Horner fue de: 10 , que es el número de operaciones mínimo para evaluar el polinomio, con  5  sumas y  5  multiplicaciones.
    
    El resultado del polinomio -5x^6+3x^4+2x^2-4x evaluado en x0=-1 es: 4 
    El número de operaciones realizadas evaluando convencinalmente fue de: 27 , con  6  sumas y  21  multiplicaciones.
    El número de operaciones realizadas con el método de Horner fue de: 12 , que es el número de operaciones mínimo para evaluar el polinomio, con  6  sumas y  6  multiplicaciones.

## Punto 2

La eficiencia de un algoritmo es denotada por **T(n)**
Dado el siguiente algoritmo

	leer n
	Mientras n>0 repita
		d<- mod(n,2)
		n<-fix(n/2)
		Mostrar d
	fin

 a) Recorra el algoritmo con n=73
 b) Suponga que **T(n)** representa la cantidad de operaciones aritméticas de división se realizan para resolver el problema de tamaño n. Encuentre **T(n)** y exprésela con la notación **O( )** Para obtener **T(n)** observe el hecho de que en cada ciclo el valor de **n** se reduce aproximadamente a la mitad.

    p <- function(val)
    {
      
      #val <- readline(" cantidad ")
      val <- as.numeric((val))
      cant <- 0
      while(val>0)
      {
        cant <-cant+1
        d    <- val%%2
        val  <-val/2
        val  <- floor(val)
        
        if(val > 1)
        {
          print("-----------")
          print(val)
          print(d) 
        }
      } 
      
      print ("cantidad de repeticiones")
      print(cant)
    }
    
    p(73)
    
    T<- function(n){floor(log2(n)+1)}
    
    print(T(73))

Tras ejecutar el código obtenemos la siguiente salida:

    [1] "-----------"
    [1] 36
    [1] 1
    [1] "-----------"
    [1] 18
    [1] 0
    [1] "-----------"
    [1] 9
    [1] 0
    [1] "-----------"
    [1] 4
    [1] 1
    [1] "-----------"
    [1] 2
    [1] 0
    [1] "cantidad de repeticiones"
    [1] 7
    [1] 7


## Punto 3
Utilice el método de Newton para resolver el problema, muestre gráficamente cómo se comporta la convergencia a la solución
Una partícula se mueve en ele espacio con el vector de posición **R(t)=(2cos(t), sen(t),0)**. Se requiere conocer el tiempo en el que el objeto se encuentra más cerca de punto **P(2,1,0)**. Utilice el método Newton con cuatro decimades de precisión.

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

Al realizar la entrada de Newton(0,1,0.5) obtenemos como salida 

    El primer tiempo (positivo) en el cual la distancia es mínima es de: 0.5872198 .
    En este tiempo la distancia de la párticula al punto es de: 0.5577801 .
Y la gráfica 
![gráfica ejercicio Newton ](https://github.com/Laura-Mora/Analisis_Numerico/blob/master/Talleres/Taller%201/Grafica%20newton.png)

## Punto 4

Resolver por dos métodos diferentes, grafique las soluciones y comparar sus soluciones 
Encuentre una intersección de las siguientes ecuaciones en coordenadas polares **r=2+cos(3*t), r=3-e^t)**

    # Se tienen dos ecuaciones, igualándolas y dejando los términos de un solo lado resulta la siguiente ecuación
    f<-function(t){cos(3*t)+exp(t)}
    
    # Para el método de Newton la derivada de f(t) es
    dfdt<-function(t){exp(t)-3*sin(3*t)}
    
    # Se declara función para cálculo de r luego de obtener t
    r<-function(t){2-exp(t)}
    
    # Se declara otra función para el cálculo de r para poder graficar soluciones
    r1<-function(t){2+cos(3*t)}
    
    #Se resolverá por el método de bisección y el método de Newton
    
    biseccion<-function(a,b){
      if(f(a)*f(b)<0){
        c=(a+b)/2
        error=100
        ant=c
        while(error>0.00000001){
          if(f(c)==0){
            break
          }else{
            if(f(a)*f(c)<0){
              b=c
            }else{
              a=c
            }
          }
          c=(a+b)/2
          error=abs(c-ant)/abs(c)
          ant=c
        }
      }else{
        print("No hay una solución única en el intervalo escogido")
      }
      return(c)
    }
    
    Newton<-function(a,b,t0){
      if(f(a)*f(b)<0){
        error=100
        ant=t0
        while(error>0.00000001){
          if(dfdt(t0)!=0){
            t0=t0-f(t0)/dfdt(t0)
          }else{
            print("El método no converge para el t0 inicial")
          }
          error=abs(t0-ant)/abs(t0)
          ant=t0
        }
      }else{
        print("El intervalo escogido no tiene una raiz única.")
      }
      return(t0)
    }
    
    # Se usan ambos métodos para hallar una solución de la ecuación (Preferiblemente la misma, ya que tiene infinitas soluciones por las oscilaciones del coseno)
    # Se calcula el punto de intersección con ambos métodos y se muestran por la consola
    
    tRaizBi=biseccion(-1,-0.5)
    rBi=r(tRaizBi)
    cat("La solución dada por el método de bisección fue: t=",tRaizBi,", r= ",rBi,"\n")
    
    tRaizNe=Newton(-1,-0.5,-0.6)
    rNe=r(tRaizNe)
    cat("La solución dada por el método de Newton fue: t=",tRaizNe,", r= ",rNe,"\n")
    
    # Realización de la gráfica
    plot(seq(-1,-0.5,0.000001),r(seq(-1,-0.5,0.000001)),type="l",col="blue")
    lines(seq(-1,-0.5,0.000001),r1(seq(-1,-0.5,0.000001)),type="l",col="red")
    abline(h=0,col="black")
    points(rbind(c(tRaizBi,rBi)),pch=17,cex=1.5,col="red")
    points(rbind(c(tRaizNe,rNe)),pch=17,cex=1.5,col="blue")

    Tras ejecutar obtenemos la siguiente gráfica y salida
    La solución dada por el método de bisección fue: t= -0.6973291 , r=  1.502087 
    La solución dada por el método de Newton fue: t= -0.6973291 , r=  1.502087 

![gráfica 1 punto 4 ](https://github.com/Laura-Mora/Analisis_Numerico/blob/master/Talleres/Taller%201/grafica%20p4.png)

Se puede observar tanto en la consola como en la gráfica que las soluciones dadas por el método
de bisección y por el método de Newton son muy cercanas (comienzan a diferir en luego de de 1*10^-8), por lo cual en la gráfica cuando se
grafican ambos puntos no se puede diferenciar el rojo del azul ya que está uno encima del otro.
Sin embargo, la solución del método de Newton fue más acertada en cantidad de cifras significativas
a la solución real luego de realizar una comparación con una calculadora que tuviera una mayor
precisión. Es posible que esto se de debido a que como la convergencia del método de Newton es
cuadrática cuando el error se acerca al límite dado este se acerque en su última iteración más
a la solución real que el método de bisección que tiene convergencia lineal.

Para observarlo en coordenadas polares se agrego el siguiente código

    plot.new()
    polar <- function (theta, r, color=4){
      y <- 0
      x <- 0
      ejex <- 1
      for (i in 1:length(r)){
        if(is.nan(r[i])== T){
          r[i] <- 0
        }
      }
      
      angulo <- seq(-max(theta),max(theta),by=theta[2]-theta[1])
      y <- r*sin(theta)
      x <- r*cos(theta)
      plot.window(xlim = c(-max(r), max(r)), ylim = c(-max(r), max(r)), asp = 1)
      
      aux <- max(r)
      while (aux > 0){
        fi <- aux*sin(angulo)
        cir <- aux*cos(angulo)
        points(cir,fi,pch="-",col="gray",cex=0.3)
        text(ejex+0.2,-0.2,ejex,col="gray")
        ejex <- ejex + 1
        aux <- aux - 1
      }
      
      abline(v=((max(cir)+min(cir))/2),col="gray")
      abline(h=((max(cir)+min(cir))/2),col="gray")
      segments(-max(r)+0.5,-max(r)+0.5,max(r)-0.5,max(r)-0.5,col="gray")
      segments(-max(r)+0.5,max(r)-0.5,max(r)-0.5,-max(r)+0.5,col="gray")
      
      points(x,y,pch=20,col=color,cex=1)
    }
    
    dim <- seq(-1,1,by=0.01)
    r2=r(dim)
    polar(dim,r2,"blue")
    r3=r1(dim)
    polar(dim,r3,"red")

Y se obtiene como gráfica: 
![gráfica polar](https://github.com/Laura-Mora/Analisis_Numerico/blob/master/Talleres/Taller%201/grafica4-2.png)

## Punto 13

Encuentre una fórmula iterativa de convergencia cuadrática y defina un intervalo de convergencia apropiado para calcular la raíz real n-ésima de un número real. El algoritmo solamente debe incluir operaciones aritméticas elementales.

    #La función sale de despejar de el método Newton y el como hallar una raíz
    #n-> indice de la raíz a-> radicando x-> por donde se empieza a iterar
    Fx<-function(n,a,x){
      return ((1/n)*(x+a/x))
    }
    #Función de raíz
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
Para el ejemplo propuesto en el código obtenemos la siguiente salida

    X =  2 	 n= 2 	 a= 81 	 error= 9.625 
    X =  21.25 	 n= 2 	 a= 81 	 error= 0.4103114 
    X =  12.53088 	 n= 2 	 a= 81 	 error= 0.242076 
    X =  9.497456 	 n= 2 	 a= 81 	 error= 0.05100612 
    X =  9.013028 	 n= 2 	 a= 81 	 error= 0.001444401 
    X =  9.000009 	 n= 2 	 a= 81 	 error= 1.046167e-06 
    X =  9 	 n= 2 	 a= 81 	 error= 5.471179e-13 
    La raiz n-esima (n= 2 ) de  81  es  9  con un total de  7  iteraciones. 

## Punto 14

El siguiente es un proceso intuitivo para calcular una raíz real positiva de la ecuación **f(x)=0** en un intervalo **[a,b]** con precisión **E**:
A partir de **x=a** evalúe **f(x)** incrementando x en un valor d. Inicialmente **d=(b-a)/10**. Cuando f cambie de signo, retroceda **x** al punto anterior **x-d,** reduzca **d** al valor **d/10** y evalúe nuevamente f hasta que cambie de signo. Repita este procedimiento hasta que d sea menor que **E**.

a) De manera formal escriba las condiciones necesarias para que la raíz exista, sea única y pueda ser calculada.
b) Indique el orden de convergencia y estime el factor de convergencia del método.
c) Describa el procedimiento anterior en notación algorítmica, o en MATLAB o en Python.

        #Fx <- function(x){(exp(x))}
    Fx <- function(x){  5*x-exp(x)-1}
    
    #Fx <- function(t){sqrt((2*cos(t)-2)^2+(sin(t)-1)^2)}
    
    func <- function(a,b)
    {  
      E<-0.000000001
    
      print(Fx(a))
      print(Fx(b))
      
      print("inicio")
    
      if(b<a) # b tiene que ser mayor a "a"
      {
        temp<-a
        a<-b
        b<-temp
      }
    
      return(funcrec(a,b,a,(b-a)/10,E))
    }
    
    funcrec <- function(ini,fin,pos,mov,E)
    {
      if ( mov < E)
      {
        return(pos)
      }
      else
      {
        if(pos < ini || pos > fin)
        {
          print("rango superado")
          return(NaN)
        }
    
        print("-------")
        print(Fx(pos))
        #print(pos)
        #print("mov")
        #print(mov)
        
        #mov <- (fin-ini)/10
    
        if( abs(Fx(pos)) < 1 ) # si esta cerca, pues que se acerque mas 
        {
          print("prueba")
          print(abs(Fx(pos))*mov)
          #mov <- (fin-pos)/10
    
          mov <- abs(Fx(pos))*mov
        }
        else
        {
          mov <- (fin-pos)/10
        }
        
        if(Fx(pos) < 0)
        {
          print("suma")
          return(funcrec(ini,fin,pos+mov,mov,E))
        }
        else if( 0 < Fx(pos) )
        {
          print("resta")
          return(funcrec(ini,fin,pos-mov,mov,E))
        }
        else
        {
          # puro caso hipotetico en que caiga en el valor exacto
          return(pos)
        }
      }
    }
    
    
    resp <- func(-20,5)
    
    print("el resultado es:")
    print(resp)
    
    print(Fx(resp))

Tras ejecutar el código obtenemos la siguiente salida:

     -101
    [1] -124.4132
    [1] "inicio"
    [1] "-------"
    [1] -101
    [1] "suma"
    [1] "-------"
    [1] -88.5
    [1] "suma"
    [1] "-------"
    [1] -77.25
    [1] "suma"
    [1] "-------"
    [1] -67.125
    [1] "suma"
    [1] "-------"
    [1] -58.01251
    [1] "suma"
    [1] "-------"
    [1] -49.81131
    [1] "suma"
    [1] "-------"
    [1] -42.43038
    [1] "suma"
    [1] "-------"
    [1] -35.78806
    [1] "suma"
    [1] "-------"
    [1] -29.81155
    [1] "suma"
    [1] "-------"
    [1] -24.43679
    [1] "suma"
    [1] "-------"
    [1] -19.60911
    [1] "suma"
    [1] "-------"
    [1] -15.28444
    [1] "suma"
    [1] "-------"
    [1] -11.43105
    [1] "suma"
    [1] "-------"
    [1] -8.031357
    [1] "suma"
    [1] "-------"
    [1] -5.083133
    [1] "suma"
    [1] "-------"
    [1] -2.599445
    [1] "suma"
    [1] "-------"
    [1] -0.6067993
    [1] "prueba"
    [1] 0.3123365
    [1] "suma"
    [1] "-------"
    [1] 0.4254746
    [1] "prueba"
    [1] 0.1328912
    [1] "resta"
    [1] "-------"
    [1] 0.006594581
    [1] "prueba"
    [1] 0.0008763621
    [1] "resta"
    [1] "-------"
    [1] 0.003726355
    [1] "prueba"
    [1] 3.265636e-06
    [1] "resta"
    [1] "-------"
    [1] 0.003715665
    [1] "prueba"
    [1] 1.213401e-08
    [1] "resta"
    [1] "-------"
    [1] 0.003715625
    [1] "prueba"
    [1] 4.508542e-11
    [1] "resta"
    [1] "el resultado es:"
    [1] 0.5460151
    [1] 0.003715625

## Punto 15

Se propone resolver la ecuación $$\int_0^x (5-e^y)dy =2 con el método del punto fijo 
a) Obtenga la ecuación **f(x)=0** resolviendo el integral
b) Mediante un gráfico aproximado, o evaluado directamente, localice la raíces reales. 
c)  Proponga una ecuación equivalente **x=g(x)** y determine el intervalo de convergencia para calcular una de las dos raíces.
d) Del intervalo anterior, elija un valor inicial y realice 5 iteraciones. En cada iteración verifique que se cumple la condición de convergencia del punto fijo y estime el error de truncamiento en el último resultado.

    # Parte a
    
    # Resolviendo y despejando la ecuación se obtiene la siguiente función
    
    f<-function(x){
      5*x-exp(x)-1
    }
    
    # Parte b
    
    # Se grafica la función
    inter=seq(0,3,0.0001)
    plot(inter,f(inter),type="l",col="blue")
    abline(h=0,col="black")
    # Mediante el gráfico aproximado mostrado, se puede observar que las raices reales se encuentran aproximadamente en un poco más de 0,5 y un poco menos de 2,5.
    
    # Parte c
    
    # Se propone utilizar la siguiente ecuación despejada g(x)=x
    
    g<-function(x){
      (1+exp(x))/5
    }
    
    # Para que la serie converja la magnitud de la derivada de g debe ser menor o igual a 1 en cada iteración.
    # Si se deriva g(x), se obtiene g'(x)=(e^x)/5, e igualando a 1, x<=ln(5) para que el método converja con dicho despeje
    
    # Parte d
    
    # Se declara la derivada de g, que se usará para verificar que se cumple la condición de convergencia en cada iteración
    dgdx<-function(x){
      exp(x)/5
    }
    # Se declara función del método de punto fijo solicitado
    puntoFijo<-function(a,b,x0){
      if((g(a)>a && g(b)<b)||(g(a)<a && g(b)>b)){
        ant=x0
        for(i in c(1:5)){
          if(abs(dgdx(x0))>1){ # Verificacion de criterio de convergencia del punto fijo
            print("Se ha dejado de cumplir la condición de convergencia.")
            break
          }
          cat("|g'(x0)|=",abs(dgdx(x0)),"\n") # Se muestra magnitud de derivada para verificar criterio
          x0=g(x0)
          error=abs(x0-ant)/abs(x0)
          ERROR=abs(x0-ant)
          ant=x0
        }
        return(c(x0,error,ERROR))
      }else{
        print("Dicho intervalo no es apto para el método de punto fijo, utilizar otro.")
      }
    }
    # Se llama la función con un intervalo apropiado, y un valor inicial que cumple con los criterios de convergencia, luego se muestran resultados finales
    res=puntoFijo(0,1,0.5)
    cat("Resultado:",res[1],"\nError relativo final:",res[2],"\nError absoluto final:",res[3])

Tras ejecutar el código obtenemos la siguiente salida con la siguiente gráfica: 

    |g'(x0)|= 0.3297443 
    |g'(x0)|= 0.3396996 
    |g'(x0)|= 0.3430983 
    |g'(x0)|= 0.3442664 
    |g'(x0)|= 0.3446687 
    Resultado: 0.5446687 
    Error relativo final: 0.0007387311 
    Error absoluto final: 0.0004023637

![gráfica punto 15](https://github.com/Laura-Mora/Analisis_Numerico/blob/master/Talleres/Taller%201/grafica%20p15.png)

A partir de los cálculos de error se puede estimar que el error relativo estuvo en al rededor de 0.000739 con respecto al último resultado
y 0.000402 de error absoluto con respecto al último y penúltimo resultado. Además luego de comparar el resultado de la última iteración con
un resultado más exacto dado por una calculadora de alto desempeño se encontró que el error fue aún menor, siendo de al rededor de 0.0002.



