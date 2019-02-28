secante = function(f, x0, x1, tol, maxiter = 100){ 
  f0 = f(x0)
f1 = f(x1)
k=0
while (abs(x1 - x0) > tol && k <= maxiter ) {
  k = k+1
  pendiente = (f1 - f0)/(x1 - x0)
  if (pendiente == 0) 
    return( cero = NA, f.cero = NA, iter = k, ErrorEst = NA) 
  x2 = x1 - f1/pendiente
  f2 = f(x2)
  x0 = x1; f0 = f1
  x1 = x2; f1 = f2
# Imprimir iteraciones
  cat(x1, x2, abs(x1-x0), "\n")
}
if (k > maxiter) {
  warning("No se alcanzó el número de iteraciones")
}
#return(list(cero=x2, f.cero=f2, iter=k, ErrorEst =abs(x2-x1)))
}
##--- Pruebas
f =  function(x) x-cos(x)
plot(f,0, 1)
secante(f, 0, 2, 1e-15, 10)
#0.585454927933219 0.585454927933219 1.41454507206678
#0.717134868255196 0.717134868255196 0.131679940321977
#0.739900765490124 0.739900765490124 0.0227658972349276
#0.739081136054205 0.739081136054205 0.000819629435918068
#0.739085132495594 0.739085132495594 3.99644138893152e-06
#0.739085133215161 0.739085133215161 7.19566961571161e-10
#0.739085133215161 0.739085133215161 7.7715611723761e-16
