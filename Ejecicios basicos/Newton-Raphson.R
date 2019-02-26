newton1 = function(f, fp, x0, tol, maxiter){ k=0
# Imprimir estado
cat("---------------------------------------------------------------------------\n") 
cat(formatC( c("x_k"," f(x_k)","Error est."), width = -20, format = "f", flag = " "), "\n") 
cat("---------------------------------------------------------------------------\n")
repeat{
  correccion = f(x0)/fp(x0)
  x1 = x0 - correccion
  dx = abs(x1-x0)
  # Imprimir iteraciones
  
  cat(formatC( c(x1 ,f(x1), dx), digits=15, width = -15, format = "f", flag = " "), "\n")
  x0 = x1
  k = k+1
  # until
  if(dx <= tol || k > maxiter ) break;
}
cat("---------------------------------------------------------------------------\n") 
if(k > maxiter){
  cat("Se alcanzó el máximo número de iteraciones.\n")
  cat("k = ", k, "Estado: x = ", x1, "Error estimado <= ", correccion) 
  } else {
    cat("k = ", k, " x = ", x1, " f(x) = ", f(x1), " Error estimado <= ", correccion) 
  }
}
## --- Pruebas
f  = function(x) x-cos(x)
fp = function(x) 1+sin(x)
options(digits = 15)
newton1(f,fp, 0, 0.0000005, 10)
# ---------------------------------------------------------------------------
#     x_k                   f(x_k)              Error est.
# ---------------------------------------------------------------------------
#   1.000000000000000
#   0.750363867840244
#   0.739112890911362
#   0.739085133385284
#   0.739085133215161
# ---------------------------------------------------------------------------
# k = 5 x = 0.739085133215161 f(x) = 0 Error estimado <= 1.70123407014033e-10
## --- Comentario: con 15 decimales:
# x = 0.739085133215161 y f(0.739085133215161)=5.55111512312578e-16