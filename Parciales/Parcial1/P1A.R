#PUNTO 1A

gauss = function(A, b){  # Se supone det(A) != 0
  cont=0
  n = nrow(A) 
  
  Ab = cbind(A,b)
  
  for (k in 1:(n-1)){    
    if(Ab[k,k]==0){      # intercambio de fila
      fila = which(Ab[k, ]!=0)[1]
      Ab[c(k, fila),  ] = Ab[c(fila, k),  ]
    }
    # Eliminación columna k
    for (i in (k+1):n){# debajo de la diagonal
      # Fi = Fi - a_ik/a_kk * Fk, i=k+1,...,n
      Ab[i, ] = Ab[i, ] - Ab[i, k]/Ab[k,k]*Ab[k, ]
      cont=cont+1;
    } }
  # Sustitución hacia atrás------------------------- # b(i) = A[i, n+1]
  x = rep(NA, times=n)
  x[n] = Ab[n, n+1]/Ab[n,n] # xn = bn/a_nn for(i in (n-1):1 ){
  for(i in(n-1):1){
    x[i]= (Ab[i, n+1]-sum(Ab[i,(i+1):n]*x[(i+1):n]))/Ab[i,i]
  }
  cat("# multiplicaciones Gauss sin pivoteo", cont,"\n")
  return(x) 
}
#--- Pruebas

A = matrix(c( 0,  2,  3, 3,
              -5, -4,  1, 4,
              0,  0,  0, 3,
              -4, -7, -8, 9), nrow=4, byrow=TRUE)
b = c(1,0,0,0)
##
gauss(A,b)
solve(A,b)

gaussPP = function(A, b){
  cant=0
  if(is.matrix(A)) {
    n = nrow(A); m = ncol(A)
    if (m != n) stop("’A’ debe ser una matriz cuadrada.")
  }
  # matriz ampliada
  Ab = cbind(A,b)
  # Eliminación
  for (k in 1:(n-1)){   # desde columna k=1 hasta k=n-1
    # índice del pivote máximo, en valor absoluto
    # wich.max( A[k:n,k] ) retorna índice del vector A[k:n,k] = (a_kk, a_(k+1)k,...,a_nk) # Como a_kk tendría índice 1, hay que corregir el índice sumando k-1.
    fila = which.max( abs(A[k:n,k]) ) + k-1
    Ab[c(k, fila), ] = Ab[c(fila, k), ]
    
    # Si pivote es cero, det A = 0!
    if(A[fila,k]==0)  stop("La matriz es singular")
    # Eliminación columna k
    for (i in (k+1):n){# debajo de la diagonal
      # Fi = Fi - a_ik/a_kk * Fk, i=k+1,...,n
      Ab[i, ] = Ab[i, ] - Ab[i, k]/Ab[k,k]*Ab[k, ]
      cant=cant+1
    }
  }
  # Sustitución hacia atrás------------------------- # b(i) = A[i, n+1]
  x = rep(NA, times=n)
  x[n] = Ab[n, n+1]/Ab[n,n] # xn = bn/a_nn 
  for(i in (n-1):1 ){ # for
  x[i]= (Ab[i, n+1] -sum(Ab[i, (i+1):n]*x[(i+1):n]) ) /Ab[i,i] 
  cant=cant+1
  }
  cat("# multiplicaciones Gauss con pivoteo", cant,"\n")
return(x) 
  }
#--- Pruebas
A = matrix(c( 0,  2,  3, 3,
              -5, -4,  1, 4,
              0, 0, 0, 3,
              -4, -7, -8, 9), nrow=4, byrow=TRUE)
b = c(1,0,0,0)

gaussPP(A,b) # Se obtine la solución -1.2580645 1.4193548 -0.6129032 0.0000000 solve(A,b) # [1] -1.2580645 1.4193548 -0.6129032 0.0000000