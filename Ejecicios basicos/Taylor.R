require(pracma)
f = function(x) log(1+x)
p = taylor(f, 0, 4) # Polinomio de Taylor de orden 4, alrededor de a=0. p # Coeficientes
# [1] -0.250004 0.333334 -0.500000 1.000000 0.000000
# Evaluar en x=0.1
polyval(p, 0.1)
# [1] 0.09530833
log(1+0.1)
# [1] 0.09531018
