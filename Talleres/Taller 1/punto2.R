

p <- function(val)
{
  
  #val <- readline(" cantidad ")
  val <- as.numeric((val))
  cant <- 0
  while(val>0)
  {
    cant<-cant+1
    d<- val%%2
    val<-val/2
    val <- floor(val)
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


# algo asi como T(n/2) -> O(log n) si no estoy mal 