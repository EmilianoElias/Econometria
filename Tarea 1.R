###
###   Econometria
###   Tarea 1
####

my_mean <- function(x, na.rm = FALSE, ...){
  if(!is.numeric(x)){
    warning("El argumento no es numérico")
    return(NA_real_)
  }
  if(na.rm){
    x <- x[!is.na(x)]
  }
  n <- length(x)
  k <- 0
  for (i in 1:n) {
    k = k + x[i]
  }
  return(k/n)
}

my_var <- function(x, na.rm = FALSE, ...){
  if(!is.numeric(x)){
    warning("El argumento no es numérico")
    return(NA_real_)
  }
  if(na.rm){
    x <- x[!is.na(x)]
  }
  n <- length(x)
  k <- 0
  m <- 0
  for (i in 1:n) {
    k = k + x[i]
    m = m + x[i]^2
  }
  return(m/n - (k/n)^2)
}
    
