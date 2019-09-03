###
###   Econometria
###   Tarea 1
###

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

my_cov <- function(x, y, na.rm = FALSE, data = FALSE, ...){
  if(!is.numeric(x)){
    warning("El argumento x no es numérico")
    return(NA_real_)
  }
  if(!is.numeric(y)){
    warning("El argumento y no es numérico")
    return(NA_real_)
  }
  if(!length(x)==length(y)){
    warning("Los argumentos no tienen el mismo tamaño")
    return(NA_real_)
  }
  if(na.rm){
    x <- x[!is.na(x)]
    y <- y[!is.na(y)]
  }
  n <- length(x)
  k <- 0
  m <- 0
  for (i in 1:n) {
    k = k + x[i]
    m = m + x[i]
  }
  x_bar <- k/(n - 1)
  y_bar <- m/(n - 1)
  a <- 0
  for (i in 1:n) {
    a = a + (x[i] - x_bar)*(y[i] - y_bar)
  }
  if(data == FALSE){return(a/(n-1))}
  else{return(a/n)}
} 

my_cor <- function(x, y, na.rm = FALSE, ...){
  if(!is.numeric(x)){
    warning("El argumento x no es numérico")
    return(NA_real_)
  }
  if(!is.numeric(y)){
    warning("El argumento y no es numérico")
    return(NA_real_)
  }
  if(!length(x)==length(y)){
    warning("Los argumentos no tienen el mismo tamaño")
    return(NA_real_)
  }
  n <- length(x)
  k <- 0
  m <- 0
  p <- 0
  q <- 0
  for (i in 1:n) {
    k = k + x[i]
    p = p + x[i]^2
    m = m + y[i]
    q = q + y[i]^2
  }
  x_bar <- k/n
  x_var <- p/n - (k/n)^2 
  y_bar <- m/n
  y_var <- q/n - (m/n)^2
  a <- 0
  for (i in 1:n) {
    a = a + (x[i] - x_bar)*(y[i] - y_bar)
  }
  cov <- a/n
  return(cov/sqrt(x_var*y_var))
}
