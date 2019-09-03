###
###   Econometría
###   Clase 1
###

E <- c("Eco", 1, "E")

N <- 1:1000

M <- matrix(1:9, 3, 3)

mean(N)

length(N)

function(X){
  X_sum <- X
  sum <- c(0)
  for (i in 1:length(X)) {
    sum = X_sum[i] + sum
  }
  sum 
}

prom <- function(X){
  X_sum <- X
  sum <- c(0)
  for (i in 1:length(X)) {
  sum = X_sum[i] + sum
  }
 sum/length(X) 
}

sum_1000 <- sumao(1:1000)
sum_m <- sumao(M)

prom_1000 <- prom(1:1000)
my_cov(1:100, 101:200)
cov(1:100, 101:200)
my_p(1:100, 101:200)
cor(1:100, 101:200)
my_cor(1:100, 101:200)
