###
###   Econometría
###   Clase 2
###

set.seed(21)
x <- rnorm(500)
head(x)
str(x)

plot(x)
hist(x, probability = TRUE)

my_var(x)
var(x)

y <- x*4.5
z <- -x/5
w <- rnorm(500)

a <- cbind(x, y, z, w)
pairs(a)

my_cor(x, y)
cor(a, method = "pearson")
cor(a, method = "kendall")
cor(a,method = "spearman")
