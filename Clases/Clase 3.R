###
###     Clase 3
###     Emiliano Elias Dena
###

library(haven)
ceosal1 <- read_dta("Data/ceosal1.dta")
str(ceosal1)
plot(ceosal1$finance, ceosal1$salary)
pairs(ceosal1)
