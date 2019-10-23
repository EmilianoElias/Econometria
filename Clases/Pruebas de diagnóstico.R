#Se usaran los datos del ejemplo 8.4 del libro de Wooldridge 
library(wooldridge)
library(lmtest)
library(tseries)

reg <- lm(log(price)~log(lotsize)+log(sqrft)+bdrms, data=hprice1)
summary(reg)

#Heteroscedasticidad
#Prueba BPG
bptest(reg)

#Prueba White sin terminos cruzados
bptest(reg, ~ fitted(reg) + I(fitted(reg)^2))

#Forma funcional
resettest(reg)

#Autocorrelacion
dwtest(reg)
bgtest(reg)

#Normalidad
jarque.bera.test(residuals(reg))
