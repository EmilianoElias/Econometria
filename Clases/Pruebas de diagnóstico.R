install.packages(c("wooldridge", "lmtest", "tseries"))
#Se usaran los datos del ejemplo 8.4 del libro de Wooldridge 
library(wooldridge)
library(lmtest)
library(tseries)
library(dplyr)

reg <- lm(log(price)~log(lotsize)+log(sqrft)+bdrms, data=hprice1)
summary(reg)

data1 <- select(hprice1, price, lotsize, sqrft, bdrms) %>% 
  mutate(price = log(price), 
         lotsize = log(lotsize), 
         sqrft = log(sqrft))
plot(data1) 
reg1 <- lm(log(price)~log(lotsize)+log(sqrft), data=hprice1)
summary(reg1)
cor(hprice1, use = "everything")

anova2 <- anova(reg)
#Heteroscedasticidad
#Prueba BPG
bptest(reg1, studentize = FALSE)

#Prueba White sin terminos cruzados
bptest(reg1, ~ fitted(reg1) + I(fitted(reg1)^2), studentize = FALSE)

#Forma funcional
resettest(reg1)

#Autocorrelacion
dwtest(reg1)
bgtest(reg1)

#Normalidad
jarque.bera.test(residuals(reg1))
