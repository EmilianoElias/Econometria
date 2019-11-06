#Datos panel en R
library(plm)
library(stargazer)

data<-read.csv("panel_wage.csv",header = TRUE)
datos.panel1<-pdata.frame(data, index = c("id","t"))
pdim(datos.panel1)

pool<-plm(lwage ~ exp+exp2+wks+ed,data = datos.panel1,model = "pooling")
summary(pool)

fixed<-plm(lwage ~ exp+exp2+wks+ed,data = datos.panel1,model = "within")
summary(fixed)

random<-plm(lwage ~ exp+exp2+wks+ed,data = datos.panel1,model = "random")
summary(random)

#Prueba aleatorio versus agrupados
plmtest(pool)

#Prueba fijos versus agrupados
pFtest(fixed,pool)

#Prueba fijos versus aleatorios (Hausman)
phtest(random,fixed)

#Tabla resumen de resultados
stargazer(pool,fixed,random, type="text",column.labels=c("OLS","FE","RE"),keep.stat=c("n","rsq"),keep=c("exp","exp2","wks","ed"))
