###
###   Econometría
###   Clase 3
###

library(foreign)
data1<-read.dta("Data/ceosal1.dta")

Y<-data1[,1]
X<-data1[,4]

reglin<-function(y,x){
  xsum<-x
  ysum<-y
  x2<-x^2
  n<-length(x)
  for (i in 2:n) {xsum[i]=xsum[i -1]+xsum[i]}
  for (i in 2:n) {ysum[i]=ysum[i -1]+ysum[i]}
  for (i in 2:n) {x2[i]=x2[i-1]+x2[i]}
  
  xbarra<-xsum[n]/n
  ybarra<-ysum[n]/n
  
  xyprod<-(x -xbarra)*y
  xsum2<-(x - xbarra)^2
  
  ysum2<-(y - ybarra)^2
  for(i in 2:n) {xyprod[i]=xyprod[i -1]+xyprod[i]}
  for(i in 2:n) {xsum2[i]=xsum2[i-1] + xsum2[i]}
  for(i in 2:n) {ysum2[i]=ysum2[i -1] + ysum2[i]}
  
  beta1<-xyprod[n] /xsum2[n]
  beta0<-ybarra-(beta1*xbarra)
  ygorro<-beta0+(beta1*x)
  e2<-(y-ygorro)^2
  
  for(i in 2:n) {e2[i]=e2[i - 1] +e2[i]}
  var_e<-e2[n]/(n -2)
  se_e<-var_e^.5
  var_beta1<-var_e/xsum2[n]
  se_beta1<-var_beta1^.5
  var_beta0<-(var_e*x2[n])/(n*xsum2[n])
  se_beta0<-var_beta0^.5
  t_beta1<-beta1/se_beta1
  t_beta0<-beta0/se_beta0
  if(t_beta1>0){pvalue_beta1<-2*pt(t_beta1,(n-2),lower.tail = FALSE)}
  else{pvalue_beta1<-2*pt(t_beta1,(n- 2),lower.tail = TRUE)}
  if(t_beta0>0){pvalue_beta0<-2*pt(t_beta0,(n - 2),lower.tail = FALSE)}
  else{pvalue_beta0<-2*pt(t_beta0,(n - 2),lower.tail = TRUE)}
  rcuad<-1-(e2[n]/ysum2[n])
  rcuad_aj<-1-((1 - rcuad)*((n - 1)/(n- 2)))
  SST<-ysum2[n]
  SSE<-xyprod[n]*beta1
  SSR<-SST-SSE
  f_reg<-SSE/(SSR/(n- 2))
  pvalue_f<-pf(f_reg,df1 = 1,df2 = n-2,lower.tail = FALSE)
  intercepto<-cbind(beta0,se_beta0,t_beta0,pvalue_beta0)
  independiente<-cbind(beta1,se_beta1,t_beta1,pvalue_beta1)
  resultado<-rbind(intercepto,independiente)
  resum<-rbind(rcuad,rcuad_aj,f_reg,pvalue_f)
  colnames(resultado)<-c("Coeficientes","Error Est.", "Estad. t", "P-value")
  rownames(resultado)<-c("Intercepto","X")
  colnames(resum)<-c(" ")
  rownames(resum)<-c("R^2","R^2 Aj.","Estad. F","P-value")
  output<-list(resultado,resum)
  names(output)<-c("Resultados de la estimacion por MCO","Bondad de ajuste")
  return(output)
}

reglin(X, Y)

regresion <- lm(X ~ Y)
plot(X, Y, ylim = c(0, 4000))
abline(regresion)