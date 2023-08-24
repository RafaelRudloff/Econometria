
getwd()
setwd(choose.dir())
install.packages("haven")
library(haven)

  

# Lee el archivo Stata

general = read_dta("base.dta")
provi = read_dta("provi.dta")
jefe_hogar=general[general$pco1_a==1,]
base=merge(jefe_hogar,provi,by=c("folio","id_persona"))
remove(general,provi,jefe_hogar) #Borramos para ahorrar memoria
base #Archivo a utilizar durante toda la tarea

############################
######## Pregunta 1 ########
############################

extranjeros = base[base$r1a == 3,]
nacionales = base[base$r1a !=3,]
mean_nacionales = mean(nacionales$ypc)
mean_extranjeros = mean(extranjeros$ypc)

print(mean_nacionales)
print(mean_extranjeros)  

#Por enunciado nos piden:
brecha_salarial=mean_nacionales - mean_extranjeros
print(brecha_salarial)  #Falta interpretar esta brecha

#############################
######## Pregunta 2a ########   #Falta interpretar el valor de esta regresión lineal 
#############################

y1=base$ypc
xx
suma_1= base$r1a==1
suma_2=base$r1a==2
x1=suma_1+suma_2
cov1 = cov(x1, y1, use = "complete.obs") 
var1=var(x1)
beta=cov1/var1 #26005.97
alpha=mean(y1, na.rm = TRUE)-beta*mean(x1, na.rm = TRUE)
cat("Los coeficientes estimados corrresponden a: Alpha=", alpha, "y Beta=", beta)


#############################
######## Pregunta 2b ########  #Falta interpretar el valor de esta regresión lineal 
#############################

y2=base$ypc
suma_1= base$r1a==1
suma_2=base$r1a==2
x2=suma_1+suma_2
x2_transpose=t(x2)
beta2=solve(x2_transpose%*%x2)%*%x2_transpose%*%y2
alpha2=mean(y2, na.rm = TRUE)-beta2*mean(x2, na.rm = TRUE)
cat("Los coeficientes estimados corrresponden a: Alpha=", alpha2, "y Beta=", beta2)

#############################
######## Pregunta 2c ########
#############################


