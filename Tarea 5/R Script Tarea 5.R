#Instalacion de Librerias

install.packages("readxl")

install.packages("ggplot2")

install.packages("tidyverse")


#Se llaman las librerias
library(readxl)
library(ggplot2)
library(tidyverse)


#Se descarga los datos o la tabla de datos de http://datos.gob.do/

download.file(url ="http://hmra.gob.do/transparencia/index.php/publicacion-en-el-portal-de-datos-abiertos/category/456-estadistica-de-parto-y-nacimiento?download=819:estadistica-de-parto-y-nacimiento-xlsx", destfile = "Estadistica-de-parto-y-nacimiento-xlsx.xlsx", mode = "wb")

#Se importa el archivo excel con los datos.
archivo <- readxl::read_excel("Estadistica-de-parto-y-nacimiento-xlsx.xlsx")


#ATENCION CUANDO SE CLONE EL ARCHIVO TAL VEZ DE ERROR PORQUE LA i
#DE LA VARIABLE "Partos Vía Vaginal" lleva acento y por esto pone un
#signo de interrogacion solo cambie el ? por una i con acento o sea í.

#Se grafica la grafica de REGRESION LINEAL SIMPLE
archivo %>%
  ggplot(aes(x=`Partos Vía Vaginal`,y=Vivos)) + geom_point()+ geom_abline(intercept =67.263 ,slope = 1.863,col = 'blue')+ geom_vline(xintercept = 300, col = "red")

#Se hace la regresion
regresion <- lm(Vivos~ `Partos Vía Vaginal`,data = archivo)

#se obtiene un resumen de la regresion
summary(regresion)

#Tabla anova
anova(regresion)

#se obtienen los residuos estandarizados
residuo <- rstandard(regresion)

#se obtienen los residuos ajustados
ajustado <- fitted(regresion)

#Se crea la grafica de ambos
plot(ajustado,residuo)

#se pone la linea en 0
abline(h=0)

