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

#Se grafica la grafica de REGRESIÓN LINEAL SIMPLE
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

plot(ajustado,residuo)
abline(h=0)

