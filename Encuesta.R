#Lectura del csv
encuesta = read.csv("C:/Users/UAH/Documents/Estadistica/EstadisticaLab/EncuestaLab.csv")
print(encuesta)
#Creacion de tablas con las diferentes columnas
edad = encuesta$Edad;
print(edad)

sexo = encuesta$Sexo;
print(sexo)

total = encuesta$Total
print(total)

mayoresEdad = encuesta$Total[encuesta$Edad>=18];
print(mayoresEdad)
print(length(mayoresEdad))

menoresEdad = encuesta$Total[encuesta$Edad<18];
print(menoresEdad)
print(length(menoresEdad))

hombres = encuesta$Total[encuesta$Sexo == "Hombre"]
mujeres = encuesta$Total[encuesta$Sexo == "Mujer"]

#medias de centralizacion
media_hombres_por_edad = mean(hombres)
print(media_hombres_por_edad)
media_mujeres_por_edad = mean(mujeres)
print(media_mujeres_por_edad)

mediana_hombres_por_edad = median(hombres)
print(mediana_hombres_por_edad)
mediana_mujeres_por_edad = median(mujeres)
print(mediana_mujeres_por_edad)

#medias de dispersion
rango_de_edad = range(edad)
max(edad)-min(edad)

var(hombres) 
var(mujeres)

sd(hombres) 
sd(mujeres)

sd(hombres)/mean(hombres) 
sd(mujeres)/mean(mujeres) 

#Medidas de localización
summary(hombres)
summary(mujeres)

IQR(hombres)
IQR(mujeres)

#Medidas de forma
install.packages("e1071")
library(e1071)

skewness(hombres)
skewness(mujeres)

kurtosis(hombres)
kurtosis(mujeres)