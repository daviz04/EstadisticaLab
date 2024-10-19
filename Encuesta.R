#Lectura del csv
encuesta = read.csv("C:/Users/UAH/Documents/Estadistica/EstadisticaLab/EncuestaLab.csv")
print(encuesta)
#Creacion de tablas con las diferentes columnas
edad = encuesta$Edad;
print(edad)

 #División entre sexos
sexo = encuesta$Sexo;
print(sexo)

#Número de personas con cada edad
hombres = encuesta$Total[encuesta$Sexo == "Hombre"]
mujeres = encuesta$Total[encuesta$Sexo == "Mujer"]
print(hombres)
print(mujeres)

#Total de inmigraciones por sexos
total = encuesta$Total
print(total)

 #Hombres
hombresInmgr = sum(hombres)
print(hombresInmgr)

 #Mujeres
mujeresInmgr = sum(mujeres)
print(mujeresInmgr)

#Mayores y menores de edad en hombres y mujeres y el total
 #Hombres
     #Mayores de edad
mayoresEdadHombres = encuesta$Total[encuesta$Edad>=18 & encuesta$Sexo == "Hombre"];
print(mayoresEdadHombres)
print(length(mayoresEdadHombres))

totalMayoresEdadHombres = sum(mayoresEdadHombres)
print(totalMayoresEdadHombres) 

     #Menores de edad
menoresEdadHombres = encuesta$Total[encuesta$Edad<18 & encuesta$Sexo == "Hombre"];
print(menoresEdadHombres)
print(length(menoresEdadHombres))

totalMenoresEdadHombres = sum(menoresEdadHombres)
print(totalMenoresEdadHombres)

  #Mujeres
      #Mayores de edad
mayoresEdadMujeres = encuesta$Total[encuesta$Edad>=18 & encuesta$Sexo == "Mujer"];
print(mayoresEdadMujeres)
print(length(mayoresEdadMujeres))

totalMayoresEdadMujeres = sum(mayoresEdadMujeres)
print(totalMayoresEdadMujeres)

       #Menores de edad
menoresEdadMujeres = encuesta$Total[encuesta$Edad<18 & encuesta$Sexo == "Mujer"];
print(menoresEdadMujeres)
print(length(menoresEdadMujeres))

totalMenoresEdadMujeres = sum(menoresEdadMujeres)
print(totalMenoresEdadMujeres)

totalMujeres = totalMayoresEdadMujeres + totalMenoresEdadMujeres
print(totalMujeres)

# Medias de centralización
  #Media
  media_hombres_por_edad = mean(hombres)
  print(media_hombres_por_edad)
  media_mujeres_por_edad = mean(mujeres)
  print(media_mujeres_por_edad)
  
  #Mediana
  mediana_hombres_por_edad = median(hombres)
  print(mediana_hombres_por_edad)
  mediana_mujeres_por_edad = median(mujeres)
  print(mediana_mujeres_por_edad)
  
  #Moda
  install.packages("modeest")
  library(modeest)
     #Moda de hombres
  mlv(hombres, method="mfv")
     #Moda de mujeres
  mlv(mujeres, method ="mfv")

#medias de dispersion
  #Rango
  rango_de_edad = range(edad)
  max(edad)-min(edad)
  print(rango_de_edad)
  
  #Varianza (muestral) 
  var(hombres) 
  var(mujeres)
  
  #Desviación típica 
  sd(hombres) 
  sd(mujeres)
  
  #Coeficiente de variación 
  sd(hombres)/mean(hombres) 
  sd(mujeres)/mean(mujeres) 

#Medidas de localización
  #Cuartiles
  summary(hombres)
  summary(mujeres)
  
  #Rango intercuartílico
  IQR(hombres)
  IQR(mujeres)

#Medidas de forma
  install.packages("e1071")
  library(e1071)
  #Coeficiente de asimetría 
  skewness(hombres)
  skewness(mujeres)
  #Coeficiente de apuntamiento o curtosis
  kurtosis(hombres)
  kurtosis(mujeres)

#Distribuciones de frecuencia
  #Intervalos nº de personas inmigrantes por cantidades y sexo(50, 5000, 10000, 15000, 20000)
  #Marcas de clases (como son los mismos intervalos para los 2 simplemente lo haré una vez con las mujeres)
  h=hist(mujeres, breaks= c(50, 5000, 10000, 15000, 20000), plot=FALSE)
  h$mids
  
  #Hombres
  #Frecuencias absolutas
  inmigracionesHombres = cut(hombres, breaks=c(50, 5000, 10000, 15000, 20000), right=FALSE, include.lowest=TRUE)
table(inmigracionesHombres)
  #Frecuencias absolutas acumuladas
cumsum(table(inmigracionesHombres))

  #Frecuencias relativas
prop.table(table(inmigracionesHombres))
  #Frecuencias relativas acumuladas
cumsum(prop.table(table(inmigracionesHombres)))



  #Mujeres
  #Frecuencias absolutas
inmigracionesMujeres = cut(mujeres, breaks=c(50, 5000, 10000, 15000, 20000), right=FALSE, include.lowest=TRUE)
table(inmigracionesMujeres)
  #Frecuencias absolutas acumuladas
cumsum(table(inmigracionesMujeres))
  #Frecuencias relativas
prop.table(table(inmigracionesMujeres))
  #Frecuencias relativas acumuladas
cumsum(prop.table(table(inmigracionesMujeres)))

#Dibujar diagramas
  #Diagrama de caja
  par(mfrow=c(1,2))
  boxplot(hombres,
          ylim=c(0,17000),
          main="hombres",
          col = "skyblue")
  boxplot(mujeres,
          ylim=c(0,17000),
          main="mujeres",
          col = "pink")
  
  #Diagrama de barras
  par(mfrow=c(2,1))
  barplot(hombres,
          main = "Número de Hombres Inmigrantes por Edad",
          xlab = "Edad",
          ylab = "Número de Hombres",
          ylim = c(0, 20000),
          xlim = c(0, 91),
          col = "skyblue")
  barplot(mujeres,
          main = "Número de Mujeres Inmigrantes por Edad",
          xlab = "Edad",
          ylab = "Número de Mujeres",
          ylim = c(0, 17000),
          xlim = c(0, 91))
          col = "pink"
  