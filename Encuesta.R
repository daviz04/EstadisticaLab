#Lectura del csv
encuesta = read.csv("C:/Users/UAH/Documents/Estadistica/EstadisticaLab/EncuestaLab.csv")
print(encuesta)
#Creacion de tablas con las diferentes columnas
edad = encuesta$Edad;
print(edad)

sexo = encuesta$Sexo;
print(sexo)

periodo = encuesta$Periodo;
print(periodo);

total = encuesta$Total
print(total)

mayoresEdad = encuesta$Total[encuesta$Edad>=18];
print(mayoresEdad)
print(length(mayoresEdad))

menoresEdad = encuesta$Total[encuesta$Edad<18];
print(menoresEdad)
print(length(menoresEdad))

media_hombres = mean(encuesta$Total[encuesta$Sexo == "Hombre"])
media_mujeres = mean(encuesta$Total[encuesta$Sexo == "Mujer"])

mediana_hombres = median(encuesta$Total[encuesta$Sexo == "Hombre"])
mediana_mujeres = median(encuesta$Total[encuesta$Sexo == "Mujer"])

rango_de_edad = range(edad)
