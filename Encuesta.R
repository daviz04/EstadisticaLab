#Lectura del csv
encuesta = read.csv2("C:/Users/UAH/Documents/Estadistica/EstadisticaLab/EncuestaLab.csv")
print(encuesta)
rm(sexo)

#Creacion de tablas con las diferentes columnas
edad = encuesta$Edad;
print(edad)

sexo = encuesta$Sexo;
print(sexo)

periodo = encuesta$Periodo;
print(periodo);

total = encuesta$Total
print(total)

mayoresEdad = encuesta$Edad[encuesta$Edad>=18];
print(mayoresEdad)
print(length(mayoresEdad))

menoresEdad = encuesta$Edad[encuesta$Edad<18];
print(menoresEdad)
print(length(menoresEdad))
