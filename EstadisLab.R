archivo = "C:/Users/UAH/Downloads/61622.csv";
datos <- read.csv2(archivo);
print(datos);
rm(datos)

comunidades = datos$Edad;
print(comunidades)
mean(comunidades)

