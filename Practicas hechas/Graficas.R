#1.
archivo = "C:/Users/UAH/Documents/Estadistica/EstadisticaLab/Practicas hechas/encuesta.csv"
datos = read.csv2(archivo)
datos = na.omit(datos)
media = mean(datos$VIAJE)
desv = sd(datos$VIAJE)
print(media)
print(desv)
print(datos)


#2.
esp = media
var = desv^2

print(esp)
print(var)



#3.
par(mfrow=c(1,2))
# Función de densidad
curve(dnorm(x,media,desv),0,200, main="Función de densidad") 

# Función de distribución
curve(pnorm(x,media,desv),0,200, main="Función de distribución")

par(mfrow=c(1,1)) # Restaurar la ventana gráfica

#4.
# Convertir 1 hora a minutos P[x] <=60
tiempo_limite <- 60

# Calcular la probabilidad de que un estudiante tarde menos de 60 minutos
probabilidad <- pnorm(tiempo_limite, media, desv)

# Mostrar la probabilidad
print(probabilidad)

# Dibujar la función de densidad
curve(dnorm(x, media, desv), -50, 200, ylab="f(x)", main='Tiempo de viaje menor que 60 minutos')

# Sombrear el área correspondiente a la probabilidad
intervaloX <- seq(0, tiempo_limite, 0.01)
xBasePoligonoSombra <- c(0, intervaloX, tiempo_limite)
yTapaPoligonoSombra <- c(0, dnorm(intervaloX, media, desv), 0)
polygon(xBasePoligonoSombra, yTapaPoligonoSombra, col="green")

# Añadir el valor de la probabilidad al gráfico
texto.porcentaje <- paste(round(probabilidad * 100, 2), "%")
text(tiempo_limite / 2, 0.01, texto.porcentaje)


#5.
# Convertir los límites de tiempo a minutos
tiempo_limite_inferior <- 60
tiempo_limite_superior <- 90

# Calcular la probabilidad de que un estudiante tarde entre 60 y 90 minutos
probabilidad <- pnorm(tiempo_limite_superior, media, desv) - pnorm(tiempo_limite_inferior, media, desv)

# Mostrar la probabilidad
print(probabilidad)

# Dibujar la función de densidad
curve(dnorm(x, media, desv), 0, 120, ylab="f(x)", main='Tiempo de viaje entre 60 y 90 minutos')

# Sombrear el área correspondiente a la probabilidad
intervaloX <- seq(tiempo_limite_inferior, tiempo_limite_superior, 0.01)
xBasePoligonoSombra <- c(tiempo_limite_inferior, intervaloX, tiempo_limite_superior)
yTapaPoligonoSombra <- c(0, dnorm(intervaloX, media, desv), 0)
polygon(xBasePoligonoSombra, yTapaPoligonoSombra, col="green")

# Añadir el valor de la probabilidad al gráfico
texto.porcentaje <- paste(round(probabilidad * 100, 2), "%")
text((tiempo_limite_inferior + tiempo_limite_superior) / 2, 0.01, texto.porcentaje)


#6.

intervaloX = seq(90, max(datos$VIAJE), 0.01) # Ajustar el límite superior si necesario
xBasePoligonoSombra = c(90, intervaloX, max(datos$VIAJE)) # Ajustar el límite superior si necesario
yTapaPoligonoSombra = c(0, dnorm(intervaloX, media, desv), 0)
curve(dnorm(x, media, desv), min(datos$VIAJE), max(datos$VIAJE), ylab="f(x)",main='Tiempo mayor que 90 minutos') # Ajustar los límites si necesario
polygon(xBasePoligonoSombra, yTapaPoligonoSombra, col="green")
texto.porcentaje = paste(round(P.mayor.90 * 100, 2), "%")
text(mean(intervaloX), 0.1, texto.porcentaje)

#7.

#Creación de cuartiles
(cuartiles = qnorm(c(0.25, 0.50, 0.75), media, desv))
(segundoCuartil=cuartiles[2])
ProbsegundoCuartil = pnorm(segundoCuartil,media,desv)
print(ProbsegundoCuartil)

#Representación gráfica
intervaloX = seq(0, ProbsegundoCuartil, 0.01)
xBasePoligonoSombra = c(0, intervaloX, cuartiles)
yTapaPoligonoSombra = c(0, dnorm(intervaloX, media, desv), 0)
curve(dnorm(x, media, desv), -50, 200, ylab="f(x)", main='Segundo Cuartil')
polygon(xBasePoligonoSombra, yTapaPoligonoSombra, col="green")

texto.porcentaje = paste(round(segundoCuartil * 100, 2), "%")
text(mean(intervaloX), 0.1, texto.porcentaje)




#2.
 #a) Definir una variable aleatoria Normal aproximando a la Binomial B(n,p) a N(m,d)
  media = 50;
  desv = 1/6
  
  m = media * desv
  print(m)
  d = sqrt(m*(1-desv))
  print(d)
  normal = c(m, d)
  #b) Calcular la esperanza y varianza de la variable aleatoria
  esperanza = m^2
  var = d^2  

  #c) Dibujar las funciones de probabilidad y distribución de la variable aleatoria
  par(mfrow = c(1,2))
  curve(pbinom(x, m, d), nmain = "Funcion de probabilidad")
  curve(dbinom(x, m, d), -10, 20)  
  