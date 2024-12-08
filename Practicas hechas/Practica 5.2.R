#################### 1.    
#a. Definir una variable aleatoria Normal para el tiempo del 
#   viaje de un estudiante
encuesta = read.csv2("D:/Universidad/Estadistica/EstadisticaLab/encuestaViaje.csv")
encuesta = na.omit(encuesta)
print(encuesta)

m = mean(encuesta$VIAJE)
d = sd(encuesta$VIAJE)
print(d)

#b. Calcular la media o esperanza y la varianza de la variable aleatoria
print(m)
var = d^2

#c. Dibujar las funciones de densidad y distribución de la variable 
#   aleatoria
a = -80
b = 200
curve(dnorm(x, m, d), a, b)
curve(pnorm(x, m, d), a, b)

#d. Cuál es la probabilidad de que un estudiante seleccionado al azar 
#   tarde menos de 1 hora en llegar a la escuela.Dibujar en el 
#   diagrama de la función de densidad el área correspondiente a la 
#   acumulación de la probabilidad obtenida.
menosDe1Hora = pnorm(60, m, d)

intervaloX=seq(-100,60,0.01)
xBasePoligonoSombra = c(-100,intervaloX, 60)
yTapaPoligonoSombra = c(0,dnorm(intervaloX, m, d),0)

curve(dnorm(x,m,d),a ,b, ylab="f(x)",main='Menor de 1 hora')
polygon(xBasePoligonoSombra,yTapaPoligonoSombra,col="green")
texto.porcentaje=paste(round(menosDe1Hora*100,2),"%")
text(25, 0.001, texto.porcentaje)

#e. Cuál es la probabilidad de que un estudiante seleccionado al azar 
#   tarde entre 1 hora y 1 hora y media. Dibujar en el diagrama de la 
#   función de densidad el área correspondiente a la acumulación de la 
#   probabilidad obtenida.
entre1HoraYHoraYMedia = pnorm(90, m, d) - pnorm(60,m, d)

intervaloX=seq(60,90,0.01)
xBasePoligonoSombra = c(60,intervaloX, 90)
yTapaPoligonoSombra = c(0,dnorm(intervaloX, m, d),0)

curve(dnorm(x,m,d),a ,b, ylab="f(x)",main='Entre 1 hora y hora y media')
polygon(xBasePoligonoSombra,yTapaPoligonoSombra,col="blue")
texto.porcentaje=paste(round(entre1HoraYHoraYMedia*100,2),"%")
text(50, 0.007, texto.porcentaje)

#f. Cuál es la probabilidad de que un estudiante seleccionado al azar 
#   tarde más de 1 hora y media. Dibujar en el diagrama de la función 
#   de densidad el área correspondiente a la acumulación de la 
#   probabilidad obtenida.
masDe1Hora = 1 - pnorm(90, m, d)

intervaloX=seq(90,b,0.01)
xBasePoligonoSombra = c(90,intervaloX, b)
yTapaPoligonoSombra = c(0,dnorm(intervaloX, m, d),0)

curve(dnorm(x,m,d),a ,b, ylab="f(x)",main='Más de 1 hora')
polygon(xBasePoligonoSombra,yTapaPoligonoSombra,col="purple")
texto.porcentaje=paste(round(masDe1Hora*100,2),"%")
text(70, 0.007, texto.porcentaje)

#g. Cuál es el valor de los cuartiles (0.25, 0.5, 0.75) de la variable 
#   aleatoria. Dibujar en el diagrama de la función de densidad el 
#   área correspondiente al segundo cuartil.

qnorm(c(0.25, 0.5, 0.75), m, d)

intervaloX=seq(qnorm(0.25, m, d),qnorm(0.5, m, d),0.01)
xBasePoligonoSombra = c(qnorm(0.25, m, d),intervaloX, qnorm(0.5, m, d))
yTapaPoligonoSombra = c(0,dnorm(intervaloX, m, d),0)

curve(dnorm(x,m,d),a ,b, ylab="f(x)",main='Segundo cuartil')
polygon(xBasePoligonoSombra,yTapaPoligonoSombra,col="purple")
texto.porcentaje=paste(round((qnorm(0.5, m, d) - qnorm(0.25, m, d)),2),"%")
text(50, 0.007, texto.porcentaje)

#h. Si tenemos los datos de la encuesta original a los 74 estudiantes, 
#   comprobar si tomando los tiempos reales, el histograma se ajusta 
#   realmente a la distribución Normal.
par(mfrow=c(1,2))
hist(encuesta$VIAJE,breaks=a:b, freq=FALSE, main="Histograma")
curve(dnorm(x,m,d),a, b, add=TRUE, col ="red")

curve(dnorm(x, m, d), a, b)

#i. Repetir el apartado anterior pero haciendo la comprobación utilizando 
#   el diagrama de la función de densidad generada con density().
plot(density(encuesta$VIAJE))
curve(dnorm(x,m,d),a, b, add=TRUE, col ="red")


##################### 2.
#j. Definir una variable aleatoria Normal aproximando a la Binomial
# N(50, 1/6)
n = 50
p = 1/6
q = 1 -p

m = n*p
d = sqrt(n*p*q)
#N(m, d)

#k. Calcular la esperanza y varianza de la variable aleatoria
print(m) #Esperanza
var = d^2

#l. Dibujar las funciones de probabilidad y distribución de la 
#   variable aleatoria
a = 0
b = 17
curve(dnorm(x,m,d), a, b)
curve(pnorm(x,m,d), a, b)

#m. Calcular la probabilidad de que nunca aparezca el lado con 
#   el valor 6.
1- (pnorm(6 + 0.5, m, d) - pnorm(6-0.5,m , d))


#n. Calcular la probabilidad de que siempre aparezca el lado con el 
#   valor 6.
z = 6

pnorm(z + 0.5, m, d) - pnorm(z -0.5, m, d)

#o. Calcular la probabilidad de que el lado con el valor 6 aparezca 
#   menos de 10 veces.
z = 6
pnorm(6 + 0.5, m, d)

#p. Calcular la probabilidad de que el lado con el valor 6 aparezca más 
#   de 10 veces.
pnorm(11 - 0.5, m, d)

#q. Calcular la probabilidad de que el lado con el valor 6 aparezca 
#   entre 20 y 40 veces.
pnorm(40-0.5, m, d) - pnorm(20+0.5, m, d)

#r. Calcular los cuantiles 50 y 90.
qnorm(c(0.5, 0.9), m, d)

#s. Generar aleatoriamente 100 valores para la variable y comprobar si el 
#   histograma se ajusta realmente a la función de densidad de la Normal.
rnorm(100, m, d)


###################3.
#t. Definir una variable Normal aproximando a la de Poisson para el 
#   número de visitas en una hora
media = 120/24
#P(media) -> N(media, sqrt(media)

#u. Calcular la esperanza y varianza de la variable aleatoria
esp = media
var = media

#v. Dibujar las funciones de probabilidad y distribución de la variable 
#   aleatoria
m = esp
d = sqrt(var)
a =-5
b = 10

curve(dnorm(x, m, d), a, b)
curve(pnorm(x,m,d), a, b)

#w. Cuál es la probabilidad de que haya 4 visitas en una hora
dnorm(4, m, d)

#x. Cuál es la probabilidad de que haya 4 o menos visitas en una hora
pnorm(4, m, d)

#y. Cuál es la probabilidad de que haya más de 4 visitas en una hora
1 - pnorm(4, m, d)

#z. Cuál es la probabilidad de que no haya ninguna visita en una hora
dnorm(0, m, d)


#aa. Cuál es la probabilidad de que haya entre 4 y 6 visitas en una hora
pnorm(6, m, d) - pnorm(4, m, d)

#ab. Cuál es el valor de los cuartiles (25%, 50%, 75%, 100%) de la 
#    variable aleatoria
qnorm(c(0.25, 0.5, 0.75, 1), m, d)

#ac. Generar aleatoriamente 100 valores para la variable y comprobar 
# si el histograma se ajusta realmente a la función de densidad de Poisson.
datos = rnorm(100,m,d)
print(datos)
a = 85
b = 150
hist(datos, breaks=a:b, freq=FALSE)
curve(dnorm(x,m,d), a, b, add=TRUE)

#ad. Definir una nueva variable aleatoria para calcular la probabilidad 
#    de que en un día haya 100 visitas.
lambda = 120
dpois(100, lambda)
m = lambda
d = sqrt(lambda)
pnorm(100+0.5, m, d) - pnorm(100-0.5, m, d)

#ae. Generar aleatoriamente 100 valores para la nueva variable y 
#    comprobar si el histograma se ajusta realmente a la función de 
#   densidad de Poisson.
par(mfrow = c(1, 2))

datos = rnorm(100, m, d)
print(datos)

a = 90
b = 170
hist(datos,breaks= a:b, freq=FALSE)
curve(dnorm(x,m,d), a, b, add=TRUE)

barplot(dpois(a:b,m))
