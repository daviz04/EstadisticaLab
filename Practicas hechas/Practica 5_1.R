#a Variable aleatoria binomial B(n, p) = B(50, 1/6)
n = 50
p = 1/6

#b Esperanza y varianza
esp = n*p
var = n*p*(1-p)
print(esp)
print(var)

#c
par(mfrow=c(1,2))
barplot(dbinom(0:n,n,p))
barplot(pbinom(0:n,n,p))

#d
prob6 = dbinom(5,n,p)
probNunca6 = 1 - prob6
print(probNunca6)

#e
probSiempre6 = 1 - probNunca6
print(probSiempre6)

#f. Calcular la probabilidad de que el lado con el valor 6 aparezca menos de 10 veces
pbinom(9, n, p)

#Calcular la probabilidad de que el lado con el valor 6 aparezca más de 10 veces
hasta10 = pbinom(10, n, p)
masDe10 = 1 - hasta10
print(masDe10)

#Calcular la probabilidad de que el lado con el valor 6 aparezca entre 20 y 40 veces.
probEntre20Y40 = pbinom(40,n,p)- pbinom(20,n,p)
print(probEntre20Y40)


#i. Calcular los cuantiles 50 y 90.
qbinom(c(0.50,0.90),n,p)

#j. Generar aleatoriamente 100 valores para la variable y comprobar si el histograma se
#ajusta realmente a una función de probabilidad Binomial
rbinom(100,n,p)

hist(rbinom(100,n,p),
     breaks=0:n,
     freq=FALSE)

lines(0:n,dbinom(0:n,n,p))



#2.
#a. Definir una variable aleatoria binomial para el experimento B(n, p) = B(74, (26/74))
encuesta = read.csv2("C:/Users/UAH/Documents/Universidad/Estadistica/EstadisticaTeoria/Estadistica/encuesta.csv")
na.omit(encuesta)
n = 6
p = 26/74

#b. Calcular la esperanza y varianza de la variable aleatoria
esp = n*p
var = n*p*(1-p)
print(esp)
print(var)

#c. Dibujar las funciones de probabilidad y distribución de la variable aleatoria

barplot(dbinom(0:n,n,p))

plot(0:n,pbinom(0:n,n,p),type="s")
lines(0:n,pbinom(0:n,n,p))

#d. Cuál es la probabilidad de que exactamente 3 de los 6 estudiantes tengan contrato
#con Jazzte/Orange/Simyo
x = 3
dbinom(x,n,p)

#e. Cuál es la probabilidad de que 3 o menos de los 6 estudiantes tengan contrato con
#Jazzte/Orange/Simyo
pbinom(x,n,p)

#f. Cuál es la probabilidad de que tengan contrato con Jazzte/Orange/Simyo más de 3
#de los 6 estudiantes
1 - pbinom(x,n,p)

#g. Cuál es la probabilidad de que los 6 estudiantes tengan contrato con
#Jazzte/Orange/Simyo
x = 6
dbinom(x,n,p)

#h. Cuál es la probabilidad de que ninguno de los 6 estudiantes tenga contrato con
#Jazzte/Orange/Simyo
x = 0
dbinom(x,n,p)

#i. Cuál es la probabilidad de que entre 2 y 4 estudiantes tengan contrato con
#Jazzte/Orange/Simyo
pbinom(4, n, p) - pbinom(2, n, p)

#j. Cuál es el valor de los cuartiles (25%, 50%, 75%, 100%) de la variable aleatoria
qbinom(c(0.25, 0.50, 0.75, 1),n,p)

pbinom(1, n, p)
pbinom(2, n, p)
pbinom(3, n, p)


#k. Si tenemos los datos de la encuesta original a los 74 alumnos, comprobar si tomando
#las respuestas en grupos de 6 alumnos, el histograma se ajusta realmente a la función
#de probabilidad binomial
par(mfrow=c(1,2))
nrow(encuesta)
encuesta$MOVIL
(jazztel = c(1,1,3,3,1,2,4,3,2,3,2,1))
hist(jazztel, breaks = 0:n)
lines(0:n,12*dbinom(0:n,n,p))

#l. Repetir el apartado anterior pero utilizando datos generados aleatoriamente con R, en
#lugar de los datos reales
jazztelGenerado = rbinom(12,n,p)
hist(jazztelGenerado, breaks = 0:n)
lines(0:n, 12*dbinom(0:n, n, p))


# 3.
#a. Definir una variable aleatoria de Poisson para el número de visitas en una hora.
     #X~P(x, media/24)
dpois(1,media)

#b. 

media = 120/24
var = 120/24
desv = sqrt(var)
n = 15

minValor = -5
print(desv)

#c. Dibujar las funciones de probabilidad y distribución de la variable aleatoria
plot(minValor:n,dpois(minValor:n,media),type="h")
lines(minValor:n,dpois(minValor:n,media))

plot(minValor:n,ppois(minValor:n,media),type="s")
lines(minValor:n,ppois(minValor:n,media))


#d. 
dpois(4, media)

#e. 
ppois(4, media)

#f. 
1 - ppois(4, media)

#g. Cuál es la probabilidad de que no haya ninguna visita en una hora
dpois(0, media)

#h. Cuál es la probabilidad de que haya entre 4 y 6 visitas en una hora
ppois(6, media) - ppois(4, media)

#i. Cuál es el valor de los cuartiles (25%, 50%, 75%, 100%) de la variable aleatoria
qpois(c(0.25, 0.50, 0.75, 1), media)

#j. Generar aleatoriamente 100 valores para la variable y comprobar si el histograma 
#   se ajusta realmente a la función de probabilidad de Poisson.
rpois(100,media)

hist(rpois(100,media), breaks=0:20, freq=FALSE)
lines(0:20,dpois(0:20,media))
#Si se ajusta

#k. Definir una nueva variable aleatoria para calcular la probabilidad de que en un 
#   día haya 100 visitas.
media = 120
dpois(100,media)

#l. Generar aleatoriamente 100 valores para la nueva variable y comprobar si el 
#   histograma se ajusta realmente a la función de probabilidad de Poisson.
rpois(100, media)
hist(rpois(100,media), breaks=90:150, freq=FALSE)
lines(90:150,dpois(90:150,media))
#Se ajusta