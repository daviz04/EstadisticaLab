#1. Si se lanza 50 veces un dado de seis lados con valores 1, 2, 3, 4, 5 y 6 en cada lado:
  #a. Definir una variable aleatoria binomial para el experimento
X <- 0:50

# Probabilidad de obtener cada valor de X
probabilidad <- dbinom(X, size = 50, prob = 1/6)

# Crear la gráfica
plot(X, probabilidad, type = "h", 
     main = "Función de Probabilidad Binomial (n=50, p=1/6)",
     xlab = "Número de veces que aparece el 6",
     ylab = "Probabilidad")
lines(X, probabilidad, type = "l")


#2. Calcular la esperanza y varianza de la variable aleatoria
n <- 50

# Probabilidad de éxito
p <- 1/6

# Esperanza
esperanza <- n * p
print(esperanza)

# Varianza
varianza <- n * p * (1 - p)
print(varianza)


#3. Dibujar las funciones de probabilidad y distribución de la variable aleatoria
# Variable aleatoria
X <- 0:n

# Probabilidad de cada valor de X
probabilidad <- dbinom(X, size = n, prob = p)

# Gráfica de la función de probabilidad
plot(X, probabilidad, type = "h", 
     main = "Función de Probabilidad Binomial (n=50, p=1/6)",
     xlab = "Número de veces que aparece el 6",
     ylab = "Probabilidad")
lines(X, probabilidad, type = "l")
 #Distribucion

# Variable aleatoria
X <- 0:n

# Probabilidad acumulada hasta cada valor de X
probabilidad_acumulada <- pbinom(X, size = n, prob = p)

# Gráfica de la función de distribución
plot(X, probabilidad_acumulada, type = "s", 
     main = "Función de Distribución Binomial (n=50, p=1/6)",
     xlab = "Número de veces que aparece el 6",
     ylab = "Probabilidad Acumulada")

#4. Calcular la probabilidad de que nunca aparezca el lado con el valor 6
# Número de éxitos (queremos 0 éxitos, es decir, ningún 6)
x <- 0

# Calcular la probabilidad usando dbinom()
probabilidad <- dbinom(x, size = n, prob = p)

# Imprimir la probabilidad
print(probabilidad)


#5. Calcular la probabilidad de que siempre aparezca el lado con el valor 6.
# Número de éxitos (queremos 50 éxitos, es decir, todos 6)
# Número de éxitos (queremos 0 éxitos, es decir, ningún 6)
x <- 0

# Calcular la probabilidad usando dbinom()
probabilidad <- dbinom(x, size = n, prob = p)

# Imprimir la probabilidad
print(probabilidad)

#6. Calcular la probabilidad de que el lado con el valor 6 aparezca menos de 10 veces.
x <- 9

# Calcular la probabilidad usando pbinom()
probabilidad <- pbinom(x, size = n, prob = p)

# Imprimir la probabilidad
print(probabilidad)

#7. Calcular la probabilidad de que el lado con el valor 6 aparezca más de 10 veces.
# Número de éxitos (queremos más de 10, calculamos hasta 10)
x <- 10

# Calcular la probabilidad de obtener 10 veces o menos 
probabilidad_10_o_menos <- pbinom(x, size = n, prob = p)

# Calcular la probabilidad de obtener más de 10 veces
probabilidad_mas_de_10 <- 1 - probabilidad_10_o_menos

# Imprimir la probabilidad
print(probabilidad_mas_de_10)

#8. Calcular la probabilidad de que el lado con el valor 6 aparezca entre 20 y 40 veces.
# Calcular la probabilidad de obtener 40 veces o menos 
probabilidad_40_o_menos <- pbinom(40, size = n, prob = p)

# Calcular la probabilidad de obtener 19 veces o menos
probabilidad_19_o_menos <- pbinom(19, size = n, prob = p)

# Calcular la probabilidad de obtener entre 20 y 40 veces
probabilidad_entre_20_y_40 <- probabilidad_40_o_menos - probabilidad_19_o_menos

# Imprimir la probabilidad
print(probabilidad_entre_20_y_40)

#9. Calcular los cuantiles 50 y 90
# Vector de cuantiles
cuantiles <- c(0.50, 0.90)

# Calcular los cuantiles usando qbinom()
cuantiles_calculados <- qbinom(cuantiles, size = n, prob = p)

# Imprimir los cuantiles
print(cuantiles_calculados)

#10. Generar aleatoriamente 100 valores para la variable y comprobar si el histograma se
# ajusta realmente a una función de probabilidad Binomial.
# Número de ensayos
n <- 50

# Probabilidad de éxito (obtener un 6)
p <- 1/6

# Generar 100 valores aleatorios de una distribución binomial
valores_aleatorios <- rbinom(100, size = n, prob = p)

# Crear un histograma de los valores aleatorios
hist(valores_aleatorios, breaks = 0:n, freq = FALSE, 
     main = "Histograma de valores aleatorios binomiales", 
     xlab = "Número de éxitos", 
     ylab = "Densidad de probabilidad")

# Superponer la función de probabilidad binomial teórica
lines(0:n, dbinom(0:n, size = n, prob = p), col = "red")




#2.
 #Mediante una encuesta cuyas respuestas están disponibles en el archivo encuesta.csv, y
 #después de eliminar las respuestas de 5 personas que no contestaron alguna pregunta, se
 #sabe que 26 de 74 estudiantes de la asignatura Estadística del Grado en Ingeniería en
 #Sistemas de Información de la Universidad de Alcalá del curso 2021-22 tienen una línea
 #telefónica contratada con las compañías Jazzte/Orange/Simyo. Si se pregunta aleatoriamente

 #a 6 de los estudiantes, responder a las siguientes preguntas:
# Generar 100 valores aleatorios de una distribución binomial
valores_aleatorios <- rbinom(100, size = n, prob = p)

# Crear un histograma de los valores aleatorios
hist(valores_aleatorios, breaks = 0:n, freq = FALSE, 
     main = "Histograma de valores aleatorios binomiales", 
     xlab = "Número de éxitos", 
     ylab = "Densidad de probabilidad")

# Superponer la función de probabilidad binomial teórica
lines(0:n, dbinom(0:n, size = n, prob = p), col = "red")

#b Calcular la esperanza y varianza de la variable aleatoria
# Esperanza
esperanza <- n * p
print(esperanza)

# Varianza
varianza <- n * p * (1 - p)
print(varianza)

#c Dibujar las funciones de probabilidad y distribución de la variable aleatoria
# Crear un vector con el rango de posibles éxitos
x <- 0:n

# Calcular la probabilidad para cada valor de x
probabilidades <- dbinom(x, size = n, prob = p)

# Crear el gráfico de la función de probabilidad
plot(x, probabilidades, type = "h", lwd = 2,
     main = "Función de Probabilidad Binomial B(50, 1/6)",
     xlab = "Número de éxitos (obtener un 6)",
     ylab = "Probabilidad")

#d Cuál es la probabilidad de que exactamente 3 de los 6 estudiantes tengan contrato
#con Jazzte/Orange/Simyo
n <- 6  # Número de ensayos
p <- 26/74  # Probabilidad de éxito

# Calcular la probabilidad de exactamente 3 éxitos
probabilidad <- dbinom(3, size = n, prob = p)

# Imprimir el resultado
print(probabilidad)

#e Cuál es la probabilidad de que 3 o menos de los 6 estudiantes tengan contrato con
#Jazzte/Orange/Simyo
# Calcular la probabilidad de 3 o menos éxitos
probabilidad_acumulada <- pbinom(3, size = n, prob = p)

# Imprimir el resultado
print(probabilidad_acumulada)

#f Cuál es la probabilidad de que tengan contrato con Jazzte/Orange/Simyo más de 3
#de los 6 estudiantes
n <- 6  # Número de ensayos
p <- 26/74  # Probabilidad de éxito

# Calcular la probabilidad de 3 o menos éxitos (evento complementario)
probabilidad_complementaria <- pbinom(3, size = n, prob = p)

# Calcular la probabilidad de más de 3 éxitos
probabilidad_deseada <- 1 - probabilidad_complementaria

# Imprimir el resultado
print(probabilidad_deseada)


#g Cuál es la probabilidad de que los 6 estudiantes tengan contrato con
#Jazzte/Orange/Simyo
# Calcular la probabilidad de que los 6 estudiantes tengan contrato
probabilidad <- dbinom(6, size = n, prob = p)

# Imprimir el resultado
print(probabilidad)


#g Cuál es la probabilidad de que ninguno de los 6 estudiantes tenga contrato con
#Jazzte/Orange/Simyo
# Calcular la probabilidad de que ningún estudiante tenga contrato (0 éxitos)
probabilidad <- dbinom(0, size = n, prob = p)

# Imprimir el resultado
print(probabilidad)

#i Cuál es la probabilidad de que entre 2 y 4 estudiantes tengan contrato con
#Jazzte/Orange/Simyo

# Calcular P[X ≤ 4]
prob_hasta_4 <- pbinom(4, size = n, prob = p)

# Calcular P[X ≤ 1]
prob_hasta_1 <- pbinom(1, size = n, prob = p)

# Calcular P[2 ≤ X ≤ 4]
probabilidad <- prob_hasta_4 - prob_hasta_1

# Imprimir el resultado
print(probabilidad)

#j Cuál es el valor de los cuartiles (25%, 50%, 75%, 100%) de la variable aleatoria
# Calcular los cuartiles
cuartiles <- qbinom(c(0.25, 0.50, 0.75, 1), size = n, prob = p)

# Imprimir los resultados
print(cuartiles)

# k Si tenemos los datos de la encuesta original a los 74 alumnos, comprobar si tomando
# las respuestas en grupos de 6 alumnos, el histograma se ajusta realmente a la función
# de probabilidad binomial
