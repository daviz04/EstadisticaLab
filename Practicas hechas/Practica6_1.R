###############1. 
#a. Calcular los intervalos de confianza para la media poblacional con niveles de
#   confianza del 90%, 95% y 99%
encuesta = read.csv2("D:/Universidad/Estadistica/EstadisticaLab/encuestaViaje.csv")
encuesta = na.omit(encuesta)
print(encuesta)

tiempo = encuesta$VIAJE

ic.media(tiempo, 0.90)
ic.media(tiempo, 0.95)
ic.media(tiempo, 0.99)

test=t.test(tiempo, conf.level = 0.90)
test$conf.int
test=t.test(tiempo, conf.level = 0.95)
test$conf.int
test=t.test(tiempo, conf.level = 0.99)
test$conf.int


#b. Calcular los intervalos de confianza para la varianza poblacional con 
#   niveles deconfianza del 90%, 95% y 99%
ic.varianza(tiempo, 0.90)
ic.varianza(tiempo, 0.95)
ic.varianza(tiempo, 0.99)

################2. 
#a. Calcular los intervalos de confianza para la media poblacional con niveles 
#   de confianza del 90%, 95% y 99%
tiempo = encuesta$VIAJE[encuesta$GRUPO == "A1"]
print(tiempo)

ic.media(tiempo, 0.90)
ic.media(tiempo, 0.95)
ic.media(tiempo, 0.99)

test=t.test(tiempo, conf.level = 0.90)
test$conf.int
test=t.test(tiempo, conf.level = 0.95)
test$conf.int
test=t.test(tiempo, conf.level = 0.99)
test$conf.int

#b. Calcular los intervalos de confianza para la varianza poblacional con niveles de
#   confianza del 90%, 95% y 99%
ic.varianza(tiempo, 0.90)
ic.varianza(tiempo, 0.95)
ic.varianza(tiempo, 0.99)

##################3.
#a. Calcular los intervalos de confianza para la proporción poblacional de 
#   estudiantes que tienen una línea telefónica contratada con las compañías 
#   Jazzte/Orange/Simyo con con un nivel de confianza del 90%, 95% y 99%

tiempo = encuesta$VIAJE[encuesta$MOVIL == "Jazztel/Orange/Simyo"]
n = length(encuesta$MOVIL)
print(n)
ng = length(tiempo)

print(ng)
ic.proporcion(ng, n, 0.90)
ic.proporcion(ng, n, 0.95)
ic.proporcion(ng, n, 0.99)

test=prop.test(ng, n, conf.level = 0.90)
test$conf.int
test=prop.test(ng, n, conf.level = 0.95)
test$conf.int
test=prop.test(ng, n, conf.level = 0.99)
test$conf.int


#b. Estimar el mínimo y máximo número de estudiantes de la población que tienen una
#línea telefónica contratada con las compañías Jazzte/Orange/Simyo móvil con una
#confianza del 90%, 95% y 99%. NOTA: Utilizar los intervalos de confianza calculados
#con las fórmulas, no con la función prop.test().
(tiempo = 108*ic.proporcion(ng,n,0.90))
(tiempo = 108*ic.proporcion(ng,n,0.95))
(tiempo = 108*ic.proporcion(ng,n,0.99))
