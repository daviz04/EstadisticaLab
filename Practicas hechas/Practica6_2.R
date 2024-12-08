####################1.
#a. Calcular los intervalos de confianza para la diferencia de medias poblacionales del
#tiempo del viaje a la Escuela de los turnos de mañana y tarde con niveles de confianza
#del 90%, 95% y 99%. %. ¿Existe una diferencia significativa entre las medias de las
#dos poblaciones?
encuesta = read.csv2("D:/Universidad/Estadistica/EstadisticaLab/encuestaViaje.csv")
encuesta = na.omit(encuesta)
encuestaMañana = encuesta$VIAJE[encuesta$GRUPO == "A1" | encuesta$GRUPO == "A2"]
encuestaTarde = encuesta$VIAJE[encuesta$GRUPO == "B1" | encuesta$GRUPO == "B2"]
length(encuestaMañana)
length(encuestaTarde)

#creacion de medias a diferenciar y de sus limites
ic.1=ic.media(encuestaMañana,0.95)
li.1=ic.1[1]
ls.1=ic.1[2]
ic.2=ic.media(encuestaTarde,0.95)
li.2=ic.2[1]
ls.2=ic.2[2]

#Grafica de las medias
plot(c(1,1,2,2),c(li.1,ls.1,li.2,ls.2), xlim = c(0,4))
lines(c(1,1),c(li.1,ls.1))
lines(c(2,2),c(li.2,ls.2))

#diferencia de las medias
ic.dif.medias(encuestaMañana,encuestaTarde,0.90)
ic.dif.medias(encuestaMañana,encuestaTarde,0.95)
ic.dif.medias(encuestaMañana,encuestaTarde,0.99)


#test para ver si esta bien
test=t.test(encuestaMañana,encuestaTarde,conf.level=0.90)
test$conf.int
test=t.test(encuestaMañana,encuestaTarde,conf.level=0.95)
test$conf.int
test=t.test(encuestaMañana,encuestaTarde,conf.level=0.99)
test$conf.int

###################2.
#a. Calcular los intervalos de confianza para la diferencia de medias poblacionales del
#tiempo de viaje de ida y del tiempo de viaje de vuelta de los estudiantes, con niveles
#de confianza del 90%, 95% y 99%. %. ¿Existe una diferencia significativa entre las
#medias de las dos poblaciones?
viajeIda=encuesta$VIAJE
viajeVuelta=0.9*encuesta$VIAJE

ic.dif.medias(viajeIda,viajeVuelta,0.90,pareados=TRUE)
ic.dif.medias(viajeIda,viajeVuelta,0.95,pareados=TRUE)
ic.dif.medias(viajeIda,viajeVuelta,0.99,pareados=TRUE)

test=t.test(viajeIda,viajeVuelta,conf.level=0.90,paired=TRUE)
test$conf.int
test=t.test(viajeIda,viajeVuelta,conf.level=0.95,paired=TRUE)
test$conf.int
test=t.test(viajeIda,viajeVuelta,conf.level=0.99,paired=TRUE)
test$conf.int


######################3.
#a. Calcular los intervalos de confianza para la diferencia de las proporciones
#  poblacionales de estudiantes que tienen una línea telefónica contratada con las
#  compañías Jazzte/Orange/Simyo en los turnos de mañana y tarde, con un nivel de
#  confianza del 90%, 95% y 99%. ¿Existe una diferencia significativa entre las
#  proporciones de las dos poblaciones?
movilMañana = encuesta$MOVIL[(encuesta$GRUPO == "A1" | encuesta$GRUPO == "A2") & encuesta$MOVIL == "Jazztel/Orange/Simyo"]
movilTarde = encuesta$MOVIL[(encuesta$GRUPO == "B1" | encuesta$GRUPO == "B2") & encuesta$MOVIL == "Jazztel/Orange/Simyo"]
length(movilMañana)
length(movilTarde)
n = c(length(encuesta$MOVIL[(encuesta$GRUPO == "A1" | encuesta$GRUPO == "A2")]), length(encuesta$MOVIL[(encuesta$GRUPO == "B1" | encuesta$GRUPO == "B2")]))

ng=c(length(movilMañana),length(movilTarde))
print(ng)
ic.1=ic.proporcion(ng[1],n[1],0.95)
li.1=ic.1[1]
ls.1=ic.1[2]
ic.2=ic.proporcion(ng[2],n[2],0.95)
li.2=ic.2[1]
ls.2=ic.2[2]
plot(c(1,1,2,2),c(li.1,ls.1,li.2,ls.2), xlim = c(0,4))
lines(c(1,1),c(li.1,ls.1))
lines(c(2,2),c(li.2,ls.2))


ic.dif.proporciones(ng,n,0.90)
ic.dif.proporciones(ng,n,0.95)
ic.dif.proporciones(ng,n,0.99)

test=prop.test(ng,n,conf.level=0.90)
test$conf.int
test=prop.test(ng,n,conf.level=0.95)
test$conf.int
test=prop.test(ng,n,conf.level=0.99)
test$conf.int
