# Si se forman aleatoriamente palabras (con o sin sentido) de 3 letras con las letras L, O y S:
# a. ¿Cuál es la probabilidad de obtener la palabra SOL si no se pueden repetir letras?
  permutacion = 1/factorial(3)

# b. ¿Cuál es la probabilidad de obtener la palabra SOL si se pueden repetir letras?
  VR = function(m,n){
    m^n
  }
  varRep = 1/VR(3, 3)
# c. ¿Cuál de los dos sucesos o eventos anteriores es más probable? NOTA: Sí importa el orden. 
# En a) hay que usar permutaciones sin repetición (P3) para los casos posibles y sólo hay un caso 
# favorable. En b) hay que usar variaciones con repetición (𝑉𝑅33) para los casos posibles, y sólo hay un caso favorable.
 print(permutacion)
 print(varRep) 
 #Es más probable la permutación
 
#2. Al lanzar dos veces un dado con seis caras:
#a. ¿Cuál es la probabilidad de que los resultados de los dos lanzamientos sean distintos?
  probDadosFavo = VR(6, 2)
print(probDados)
V = function(m,n){
  factorial(m)/factorial(m-n)
}
totalDadosPosi = V(6, 2)
print(totalDados)
print(totalDadosPosi / probDadosFavo)

#b. ¿Cuál es la probabilidad de que los resultados de los dos lanzamientos sean iguales?
iguales = 1- totalDadosPosi/probDadosFavo
print(iguales)

#c. ¿Cuál de los dos sucesos o eventos anteriores es más probable?
  # Es mas probable que los dos lanzamientos sean iguales

#3. Al sacar simultáneamente cuatro cartas de una baraja:
#a. ¿Cuál es la probabilidad de sacar los cuatro ases si la baraja es española (40 cartas)?
C = function(m,n){
  choose(m,n)
}
casosPosibles = C(40, 4)
print(casosPosibles)
casosFavorables = 4

print(casosFavorables/casosPosibles)

#b. ¿Cuál es la probabilidad de sacar cuatro o tres ases si la baraja es francesa (52 cartas)?
bCasosPos = C(52, 4)
print(bCasosPos)
  #Sacar 4 ases
   cuatroAses = 1/bCasosPos
   print(cuatroAses)   
  #Sacar 3 ases
   tresAsesCasosFav = C(4, 3)
   tresAses = tresAsesCasosFav/bCasosPos 
   tresAses = tresAses*48 #Se multiplica por el resto de cartas que no son ases
   print(tresAses)   

   #Probabilidad total de ellos juntos
   probTotal = cuatroAses + tresAses
   print(probTotal)   
   193/270725
   