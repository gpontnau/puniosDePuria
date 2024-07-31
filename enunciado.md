# Puños de Puria

Queremos desarrollar un programa que analice y calcule el resultado de los enfrentamientos entre peleadores de artes marciales, utilizando el paradigma funcional.

## 1. Modelo base

### a. Modelar peleadores
Modelar a los peleadores, de los cuales se conocen sus puntos de vida, su resistencia y el conjunto de los ataques que conoce.

### b. Implementar operaciones sobre peleadores
Basándonos en el modelo anterior:

i. `estaMuerto`: Un peleador está muerto si su vida es menor a 1.

ii. `esHabil`: Un peleador es hábil cuando conoce más de 10 ataques.

### c. Modelar ataques
Implementar los siguientes ataques de forma tal que puedan ser incluidos en el conjunto conocido por un peleador:

i. **golpe**: Reduce la vida del peleador oponente en una cantidad igual a la intensidad del golpe dividido la resistencia del oponente. (Por ejemplo, un golpe de intensidad 25 haría perder 3 puntos de vida a un oponente con resistencia 7: 25 `div` 7 == 3).

ii. **toque de la muerte**: El toque de la muerte hace que el oponente pierda toda su vida.

iii. **patada**: Las patadas causan distintos efectos dependiendo en qué parte del cuerpo el oponente las reciba:
   - Una patada en el pecho hace que un oponente vivo pierda 10 puntos de vida, pero reanima el corazón de un oponente muerto, haciéndole ganar 1 punto de vida.
   - Una patada en la carita hace que cualquier oponente pierda la mitad de la vida que le queda.
   - Una patada en la nuca no causa ningún daño, pero hace que el oponente olvide su primer ataque, si es que conoce alguno.
   - Una patada en cualquier otra parte del cuerpo no hace nada. Es necesario poder representar los otros lugares aunque no causen ningún efecto. (Por ejemplo, una patada en la nuca haría que un oponente que sólo sabe hacer el toque de la muerte se quede sin ataques, mientras que una patada a, digamos, la pantorrilla o la axila, no le haría nada).

### d. Crear a Bruce Lee
Escribir el código con el que crearía a Bruce Lee, un peleador con 200 de vida y 25 de resistencia que tiene entre sus ataques el toque de la muerte, un golpe de intensidad 500 y un ataque más que consiste en una secuencia de tres patadas a la carita.

## 2. Mejor ataque

Dados un peleador y un enemigo, encontrar el mejor ataque del peleador contra ese enemigo (es decir, encontrar el ataque del peleador que deja con menos vida al enemigo)

## 3. Operaciones avanzadas

Implementar las siguientes operaciones tratando de aprovechar al máximo los conceptos de orden superior, composición y aplicación parcial:

a. `terrible`: un ataque es terrible para un conjunto de enemigos si, luego de realizarlo contra todos ellos, quedan vivos menos de la mitad.

b. `peligroso`: un peleador es peligroso para un conjunto de enemigos si todos sus ataques son terribles para los miembros del conjunto que son hábiles.

c. `invencible`: un peleador es invencible para un conjunto de enemigos si, luego de recibir el mejor ataque de cada uno de ellos, sigue teniendo la misma vida que antes de ser atacado. (No importa el orden en que se apliquen los ataques).