{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use section" #-}
{-# HLINT ignore "Fuse foldr/map" #-}
module Library where
import PdePreludat

doble :: Number -> Number
doble x = x * 2


-- a. Modelar Peleador
-- se conocen sus puntos de vida, su resistencia y el conjunto de los ataques que conoce.
data Peleador = Peleador {
    puntosVida  :: Number,
    resistencia :: Number,
    ataques     :: [Ataques]
} deriving (Show)

-- b. Implementar operaciones sobre Peleador
type Operaciones = Peleador -> Bool

-- i. estaMuerto: Un peleador está muerto si su vida es menor a 1. 
estaMuerto :: Operaciones
estaMuerto = (<1) . puntosVida


-- ii. esHabil: Un peleador es hábil cuando conoce más de 10 ataques.
esHabil :: Operaciones
esHabil = (>10) . length . ataques

-- c. Modelar ataques
type Ataques = Peleador -> Peleador
type Ataque = (NombreAtaque, Intensidad)
type NombreAtaque = String
type Intensidad = Number

cambiarVida :: Number -> Ataques
cambiarVida cantidad peleador = peleador {puntosVida = cantidad}

-- i. golpe: Reduce la vida del peleador oponente en una cantidad igual a la intensidad del golpe dividido la resistencia del oponente. (Por ejemplo, un golpe de intensidad 25 haría perder 3 puntos de vida a un oponente con resistencia 7: 25 div 7 == 3).
golpe :: Number -> Ataques
golpe intensidad oponente = cambiarVida (puntosVida oponente - (intensidad `div` resistencia oponente)) oponente

-- ii. toque de la muerte: El toque de la muerte hace que el oponente pierda toda su vida.
toqueDeLaMuerte :: Ataques
toqueDeLaMuerte = cambiarVida 0 

-- iii. patada: Las patadas causan distintos efectos dependiendo en qué parte del cuerpo el oponente las reciba:
patada :: String -> Ataques
patada parteDelCuerpo oponente
    | parteDelCuerpo == "pecho"  = oponente {puntosVida = (-) 10 (puntosVida oponente) }
    | parteDelCuerpo == "carita" = oponente {puntosVida = puntosVida oponente / 2}
    | parteDelCuerpo == "nuca"   = oponente {ataques = tail (ataques oponente)}
    | otherwise                  = oponente

-- d Bruce Lee
-- Escribir el código con el que crearía a Bruce Lee, un peleador con 200 de vida y 25 de resistencia que tiene entre sus ataques el toque de la muerte, un golpe de intensidad 500 y un ataque más que consiste en una secuencia de tres patadas a la carita.
bruceLee :: Peleador
bruceLee = Peleador {
    puntosVida  = 200,
    resistencia = 25,
    ataques     = 
        [toqueDeLaMuerte . golpe 500 .  patada "carita" . patada "carita" . patada "carita"]
}
enemigo :: Peleador
enemigo = Peleador {
    puntosVida  = 100,
    resistencia = 10,
    ataques     = 
        [toqueDeLaMuerte . golpe 500 .  patada "carita" . patada "carita" . patada "carita"]
}

-- 2 Dados un peleador y un enemigo, encontrar el ataque del peleador que deja con menos vida al enemigo  
mejorAtaque :: Peleador -> Peleador -> Ataques
mejorAtaque peleador enemigo = foldl1 (mejorAtaque' enemigo) . ataques $ peleador

mejorAtaque' :: Peleador -> Ataques -> Ataques -> Ataques   
mejorAtaque' enemigo ataque1 ataque2
    | (puntosVida . ataque1) enemigo < (puntosVida . ataque2) enemigo = ataque1
    | otherwise = ataque2

-- 3 
-- a  para un conjunto de enemigos si, luego de realizarlo contra todos ellos, quedan vivos menos de la mitad.
terrible :: [Peleador] -> Ataques -> Bool
terrible enemigos ataque = (<) (length enemigos `div` 2) (length (filter (not . estaMuerto . ataque) enemigos))

-- b para un conjunto de enemigos si todos sus ataques son terribles para los miembros del conjunto que son hábiles.
peligroso :: Peleador -> [Peleador] -> Bool
peligroso atacante enemigos = all (terrible enemigos) (ataques atacante) && esHabil atacante

-- c para un conjunto de enemigos si, luego de recibir el mejor ataque de cada uno de ellos, sigue teniendo la misma vida que antes de ser atacado.
invencible :: [Peleador] -> Peleador -> Bool
invencible enemigos peleador = (==) (puntosVida peleador) . puntosVida . foldl1 (mejorAtaque peleador) $ enemigos



