module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

type Nombre = String

type TrucosDeCocina = Number -> Number -> Plato -> Plato

data Participante = UnParticipante {

    nombre :: Nombre,
    trucosDeCocina :: TrucosDeCocina,
    platoDeEspecialidad :: Plato

}

data Plato = UnPlato {

    dificultad :: Dificultad,
    componentes :: [Componente]

} deriving (Show, Eq)

type Componente = (Ingrediente, Peso)

type Dificultad = Number

type Ingrediente = String

type Peso = Number


endulzar :: TrucosDeCocina

endulzar _ gramosAzucar platoOriginal = UnPlato {
    dificultad = dificultad platoOriginal,
    componentes = ("Azucar", gramosAzucar) : componentes platoOriginal
}

salar :: TrucosDeCocina

salar  gramosDeSal _ unPlato = unPlato {
    componentes = ("Sal", gramosDeSal) : componentes unPlato
}

darSabor :: TrucosDeCocina

darSabor cantSal cantAzucar unPlato = unPlato {
    componentes = ("Sal", cantSal) : ("Azucar", cantAzucar) : componentes unPlato
}

type ModificarPlato = Plato -> Plato 

duplicarPorcion :: ModificarPlato

duplicarPorcion unPlato = unPlato {
    componentes = map (modificarComponentes (*2)) (componentes unPlato)
}

modificarComponentes :: (Number -> Number) -> Componente -> Componente

modificarComponentes unaFuncion (ingrediente, peso) = (ingrediente, max(0) (unaFuncion peso))

simplificar :: ModificarPlato

simplificar unPlato
    | length (componentes unPlato) > 5 && (dificultad unPlato) > 7 = unPlato {
        dificultad = 5,
        componentes =  (filter esLiviano) (componentes unPlato)
    }

    | otherwise = unPlato

esLiviano :: Componente -> Bool

esLiviano (_ , peso) = peso < 10

esVegano :: Componente -> Bool

esVegano ("Carne", _) = False
esVegano ("Huevos", _) = False
esVegano ("Alimentos Lacteos", _) = False
esVegano _  = True

esSinTacc :: Componente -> Bool

esSinTacc ("harina", _) = False

esComplejo :: Plato -> Bool

esComplejo unPlato =  masDe5Componentes unPlato && esDificil unPlato

masDe5Componentes :: Plato -> Bool

masDe5Componentes (UnPlato _ componentes) = length componentes > 5

esDificil :: Plato -> Bool

esDificil (UnPlato dificultad _) =  dificultad > 7

noAptoHipertension :: Componente -> Bool

noAptoHipertension ("Sal", gramosSal) = gramosSal > 2
noAptoHipertension _ = False

---------------- PARTE B 
pepe :: Participante

pepe = UnParticipante {
    nombre = "Pepe Ronccino",
    trucosDeCocina = trucoDePepe,
    platoDeEspecialidad = platoDePepe

}

trucoDePepe :: TrucosDeCocina

trucoDePepe _ _ unPlato = (duplicarPorcion.simplificar.darSabor 2 5) unPlato

platoDePepe :: Plato

platoDePepe = UnPlato{
    dificultad = 8, 
    componentes = [("Sal", 5), ("Carne", 500), ("Papas", 300), ("Aceite", 20), ("Pimienta", 5), ("Huevo", 60)]
    }

---------------- PARTE C

cocinar :: Participante -> Plato

cocinar unParticipante = (trucosDeCocina unParticipante 0 0) (platoDeEspecialidad unParticipante) 

esMejorQue :: Plato -> Plato -> Bool

esMejorQue unPlato otroPlato = (dificultad unPlato) > (dificultad otroPlato) && esMenorElPeso unPlato otroPlato

esMenorElPeso :: Plato -> Plato -> Bool

esMenorElPeso unPlato otroPlato = sum (map snd(componentes unPlato)) < sum  (map snd(componentes otroPlato))

participanteEstrella :: [Participante] -> Participante

participanteEstrella [unParticipante] = unParticipante  

participanteEstrella (x:xs)
    | esMejorQue (cocinar x) (cocinar (participanteEstrella xs)) = x
    | otherwise                                                  = participanteEstrella xs