module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

type Nombre = String

type TrucosDeCocina = Number -> Plato -> Plato

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

endulzar gramosAzucar platoOriginal = UnPlato {
    dificultad = dificultad platoOriginal,
    componentes = ("Azucar", gramosAzucar) : componentes platoOriginal
}

salar :: TrucosDeCocina

salar gramosDeSal unPlato = unPlato {
    componentes = ("Sal", gramosDeSal) : componentes unPlato
}

darSabor :: Number -> TrucosDeCocina

darSabor cantSal cantAzucar unPlato = unPlato {
    componentes = ("Sal", cantSal) : ("Azúcar", cantAzucar) : componentes unPlato
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

esVegano ("carne", _) = False
esVegano ("huevos", _) = False
esVegano ("alimentos lácteos", _) = False
esVegano _  = True

esSinTacc :: Componente -> Bool

esSinTacc ("harina", _) = True

esComplejo :: Plato -> Bool

esComplejo unPlato =  masDe5Componentes unPlato && esDificil unPlato

masDe5Componentes :: Plato -> Bool

masDe5Componentes (UnPlato _ componentes) = length componentes > 5

esDificil :: Plato -> Bool

esDificil (UnPlato dificultad _) =  dificultad > 7

noAptoHipertension :: Componente -> Bool

noAptoHipertension ("sal", gramosSal) = gramosSal > 2





