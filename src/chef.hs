
-- ParteA
-- Modelando los datos 
type Componente = (String, Int)
type Plato = (Int, [Componente])
type Truco = Plato -> Plato
type Participante = (String, [Truco], Plato)

dificultad :: Plato -> Int
dificultad = fst

ingredientes :: Plato -> [Componente]
ingredientes  = snd

modificarIngredientes :: ([Componente] -> [Componente]) -> Plato -> Plato
modificarIngredientes funcion unPlato =  (dificultad unPlato, funcion (ingredientes unPlato))

-- Trucos
endulzar :: Int -> Truco
endulzar gramos = modificarIngredientes (("azucar", gramos):)

salar :: Int -> Truco
salar gramos = modificarIngredientes (("sal", gramos):)

darSabor :: Int -> Int -> Truco
darSabor gramosSal gramosAzucar = salar gramosSal . endulzar gramosAzucar

duplicarComponente :: Componente -> Componente
duplicarComponente (nombre, gramos) = (nombre, gramos * 2)

duplicarPorcion :: Truco
duplicarPorcion = modificarIngredientes (map duplicarComponente)

cantidadDeComponentes :: Plato -> Int
cantidadDeComponentes = length . ingredientes

tieneDificultadAlta :: Plato -> Bool
tieneDificultadAlta = (>7) . dificultad

esComplejo :: Plato -> Bool
esComplejo unPlato = cantidadDeComponentes unPlato > 5 && tieneDificultadAlta unPlato

pesoMayorA10 :: Componente -> Bool
pesoMayorA10 = (>=10) . snd

simplificar :: Truco
simplificar unPlato
    | esComplejo unPlato = (5, filter pesoMayorA10 (ingredientes unPlato))
    | otherwise = unPlato

tieneIngrediente :: String -> Plato -> Bool
tieneIngrediente nombre = any ((== nombre) . fst) . ingredientes


esVegano :: Plato -> Bool
esVegano unPlato =
    not (tieneIngrediente "carne" unPlato) &&
    not (tieneIngrediente "huevo" unPlato) &&
    not (tieneIngrediente "leche" unPlato)

esSinTacc :: Plato -> Bool
esSinTacc = not . tieneIngrediente "harina"

gramosDeIngrediente :: String -> Plato -> Int
gramosDeIngrediente nombre = sum . map snd . filter ((== nombre) . fst) . ingredientes

noAptoHipertension :: Plato -> Bool
noAptoHipertension = (>2) . gramosDeIngrediente "sal"

-- Parte B 

platoPepe :: Plato
platoPepe = (8,[("carne",100), ("queso",20), ("papa",10),("cebolla",15), ("ajo",12), ("tomate",18)])

trucosPepe :: [Truco]
trucosPepe = [darSabor 2 5, simplificar, duplicarPorcion]

pepeRonccino :: Participante
pepeRonccino = ("Pepe Ronccino", trucosPepe, platoPepe)

-- Parte C
-- funciones auxiliares
nombreParticipante :: Participante -> String
nombreParticipante (nombre, _, _) = nombre

trucos :: Participante -> [Truco]
trucos (_, unosTrucos, _) = unosTrucos

especialidad :: Participante -> Plato
especialidad (_, _, unPlato) = unPlato

aplicarTruco :: Plato -> Truco -> Plato
aplicarTruco unPlato unTruco = unTruco unPlato

-- cocinar
cocinar :: Participante -> Plato
cocinar unParticipante = foldl aplicarTruco (especialidad unParticipante) (trucos unParticipante)

-- esMejorQue
pesoTotal :: Plato -> Int
pesoTotal = sum . map snd . ingredientes

tieneMenorPeso :: Plato -> Plato -> Bool
tieneMenorPeso unPlato otroPlato = pesoTotal unPlato < pesoTotal otroPlato

tieneMayorDificultad :: Plato -> Plato -> Bool
tieneMayorDificultad unPlato otroPlato = dificultad unPlato > dificultad otroPlato

esMejorQue :: Plato -> Plato -> Bool
esMejorQue unPlato otroPlato = tieneMayorDificultad unPlato otroPlato && tieneMenorPeso unPlato otroPlato

-- ParticipanteEstrella 

participanteEstrella :: [Participante] -> Participante
participanteEstrella [unParticipante] = unParticipante
participanteEstrella (unParticipante:otrosParticipantes)
    | cocinar unParticipante `esMejorQue` cocinar (participanteEstrella otrosParticipantes) = unParticipante
    | otherwise = participanteEstrella otrosParticipantes



