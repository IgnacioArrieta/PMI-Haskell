--PMI
--Integrantes: Ignacio Leonel Arrieta y Leandro Sebastián de la Reta

--Ejercicio 1: defina el tipo Turno para representar número, paciente, especialidad y estado.

type Numero = Int
type Paciente = String
type Especialidad = String

data EstadoTurno = Pendiente|Atendido|Cancelado 
 deriving (Show, Eq)

type Turno = (Numero, Paciente, Especialidad, EstadoTurno)

--Ejercicio 2: implemente las funciones que se describen a continuación.

--Función 1
pendientes :: [Turno] -> [Turno]
pendientes [] = []
pendientes ((a, b, c, est):xs) 
 |est == Pendiente = (a, b, c, est) : pendientes xs
 |otherwise = pendientes xs

--Casos de prueba para probar:
--pendientes [(1001, "Leon S. Kennedy", "Agente", Pendiente), (2003, "Camila Pablo Gomez", "Profesor", Pendiente)]
--pendientes [(3210, "Gonzalo Valentin Perez", "Bombero", Cancelado), (5020, "Facundo Mateo Ramirez", "Doctor", Atendido)]
--pendientes [(7777, "Maria Fernandez", "Reportero", Pendiente), (4323, "Sherlock Holmes", "Detective", Atendido)]

--Función 2
data Arbol a = Vacio | Nodo a (Arbol a) (Arbol a) 
 deriving Show

buscarTurno :: Numero -> Arbol Turno -> Maybe Turno
buscarTurno x Vacio = Nothing
buscarTurno x (Nodo valor izq der) 
 |x == returnNumero valor = Just valor
 |x > returnNumero valor = buscarTurno x der
 |otherwise = buscarTurno x izq
 where
  returnNumero (num, _, _, _) = num

--Casos de prueba para probar:
--
--
-- 

--Función 3
cantidadEspecialidad :: String -> [Turno] -> Int
cantidadEspecialidad esp_c list = foldr f 0 list
 where 
  f (_, _, esp, _) acc  
   |esp == esp_c = acc + 1
   |otherwise = acc

--Casos de prueba para probar:
--cantidadEspecialidad "Inspector" [(1991, "Guybrush Threepwood", "Inspector", Pendiente), (2001, "Roberto Martez", "Biologo", Atendido)]
--cantidadEspecialidad "Plomero" [(1998, "Albert Wesker", "Cientifico", Atendido), (1342, "Lucia Santiago Romero", "Contador", Cancelado)]
--cantidadEspecialidad "Abogado" [(2342, "Phoenix Wright", "Abogado", Cancelado), (9283, "Apollo Justice", "Abogado", Pendiente)]

--Funcion 4:
atenderTurno :: Int -> [Turno] -> Either String [Turno]
atenderTurno n _ | n < 0  = Left "No existe la posicion en la lista"
atenderTurno _ [] = Left "No existe el turno"
atenderTurno 0 ((num, pac, esp, est):xs)
  |est == Pendiente = Right ((num, pac, esp, Atendido) : xs)
  |est == Atendido = Left "El turno ya se Atendio"
  |est == Cancelado = Left "El turno esta Cancelado"
  |otherwise = Left "El turno tiene valores extraños"
atenderTurno n (t : xs) = fmap (t :) (atenderTurno (n - 1) xs)

--Funcion 5:
foldArbol :: (a -> b -> b -> b) -> b -> Arbol a -> b
foldArbol _ z Vacio = z
foldArbol f z (Nodo x izq der)  = f x (foldArbol f z izq) (foldArbol f z der) 

cantidadTotal :: Arbol Turno -> Int
cantidadTotal a = foldArbol f 0 a
  where
    f _ i d = 1 + i + d 

cantidadPendiente :: Arbol Turno -> Int
cantidadPendiente a = foldArbol f 0 a 
  where
    f (_,_,_,est) i d = (if est == Pendiente then 1 else 0) + i + d 

--Funcion 6:
foldrTurnos :: [Turno] -> (Int, Int, Int)
foldrTurnos lista = foldr f (0, 0, 0) lista 
  where
    f (num, pac, esp, est) (pen, ate, can)
      |est == Pendiente = (pen+1,ate,can) 
      |est == Atendido = (pen,ate+1,can)
      |est == Cancelado = (pen,ate,can+1)