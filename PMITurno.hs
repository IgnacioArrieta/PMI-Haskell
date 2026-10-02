--PMI de Haskell
--Programación 3
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
--pendientes []
--pendientes [(2034, "Bryan Hodukavich", "Mensajero", Atendido)]
--pendientes [(1998, "Claire Redfield", "Estudiante", Pendiente)]

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
--buscarTurno 2000 (Nodo (1998, "Albert Wesker", "Cientifico", Atendido) Vacio Vacio)
--buscarTurno 1 Vacio
--buscarTurno 2001 (Nodo (2001, "Roberto Martez", "Biologo", Cancelado) (Nodo (1991, "Guybrush Threepwood", "Inspector", Pendiente) Vacio Vacio) (Nodo (9283, "Apollo Justice", "Abogado", Pendiente) Vacio Vacio))
--buscarTurno 1990 (Nodo (2001, "Roberto Martez", "Biologo", Cancelado) (Nodo (1991, "Guybrush Threepwood", "Inspector", Pendiente) Vacio Vacio) (Nodo (9283, "Apollo Justice", "Abogado", Pendiente) Vacio Vacio))
--buscarTurno 5000 (Nodo (7621, "Mila Evans", "Artista", Pendiente) (Nodo (3572, "Ashely M. Robbins", "Estudiante", Pendiente) Vacio Vacio) (Nodo (4723, "Ben Throttle", "Acróbata", Pendiente) Vacio Vacio))

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
--cantidadEspecialidad "Policia" []
--cantidadEspecialidad "Policia" [(1979, "Kyle Hyde", "Policia", Cancelado)]

--Funcion 4:
atenderTurno :: Int -> [Turno] -> Either String [Turno]
atenderTurno _ [] = Left "No existe el turno"
atenderTurno n (x@(num, pac, esp, est):xs)
  |num == n =
    case est of
     Pendiente -> Right ((num, pac, esp, Atendido) : xs)
     Atendido -> Left "El turno ya se atendio."
     Cancelado -> Left "El turno esta cancelado."
  |otherwise = fmap (x:) (atenderTurno n xs)

--Casos de prueba para probar:
--atenderTurno 1991 [(1991, "Sam", "Policia", Pendiente), (2001, "Max", "Policia", Cancelado)]
--atenderTurno 2001 [(1523, "Valentina Perez", "Doctor", Pendiente), (2001, "Martina Sosa", "Psicologo", Pendiente)]
--atenderTurno 1998 [(1998, "Facundo Rios", "Empresario", Atendido)]
--atenderTurno 2001 [(1991, "Ana Romero", "Matematico", Pendiente), (2001, "Roberto Martez", "Biologo", Cancelado)]
--atenderTurno 5 [(1991, "Guybrush Threepwood", "Inspector", Pendiente)]
--atenderTurno 1 [(1991, "Guybrush Threepwood", "Inspector", Pendiente), (2001, "Roberto Martez", "Biologo", Pendiente)]
--atenderTurno (-1) [(1991, "Guybrush Threepwood", "Inspector", Pendiente)]
--atenderTurno 0 []


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

--Casos de prueba para probar:
--cantidadTotal (Nodo (2001, "Roberto Martez", "Biologo", Cancelado) (Nodo (1991, "Elaine Marley", "Gobernante", Pendiente) Vacio Vacio) (Nodo (9283, "Rivero Ignacio", "Abogado", Pendiente) Vacio Vacio))
--cantidadPendiente (Nodo (2001, "Austin Abraham", "Actor", Cancelado) (Nodo (1991, "James Wilson", "Doctor", Pendiente) Vacio Vacio) (Nodo (9283, "Ron Gilbert", "Programador", Pendiente) Vacio Vacio))
--cantidadTotal Vacio
--cantidadPendiente Vacio
--cantidadTotal (Nodo (1998, "Albert Wesker", "Cientifico", Atendido) Vacio Vacio)
--cantidadPendiente (Nodo (1998, "Alyssa Ashcroft", "Periodista", Atendido) Vacio Vacio)
--cantidadPendiente (Nodo (2001, "Luis Villega", "Policia", Pendiente) (Nodo (1991, "Ramon Salazar", "Gobernante", Pendiente) Vacio Vacio) (Nodo (9283, "Apollo", "Abogado", Pendiente) Vacio Vacio))

--Funcion 6:
foldrTurnos :: [Turno] -> (Int, Int, Int)
foldrTurnos lista = foldr f (0, 0, 0) lista 
  where
    f (num, pac, esp, est) (pen, ate, can)
      |est == Pendiente = (pen+1,ate,can) 
      |est == Atendido = (pen,ate+1,can)
      |est == Cancelado = (pen,ate,can+1)

--Casos de prueba para probar:
--foldrTurnos [(1991, "Larry Butz", "Artista", Pendiente), (2001, "", "Biologo", Atendido), (9283, "Apollo Justice", "Abogado", Pendiente), (2342, "Phoenix Wright", "Abogado", Cancelado)]
--foldrTurnos []
--foldrTurnos [(1998, "Grace Ashcroft", "Analista", Atendido), (1342, "Klavier Gavin", "Musico", Atendido)]
--foldrTurnos [(2342, "Gregory House", "Doctor", Cancelado)]
