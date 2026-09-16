type Numero = Int
type Paciente = String
type Especialidad = String

data EstadoTurno = Pendiente|Atendido|Cancelado 
 deriving (Show, Eq)

type Turno = (Numero, Paciente, Especialidad, EstadoTurno)

pendientes :: [Turno] -> [Turno]
pendientes [] = []
pendientes ((a, b, c, est):xs) 
 |est == Pendiente = (a, b, c, est) : pendientes xs
 |otherwise = pendientes xs
   