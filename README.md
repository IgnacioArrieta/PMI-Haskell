# README

## 1. Condiciones

- El laboratorio se realizará en grupos de 3 estudiantes.
- Se deberá entregar un archivo `.hs` correctamente organizado, con las definiciones solicitadas y casos de prueba.
- Los casos de prueba deben incluir ejemplos de ejecución que permitan verificar casos normales y casos límite.
- Todos los integrantes deberán conocer y poder explicar la solución completa.
- Durante la defensa, que será individual, se podrá solicitar la explicación, ejecución o modificación de alguna función.

## 2. Ejercicios

### 1. Tipo `Turno`

Defina el tipo `Turno` para representar número, paciente, especialidad y estado.

```haskell
data EstadoTurno = Pendiente | Atendido | Cancelado
```

### 2. Funciones

Implemente las funciones que se describen a continuación.

#### a)

```haskell
pendientes :: [Turno] -> [Turno]
```

#### b)

```haskell
buscarTurno :: Numero -> Arbol Turno -> Maybe Turno
```

Retorna un turno si lo encuentra.

i. Utilizar:

```haskell
data Arbol a = Vacio | Nodo a (Arbol a) (Arbol a) deriving Show
```

ii. El árbol se debe encontrar ordenado por `Numero` y la función debe realizar una búsqueda binaria.

#### c)

```haskell
cantidadEspecialidad :: String -> [Turno] -> Int
```

Debe resolverse con `foldr`.

#### d)

```haskell
atenderTurno :: Int -> [Turno] -> Either String [Turno]
```

Debe contemplar turno inexistente, cancelado o ya atendido.

#### e)

Sobre `Arbol Turno`, calcular cantidad total y cantidad de pendientes usando:

```haskell
foldArbol :: (a -> b -> b -> b) -> b -> Arbol a -> b
```

#### f)

Determinar mediante un único `foldr` la cantidad de turnos pendientes, atendidos y cancelados.
