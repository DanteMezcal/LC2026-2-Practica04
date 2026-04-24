module Practica04 where

--Sintaxis de la logica proposicional
data Prop = Var String | Cons Bool | Not Prop
            | And Prop Prop | Or Prop Prop
            | Impl Prop Prop | Syss Prop Prop
            deriving (Eq)

instance Show Prop where 
                    show (Cons True) = "⊤"
                    show (Cons False) = "⊥"
                    show (Var p) = p
                    show (Not p) = "¬" ++ show p
                    show (Or p q) = "(" ++ show p ++ " ∨ " ++ show q ++ ")"
                    show (And p q) = "(" ++ show p ++ " ∧ " ++ show q ++ ")"
                    show (Impl p q) = "(" ++ show p ++ " → " ++ show q ++ ")"
                    show (Syss p q) = "(" ++ show p ++ " ↔ " ++ show q ++ ")"

type Literal = Prop
type Clausula = [Literal]

p, q, r, s, t, u :: Prop
p = Var "p"
q = Var "q"
r = Var "r"
s = Var "s"
t = Var "t"
u = Var "u"

void :: Clausula
void = []

--Definicion de los tipos para la practica
type Interpretacion = [( String , Bool ) ]
type ParInterp = ( String , Bool )
type Estado = ( Interpretacion , [Clausula])
data ArbolDPLL = Node Estado ArbolDPLL | Branch Estado ArbolDPLL ArbolDPLL | Void deriving Show

--IMPLEMENTACION PARTE 1
--Ejercicio 1
conflict :: Estado -> Bool
conflict e
  | clausulaEnEstado void e = True
  | otherwise = False

--Ejercicio 2
success :: Estado -> Bool
success (inter, clausulas)
  | clausulas == [] = True
  | otherwise = False

--Ejercicio 3
unit :: Estado -> Estado
unit (modelo, []) = (modelo, [])
unit (modelo, c:cs)
  | esUnitaria c =
      let l = obtenerLiteral c
          nombre = obtenerNombre l
      in if tieneInterpretacion nombre modelo
            then (modelo, c:cs)
            else (modelo ++ darValor c, cs)
  | otherwise =
      let (m, cs') = unit (modelo, cs)
      in (m, c:cs')

--Ejercicio 4
elim :: Estado -> Estado
elim ([], cs) = ([], cs)
elim (x:xs, cs) = acumularModelo ([x], []) (elim (xs, auxiliarElim (regresarLiteral x) cs))

--Ejercicio 5
red :: Estado -> Estado
red ([], cs) = ([], cs)
red (x:xs, cs) = acumularModelo ([x], []) (red (xs, auxiliarRed (regresarLiteral x) cs))


--Ejercicio 6
sep :: Literal -> Estado -> (Estado, Estado)
sep l (i, cs) =
  ( (agregar (obtenerNombre l, True) i, cs)
  , (agregar (obtenerNombre l, False) i, cs)
  )
  where
    agregar par [] = [par]
    agregar (x,b) ((y,c):ys)
      | x == y = (x,b):ys
      | otherwise = (y,c) : agregar (x,b) ys

--IMPLEMENTACION PARTE 2

--Ejercicio 1
heuristicsLiteral :: [Clausula] -> Literal
heuristicsLiteral cs = fst (maxApariciones lista)
  where
    lista = contar (concat cs)

--EJERCICIO 2
dpll :: [Clausula] -> Interpretacion
dpll cs =
  let (modelo, clausulasFinales) =
        explorarArbolDPLL (construirArbolDPLL ([], cs))
  in if clausulasFinales == []
        then modelo
        else []

--EXTRA
dpll2 :: Prop -> Interpretacion
dpll2 p = dpll (aClausulas (fnc p))

distribuir :: Prop -> Prop
distribuir (Or p (And q r)) = And (distribuir (Or p q)) (distribuir (Or p r))
distribuir (Or (And q r) p) = And (distribuir (Or p q)) (distribuir (Or p r))
distribuir (Or p q) = Or (distribuir p) (distribuir q)
distribuir (And p q) = And (distribuir p) (distribuir q)
distribuir  p = p

fnn :: Prop -> Prop
fnn (Var p) = Var p
fnn (Cons b) = Cons b
fnn (Not (Var p)) = Not (Var p)
fnn (Not (Cons b)) = Cons (not b)
fnn (Not (Not p)) = fnn p
fnn (Not (And p q)) = Or (fnn (Not p)) (fnn (Not q))
fnn (Not (Or p q)) = And (fnn (Not p)) (fnn (Not q))
fnn (Not (Impl p q)) = fnn (Not (Or (Not p) q))
fnn (Not (Syss p q)) = fnn (Not (And (Impl p q) (Impl q p)))
fnn (And p q) = And (fnn p) (fnn q)
fnn (Or p q) = Or (fnn p) (fnn q)
fnn (Impl p q) = fnn (Or (Not p) q)
fnn (Syss p q) = fnn (And (Impl p q) (Impl q p))

fnc :: Prop -> Prop
fnc p = distribuir (fnn p)

aClausulas :: Prop -> [Clausula]
aClausulas (And p q) = aClausulas p ++ aClausulas q
aClausulas p = [aClausula p]

aClausula :: Prop -> Clausula
aClausula (Or p q) = aClausula p ++ aClausula q
aClausula (Var x) = [Var x]
aClausula (Not (Var x)) = [Not (Var x)]
aClausula _ = []

--Funciones auxiliares para la practica

regresarLiteral :: ParInterp -> Literal
regresarLiteral (x, True) = Var x
regresarLiteral (x, False) = Not (Var x)

auxiliarElim :: Literal -> [Clausula] -> [Clausula]
auxiliarElim _ [] = []
auxiliarElim l (c:cs)
  | l `elem` c = auxiliarElim l cs
  | otherwise = c : (auxiliarElim l cs)

auxiliarRed :: Literal -> [Clausula] -> [Clausula]
auxiliarRed _ [] = []
auxiliarRed l (c:cs)
  | negar l `elem` c = removerLiteral (negar l) c : (auxiliarRed l cs)
  | otherwise = c : (auxiliarRed l cs)

removerLiteral :: Literal -> Clausula -> Clausula
removerLiteral _ [] = []
removerLiteral l (x:xs)
  | l == x = removerLiteral l xs
  | otherwise = x : (removerLiteral l xs)

negar :: Literal -> Literal
negar (Not l) = l
negar l = Not l

esUnitaria :: Clausula -> Bool
esUnitaria [x] = True
esUnitaria xs = False

clausulaEnEstado :: Clausula -> Estado -> Bool
clausulaEnEstado c (_, []) = False
clausulaEnEstado c (inter, x:xs)
  | c == x = True
  | otherwise = clausulaEnEstado c (inter, xs)

obtenerNombre :: Literal -> String
obtenerNombre (Var x) = x
obtenerNombre (Not (Var x)) = x

tieneInterpretacion :: String -> Interpretacion -> Bool
tieneInterpretacion _ [] = False
tieneInterpretacion x ((y,b):ys) = if x == y
                                   then True
                                   else tieneInterpretacion x ys

obtenerLiteral :: Clausula -> Literal
obtenerLiteral [x] = x
obtenerLiteral xs = Var "foo"

darValor :: Clausula -> Interpretacion
darValor [Var p] = [(p, True)]
darValor [Not (Var p)] = [(p, False)]

acumularClausula :: Estado -> Estado -> Estado
acumularClausula (_ , xs) (l2 , ys) = (l2, xs ++ ys)

acumularModelo :: Estado -> Estado -> Estado
acumularModelo (m1, _) (m2, ys) = (m1 ++ m2, ys)

segundoElemto :: (a , b) -> b
segundoElemto (_ , b) = b

construirArbolDPLL :: Estado -> ArbolDPLL
construirArbolDPLL estado
  | conflict estado = Node estado Void
  | success estado = Node estado Void
  | segundoElemto propuesto /=  segundoElemto estado = Node estado (construirArbolDPLL propuesto)
  | otherwise = Branch estado (construirArbolDPLL izq) (construirArbolDPLL der)
  where
    propuesto = red (elim(unit estado))
    literal = heuristicsLiteral (segundoElemto estado)
    (izq, der) = sep literal estado 

explorarArbolDPLL :: ArbolDPLL -> Estado
explorarArbolDPLL (Node estado Void) = estado
explorarArbolDPLL (Node _ subarbol) = explorarArbolDPLL subarbol
explorarArbolDPLL (Branch estado izq der) = if conflict x
                                               then explorarArbolDPLL der
                                               else x
                                            where
                                              x = explorarArbolDPLL izq

-- Cuenta apariciones
contar :: [Literal] -> [(Literal, Int)]
contar [] = []
contar (x:xs) = (x, 1 + contarLit x xs) : contar (filter (/= x) xs)

contarLit :: Literal -> [Literal] -> Int
contarLit _ [] = 0
contarLit l (x:xs)
  | l == x = 1 + contarLit l xs
  | otherwise = contarLit l xs

-- Obtener el máximo
maxApariciones :: [(Literal, Int)] -> (Literal, Int)
maxApariciones [x] = x
maxApariciones (x:xs)
  | snd x >= snd y = x
  | otherwise = y
  where
    y = maxApariciones xs