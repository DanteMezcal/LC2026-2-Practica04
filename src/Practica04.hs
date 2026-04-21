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
unit (modelo , []) = (modelo , [])
unit (modelo , c:xs) = if esUnitaria c
                          then if tieneInterpretacion (obtenerNombre (obtenerLiteral c)) modelo
                                  then acumularClausula ([], [c]) (unit (modelo , xs))
                                  else (modelo ++ darValor c, xs)
                         else acumularClausula ([], [c]) (unit (modelo , xs))

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
sep = undefined

--IMPLEMENTACION PARTE 2


--Ejercicio 1
heuristicsLiteral :: [Clausula] -> Literal
heuristicsLiteral = undefined

--EJERCICIO 2
dpll :: [Clausula] -> Interpretacion
dpll = undefined

--EXTRA
dpll2 :: Prop -> Interpretacion
dpll2 = undefined

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
  | Not l `elem` c = removerLiteral (Not l) c : (auxiliarRed l cs)
  | otherwise = c : (auxiliarRed l cs)

removerLiteral :: Literal -> Clausula -> Clausula
removerLiteral _ [] = []
removerLiteral l (x:xs)
  | l == x = removerLiteral l xs
  | otherwise = x : (removerLiteral l xs)

esUnitaria :: Clausula -> Bool
esUnitaria [x] = True
esUnitaria xs = False

clausulaEnEstado :: Clausula -> Estado -> Bool
clausulaEnEstado c (inter, x:xs)
  | c == x = True
  | [] == xs = False
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
darValor [Var p] = [("p", True)]
darValor [Not (Var p)] = [("p", False)]

acumularClausula :: Estado -> Estado -> Estado
acumularClausula (_ , xs) (l2 , ys) = (l2, xs ++ ys)

acumularModelo :: Estado -> Estado -> Estado
acumularModelo (m1, _) (m2, ys) = (m1 ++ m2, ys)

segundoElemto :: (a , b) -> b
segundoElemto (_ , b) = b

clausulas :: Prop -> [Clausula]
clausulas (Var p) = [ [Var p] ]
clausulas (Not (Var p)) = [ [Not (Var p)] ] --porque en FNC las negaciones aparecen en frente de literales
clausulas (And p q) = (clausulas p ++ clausulas q)
clausulas (Or p q) = [litInOr (Or p q)]

--construirArbolDPLL :: Estado -> ArbolDPLL
--construirArbolDPLL estado
--  | conflict estado = Node estado Void
--  | success estado = Nodo estado void
--  | segundoElemento propuesto /=  segundoElemento estado = Nodo estado (construirArbolDPLL propuesto)
--  otherwise = Branch estado (construirArbolDPLL izq) (construirArbolDPLL der)
--  where
--    propuesto = red (elim(unit estado))
--    literal = heuristicsLiteral (segundoElemento estado)
--    (izq, der) = sep literal estado 

explorarArbolDPLL :: ArbolDPLL -> Estado
explorarArbolDPLL (Node estado Void) = estado
explorarArbolDPLL (Node _ subarbol) = explorarArbolDPLL subarbol
explorarArbolDPLL (Branch estado izq der) = if conflict x
                                               then explorarArbolDPLL der
                                               else x
                                            where
                                              x = explorarArbolDPLL izq
