data Var = A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z deriving (Show, Eq, Ord)

data Formula = Prop Var
              | Neg Formula
              | Formula :&: Formula
              | Formula :|:Formula
              | Formula :=>: Formula
              | Formula :<=>: Formula deriving (Show, Eq, Ord)
infixl 9 :&:
infixl 9 :|:
infixl 7 :=>:
infixl 8 :<=>:

-- Ejercicio 1: Negación
negar :: Formula -> Formula
negar (Prop p) = Neg (Prop p)
negar (Neg p) = p
negar (p :&: q) = (negar p) :|: (negar q)
negar (p :|: q) = (negar p) :&: (negar q)
negar (p :=>: q) = (p) :&: (negar q)
negar (p :<=>: q) = negar ((p :=>: q) :&: (q :=>: p))

-- Ejercicio 2: Variables de la formúla
quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort (menores x xs) ++ [x] ++ quickSort (mayores x xs)

menores :: Ord a => a -> [a] -> [a]
menores x [] = []
menores x (y:ys) = if y < x
                    then [y] ++ (menores x ys)
                    else menores x ys

mayores :: Ord a => a -> [a] -> [a]
mayores x [] = []
mayores x (y:ys) = if y >= x
                    then [y] ++ (mayores x ys)
                    else mayores x ys

uni :: Eq a => [a] -> [a]
uni [x] = [x]
uni (x:(y:ys)) = if (x==y)
                then uni (y:ys)
                else [x] ++ uni (y:ys)

variables :: Formula -> [Var]
variables (Prop p) = [p]
variables (Neg p) = variables p
variables (p :&: q) = uni(quickSort(variables p ++ variables q))
variables (p :|: q) = uni(quickSort(variables p ++ variables q))
variables (p :=>: q) = uni(quickSort(variables p ++ variables q))
variables (p :<=>: q) = uni(quickSort(variables p ++ variables q))

-- Ejercicio 3: Equivalencia
equivalencia :: Formula -> Formula
equivalencia (Prop p) = Prop p
equivalencia (Neg p) = negar p
equivalencia (p :&: q) = equivalencia p :&: equivalencia q
equivalencia (p :|: q) = equivalencia p :|: equivalencia q
equivalencia (p :=>: q) = equivalencia (Neg p) :|: equivalencia q
equivalencia (p :<=>: q) = equivalencia(p :=>: q) :&: equivalencia(q :=>: p)

-- Ejercicio 4. Interpretación
valor :: Var -> [(Var,Bool)] -> Bool
valor a [(x,b)] = b
valor a ((x,b):xs) = if (a == x)
                      then b
                      else valor a xs

non :: Bool -> Bool
non True = False
non False = True

conjuncion :: Bool -> Bool -> Bool
conjuncion True True = True
conjuncion x y = False

disyuncion :: Bool -> Bool -> Bool
disyuncion True y = True
disyuncion x True = True
disyuncion x y = False

condicional :: Bool -> Bool -> Bool
condicional False x = True
condicional True True = True
condicional True False = False

bicondicional :: Bool -> Bool -> Bool
bicondicional True True = True
bicondicional False False = True
bicondicional x y = False

interpretacion :: Formula -> [(Var,Bool)] -> Bool
interpretacion (Prop p) xs = valor p xs
interpretacion (Neg p) xs = non (interpretacion p xs)
interpretacion (p :&: q) xs = conjuncion (interpretacion p xs) (interpretacion q xs)
interpretacion (p :|: q) xs = disyuncion (interpretacion p xs) (interpretacion q xs)
interpretacion (p :=>: q) xs = condicional (interpretacion p xs) (interpretacion q xs)
interpretacion (p :<=>: q) xs = bicondicional (interpretacion p xs) (interpretacion q xs)

--Ejercicio 5: Tabla de verdad
combV :: Var -> [[(Var,Bool)]] -> [[(Var,Bool)]]
combV p [] = []
combV p (x:xs) = (((p,True):x):(combV p xs))

combF :: Var -> [[(Var,Bool)]] -> [[(Var,Bool)]]
combF p [] = []
combF p (x:xs) = (((p,False):x):(combF p xs))


tablaVerdadAux :: [Var] -> [[(Var,Bool)]] -> [[(Var,Bool)]]
tablaVerdadAux [] xs = xs
tablaVerdadAux (x:xs) ys = tablaVerdadAux xs ((combV x ys) ++ (combF x ys))

tablaVerdad :: [Var] -> [[(Var,Bool)]]
tablaVerdad [] = []
tablaVerdad (x:xs) = tablaVerdadAux xs [[(x,True)],[(x,False)]]

-- Ejercicio 6: Tautología
interTau :: Formula -> [[(Var,Bool)]] -> Bool
interTau f [] = True
interTau f (x:xs) = if interpretacion f x
                  then interTau f xs
                  else False

tautologia :: Formula -> Bool
tautologia f = interTau f (tablaVerdad (variables f))
