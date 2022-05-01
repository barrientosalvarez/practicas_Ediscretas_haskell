--[1] Maximo comun divisor
mcd::Int->Int->Int
mcd x 0 = x
mcd x y = mcd y (mod x y)


--[2] Minimo comun multiplo

division::Int->Int->[Int]
division x y = [n | n<-[1..x], y*n==x]

elementoDiv::[Int]->Int
elementoDiv (x:xs)=x

mcm::Int->Int->Int
mcm x y = elementoDiv(division (x*y) (mcd x y))


--[3] Longitud de una lista
longitud:: [a]->Int
longitud (x:[])=1
longitud (x:xs)=1+longitud xs


--[4] Maximo de una lista
maximo:: Ord a => [a] -> a
maximo (a:[])= a
maximo (a:as)= if(a>maximo as) then a
                else maximo as

                     
--[5] Reversa de una lista
reversa:: [a]->[a]
reversa (x:[])=[]++[x]
reversa (x:xs)= reversa xs ++[x]


--[6] Palindromo
palindromo:: Eq a =>[a]->Bool
palindromo xs = reversa xs== xs


--[7] Divisores de un entero
divisores:: Int->[Int]
divisores x=[y | y<-[1..x], mod x y ==0]


--[8] Diferencia simétrica

elemento:: Eq a => a->[a]->Bool
elemento x []= False
elemento x (y:ys)= if (x==y) then True
                    else elemento x ys


union:: Eq a=>[a]->[a]->[a]
union xs ys = xs++ [y | y<-ys, elemento y xs == False]


diferencia:: Eq a=>[a]->[a]->[a]
diferencia xs ys= [x | x<-xs, elemento x ys==False]

diferenciaSimetrica:: Eq a=>[a]->[a]->[a]
diferenciaSimetrica xs ys = union (diferencia xs ys) (diferencia ys xs)

 
--[9] Multiplicación de matrices


--[10] conjuntoPotencia
conjuntoPotencia::[a]->[[a]]
conjuntoPotencia []=[[]]
conjuntoPotencia (x:xs) = poner x (conjuntoPotencia xs) ++ (conjuntoPotencia xs)

poner:: a->[[a]]->[[a]]
poner x []=[]
poner x (y:ys)= (x:y) : poner x ys


