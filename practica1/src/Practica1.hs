--Ejercicio 1
funcionCuadratica :: Float->Float->Float->Float->Float
funcionCuadratica a b c v  = a*v*v + b*v +c

--Ejercicio 2
hipotenusa :: Float->Float->Float
hipotenusa b h= sqrt(b^2+h^2)

--Ejercicio 3
normaVectorial :: (Float,Float)->Float
normaVectorial (x,y) = sqrt(x^2+y^2)

--Ejercicio 4
comparador :: Float->Float->Int
comparador a b = if a==b then 0
                 else if a>b then 1
                      else -1

--Ejercicio 5
sumaFracciones :: (Int, Int)->(Int, Int)->(Int, Int)
sumaFracciones (a,b) (c,d)= if (b==d) then (a+c,b)
                            else (a+c, b*d)

--Ejercicio 6
productoPunto :: (Float,Float)->(Float,Float)->Float
productoPunto (x1,y1) (x2,y2)=x1*x2+y1*y2

--Ejercicio 7
distanciaPuntos :: (Float,Float)->(Float,Float)->Float
distanciaPuntos (x1,y1) (x2,y2) = sqrt((x1-x2)^2+(y1-y2)^2)

--Ejercicio 8
pendienteRecta :: (Float,Float)->(Float,Float)->Float
pendienteRecta (x1,y1) (x2,y2) = (y2-y1)/(x2-x1)

--Ejercicio 9
raiz1 :: Float->Float->Float->(Float,Float)
raiz1 a b c = if b^2<4*a*c
                 then (-b/(2*a),sqrt(4*a*c-b^2)/2*a)
                 else ((-b+ sqrt(b^2-(4*a*c)))/2*a,0)

raiz2 :: Float->Float->Float->(Float,Float)
raiz2 a b c = if b^2<4*a*c
                 then (-b/(2*a),- sqrt(4*a*c-b^2)/2*a)
                 else ((-b - sqrt(b^2-(4*a*c)))/2*a,0)
                      
raicesCuadraticas :: Float->Float->Float->((Float,Float),(Float,Float))
raicesCuadraticas a b c = ((raiz1 a b c),(raiz2 a b c))

--Ejercicio 10
angulo :: Float->Float
angulo n = (2*pi)/n

apotema :: Float->Float->Float
apotema l n = l/(2*tan((angulo n)/2))

areaBase :: Float->Float->Float
areaBase l n = (l*n*(apotema l n))/2

volumenPiramidal :: Float->Float->Float->Float
volumenPiramidal l n h = (areaBase l n) * h

