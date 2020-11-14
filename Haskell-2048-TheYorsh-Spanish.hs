---------------------HASKELL-2048-DEYORS-ENERO 2020---------------------
--------------------------------JUEGO 2048--------------------------------

{-Instrucciones: Utiliza las teclas WASD para mover los números de tu 
tablero en la dirección correspondiente. Si hay números adyacentes en esa 
dirección que sean iguales se sumarán y darán lugar a un número más 
grande. ¡El objetivo es conseguir sumar 2048! Buena suerte.".-}

--PROGRAMACIÓN:

import Data.List (transpose) --Necesitaremos calcular la matriz traspuesta
import System.Random --Para generar números aleatorios

data Jugada = Arriba | Abajo | Izquierda | Derecha
type Tablero = [[Int]]

--------------------------MOSTRAR TABLERO EN PANTALLA---------------------

{- La primera necesidad es, utilizando IO para poder incorporarlo en el 
programa central, mostrar en pantalla un tablero. Para ello: -}

--Mostramos un elemento:

muestraElemento :: Int -> String
muestraElemento n = show n

{- Damos dos formas de representar un tablero debido a que a veces jugar
con mucho diseño en la pantalla puede resultar agotador.-}

--Mostramos una fila:

muestraFila :: [Int] -> String
muestraFila [] = "|"
muestraFila (x:xs)
     | (long == 1) = "|    " ++ mE ++ "   " ++ mF
     | (long == 2) = "|   " ++ mE ++ "   " ++ mF
     | (long == 3) = "|   " ++ mE ++ "  " ++ mF
     | (long == 4) = "|  " ++ mE ++ "  " ++ mF
         where long = length (muestraElemento x)
               mE = muestraElemento x
               mF = muestraFila xs

--(Tenemos en cuenta la longitud del elemento para que queden centrados)

muestraFila2 :: [Int] -> String
muestraFila2 [] = " "
muestraFila2 (x:xs)
     | (long == 1) = "     " ++ mE ++ "   " ++ mF
     | (long == 2) = "    " ++ mE ++ "   " ++ mF
     | (long == 3) = "    " ++ mE ++ "  " ++ mF
     | (long == 4) = "   " ++ mE ++ "  " ++ mF
         where long = length (muestraElemento x)
               mE = muestraElemento x
               mF = muestraFila2 xs

--Y ahora el tablero entero:

{- Con cubrearriba y barras lo que se intenta es encapsular por arriba 
y por abajo con "-" y "+"-}

barras :: Int -> Char -> String
barras 0 _ = ""
barras n ch = ch:(barras (n-1) ch)

cubreArriba :: Int -> String
cubreArriba 0 = "+"
cubreArriba n = "+" ++ (barras 8 '-') ++ (cubreArriba (n-1))

muestraTablero :: Tablero -> IO ()
muestraTablero [x] = do 
         putStrLn $ cubreArriba (length x)
         putStrLn (muestraFila x)
         putStrLn $ cubreArriba (length x)
muestraTablero (x:xs) = do
         putStrLn (cubreArriba (length x))
         putStrLn (muestraFila x)
         muestraTablero (xs)

muestraTablero2 :: Tablero -> IO ()
muestraTablero2 [x] = do 
         putStrLn "   "
         putStrLn (muestraFila2 x)
         putStrLn "   "
muestraTablero2 (x:xs) = do
         putStrLn "   "
         putStrLn (muestraFila2 x)
         muestraTablero2 (xs)

--(muestraTablero es la versión con diseño y muestraTablero2 la simple)

-------------------------REACCIÓN A MOVIMIENTO----------------------------

{-El objetivo es, dado un tablero, que al aplicarle un movimiento WASD,
traducido en un elemento del tipo Move, tengamos otro tablero de vuelta
con los elementos desplazados en la dirección que tengamos.-}

{-Empezaremos suponiendo que queremos mover todo el tablero a la izquierda
y nos centraremos solo en una fila. Esa fila será una lista donde todos 
los elementos adyacentes o separados por un cero se junten y se desplacen
a la izquierda, quedando huecos a la derecha que deberán rellenarse con 
ceros.-}

{- juntaAIzquierda, dada una lista, nos añadirá recursivamente a otra 
lista la suma de los elementos adyacentes o separados por cero, junto
a los elementos que no han podido ser sumados, de forma ordenada.-}

juntaAIzquierda :: [Int] -> [Int]
juntaAIzquierda [] = []
juntaAIzquierda [x] = [x]
juntaAIzquierda (x:y:xs)
     | (x == 0) = juntaAIzquierda(y:xs)
     | (y == 0) = juntaAIzquierda(x:xs)
     | (x == y) = (x + y):juntaAIzquierda(xs)
     | otherwise = x:juntaAIzquierda(y:xs)

{- aIzquierda simplemente nos devolverá los elementos de juntaAIzquierda
pero añadiendo ceros a la derecha para que el tamaño de la lista no
varíe.-}

aIzquierda :: [Int] -> [Int]
aIzquierda xs = (juntaAIzquierda xs) ++ ceros
     where ceros = replicate (length xs - length (juntaAIzquierda xs)) 0

{- Ahora toca generalizar con todos los movimientos. Haremos una función
que, dado un elemento de tipo Jugada, devuelva una función que acepte 
un Tablero y devuelva ese Tablero afectado por el movimiento.

Mover hacia la derecha es como dar la vuelta a la fila, moverlo todo
hacia la izquierda, y despues volver a darle la vuelta.

Para mover hacia arriba o hacia abajo cogemos el Tablero entero y 
hacemos la traspuesta de esa matriz. Cada columna se convertira en fila, 
por lo tanto usando movimiento a la derecha es como si hicieramos el 
movimiento arriba, y lo mismo con izquierda y abajo.-}

muevo :: Jugada -> Tablero -> Tablero
muevo Izquierda = map aIzquierda
muevo Derecha = map (reverse . aIzquierda . reverse)
muevo Arriba = transpose . muevo Izquierda . transpose
muevo Abajo = transpose . muevo Derecha . transpose

{- Ahora toca relacionar las teclas WASD con cada movimiento (no
distinguiremos entre mayúscula o minúscula). La siguiente función acepta
estos carácteres y los aplica al tablero, y en caso de pulsar por 
equivocación otro comando devolverá el tablero incial.-}

movimientoTablero :: Char -> Tablero -> Tablero
movimientoTablero x tab
     |(x `elem` "wW") = muevo Arriba tab
     |(x `elem` "aA") = muevo Izquierda tab
     |(x `elem` "sS") = muevo Abajo tab
     |(x `elem` "dD") = muevo Derecha tab
     |otherwise = tab

------------------------AÑADIR ELEMENTOS AL TABLERO-----------------------

{-Tras cada jugada el tablero se actualiza no solo con nuestro movimiento,
sino que se le añaden dos números (doses o cuatros) de forma aleatoria
en posiciones del tablero donde haya ceros.-}


-- 1. COORDENADAS: 

{-Primero necesitamos un mapa de todas las coordenadas del tablero, 
en forma de duplas (fila,columna), según la dimensión del tablero-}

{- filaUnica, dada la dimension del Tablero y un número, produce
una lista de longitud la dimensión, y el número anterior al número 
insertado.-}

filaUnica :: Int -> Int -> [Int]
filaUnica dim x = replicate dim (x-1)

{- listaDesde, dado un número, produce una lista desde ese número pasando
por todos los enteros anteriores, hasta el cero-}

listaDesde :: Int -> [Int]
listaDesde 0 = [0]
listaDesde x = (x):listaDesde(x-1)

{- listaHasta, dado un número, produce una lista desde el cero con 
todos los enteros hasta el anterior a ese número.-}

listaHasta :: Int -> [Int]
listaHasta x = reverse (tail (listaDesde x))

{- coord1 es una funcion auxiliar que dada la dimension del tablero y un 
número, produce todas las posibles combinaciones de coordenadas del
tablero que empiecen por numeros desde el cero hasta el anterior a ese
número-}

coord1 :: Int -> Int -> [(Int,Int)]
coord1 dim 1 = zip (filaUnica dim 1) (listaHasta dim)
coord1 dim x = coordIniciales ++ coordFinales
     where coordFinales = zip (filaUnica dim x) (listaHasta dim)
           coordIniciales = coord1 dim (x-1)

{- coordenadas, dado un tablero, produce una lista de duplas
con todas las coordenadas posibles del tablero, en orden.-}

coordenadas :: Tablero -> [(Int,Int)]
coordenadas tab = coord1 (length tab) (length tab)

{- Añadiremos números aleatorios sobre números del tablero que sean cero, 
por lo tanto tendremos que identificar las coordenadas de dichos
números:-}

dameCeros :: Tablero -> [(Int, Int)]
dameCeros tab = filter filtraCero (coordenadas tab)
     where filtraCero (fila,col) = (tab!!fila)!!col == 0

{- cambiaValor es una función que dado un tablero, unas coordendas y un
valor, es capaz de cambiar el valor antiguo de ese elemento por el 
nuevo-}

cambiaValor :: Tablero -> (Int,Int) -> Int -> Tablero
cambiaValor tab (fila,col) val = izq ++ [medio] ++ der
     where izq = take fila tab
           medio = izq1 ++ [val] ++ der1
                 where izq1 = take col (tab!!fila)
                       der1 = drop (col + 1) (tab!!fila)
           der = drop (fila + 1) tab

{-(Aparta primero las filas para centrarse en la fila de la coordenda, 
y luego en esa fila aparta los elementos a un lado y otro de la
coordenada para centrarse en el elemento que se va a cambiar)-}

-- 2. AÑADIR NÚMEROS ALEATORIOS:

{- Para generar números aleatorios usaremos "elige". A esta función le das
una lista con elementos y te devuelve un elemento al azar de dicha lista, 
pero dentro de la mónada IO. 

Esto se consigue gracias a randomRIO, una funcion de System.Random que 
toma un numero mínimo y máximo y te devuelve un número aleatorio entre 
esos números utilizando la semilla global de números aleatorios del 
ordenador. Esta semilla se genera automáticamente con datos aparentemente 
aleatorios como la hora, tiempo que lleva encendido el ordenador, teclas 
pulsadas, etc.-}

elige :: [a] -> IO a
elige xs = do
    i <- randomRIO (0, length xs-1)
    return (xs !! i)

-- 3. METER NÚMEROS ALEATORIOS EN CEROS DEL TABLERO:

{-La función meteAleatorio toma un tablero, busca todos los ceros y los 
mete en una lista, coge un elemento al azar de esa lista, coge otro 
elemento al azar de una lista de doses y cuatros y sustituye ese cero
por el valor escogido.-}

meteAleatorio :: Tablero -> IO Tablero
meteAleatorio tab = do
    let candidatos = dameCeros tab
    candidatoQueQuiero <- elige candidatos
    val  <- elige [2,2,2,2,2,2,2,2,4,4]
    let nuevoTab = cambiaValor tab candidatoQueQuiero val
    return nuevoTab

{-Normalmente un cuatro suele fastidiar y hacer mas tedioso juntar
números, asi que metemos mas doses que cuatros.-}

-----------------------CUANDO GANAMOS O PERDEMOS--------------------------

{-heGanado da True si algun elemento del tablero es 2048. Lo hacemos
sacando todos los elementos del tablero con concat y filtrando.-}

heGanado :: Tablero -> Bool
heGanado tab
     |(filter (== 2048) (concat tab) == []) = False
     |otherwise = True

{-hePerdido da True cuando todos los movimientos del Tablero dejan a este
como esta, es decir, no quedan mas ceros ni movimientos disponibles-}

hePerdido :: Tablero -> Bool
hePerdido tab
     |(w && a && s && d) = True
     |otherwise = False
         where w = (muevo Arriba tab == tab)
               a = (muevo Izquierda tab == tab)
               s = (muevo Abajo tab == tab)
               d = (muevo Derecha tab == tab)

---------------------------MAIN Y EL JUEGO EN SÍ--------------------------

{-Creamos el primer tablero, metiendo dos números al azar a un tablero 
vacío.-}

tableroVacio :: Int -> Tablero
tableroVacio dim = replicate dim (replicate dim 0)

primerTablero :: Int -> IO Tablero
primerTablero dim = do
     tab' <- meteAleatorio (tableroVacio dim)
     meteAleatorio tab'

{-siguienteTablero hace que, dado un tablero, te pida un carácter
y con ese carácter actualizar el tablero segun la direccion que has 
insertado.-}

siguienteTablero :: Tablero -> IO Tablero
siguienteTablero tab = do
     ch <- getChar
     if ch `elem` "mM" then error "Has salido del juego"
         else do 
             let nuevoTab = movimientoTablero ch tab
             return nuevoTab

{-Según la modalidad elegida (sencillo o con diseño), haremos dos juegos,
y en cada uno se aceptará el tablero, se comprobará si se ha ganado 
o perdido, se pedirá una dirección y se cambiará el tablero
correspondiente.-}

juego :: Tablero -> IO ()
juego tab
     |heGanado tab = do
             muestraTablero tab
             putStrLn "HAS GANADO!"
     |hePerdido tab = do
             muestraTablero tab
             putStrLn "El juego ha acabado, has perdido"
     |otherwise = do
             putStrLn "\n"
             muestraTablero tab
             nuevoTab <- siguienteTablero tab
             if tab /= nuevoTab then do
                                     nuevoTab2 <- meteAleatorio nuevoTab
                                     juego nuevoTab2
                                else do
                                     putStrLn frase
                                     juego tab
     where frase = "Prueba con otra tecla, el tablero queda igual"

juego2 :: Tablero -> IO ()
juego2 tab
     |heGanado tab = do
             muestraTablero2 tab
             putStrLn "HAS GANADO!"
     |hePerdido tab = do
             muestraTablero2 tab
             putStrLn "El juego ha acabado, has perdido"
     |otherwise = do
             putStrLn "\n"
             muestraTablero2 tab
             nuevoTab <- siguienteTablero tab
             if tab /= nuevoTab then do
                                     nuevoTab2 <- meteAleatorio nuevoTab
                                     juego2 nuevoTab2
                                else do
                                     putStrLn frase
                                     juego2 tab
     where frase = "\nPrueba con otra tecla, el tablero queda igual"

{- Creamos una funcion auxiliar que pregunte la dimensión del tablero 
y segun la respuesta cree un primerTablero de esa dimensión-}


charAInt :: Char -> Int
charAInt ch = (read [ch] :: Int)


preguntaDim :: IO Tablero
preguntaDim = do
     putStrLn "\nElige la dimension del tablero:"
     putStrLn "Dimension recomendada: 4"
     ch <- getChar
     let dim = (charAInt ch)
     putStrLn ("\nCargando tablero de dimension " ++ [ch] ++ ":")
     primerTablero dim

{- Creamos una función auxiliar que se encargue de llevar el juego,
preguntándonos la dimensión del tablero, si lo queremos sencillo 
o con margenes y empezando a jugar.-}

jugar :: IO ()
jugar = do
     putStrLn (frase1 ++ frase2)
     ch <- getChar
     if ch `elem` "sS" then do
                         tab <- preguntaDim
                         juego2 tab
     else do
         tab <- preguntaDim
         juego tab

     where frase1 = "\nPulsa S si quieres que el tablero sea sencillo"
           frase2 = ", u otra letra si quieres el juego con margenes"

{- Creamos una función auxiliar que se encargue de las
instrucciones-}

instrucciones :: IO ()
instrucciones = do
     putStrLn "\nUtiliza las teclas WASD para mover los numeros de tu "
     putStrLn "tablero en la direccion correspondiente. Si hay numeros "
     putStrLn "adyacentes en esa direccion que sean iguales se sumaran y "
     putStrLn "daran lugar a un numero mas grande. El objetivo es "
     putStrLn "conseguir sumar 2048! Buena suerte."
     putStrLn "Pulsa S para jugar"
     ch <- getChar
     if ch `elem` "sS" then do jugar else return ()

{- Creamos una función auxiliar que se encargue de las pruebas-}

tabInmediato :: IO Tablero
tabInmediato = return ([[8,1024,4],[1024,1024,1024],[16,1024,16]])

tabImposible :: IO Tablero
tabImposible = return ([[4,32,8,32],[16,64,4,16],[8,2,0,32],[16,32,8,16]])

pruebas :: IO ()
pruebas = do 
     putStrLn "\nPulsa A si quieres resolver un tablero inmediato"
     putStrLn "Pulsa B si quieres resolver un tablero imposible"
     ch <- getChar
     if ch `elem` "aA" then do
                         tab <- tabInmediato
                         juego tab
                       else do
                         if ch `elem` "bB" then do
                                             tab <- tabImposible
                                             juego tab
                                           else return ()

{- Creamos una funcion auxiliar que se encargue de filtrar las distintas
modalidades del juego-}

filtro :: Char -> IO ()
filtro ch
     |ch `elem` "sS" = do jugar 
     |ch `elem` "iI" = do instrucciones
     |ch `elem` "pP" = do pruebas
     |otherwise = return ()

{-Y ahora llega el turno de la función central, donde se dará información
de las modalidades del juego y se pedirá una respuesta, aplicando la 
funcion filtro para redirigir hacia las funciones auxiliares-}

main :: IO ()
main = do
     putStrLn "Bienvenido a 2048."
     putStrLn "Programado por DEYORS."
     putStrLn "Enero de 2020."
     putStrLn "Pulsa S para jugar"
     putStrLn "Pulsa I para las instrucciones"
     putStrLn "Pulsa P para entrar en el modo de Prueba"
     putStrLn "Pulsa M en cualquier momento para salir"
     ch <- getChar
     filtro ch
