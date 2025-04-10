--Othello (Reversi). Manuel Hernández, January 2025.
--En esta versión othello00ab.hs la preferencia es a minimax y su equivalencia
--con la poda alfa-beta.

import System.Random
import Data.List
import Data.Char
import Board

data Element = O | X | E | L deriving (Eq,Show,Read)

type Board = [Element]

data TreeG1 = T1 (Element,Integer,Board,Int,Int) [TreeG1] deriving Show

data TreeG4 = T4 (Element, -- Player
                   [Integer], -- Mov
                   (Int,Int,Int) -- (num Xs, num Os, num Movs) 
                   ) [TreeG4] deriving Show

data MvVal = MvVal {mov::Integer, xs :: Int, os :: Int} deriving Show

--- Parece que aquí se evalúa el tablero...
--- Si el tablero completo estuviera aquí como argumento, puedo saber
--- qué piezas tiene la X y la O.
--- Además, si el  número de movimientos para X también estuviera aquí,
--- o el de O, lo puedeo integrar para tener una función de evaluación
--- que buscara inmovilizar o llegado el caso, que el oponente no tenga
--- más movimientos.

lcoords = concat [[(x+1)..(x+8)]| x<-[10,20..80]]

coords pos = zipWith (\x y -> (x,y)) lcoords pos

expandx pos ((ini,X),(cand, O)) = 
        let (delta,av) = (cand-ini,cand+delta)
            es = findCoords av (coords pos)
            in if null es then [] else 
                 let h = snd (head es) in
                    case h of
		     L -> []
                     X -> []
                     E -> [(cand,O),(av,E)]
                     O ->  let rest = expandoX pos delta (av, O) in
                             if null rest then [] else (cand, O):rest 
                    
expandoX pos delta (av, O) = 
             let av1 = av + delta
                 es = findCoords av1 (coords pos)
                  in if null es then [] else
                    let h = snd (head es) in 
                      case h of
		      	L -> []   
                        X -> [] 
                        E -> [(av, O),(av1, E)]
                        O -> let rest= expandoX pos delta (av1, O) in
                               if null rest then [] else (av, O):rest

expando pos ((ini,O),(cand, X)) = 
        let
           (delta,av) = (cand-ini,cand+delta)
           es = findCoords av (coords pos)
           in 
            if null es  then [] else 
                 let h = snd (head es) in
                    case h of
		     L -> []
                     O -> []
                     E -> [(cand, X),(av, E)]
                     X ->  let rest = expandxO pos delta (av, X) in
                             if null rest then [] else  (cand, X):rest 
                    
expandxO pos delta (av, X) = let
                 av1 = av + delta
                 es = findCoords av1 (coords pos) in
                   if null es then [] else
                        let h = snd (head es) in 
                          case h of
			   L -> []
                           O -> [] 
                           E -> [(av, X),(av1, E)]
                           X -> let rest= expandxO pos delta (av1, X) in
                                    if null rest then [] else  (av, X):rest

allNum player pos = (nub . sort)  (map (fst) (validMoves player pos))

validMoves player pos = map last (allMoves player pos)

allMoves player pos = filter (/=[]) (movs player pos) 

movs player pos = if player==X then 
        map (expandx pos) (concat (onlyNBos X pos))
               else
        map (expando pos) (concat (onlyNBos O pos))

candidates (init, ls) = zip (repeat init) ls

findF e coors = (filter (\x -> (snd x)==e) coors) 

findCoords m coors = (filter (\x -> (fst x) == m) coors) 

neighbs e1 pos (n, e) = ((n, e),(only e1 
                                 (concat 
                                   [findCoords x (coords pos)| x<-dirs n])))
-- Para hallar los vecinos a una ficha X
onlyNBos player pos =  map (candidates . (neighbs (change player) pos)) 
                                     (findF player (coords pos))

only e ls = filter (\x -> (snd x)==e) ls

dirs n | elem n ([22..27]++[32..37]++[42..47]++[52..57]++[62..67]++[72..77])
                         =  map (+n) [-11,-10,-9,-1,1,9,10,11]
       | elem n [12..17] = map (+n) [-1,1,9,10,11]
       | elem n [82..87] = map (+n) [-1,1,-9,-10,-11]
       | elem n [21,31..71] = map (+n) [-10,-9,1,10,11]
       | elem n [28,38..78] = map (+n) [-11,-10,-1,9,10]
       | n==11 = [12,22,21]
       | n==88 = [87,77,78]
       | n==81 = [71,72,82]
       | n==18 = [17,27,28]

numTTT::Int
numTTT= 64

-- Este tablero tiene acceso del 1 al 64

posIni'::[Element]
posIni' =  (take 27 (repeat E))++[X,O,E,E,E,E,E,E,O,X]++(take 27 (repeat E))

posIni'' = foldr (\x -> sustn L x)
       (foldr (\x -> sustn L x) posIni' [1,3,5,7,9]) [57..64]

posIni = foldr (\x -> sustn O x)
       (foldr (\x -> sustn X x) posIni'' [9..16]) [49..56]       

semilla = 1829182918

posIniRandom = let s=take 5 (randomRs (1,64) (mkStdGen semilla) :: [Int])
	     in foldr (\x -> sustn L x) posIni' s

to a = snd (head (filter (\x -> a==fst x) (zip lcoords [1..64])))

maxV (MvVal n1 a1 b1) (MvVal n2 a2 b2) | a1<=a2 = MvVal n2 a2 b2
                                       | otherwise = MvVal n1 a1 b1

-- -------------------------- Presentar el tablero (posible módulo)
-- showB [] = ""
-- showB (a:bs) = (show a)++"\n"++(showB bs)
 
-- showBoard pos = putStr (" _ _ _ _ _ _ _ _ \n"
--                 ++[xchange x|x<-(showB (part8 pos))])

-- xchange x | x==',' = '|' 
--           | x=='E' = '_'
--           | otherwise = x
-- -----------------------------------	  

count player pos = length (filter (==player) pos)

validCoord player pos = nub (strip (validMoves player pos))

strip [] = []
strip ((n, x):ls) = fst (n, x):strip ls
------------------------------begin wrt X---------------------
allVBasic player g pos = 
           T1 (player,g,pos,count X pos,count O pos) ls
            where 
            ls = [T1 (mMoveVirtual player k pos) [] | k <- (allNum player pos)]

barrer player (T1 (p,m,pos,n1,n2) []) = allVBasic player m pos
barrer player (T1 (p,m,pos,n1,n2) (c:cs)) = 
           T1 (player,m,pos,count X pos,count O pos) 
                 (map (barrer (change player)) (c:cs))

genTree player pos n = take n (iterate (barrer player) (T1 (X,0,pos,2,2) []))
--Realmente sólo se utilizan jugador=X y posición=pos

mMoveVirtual player n pos = 
            let newpos = (applyMove 
                    (apply player (nub (concat 
                      (filter (\x->fst (head x)==n) 
                        (map reverse 
                               (allMoves player pos)))))) pos)
             in (player, n, newpos, count X newpos, count O newpos)
--Dato: (jugador,movimiento,posición,cuantosX,cuantosO)

sortby [] = []
sortby ((a1,b1):bs) = sortby [x | x<- bs, snd x < b1] 
                      ++ [(a1,b1)] ++ 
                      sortby [x | x<- bs, snd x >= b1] 

-- Static evaluation is so simple! Just counting... Wrong...
-- We need to take into account what positions are owned (specially,
-- corners)

newPos player n pos = 
            applyMove
                    (apply player (nub (concat 
                      (filter (\x->fst (head x)==n) 
                        (map reverse 
                               (allMoves player pos)))))) pos
mMvVirtual player n pos = 
            let newpos = (applyMove
                    (apply player (nub (concat 
                      (filter (\x->fst (head x)==n) 
                        (map reverse 
                               (allMoves player pos)))))) pos)
                          in MvVal n (count X newpos) (count O newpos)

transTree (T1 (n,mov,pos,xs,os) []) = T4 (n,[mov],(xs-os,os,nm)) []
                                     where
                                       nm = length (allNum n pos)
transTree (T1 (n,mov,pos,xs,os) (a:bs)) = T4 (n,[mov],(xs,os,nm)) ls 
                             where 
                               ls =  (map transTree (a:bs))
                               nm = length (allNum n pos)

abminimax alpha beta 
     (T4 (n,[mov],(valueOfxs,valueOfos,nm)) [])
        | nm==0 = ([mov],max alpha (min (-70) beta))
        | nm>0 = ([mov],max alpha (min (-valueOfxs) beta))
abminimax alpha beta 
     (T4 (n,[mov],(valueOfxs,valueOfos,nm)) (b:bs))
      = cmx mov alpha beta (b:bs)


cmx mov alpha beta []= ([mov],alpha)
cmx mov alpha beta (b:bs) = 
      (mov:mv,
       max alpha' (min ((snd (negP (minList' (map minimax bs))))) beta))
         where
         (mv,alpha')=(fst (negP (minP (minimax b) (minList' (map minimax bs)))),
                      snd (negP (abminimax (-beta) (-alpha) b)))


minimax (T4 (n,[mov],(valueOfxs,valueOfos,nm)) [])  
                        | nm==0 = ([mov],-70) -- -70 o 70?
minimax (T4 (n,[mov],(valueOfxs,valueOfos,nm)) [])
                        | nm>0 = ([mov],-valueOfxs) -- -valueOfxs 
minimax (T4 (n,[mov],(valueOfxs,valueOfos,nm)) (a:bs)) = (mov:mvT,val)
           where
             (mvT,val) = (negP (minList' (map minimax (a:bs))))

test n = (test1 n,test2 n)

test1 n= bestMv X posIni0 n       

test2 n= bestMv' X posIni0 n        

app ls (ms,t) = (ls++ms,t)
negP (a,b) = (a,-b)

bestMv player pos level = 
               minimax (transTree (last (genTree player pos level)))
bestMv' player pos level = 
               abminimax (-80) 80 (transTree (last (genTree player pos level)))

minList ls = foldr (min) (1000) ls
minP (a1,b1) (a2,b2) = if b1<b2 then (a1,b1) else (a2,b2)
val (a,b) = b
maxP (a1,b1) (a2,b2) = if b1<b2 then (a2,b2) else (a1,b1)
minList' ls = foldr (minP) ([],1000) ls

mMove player n pos = showBoard (applyMove
                  (apply player (nub (concat 
                      (filter (\x->fst (head x)==n) 
                        (map reverse 
                               (allMoves player pos)))))) pos)
---------------------------end wrt X-----------------------------
apply player [] = []
apply player ((n,e):ls) = (n,player):apply player ls 

applyMove [] pos = pos
applyMove ((n,player):ls) pos = applyMove ls (sustn player (to n) pos)     

sustn :: (Num a, Ord a) => b -> a -> [b] -> [b]
sustn a 1 (c:cs) = (a:cs) 
sustn a n (c:cs) | n>1 =  c:(sustn a (n-1) cs)
--  _ _ _ _ _ _ _ _ 
-- [_|_|_|_|_|_|_|_]
-- [_|_|_|_|_|_|_|_]
-- [_|_|O|_|_|_|_|_]
-- [_|_|_|O|O|_|_|_]
-- [_|_|_|X|O|X|_|_]
-- [_|_|_|X|X|O|X|_]
-- [_|_|_|_|_|X|O|_]
-- [_|_|_|_|_|_|_|_]

posIni0 = [E,E,E,E,E,E,E,E,
   E,E,E,E,E,E,E,E,
   E,E,O,E,E,E,E,E,
   E,E,E,O,O,E,E,E,
   E,E,E,X,O,X,E,E,
   E,E,E,X,X,O,X,E,
   E,E,E,E,E,X,O,E,
   E,E,E,E,E,E,E,E]

change X = O
change O = X

------Ahora ya juega muy bien :) (xs - os)

calcMov :: Element -> Board -> Int -> IO()
calcMov pieza pos depth = do
        putStrLn $ "Pensando..." 
        --Report winner..., missing
        let ls=(tail (fst (bestMv' pieza pos depth)))
	-- bestMv'=minimaxPodaAlfaBeta, bestMv=minimax
        let bm=head ls 
            newpos1 = newPos pieza bm pos
        mMove pieza bm  pos
        putStr $ show bm
        putStr $ "\n"
        putStrLn $ "Fichas negras: " ++ (show (count pieza newpos1))
        putStrLn $ "Fichas blancas: " ++ (show (count (change pieza) newpos1))
        putStrLn $ show (allNum (change pieza) newpos1)
        putStrLn $ "Tienes "++(show (length (allNum (change pieza) newpos1)))++" movs"       
--      putStrLn $ if length (allNum (change pieza) newpos1) == 0 then
--                            (show (allNum (change pieza) newpos1))++"\n"
--                            else "You lost!"
                              --Revisión: You lost if you don't have any
                 --- more moves. Here it should be a break.*/
        putStr "Tu movimiento: "
        input <- getLine
        let square = (read input) :: Integer
--        putStr (show square)
        let 
          newpos2 = newPos (change pieza) square newpos1
        mMove (change pieza) square newpos1  
        putStrLn $ "Fichas negras: "++ (show (count pieza newpos2))                
        putStrLn $ "Fichas blancas: "++ (show (count (change pieza) newpos2))                
        calcMov pieza (newPos (change pieza) square newpos2) depth         

main =
     do
	putStrLn "Quieres jugar contra X o contra O?"
     	input <- getLine
     	let pieza = (read input) :: Element
	putStrLn "Qué tan difícl me quieres?"
	putStrLn "0:Ratón, 1:Gato, 2:Perro, 3:Leon, 4:Tiburon" 
     	input <- getLine
     	let depth = (read input) :: Int
	-- (aceptar entradas correctas)
     	calcMov pieza posIniRandom (depth+2)
	---caso aleatorio
	
--     	calcMov pieza posIni (depth+2)
	-- caso normal

--Variants: 
--a) Three players (or more)
--b) (Previously) (Random) Scattering pieces over the board
--c) Boards with obstacles (squares, or diamonds, for example) 
--d) Boards with distinct geometrical forms. 
--e) Boards with distinct square geometry (torus, cylinder, Moebius strip).
--f) Random static token
--g) Factor number betrayed
--h) ¿Dimensions?
--i) Special turns (like to put a token over an arbitrary square)

--Nota 1: Hay dos funciones para calcular el mejor movimiento: minimax y abminimax
-- Nota 2: uso de random en general
-- Nota 3: Tableros con casillas muertas aleatorias
-- Nota 4: Solicitar número de casillas muertas y probable semilla
-- Nota 5: Notar cómo creamos un módulo Board y hacer otros módulos.