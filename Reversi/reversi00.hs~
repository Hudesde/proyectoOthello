--Othello (Reversi). Manuel Hernández, January 2025.
--En esta versión othello00ab.hs la preferencia es a minimax y su equivalencia
--con la poda alfa-beta.

-- import System.Random
import Data.List

data Element = O | X | E | L deriving (Eq,Show)

data TreeG = T Board [TreeG] deriving Show

type Board = [Element]

data TreeG1 = T1 (Element,Integer,Board,Int,Int) [TreeG1] deriving Show

data TreeG3 = T3 (Element, -- Player
                   Integer, -- Mov
                   Board, -- Position
                   (Int,Int,Int) -- (num Xs, num Os, num Movs) 
                   ) [TreeG3] deriving Show

data TreeG4 = T4 (Element, -- Player
                   [Integer], -- Mov
                   (Int,Int,Int) -- (num Xs, num Os, num Movs) 
                   ) [TreeG4] deriving Show

data MvVal = MvVal {mov::Integer, xs :: Int, os :: Int} deriving Show

lcoords = concat [[(x+1)..(x+8)]| x<-[10,20..80]]

coords pos = zipWith (\x y -> (x,y)) lcoords pos

expandx pos ((ini,X),(cand, O)) = 
        let (delta,av) = (cand-ini,cand+delta)
            es = findCoords av (coords pos)
            in if null es then [] else 
                 let h = snd (head es) in
                    case h of 
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

part8 [] = []
part8 (a:bs) = (take 8 (a:bs)):(part8 (drop 8 (a:bs)))

posIni::[Element]
posIni =  (take 27 (repeat E))++[X,O,E,E,E,
                           E,E,E,O,X]++(take 27 (repeat E))

to a = snd (head (filter (\x -> a==fst x) (zip lcoords [1..64])))

maxV (MvVal n1 a1 b1) (MvVal n2 a2 b2) | a1<=a2 = MvVal n2 a2 b2
                                       | otherwise = MvVal n1 a1 b1

showB [] = ""
showB (a:bs) = (show a)++"\n"++(showB bs)
 
showBoard pos = putStr (" _ _ _ _ _ _ _ _ \n"
                ++[xchange x|x<-(showB (part8 pos))])

xchange x | x==',' = '|' 
          | x=='E' = '_'
          | otherwise = x

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


-- step 0':
--cmx mov alpha beta ts = 
--         (mov:(fst (negP (minList' (map minimax ts)))),
--         max alpha
--        (min (snd (negP (minList' (map minimax ts)))) beta))
-- Step 0: 
--cmx mov alpha beta ts = 
--       (mov:mv,max alpha (min x beta))
--          where
--          (mv,x)=negP (minList' (map minimax ts))

-- Step 1:
-- cmx mov alpha beta []= ([mov],alpha)
-- cmx mov alpha beta (b:bs) = 
--       (mov:mv,max alpha (min x beta))
--          where
--          (mv,x)=negP (minList' (map minimax (b:bs)))

-- -- Step 2:
-- cmx mov alpha beta []= ([mov],alpha)
-- cmx mov alpha beta (b:bs) = 
--       (mov:mv,max alpha (min x beta))
--          where
--         (mv,x)=negP (minP (minimax b) (minList' (map minimax bs)))

-- Step 3:

-- cmx mov alpha beta []= ([mov],alpha)
-- cmx mov alpha beta (b:bs) = 
--       (mov:mv,max alpha' (min (-x2) beta))
--          where
--          (mv,x)=negP (minP (mv1,x1) (mv2,x2))
--          (mv1,x1) = minimax b 
--          (mv2,x2) =  (minList' (map minimax bs))
-- --         alpha' = max alpha (min (-x1) beta)
--          (mv0,alpha') = negP (abminimax (-beta) (-alpha) b)



-- Step 4:
--Main> test 5
--(([0,87,88,78,75],0),([0,87,88,78,75],0))

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

calcMov :: Board -> IO()
calcMov pos = do
	putStrLn $ "Pensando..." 
        --Report winner..., missing
        let ls=(tail (fst (bestMv X pos 4)))
        let bm=head ls 
            newpos1 = newPos X bm pos
        mMove X bm  pos
        putStr $ show bm
        putStr $ "\n"
        putStrLn $ "Fichas negras: " ++ (show (count X newpos1))
        putStrLn $ "Fichas blancas: " ++ (show (count O newpos1))
	putStrLn $ show (allNum O newpos1)
	putStrLn $ "Tienes "++(show (length (allNum O newpos1)))++" movs"	
--      putStrLn $ if length (allNum O newpos1) == 0 then
--		      	      (show (allNum O newpos1))++"\n"
--			      else "You lost!"
			      --Revisión: You lost if you don't have any
		 --- more moves. Here it should be a break.*/
        putStr "Tu movimiento: "
        input <- getLine
        let square = (read input) :: Integer
--        putStr (show square)
        let 
          newpos2 = newPos O square newpos1
        mMove O square newpos1  
        putStrLn $ "Fichas negras: "++ (show (count X newpos2))                
        putStrLn $ "Fichas blancas: "++ (show (count O newpos2))                
        calcMov (newPos O square newpos2)          

main = calcMov posIni

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
