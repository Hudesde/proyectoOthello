module Board (showB, showBoard) where

showB [] = ""
showB (a:bs) = (show a)++"\n"++(showB bs)
 
showBoard pos = putStr (" _ _ _ _ _ _ _ _ \n"
                ++[xchange x|x<-(showB (part8 pos))])
part8 [] = []
part8 (a:bs) = (take 8 (a:bs)):(part8 (drop 8 (a:bs)))

xchange x | x==',' = '|' 
          | x=='E' = '_'
          | otherwise = x
