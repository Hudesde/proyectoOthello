module Board01 (showB,showBoard,part8,xchange,par) where

showB [] = ""
showB (a:bs) = (show a)++(showB bs)

par [] = []
par (a:bs) = a:a:(par bs)

showBoard pos = putStr (" _ _ _ _ _ _ _ _ \n"
                ++ (concat [xchange x|x<-(showB (part8 pos))]))
part8 [] = []
part8 (a:bs) = (take 8 (a:bs)):(part8 (drop 8 (a:bs)))

xchange x | x==',' = " | "
          | x=='E' = "___"
          | x=='X' = "xxx"
	  | x=='L' = "@@@"
	  | x=='O' = "OOO"
	  | x=='[' = ""
	  | x==']' = "\n"
