import System.Random
main =
     do
	let
		h1 = fst ((randomR (1,6) (mkStdGen 359212121)))
		h2 = fst ((randomR (1,6) (mkStdGen (snd h1))))
		h3 = fst ((randomR (1,6) (mkStdGen (snd h2))))
      	putStr (show h1)
      	putStr (show h2)
      	putStr (show h3)