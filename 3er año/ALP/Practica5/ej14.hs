

valor :: Int
valor = 15

main :: IO ()
main = do putStrLn "Adivina:";
		  main2
	where main2 = do x <- getLine;
					 let y = read x in 
		             if y == valor then putStrLn "Ganaste!"
		             else if valor < y then do putStrLn "Es menor!Intenta de nuevo...";
		                                       main2
		                  else do putStrLn "Es mayor!Intenta de nuevo...";
				                  main2
		  
		   
		   
		   
