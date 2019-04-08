
exp1 =if true then false
 		else true
			where false = True; true = False

--Bien formada, retorna False


--exp2 = if if then then else else
--Correcion:
--Mal formada, error de sintaxis: no hay nada despues del primer then


exp3 = False == (5 > 4)
--Bien formada, retorna False


--exp4 = 1 < 2 < 3
--Mal formada, compara Bool con Int

exp5 = 1 + if ('a' < 'z') then -1	 else 0
--Bien formado, retorna 0

p = (True,2)
exp6 = if fst p then fst p else snd p
--Mal  formado, no se porque

--exp7 = if fst p then fst p else snd p where p = (True, False)
