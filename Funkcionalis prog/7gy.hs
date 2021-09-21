mySqrt nr = seged nr 1
	where 
	seged nr x0
		--| abs (xn - x0) < 0.000001 =x0
		|xn == x0 == x0
		|otherwise= seged nr xn
		where
		xn = (x0 + nr / x0)/2
		
mainConv = do
	putStr "n: "
	temp <- getLine
	let n = read temp :: Int
	let rStr = converto2Strs n
	putStrIn $"szamrendszerbeli alak: " ++ rStr