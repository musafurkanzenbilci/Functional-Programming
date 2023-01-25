rateCalculator :: (Double,Int) -> Double
rateCalculator pair | (fst pair)>=10000 && (snd pair)>=2 = 0.115
                    | (fst pair)>=10000 && (snd pair)<2 = 0.105
                    | (fst pair)<10000 && (snd pair)>=2 = 0.095
                    | (fst pair)<10000 && (snd pair)<2 = 0.090



compoundInterests :: [(Double, Int)] -> [Double]
compoundInterests investments = calculator investments 0
    where
        calculator investment loopvar
            |loopvar==((length investments)) = []
            |otherwise = getRounded((p*((1+(r/12))^(12*n)))) :  calculator investment (loopvar+1)
                where
                    p=fst (investment!!loopvar)
                    r= rateCalculator (investment!!loopvar)
                    n= snd (investment!!loopvar)