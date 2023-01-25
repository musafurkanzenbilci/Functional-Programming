process::Int->Int
process a |a `mod` 3==0 = a-1
          |a `mod` 4==0 = (a*2) `mod` 10
          |a `mod` 5==0 = (a+3) `mod` 10
          |otherwise = (a+4) `mod` 10
            
            
ff::[Int]->Int->[Int]
ff lis index |index==4 = []
             |otherwise = process (lis!!(index)) : ff lis (index+1) 
  
  
  
fromDigits = foldl addDigit 0
   where addDigit num d = 10*num + d
   
        
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]

encrypt :: Int -> Int
encrypt x = fromDigits(ff (digs x) 0)