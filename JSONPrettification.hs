-- splitOn: Split string on first occurence of character.
position x xs = [i | (x',i)<- zip xs [0..n] ,x==x'] 
    where n= length xs

splitOn :: Char -> String -> (String, String)
splitOn c str = ((take (indexfun c str) str),drop ((indexfun c str)+1) str) where
        indexfun c str = if((position c str)==[]) then (length str) else  (position c str)!!0

-- tokenizeS: Transform an SJSON string into a list of tokens.
checkstr c= (a>64 && a<91) || (a>96 && a<123) where 
        a=fromEnum c
tokenizeS :: String -> [String]
tokenizeS sjson = fun (splitOn '\'' sjson)  where
            fun ([],[]) = []
            fun sjson = process ((fst sjson):(fun (splitOn '\'' (snd sjson))))

process strlist= map (delet ) strlist where
            delet str=if(checkstr (str!!0) || checkstr (last str)) then str else  [x|x<-str,x/='\n',x/=' ',x/='\t']
-- prettifyS: Prettify SJSON, better tokenize first!
prettifyS :: String -> String
prettifyS _ = undefined