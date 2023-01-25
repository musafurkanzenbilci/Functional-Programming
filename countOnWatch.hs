import Text.Printf

countOnWatch :: [String] -> String -> Int -> Int
countOnWatch schedule employee days | days==0 =0
                                    | (schedule!!(days-1))==employee = 1 + countOnWatch schedule employee (days-1)
                                    | otherwise = countOnWatch schedule employee (days-1)