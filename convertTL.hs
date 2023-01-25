import Text.Printf


-- This function takes a Double and rounds it to 2 decimal places --
getRounded :: Double -> Double
getRounded x = read s :: Double
               where s = printf "%.2f" x


convertTL :: Double -> String -> Double
convertTL b a = x
    where
        x=case a of
            "BTC" -> getRounded (b/473497.31)
            "USD" -> getRounded (b/8.18)
            "EUR" -> getRounded (b/9.62)
