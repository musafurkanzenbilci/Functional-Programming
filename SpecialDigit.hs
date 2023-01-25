module PE4 where


-- Generic DictTree definition with two type arguments
data DictTree k v = Node [(k, DictTree k v)] | Leaf v deriving Show

-- Lightweight Char wrapper as a 'safe' Digit type
newtype Digit = Digit Char deriving (Show, Eq, Ord) -- derive equality and comparison too!

-- Type aliases
type DigitTree = DictTree Digit String
type PhoneNumber = [Digit]




-- toDigit: Safely convert a character to a digit
isDigit c | c=='0' || c=='1' || c=='2' || c=='3' || c=='4' || c=='5' || c=='6' || c=='7' || c=='8' || c=='9' = True
          | otherwise = False

toDigit :: Char -> Maybe Digit
toDigit c | (isDigit c) = Just (Digit c)
          | otherwise = Nothing

-- toDigits: Safely convert a bunch of characters to a list of digits.
--           Particularly, an empty string should fail.
isNumber [] = []
isNumber (x:xs) = (isDigit x):(isNumber xs)

res [] = True
res (x:boollist) = x && res boollist

toDigits :: String -> Maybe PhoneNumber
toDigits c =if(res (isNumber c) && c/=[]) then Just   [fromJust(toDigit x) | x<-c] else Nothing


--toDigits [] = []
--toDigits (x:s) | (isDigit x) = Just (fromJust (toDigit x)):(toDigits s)
--               | otherwise = Nothing

             

-----------
-- Some phonebook business.

-- numContacts: Count the number of contacts in the phonebook...
numContacts :: DigitTree -> Int
numContacts _ = undefined
    
-- getContacts: Generate the contacts and their phone numbers in order given a tree. 
getContacts :: DigitTree -> [(PhoneNumber, String)]
getContacts _ = undefined

-- autocomplete: Create an autocomplete list of contacts given a prefix
-- e.g. autocomplete "32" areaCodes -> 
--      [([Digit '2'], "Adana"), ([Digit '6'], "Hatay"), ([Digit '8'], "Osmaniye")]
autocomplete :: String -> DigitTree -> [(PhoneNumber, String)]
autocomplete _ _ = undefined


-----------
-- Example Trees
-- Two example trees to play around with, including exampleTree from the text. 
-- Feel free to delete these or change their names or whatever!

exampleTree :: DigitTree
exampleTree = Node [
    (Digit '1', Node [
        (Digit '3', Node [
            (Digit '7', Node [
                (Digit '8', Leaf "Jones")])]),
        (Digit '5', Leaf "Steele"),
        (Digit '9', Node [
            (Digit '1', Leaf "Marlow"),
            (Digit '2', Node [
                (Digit '3', Leaf "Stewart")])])]),
    (Digit '3', Leaf "Church"),
    (Digit '7', Node [
        (Digit '2', Leaf "Curry"),
        (Digit '7', Leaf "Hughes")])]

areaCodes :: DigitTree
areaCodes = Node [
    (Digit '3', Node [
        (Digit '1', Node [
            (Digit '2', Leaf "Ankara")]),
        (Digit '2', Node [
            (Digit '2', Leaf "Adana"),
            (Digit '6', Leaf "Hatay"),
            (Digit '8', Leaf "Osmaniye")])]),
    (Digit '4', Node [
        (Digit '6', Node [
            (Digit '6', Leaf "Artvin")])])]

