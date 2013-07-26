import Control.Monad
import System.Environment
import Data.Char
import Data.Either
import Data.Map
import Data.Maybe

type Rule = Either Int String

norm :: String -> String
norm = Prelude.map toUpper

-- | Given a string representing a Rule, parse the Rule from it
parseRule :: String -> [Rule]
parseRule = parseRule' . words
    where
        parseRule' :: [String] -> [Rule]
        parseRule' [] = []
        parseRule' (x:xs) = case reads x :: [(Int, String)] of
            [] -> Right x : parseRule' xs
            (y, _):[] -> Left y : parseRule' xs
            _ -> error "undefined"


-- | Given a rule and a sentence, try to decompose the sentence
decompose :: String -> String -> Maybe [String]
decompose r s = decompose' (parseRule r) (words s)
    where
        decompose' :: [Rule] -> [String] -> Maybe [String]
        decompose' [] _ = Just []
        decompose' (Right x:xs) (y:ys)
            | x == norm y = case decompose' xs ys of
                Nothing -> Nothing
                (Just z) -> Just (y:z)
            | otherwise = Nothing
        decompose' (Left 0 : Right y : zs) (a:bs)
            | y == norm a = decompose' (Right y : zs) (a:bs)
            | otherwise = case decompose' (Left 0 : Right y : zs) cs of
                Nothing -> Nothing
                (Just z) -> Just (unwords ds:z)
            where
                cs = dropWhile (\x -> y /= norm x) (a:bs)
                ds = takeWhile (\x -> y /= norm x) (a:bs)
        decompose' [Left 0] bs = Just [unwords bs]
        decompose' (Left n:xs) (bs) = case decompose' xs (drop n bs) of
            Nothing -> Nothing
            (Just z) -> Just (unwords (take n bs):z)
        decompose' _ _ = Nothing


-- Given a sentence and a list of decomposition rules, try to decompose the 
-- sentence
decomposeM :: String -> [String] -> Maybe (String, [String])
decomposeM _ [] = Nothing
decomposeM s (r:rs) = case decompose r s of
    Nothing -> decomposeM s rs
    Just x -> Just (r, x)

-- | Given a rule and a decomposition, assemble a sentence
reassemble :: String -> [String] -> String
reassemble rule xs = unwords $ reassemble' (parseRule rule) xs
    where
        reassemble' :: [Rule] -> [String] -> [String]
        reassemble' [] _ = []
        reassemble' (Right r:rs) ys = (:) r $ reassemble' rs ys
        reassemble' (Left n:rs) ys =  (:) (ys !! (n - 1)) $ reassemble' rs ys


-- Given a rule, get the keywords
keywords :: String -> [String]
keywords = rights . parseRule


-- Given a rule, get a map
-- TODO: This should really be a map from keyword to set of rules
makeMap :: String -> Map String [String]
makeMap r = unionsWith (++) (Prelude.map (\k -> insert k [r] empty) (keywords r))


-- Given a list of rules, map each keyword in the rule to the rule
getKeywords :: [String] -> Map String [String]
getKeywords [] = empty
getKeywords xs = unionsWith (++) (Prelude.map makeMap xs)


wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'


parseFile :: [String] -> Map String [String]
parseFile [] = empty
parseFile xs = unionsWith (++) (Prelude.map f xs)
    where
        f :: String -> Map String [String]
        f l = fromList [(head rules, tail rules)]
            where
                rules = wordsWhen (==',') l


-- Given a map of keywords -> decomposition rules, a map of decomposition rules
-- to recomposition rules, and a keyword, try to get a response
build :: Map String [String] -> Map String [String] -> String -> String -> Maybe String
build ds rs k s = case Data.Map.lookup (norm k) ds of
    Nothing -> Nothing
    Just xs -> case decomposeM s xs of
        Nothing -> Just "cant decompose"
        Just (r, x) -> case Data.Map.lookup r rs of
            Nothing -> Just "cant build" -- TODO: This case should not be possible
            Just ts -> Just (reassemble (head ts) x) -- head is probably not right here


-- Given a map of keywords -> decomposition rules, a map of decomposition rules
-- to recomposition rules, and a sentence, get a response
getResponse :: Map String [String] -> Map String [String] -> String -> String
getResponse ds rs s = scan' $ words s
    where
        scan' :: [String] -> String
        scan' [] = s
        scan' (w:ws) = fromMaybe (scan' ws) (build ds rs w s)


-- The text is read and inspected for the presence of a _keyword_.
-- If such a word is found, the sentence is transformed according to a _rule_
-- associated with the keyword, if not a content-free remark, or under certain
-- conditions, an earlier transformation is retrieved. The text so computed or
-- retrieved is then printed out.
talk :: (String -> String) -> IO ()
talk g = do
    line <- getLine
    putStrLn $ g line
    talk g

main :: IO ()
main = do
    [script] <- getArgs
    t <- readFile script
    let ds = getKeywords $ Prelude.map (head . wordsWhen (==',')) (lines t)
    let rs = parseFile $ lines t
    print ds
    print rs
    putStrLn "I am Eliza"
    -- interact init -- (getResponse ds rs)
    talk (getResponse ds rs)
