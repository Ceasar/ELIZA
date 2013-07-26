{-

Parser for ELIZA scripts.

-}
import Text.ParserCombinators.Parsec
import System.Environment

data Token = StringToken String | IntToken Int deriving Show
data Rule = Rule [Token]
type RecompositionRule = Rule
data Transformation = Transformation Rule [RecompositionRule]
data KeyWord = KeyWord String [Transformation]
type Dictionary = [KeyWord]


symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"


readExpr :: String -> String
readExpr input = case parse parseStringToken "lisp" input of
    Left err -> "No match: " ++ show err
    Right ans -> show ans


parseStringToken :: Parser Token
parseStringToken = do
    t <- many letter
    return $ StringToken t


parseIntToken :: Parser Token
parseIntToken = do
    spaces
    t <- many digit
    return $ IntToken (read t :: Int)


parseToken :: Parser Token
parseToken = parseIntToken <|> parseStringToken


parseRule :: Parser Rule
parseRule = do char '('
               ts <- many parseToken
               char ')'
               return $ Rule ts


main :: IO ()
main = do
    args <- getArgs
    putStrLn (readExpr (args !! 0))
