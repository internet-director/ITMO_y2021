module HW5.Parser
    ( parse
    ) where

import Data.Void
import HW5.Base
import Data.Text(Text, pack)
import Text.Megaparsec.Char
import Text.Megaparsec hiding (parse)
import qualified Text.Megaparsec.Char.Lexer as Lex
import Control.Monad.Combinators.Expr (makeExprParser, Operator(InfixL, InfixN, InfixR))

type Parser = Parsec Void String

spaceSkip :: Parser ()
spaceSkip = Lex.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = Lex.lexeme spaceSkip

symbol :: String -> Parser String
symbol = Lex.symbol spaceSkip

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = runParser (parseExpr <* eof) ""

parseExpr :: Parser HiExpr
parseExpr = makeExprParser parseExpr' operators

parseExpr' :: Parser HiExpr
parseExpr' = do
    v <- parseTerm
    res <- parseArgs v
    return res

parseTerm :: Parser HiExpr
parseTerm = choice
    [ parseBrackets parseExpr
    , parseValueExpr
    ]

parseValueExpr :: Parser HiExpr
parseValueExpr = HiExprValue <$> choice
    [ HiValueNull     <$  string "null"
    , HiValueBool     <$> parseValueBool
    , HiValueString   <$> parseValueString
    , HiValueNumber   <$> parseValueNumber
    , HiValueFunction <$> parseValueFun
    ]

parseValueString :: Parser Text
parseValueString = pack <$> (lexeme $ string "\"" >> manyTill Lex.charLiteral (string "\""))

parseValueBool :: Parser Bool
parseValueBool = lexeme $ choice
    [ True  <$ string "true"
    , False <$ string "false"
    ]

parseValueNumber :: Parser Rational
parseValueNumber = toRational <$> (lexeme $ Lex.signed spaceSkip Lex.scientific)

parseValueFun :: Parser HiFun
parseValueFun = lexeme $ choice
    [ HiFunIf <$ string "if"
    , HiFunEquals <$ string "equals"
    , HiFunLessThan <$ string "less-than"
    , HiFunGreaterThan <$ string "greater-than"
    , HiFunNotEquals <$ string "not-equals"
    , HiFunNotLessThan <$ string "not-less-than"
    , HiFunNotGreaterThan <$ string "not-greater-than"
    , HiFunDiv <$ string "div"
    , HiFunMul <$ string "mul"
    , HiFunAdd <$ string "add"
    , HiFunSub <$ string "sub"
    , HiFunNot <$ string "not"
    , HiFunAnd <$ string "and"
    , HiFunXor <$ string "xor"
    , HiFunOr  <$ string "or"
    , HiFunLength <$ string "length"
    , HiFunToUpper <$ string "to-upper"
    , HiFunToLower <$ string "to-lower"
    , HiFunReverse <$ string "reverse"
    , HiFunTrim <$ string "trim"
    ]

operators :: [[Operator Parser HiExpr]]
operators =
   [[ binL "*"  HiFunMul, binLDiv],
    [ binL "+"  HiFunAdd
    , binL "-"  HiFunSub ],
    [ binN "<=" HiFunNotGreaterThan
    , binN ">=" HiFunNotLessThan
    , binN "<"  HiFunLessThan
    , binN ">"  HiFunGreaterThan
    , binN "/=" HiFunNotEquals
    , binN "==" HiFunEquals ],
    [ binR "&&" HiFunAnd ],
    [ binR "||" HiFunOr ]
    ]

generateFunction :: HiFun -> HiExpr -> HiExpr -> HiExpr
generateFunction f l r = HiExprApply (HiExprValue $ HiValueFunction f) [l, r]

binL :: String -> HiFun -> Operator Parser HiExpr
binL name f = binI name InfixL f

binR :: String -> HiFun -> Operator Parser HiExpr
binR name f = binI name InfixR f

binN :: String -> HiFun -> Operator Parser HiExpr
binN name f = binI name InfixN f

binI :: String -> (Parser (HiExpr -> HiExpr -> HiExpr) -> Operator Parser HiExpr) -> HiFun -> Operator Parser HiExpr
binI name inf f = inf $ generateFunction f <$ lexeme (symbol name)

binLDiv :: Operator Parser HiExpr
binLDiv = InfixL $ generateFunction HiFunDiv <$ ((lexeme . try) $ string "/" <* notFollowedBy (string "="))

parseBrackets :: Parser a -> Parser a
parseBrackets = between (symbol "(") (symbol ")")

parseArgs :: HiExpr -> Parser HiExpr
parseArgs p = choice
    [ do
        v <- HiExprApply p <$> parseBrackets (parseExpr `sepBy` symbol ",")
        parseArgs v
    , pure p
    ]