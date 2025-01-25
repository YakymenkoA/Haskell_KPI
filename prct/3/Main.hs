data Token = Number Float | Operator Char | LParen | RParen
  deriving (Show, Eq)

parse :: String -> [Token]
parse expr = shuntingYard (tokenize expr) [] []
  where
    precedence :: Char -> Int
    precedence '^' = 4
    precedence '*' = 3
    precedence '/' = 3
    precedence '%' = 3
    precedence '+' = 2
    precedence '-' = 2
    precedence _ = 0

    isLeftAssociative :: Char -> Bool
    isLeftAssociative '^' = False
    isLeftAssociative _ = True

    isDigitChar :: Char -> Bool
    isDigitChar c = c >= '0' && c <= '9'

    tokenize :: String -> [Token]
    tokenize [] = []
    tokenize (c:cs)
      | isDigitChar c || c == '.' = let (num, rest) = span (\x -> isDigitChar x || x == '.') (c:cs)
                                    in Number (read num) : tokenize rest
      | c == '(' = LParen : tokenize cs
      | c == ')' = RParen : tokenize cs
      | c `elem` "+-*/%^" = Operator c : tokenize cs
      | c == ' ' = tokenize cs
      | otherwise = error $ "Unknown character: " ++ [c]

    shuntingYard :: [Token] -> [Token] -> [Token] -> [Token]
    shuntingYard [] output ops = reverse output ++ reverse ops
    shuntingYard (Number n : ts) output ops = shuntingYard ts (Number n : output) ops
    shuntingYard (Operator o : ts) output (Operator t : ops)
      | precedence o < precedence t || (precedence o == precedence t && isLeftAssociative o) =
          shuntingYard (Operator o : ts) (Operator t : output) ops
    shuntingYard (Operator o : ts) output ops = shuntingYard ts output (Operator o : ops)
    shuntingYard (LParen : ts) output ops = shuntingYard ts output (LParen : ops)
    shuntingYard (RParen : ts) output (LParen : ops) = shuntingYard ts output ops
    shuntingYard (RParen : ts) output (op : ops) = shuntingYard (RParen : ts) (op : output) ops
    shuntingYard _ _ _ = error "Mismatched parentheses"

eval :: [Token] -> Float
eval tokens = head $ foldl evalStep [] tokens
  where
    evalStep :: [Float] -> Token -> [Float]
    evalStep (x:y:ys) (Operator '+') = (y + x) : ys
    evalStep (x:y:ys) (Operator '-') = (y - x) : ys
    evalStep (x:y:ys) (Operator '*') = (y * x) : ys
    evalStep (x:y:ys) (Operator '/') = (y / x) : ys
    evalStep (x:y:ys) (Operator '%') = (fromIntegral (floor y `mod` floor x)) : ys
    evalStep (x:y:ys) (Operator '^') = (y ** x) : ys
    evalStep stack (Number n) = n : stack
    evalStep _ _ = error "Invalid expression"

main = do
    let expr1 = "10 * 2 ^ (3 - 1) * 3.5"
    let expr2 = "(10 / (2 % 2)) + 1"
    let expr3 = "((2 + 2)) + (((3 ^ 2 % 2)))"

    print $ "Expression: " ++ expr1
    let rpn1 = parse expr1
    print $ "RPN: " ++ show rpn1
    print $ "Result: " ++ show (eval rpn1)

    print $ "Expression: " ++ expr2
    let rpn2 = parse expr2
    print $ "RPN: " ++ show rpn2
    print $ "Result: " ++ show (eval rpn2)

    print $ "Expression: " ++ expr3
    let rpn3 = parse expr3
    print $ "RPN: " ++ show rpn3
    print $ "Result: " ++ show (eval rpn3)
