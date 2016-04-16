module Program where
-- Program
data Program = Program {
  beginning:: [Char],
  ending:: [Char]
  } deriving Show

current :: Program -> Char
current (Program _ []) = error "end of program"
current (Program _ (p:ps)) = p

next :: Program -> Program
next (Program beginning (x:ending)) = Program (x:beginning) ending

prev :: Program -> Program
prev (Program (x:beginning) ending) = Program beginning (x:ending)
prev (Program [] ps) = error $ "Unexpected: " ++ ps

matchingRightBracket :: Program -> Program
matchingRightBracket p = b' 0 p where
  b' :: Integer -> Program -> Program
  b' 0 (Program _beginning (']':ps)) = Program (']':_beginning) ps
  b' d (Program _beginning (']':ps)) = b' (d-1) $ Program (']':_beginning) ps
  b' d (Program _beginning ('[':ps)) = b' (d+1) $ Program ('[':_beginning) ps
  b' _ (Program _beginning (p:ps))  = b' 0 $ Program (p:_beginning) ps

matchingLeftBracket :: Program -> Program
matchingLeftBracket p = b' 0 p where
  b' :: Int -> Program -> Program
  b' d p  | (d==0) && (current p)=='[' = p
          | (current p) == '[' = b' (d-1) (prev p)
          | (current p) == ']' = b' (d+1) (prev p)
--          | otherwise = b' d (prev p)
  b' d p = b' d (prev p)
  b' _d _p = error $ "Unexpected" ++ (show _d) ++ (show _p)
