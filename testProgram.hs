module TestProgram where

import Test.HUnit
import Program

program = new ""

testNew :: Test
testNew = TestCase $ assertEqual "should return Program instance" (Program [] "+++") (new "+++")
--result = matchingLeftBracket program

--testMatchingLeftBracket :: Test
--testMatchingLeftBracket = TestCase $ assertEqual "should move program pointer to matching left bracket"
--  (Program "++" "[>>]--") (matchingLeftBracket (Program ">>[++" "]--"))

testMatchingRightBracket :: Test
testMatchingRightBracket = TestCase $ assertEqual "should move program pointer to matching right bracket"
  (Program "--++[" "]<>") (matchingRightBracket (Program "" "[++--]<>"))

testMatchingRightBracketNested :: Test
testMatchingRightBracketNested = TestCase $ assertEqual "should move program pointer to matching right bracket"
  (Program "-][-++[" "]<>") (matchingRightBracket (Program "" "[++-[]-]<>"))

main :: IO Counts
main = runTestTT $ TestList [testNew, testMatchingRightBracket, testMatchingRightBracketNested]
