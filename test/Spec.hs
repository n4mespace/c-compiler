{-# LANGUAGE QuasiQuotes #-}

module Main where

import           Test.Hspec
import           Text.Heredoc

import qualified Lib

-- | Run test cases
main :: IO ()
main = hspec $ do
  -- Success cases
  describe "Test.Compiler.SuccessCases" $ do
    it "test1: simple return" $ mustCompile test1
    it "test2: assign expr to a var" $ mustCompile test2
    it "test3: using var in expr" $ mustCompile test3
    it "test4: complex expr with vars" $ mustCompile test4
    it "test5: if with scoped vars" $ mustCompile test5
    it "test6: multiple scopes" $ mustCompile test6
    it "test7: big scoped expr" $ mustCompile test7
    it "test8: assign with operator" $ mustCompile test8
    it "test9: function calls with different params" $ mustCompile test9
  -- Failure cases
  describe "Test.Compiler.FailureCases" $ do
    it "test1: uninitialized var" $ mustNotCompile test1'
    it "test2: missing operand" $ mustNotCompile test2'
    it "test3: redefining a var" $ mustNotCompile test3'
    it "test4: using var from inner scope" $ mustNotCompile test4'
    it "test5: using var from if stmt scope" $ mustNotCompile test5'
    it "test6: using assing operator without var declaration" $ mustNotCompile test6'
    it "test7: function main is missing" $ mustNotCompile test7'

  where
    mustCompile :: String -> Expectation
    mustCompile test =
      Lib.compileString test >>= (`shouldSatisfy` (not . null))

    mustNotCompile :: String -> Expectation
    mustNotCompile test =
      Lib.compileString test `shouldThrow` anyException

-- Test cases
test1 :: String
test1 = [str|
            |int main() {
            |   return 3;
            |}
            |]

test2 :: String
test2 = [str|
            |int main() {
            |   int a = 5 + 2;
            |   return a;
            |}
            |]

test3 :: String
test3 = [str|
            |int main() {
            |   int c;
            |   int a = 5 + 2;
            |   c = a / 2;
            |   return a;
            |}
            |]

test4 :: String
test4 = [str|
            |int main() {
            |   int c;
            |   int a = 21 % 0b100;
            |   c = (a % (2 + a) - 3) * (-2 / a);
            |   return ~c;
            |}
            |]

test5 :: String
test5 = [str|
            |int main() {
            |    bool flag = false;
            |    int a = 3;
            |    if (flag) {
            |       return a;
            |    } else {
            |       a + 4;
            |    }
            |    return a;
            |}
            |]

test6 :: String
test6 = [str|
            |int main() {
            |   int a = 1;
            |   {
            |      a = 2;
            |      {
            |          a = 3;
            |          if (a) {
            |              a = 4;
            |          }
            |          return a;
            |      }
            |   }
            |}
            |]

test7 :: String
test7 = [str|
            |int main() {
            |    bool flag = false;
            |    int cnt = 100;
            |    if (!flag) {
            |        int b = 13;
            |        {
            |          int c = 2 * b;
            |        }
            |        return b;
            |    } else {
            |        return cnt / 4;
            |    }
            |}
            |]

test8 :: String
test8 = [str|
            |int main() {
            |    int a = 4;
            |    int b = 3;
            |    int c = 2;
            |    int d = 1;
            |    int e = 0;
            |    e += 5;
            |    d *= 3;
            |    c %= 6;
            |    b -= e;
            |    a /= 2;
            |    return a;
            |}
            |]

test9 :: String
test9 = [str|
            |int addTwoIfFlag(int value, bool flag);
            |int addOne(int value);
            |
            |int main() {
            |    bool flag = true;
            |    char ch = 'c';
            |    int a = addOne(8);
            |    int b = addTwoIfFlag(a * 2, flag);
            |    return addTwoIfFlag(ch + 1, !flag);
            |}
            |
            |int addOne(int v) {
            |    return v + 1;
            |}
            |
            |int addTwoIfFlag(int v, bool f) {
            |    if (f) {
            |        return v;
            |    } else {
            |        return v + 2;
            |    }
            |}
            |]

test1' :: String
test1' = [str|
            |int main() {
            |   int a;
            |   return a;
            |}
            |]

test2' :: String
test2' = [str|
             |int main() {
             |    int b = 43 % 2;
             |    int c = 3;
             |    return c + ;
             |}
             |]

test3' :: String
test3' = [str|
             |int main() {
             |    int b = 13 % 2;
             |    int a = 3;
             |    int a = a + b;
             |    return ~(-a % b);
             |}
             |]

test4' :: String
test4' = [str|
             |int main() {
             |    int b = 13;
             |    {
             |       int a = 3;
             |    }
             |    return -a % b;
             |}
             |]

test5' :: String
test5' = [str|
             |int main() {
             |    int b = 10;
             |    if (b > 2) {
             |       int a = 3;
             |    }
             |    return a;
             |}
             |]

test6' :: String
test6' = [str|
             |int main() {
             |   a %= 4;
             |   return a;
             |}
             |]

test7' :: String
test7' = [str|
            |int addTwoIfFlag(int value, bool flag);
            |int addOne(int value);
            |
            |int addOne(int v) {
            |    return v + 1;
            |}
            |
            |int addTwoIfFlag(int v, bool f) {
            |    if (f) {
            |        return v;
            |    } else {
            |        return v + 2;
            |    }
            |}
            |]
