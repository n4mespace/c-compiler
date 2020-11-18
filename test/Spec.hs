{-# LANGUAGE QuasiQuotes #-}

module Main where

import           Test.Hspec
import           Text.Heredoc

import           Compiler.Errors
import           Compiler.Syntax.Control
import           Compiler.Types

import qualified Lib

-- | Run test cases
main :: IO ()
main = hspec $ parallel $ do
  -- Success cases
  describe "Test.Compiler.SuccessCases" $ do
    it "test1: simple return" $ withoutError test1
    it "test2: assign expr to a var" $ withoutError test2
    it "test3: using var in expr" $ withoutError test3
    it "test4: complex expr with vars" $ withoutError test4
    it "test5: if with scoped vars" $ withoutError test5
    it "test6: multiple scopes" $ withoutError test6
    it "test7: big scoped expr" $ withoutError test7
    it "test8: assign with operator" $ withoutError test8
    it "test9: function calls with different params" $ withoutError test9
    it "test10: fibonachi with while loop" $ withoutError test10
    it "test11: loop without post clause" $ withoutError test11
    it "test12: break and continue in loop" $ withoutError test12

  -- Failure cases
  describe "Test.Compiler.FailureCases" $ do
    it "test1: uninitialized var" $ test1' `withError` uninitVarErr "a"
    it "test2: missing operand" $ test2' `withError` lexerErr
    it "test3: redefining a var" $ test3' `withError` alreadyDeclaredVarErr "a"
    it "test4: using var from inner scope" $ test4' `withError` unknownVarErr "a"
    it "test5: using var from if stmt scope" $ test5' `withError` unknownVarErr "a"
    it "test6: using assing operator without var declaration" $ test6' `withError` unknownVarErr "a"
    it "test7: function main is missing" $ test7' `withError` mainFuncNotFoundErr
    it "test8: var from while loop" $ test8' `withError` unknownVarErr "k"
    it "test9: var from for header" $ test9' `withError` unknownVarErr "i"
    it "test10: wrong args in function call" $ test10' `withError` wrongNumArgsErr "addOne" [FParam INT_T "v"]
    it "test11: break outside the loop" $ test11' `withError` breakOutsideTheLoopErr
    it "test12: continue outside the loop" $ test12' `withError` continueOutsideTheLoopErr

  where
    withoutError :: String -> Expectation
    withoutError test =
      Lib.compileString test `shouldSatisfy` (not . null)

    withError :: String -> Either ErrT String -> Expectation
    withError test err =
      Lib.compileString test `shouldBe` err

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
            |      }
            |   }
            |   return a;
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

test10 :: String
test10 = [str|
             |int fib(int n);
             |
             |int main() {
             |    int f = fib(10);
             |    return f;
             |}
             |
             |int fib(int n) {
             |    int i = 0;
             |    int j = 1;
             |    int count = 0;
             |    int d = 0;
             |    int c = i + j;
             |
             |    while (count < n - 2) {
             |        d = j + c;
             |        j = c;
             |        c = d;
             |        count += 1;
             |    }
             |    return d;
             |}
             |]

test11 :: String
test11 = [str|
             |int factorial(int n);
             |
             |int main() {
             |    return factorial(10);
             |}
             |
             |int factorial(int n) {
             |    int c = 1;
             |    for (int i = 2; i < n - 1;) {
             |        c *= i;
             |        i += 1;
             |    }
             |    return c;
             |}
             |]

test12 :: String
test12 = [str|
             |int fact(int n) {
             |    int c = 1;
             |    for (int i = 2;;) {
             |        if (!(i < n - 1)) {
             |            c *= i;
             |            i += 1;
             |        } else {
             |            break;
             |        }
             |        continue;
             |        i *= 100;
             |    }
             |    return c;
             |}
             |
             |int main() {
             |    return fact(10);
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

test8' :: String
test8' = [str|
             |int loop(int n);
             |int main() {
             |    int f = loop(10);
             |    return f;
             |}
             |int loop(int n) {
             |    int i = 0;
             |    while(i < n - 2) {
             |        i += 1;
             |        int k = 4;
             |    }
             |    return i + k;
             |}
             |]

test9' :: String
test9' = [str|
             |int loop(int n) {
             |    for (int i = 0;;) {
             |        i += 1;
             |    }
             |    return i;
             |}
             |
             |int main() {
             |    int f = loop(10);
             |    return f;
             |}
             |]

test10' :: String
test10' = [str|
              |int addOne(int v) {
              |    return v + 1;
              |}
              |
              |int main() {
              |    int a = addOne(8, 1);
              |    return a;
              |}
              |]

test11' :: String
test11' = [str|
              |int divByTen(int n) {
              |    return n / 10;
              |}
              |
              |int main() {
              |    int one = divByTen(10);
              |    break;
              |    return one;
              |}
              |]

test12' :: String
test12' = [str|
              |int divByTen(int n) {
              |    return n / 10;
              |}
              |
              |int main() {
              |    int one = divByTen(10);
              |    continue;
              |    return one;
              |}
              |]