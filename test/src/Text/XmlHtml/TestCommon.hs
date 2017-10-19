{-# LANGUAGE ScopedTypeVariables       #-}

module Text.XmlHtml.TestCommon where

import           Control.Exception as E
import           System.IO.Unsafe
import           Test.Hspec
import           Test.HUnit hiding (Test, Node)

------------------------------------------------------------------------------
-- | Tests a simple Bool property.
testIt :: String -> Bool -> Spec
testIt name b = it name $ assertBool name b

------------------------------------------------------------------------------
-- | Tests with assertEqual which gives more useful information to the user on
-- failure.  We should probably stop using testIt.  :/
testEqual :: (Eq a, Show a) => String -> a -> a -> Spec
testEqual name expected actual = it name $ assertEqual name expected actual

------------------------------------------------------------------------------
-- Code adapted from ChasingBottoms.
--
-- Adding an actual dependency isn't possible because Cabal refuses to build
-- the package due to version conflicts.
--
-- isBottom is impossible to write, but very useful!  So we defy the
-- impossible, and write it anyway.
isBottom :: a -> Bool
isBottom a = unsafePerformIO $
    (E.evaluate a >> return False) `E.catches` [
        E.Handler $ \ (_ :: PatternMatchFail) -> return True,
        E.Handler $ \ (_ :: ErrorCall)        -> return True,
        E.Handler $ \ (_ :: NoMethodError)    -> return True,
        E.Handler $ \ (_ :: RecConError)      -> return True,
        E.Handler $ \ (_ :: RecUpdError)      -> return True,
        E.Handler $ \ (_ :: RecSelError)      -> return True
        ]

------------------------------------------------------------------------------
isLeft :: Either a b -> Bool
isLeft = either (const True) (const False)

