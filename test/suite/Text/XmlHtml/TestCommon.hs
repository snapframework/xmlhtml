{-# LANGUAGE ScopedTypeVariables       #-}

module Text.XmlHtml.TestCommon where

import           Control.Exception as E
import           System.IO.Unsafe
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit hiding (Test, Node)

------------------------------------------------------------------------------
-- | Tests a simple Bool property.
testIt :: TestName -> Bool -> Test
testIt name b = testCase name $ assertBool name b

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

