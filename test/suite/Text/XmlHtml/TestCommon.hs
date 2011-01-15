module Text.XmlHtml.TestCommon where

import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit hiding (Test, Node)

------------------------------------------------------------------------------
-- | Tests a simple Bool property.
testIt :: TestName -> Bool -> Test
testIt name b = testCase name $ assertBool name b


