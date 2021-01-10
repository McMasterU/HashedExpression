module CollisionSpec where

import Test.Hspec

spec :: Spec
spec = describe "Handle hash collision spec" $
  specify "safe merge diff should resolve collision and preserve diff's semantic" $ do
    pending
