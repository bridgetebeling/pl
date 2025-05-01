-- test/TriTreeSpec.hs
-- Spec tests for TriTree. Making sure everything works.

module TriTree where


import Test.Hspec
import TriTree

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "TriTree basic operations" $ do

    it "search finds values that exist and fails for ones that don't" $ do
      let tree = insertList [1,2,3,4,5] Empty
      search 3 tree `shouldBe` True
      search 6 tree `shouldBe` False

    it "insertList sticks all the elements into the tree correctly" $ do
      let tree = insertList [1,2,3] Empty
      tree `shouldSatisfy` (\t -> all (`search` t) [1,2,3])

    it "identical checks when two trees are exactly the same" $ do
      let tree1 = insertList [1,2,3] Empty
      let tree2 = insertList [1,2,3] Empty
      identical tree1 tree2 `shouldBe` True

    it "treeMap transforms every element in the tree" $ do
      let tree = insertList [1,2,3] Empty
      let tree' = treeMap (*2) tree
      all (`search` tree') [2,4,6] `shouldBe` True

    it "treeFoldPreOrder goes through the tree preorder" $ do
      let tree = insertList [1,2,3] Empty
      treeFoldPreOrder (++) "" (treeMap show tree) `shouldSatisfy` (not . null)

    it "treeFoldInOrder goes through the tree inorder" $ do
      let tree = insertList [1,2,3] Empty
      treeFoldInOrder (++) "" (treeMap show tree) `shouldSatisfy` (not . null)

    it "treeFoldPostOrder goes through the tree postorder" $ do
      let tree = insertList [1,2,3] Empty
      treeFoldPostOrder (++) "" (treeMap show tree) `shouldSatisfy` (not . null)

    it "delete actually removes the right value" $ do
      let tree = insertList [1,2,3] Empty
      let tree' = delete 2 tree
      search 2 tree' `shouldBe` False
      search 1 tree' `shouldBe` True
      search 3 tree' `shouldBe` True

    it "buildTree builds a balanced tree with all values" $ do
      let tree = buildTree [1..7]
      all (`search` tree) [1..7] `shouldBe` True
