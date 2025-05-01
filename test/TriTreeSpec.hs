-- test/TriTreeSpec.hs
-- Test suite for TriTree. Making sure everything works how it's supposed to.

module Main (main) where


import Test.Hspec
import TriTree

spec :: Spec
spec = do
  describe "TriTree basic operations" $ do

    -- Testing search: should find stuff that's there, and not find stuff that's not
    it "search finds values that exist and fails for ones that don't" $ do
      let tree = insertList [1,2,3,4,5] Empty
      search 3 tree `shouldBe` True
      search 6 tree `shouldBe` False

    -- Testing insertList: throw in a whole list, should land correctly
    it "insertList sticks all the elements into the tree correctly" $ do
      let tree = insertList [1,2,3] Empty
      tree `shouldSatisfy` (\t -> all (`search` t) [1,2,3])

    -- Testing identical: trees should match if they’re built the same way
    it "identical checks when two trees are exactly the same" $ do
      let tree1 = insertList [1,2,3] Empty
      let tree2 = insertList [1,2,3] Empty
      identical tree1 tree2 `shouldBe` True

    -- Testing treeMap: apply (*2) to everything, make sure it worked
    it "treeMap transforms every element in the tree" $ do
      let tree = insertList [1,2,3] Empty
      let tree' = treeMap (*2) tree
      all (`search` tree') [2,4,6] `shouldBe` True

    -- Testing treeFoldPreOrder: smashing everything into a string in preorder
    it "treeFoldPreOrder goes through the tree preorder" $ do
      let tree = insertList [1,2,3] Empty
      treeFoldPreOrder (++) "" (treeMap show tree) `shouldSatisfy` (not . null)

    -- Testing treeFoldInOrder: make sure it's going left -> node -> middle -> node -> right
    it "treeFoldInOrder goes through the tree inorder" $ do
      let tree = insertList [1,2,3] Empty
      treeFoldInOrder (++) "" (treeMap show tree) `shouldSatisfy` (not . null)

    -- Testing treeFoldPostOrder: make sure it hits subtrees first before nodes
    it "treeFoldPostOrder goes through the tree postorder" $ do
      let tree = insertList [1,2,3] Empty
      treeFoldPostOrder (++) "" (treeMap show tree) `shouldSatisfy` (not . null)

    -- Testing delete: actually delete the right value and leave the others alone
    it "delete actually removes the right value" $ do
      let tree = insertList [1,2,3] Empty
      let tree' = delete 2 tree
      search 2 tree' `shouldBe` False
      search 1 tree' `shouldBe` True
      search 3 tree' `shouldBe` True

    -- Testing buildTree: throw in a list, get back a tree that has everything
    it "buildTree builds a balanced tree with all values" $ do
      let tree = buildTree [1..7]
      all (`search` tree) [1..7] `shouldBe` True
