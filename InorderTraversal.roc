# LeetCode 94. Binary Tree Inorder Traversal
interface InorderTraversal
    exposes []
    imports [
        RocUtils.BinaryTree.{
            Tree,
            getLhsIdx,
            getRhsIdx,
            getValAtIdx,
            createTreeFromStrList
        }
    ]

inorderTraversal : Tree a -> List a
inorderTraversal = \tree -> inorderTraversalRecur tree 0 []

inorderTraversalRecur : Tree a, Nat, List a -> List a
inorderTraversalRecur = \tree, index, valsList ->
    when getValAtIdx tree index is
        Ok val -> 
            valsListWithLeft = inorderTraversalRecur tree (getLhsIdx index) valsList
            valsListWithCurrent = valsListWithLeft |> List.append val
            inorderTraversalRecur tree (getRhsIdx index) valsListWithCurrent
        Err _ -> valsList

        
     
expect
    root1 = createTreeFromStrList ["1", "", "2", "", "", "3"] Str.toI64
    inorderTraversal root1 == [1, 3, 2]