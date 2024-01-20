# LeetCode 94. Binary Tree Inorder Traversal
interface InorderTraversal
    exposes []
    imports [
        RocUtils.BinaryTree.{
            Tree,
            getLhsIdx,
            getRhsIdx,
            getValAtIdx,
            createTreeFromStrList,
            createTreeFromStrList2,
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
    # Note: the list for constructing the tree has been modified from the original problem
    # in order to conform to the format expected by the RocUtils.BinaryTree.createTreeFromStrList function
    # This function was written based on the list format in problem 872.
    inorder1 = createTreeFromStrList ["1", "", "2", "", "", "3"] Str.toI64 |> inorderTraversal
    inorder2 = createTreeFromStrList2 ["1", "", "2", "3"] Str.toI64 |> inorderTraversal
    inorder1 == [1, 3, 2] &&
    inorder2 == [1, 3, 2]