# LeetCode 145. Binary Tree Postorder Traversal 
interface P145PostorderTraversal
    exposes []
    imports [
        RocUtils.BinaryTree.{
            Tree,
            getLhsIdx,
            getRhsIdx,
            getNodeVal,
            createTreeFromStrList,
        },
    ]

postorderTraversal : Tree a -> List a
postorderTraversal = \tree -> postorderTraversalRecur [] tree 0

postorderTraversalRecur : List a, Tree a, Nat -> List a
postorderTraversalRecur = \valsList, tree, index ->
    when getNodeVal tree index is
        Ok val -> 
            valsList
            |> postorderTraversalRecur tree (getLhsIdx index)
            |> postorderTraversalRecur tree (getRhsIdx index)
            |> List.append val
        Err _ -> valsList

expect
    # Empty tree
    postorder = createTreeFromStrList [] Str.toI64 TruncatedList |> postorderTraversal
    postorder == []

expect
    # Single node tree
    postorder = createTreeFromStrList ["1"] Str.toI64 TruncatedList |> postorderTraversal
    postorder == [1]
        
expect
    postorder = createTreeFromStrList ["1", "null", "2", "3"] Str.toI64 TruncatedList |> postorderTraversal
    postorder == [3, 2, 1]
