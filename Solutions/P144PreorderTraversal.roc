# LeetCode 144. Binary Tree Preorder Traversal
interface P144PreorderTraversal
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

preorderTraversal : Tree a -> List a
preorderTraversal = \tree -> preorderTraversalRecur [] tree 0

preorderTraversalRecur : List a, Tree a, Nat -> List a
preorderTraversalRecur = \valsList, tree, index ->
    when getNodeVal tree index is
        Ok val ->
            valsList
            |> List.append val
            |> preorderTraversalRecur tree (getLhsIdx index)
            |> preorderTraversalRecur tree (getRhsIdx index)
        Err _ -> valsList

# TESTS
expect
    # Empty tree
    tree = createTreeFromStrList [] Str.toI64 TruncatedList
    preorder = preorderTraversal tree
    preorder == []

expect
    # Single node tree
    tree = createTreeFromStrList ["1"] Str.toI64 TruncatedList
    preorder = preorderTraversal tree
    preorder == [1]

expect
    # Tree with root and left child
    tree = createTreeFromStrList ["1", "2"] Str.toI64 TruncatedList
    preorder = preorderTraversal tree
    preorder == [1, 2]

expect
    # Tree with root and right child
    tree = createTreeFromStrList ["1", "null", "2"] Str.toI64 TruncatedList
    preorder = preorderTraversal tree
    preorder == [1, 2]

expect
    # Tree with root and left and right child
    tree = createTreeFromStrList ["1", "2", "3"] Str.toI64 TruncatedList
    preorder = preorderTraversal tree
    preorder == [1, 2, 3]

expect
    # Complex tree
    strList = ["1", "null", "2", "3", "4", "null", "5", "6", "7", "8", "9"]
    tree = createTreeFromStrList strList Str.toI64 TruncatedList
    preorder = preorderTraversal tree
    preorder == [1, 2, 3, 5, 8, 9, 4, 6, 7]