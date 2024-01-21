# LeetCode 94. Binary Tree Inorder Traversal
interface P94InorderTraversal
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

inorderTraversal : Tree a -> List a
inorderTraversal = \tree -> inorderTraversalRecur tree 0 []

inorderTraversalRecur : Tree a, Nat, List a -> List a
inorderTraversalRecur = \tree, index, valsList ->
    when getNodeVal tree index is
        Ok val ->
            valsListWithLeft = inorderTraversalRecur tree (getLhsIdx index) valsList
            valsListWithCurrent = valsListWithLeft |> List.append val
            inorderTraversalRecur tree (getRhsIdx index) valsListWithCurrent
        Err _ -> valsList

# TESTS
expect
    # Empty tree
    inorder = createTreeFromStrList [] Str.toI64 TruncatedList |> inorderTraversal
    inorder == []

expect
    # Single node tree
    inorder = createTreeFromStrList ["1"] Str.toI64 TruncatedList |> inorderTraversal
    inorder == [1]

expect
    # Simple balanced tree
    inorder = createTreeFromStrList ["1", "2", "3"] Str.toI64 TruncatedList |> inorderTraversal
    inorder == [2, 1, 3]

expect
    # Simple Tree with nodes to right of root only
    inorder = createTreeFromStrList ["1", "", "2", "3"] Str.toI64 TruncatedList |> inorderTraversal
    inorder == [1, 3, 2]

expect
    # 6 Deep tree to left only
    inorder = createTreeFromStrList ["1", "2", "", "3", "", "4", "", "5", "", "6"] Str.toI64 TruncatedList |> inorderTraversal
    inorder == [6, 5, 4, 3, 2, 1]

expect
    # 6 Deep tree to right only
    inorder = createTreeFromStrList ["1", "", "2", "", "3", "", "4", "", "5", "", "6"] Str.toI64 TruncatedList |> inorderTraversal
    inorder == [1, 2, 3, 4, 5, 6]
    
expect
    # Complex tree
    strList = ["1", "null", "2", "3", "4", "5", "null", "6", "7", "8", "9", "null", "10", "11", "null", "null", "12", "null", "null", "null", "null", "null", "14", "null", "null", "15", "16"]
    inorder = createTreeFromStrList strList Str.toI64 TruncatedList |> inorderTraversal
    inorder == [1, 8, 12, 5, 9, 3, 2, 6, 10, 4, 11, 15, 14, 16, 7]
