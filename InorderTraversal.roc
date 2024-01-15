# LeetCode 94. Binary Tree Inorder Traversal
interface InorderTraversal
    exposes []
    imports [
        RocUtils.BinaryTree.{
            Tree,
            hasLhs,
            hasRhs,
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
    if hasLhs tree index then # tree with left subtree
        valsListWithLeft = inorderTraversalRecur tree (getLhsIdx index) valsList
        resultVal = getValAtIdx tree index
        if resultVal == Ok val then # should always be true
            valsListWithCurrent = valsListWithLeft |> List.append val
        else
            crash "Expected a value at index \(index)"
        if hasRhs tree index then # tree with left and right subtree
            inorderTraversalRecur tree (getRhsIdx index) valsListWithCurrent
        else
            valsListWithCurrent # tree with only left subtree
    else if hasRhs tree index then # tree with only right subtree
        if resultVal == Ok val then # should always be true
            valsListWithCurrent = valsList |> List.append val
        else
            crash "Expected a value at index \(index)"
        inorderTraversalRecur tree (getRhsIdx index) valsListWithCurrent # finish right subtree
    else
        when getValAtIdx tree index is # leaf node
            Ok val -> valsList |> List.append val # should always be Ok
            Err _ -> crash "Expected a value at index \(index)"
    
     
expect
    createTreeFromStrList ["1", "null", "2", "3"]
    |> inorderTraversal