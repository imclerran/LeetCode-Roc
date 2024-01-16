## LeetCode 872. Leaf-Similar Trees
interface LeafSimilar
    exposes [leafSimilar]
    imports [
        RocUtils.BinaryTree.{
            Tree,
            createTreeFromStrList,
            hasLhs,
            hasRhs,
            getLhsIdx,
            getRhsIdx,
            getValAtIdx,
        },
    ]

leafSimilar : Tree a, Tree a -> Bool where a implements Eq
leafSimilar = \tree1, tree2 ->
    leafVals1 = getLeafVals tree1 0 []
    leafVals2 = getLeafVals tree2 0 []
    List.map2 leafVals1 leafVals2 (\v1, v2 -> v1 == v2) |> List.all (\bool -> bool)

getLeafVals : Tree a, Nat, List a -> List a
getLeafVals = \tree, idx, leafVals ->
    if hasLhs tree idx then
        leafValsWithLeft = getLeafVals tree (getLhsIdx idx) leafVals
        if hasRhs tree idx then
            getLeafVals tree (getRhsIdx idx) leafValsWithLeft
        else 
            leafValsWithLeft
    else if hasRhs tree idx then
        getLeafVals tree (getLhsIdx idx) leafVals
    else when getValAtIdx tree idx is
        Ok val -> List.append leafVals val
        Err _ -> leafVals

# TESTS:
expect
    # trees with root only
    root1 = createTreeFromStrList ["1"] Str.toI64
    root2 = createTreeFromStrList ["1"] Str.toI64
    root3 = createTreeFromStrList ["0"] Str.toI64

    (leafSimilar root1 root1) && # self
    (leafSimilar root1 root2) && # identical tree
    !(leafSimilar root1 root3) # different tree

expect
    # trees with root and one leaf
    root1 = createTreeFromStrList ["1"] Str.toI64
    root2 = createTreeFromStrList ["0", "1"] Str.toI64
    root3 = createTreeFromStrList ["0", "", "1"] Str.toI64

    (leafSimilar root1 root2) && # root only and root+lhs
    (leafSimilar root1 root3) && # root only and root+rhs
    (leafSimilar root2 root3) # root+lhs and root+rhs

expect
    # complex trees
    root1 = createTreeFromStrList ["3", "5", "1", "6", "2", "9", "8", "null", "null", "7", "4"] Str.toI64 # leaf similar #1
    root2 = createTreeFromStrList ["3", "5", "1", "6", "7", "4", "2", "null", "null", "null", "null", "null", "null", "9", "8"] Str.toI64 # leaf similar #2
    root3 = createTreeFromStrList ["3", "5", "1", "6", "7", "4", "2", "null", "null", "null", "null", "null", "9", "9", "8"] Str.toI64 # not leaf similar
    
    (leafSimilar root1 root2) &&
    !(leafSimilar root1 root3) &&
    !(leafSimilar root2 root3)
