## LeetCode 872. Leaf-Similar Trees
interface P872LeafSimilar
    exposes [isLeafSimilar]
    imports [
        RocUtils.BinaryTree.{
            Tree,
            createTreeFromStrList,
            hasLhs,
            hasRhs,
            getLhsIdx,
            getRhsIdx,
            getNodeVal,
        },
    ]

isLeafSimilar : Tree a, Tree a -> Bool where a implements Eq
isLeafSimilar = \tree1, tree2 ->
    leafVals1 = getLeafVals tree1 0 []
    leafVals2 = getLeafVals tree2 0 []
    if (List.len leafVals1) == 0 && (List.len leafVals2) == 0 then
        Bool.true
    else if (List.len leafVals1) != (List.len leafVals2) then
        Bool.false
    else
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
        getLeafVals tree (getRhsIdx idx) leafVals
    else when getNodeVal tree idx is
        Ok val -> List.append leafVals val
        Err _ -> leafVals

# TESTS:
# TODO: add tests for trees with different numbers of leaves 
# (where all the leaves are the same for the shorter list)
expect
    # trees with root only
    root1 = createTreeFromStrList ["1"] Str.toI64 FullList
    root2 = createTreeFromStrList ["1"] Str.toI64 FullList 
    root3 = createTreeFromStrList ["0"] Str.toI64 FullList 

    (isLeafSimilar root1 root1) && # self
    (isLeafSimilar root1 root2) && # identical tree
    !(isLeafSimilar root1 root3) # different tree

expect
    # trees with root and one leaf
    root1 = createTreeFromStrList ["1"] Str.toI64 FullList 
    root2 = createTreeFromStrList ["0", "1"] Str.toI64 FullList 
    root3 = createTreeFromStrList ["0", "", "1"] Str.toI64 FullList 

    t1 = (isLeafSimilar root1 root2) # root only and root+lhs
    t2 = (isLeafSimilar root1 root3) # root only and root+rhs
    t3 = (isLeafSimilar root2 root3) # root+lhs and root+rhs
    t1 && t2 && t3

expect
    # complex trees
    root1 = createTreeFromStrList ["3", "5", "1", "6", "2", "9", "8", "null", "null", "7", "4"] Str.toI64 FullList # leaf similar #1
    root2 = createTreeFromStrList ["3", "5", "1", "6", "7", "4", "2", "null", "null", "null", "null", "null", "null", "9", "8"] Str.toI64 FullList  # leaf similar #2
    root3 = createTreeFromStrList ["3", "5", "1", "6", "7", "4", "2", "null", "null", "null", "null", "null", "9", "9", "8"] Str.toI64 FullList  # not leaf similar
    
    (isLeafSimilar root1 root2) && # root 1 and root 2 are similar
    !(isLeafSimilar root1 root3) && # root 1 and root 3 are not similar
    !(isLeafSimilar root2 root3) # root 2 and root 3 are not similar
