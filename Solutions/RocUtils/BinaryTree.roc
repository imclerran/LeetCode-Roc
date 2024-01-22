interface RocUtils.BinaryTree
    exposes [
        countTreeLevels,
        createRoot,
        createTreeFromStrList,
        deleteNode,
        getDepthAtIdx,
        getLhs,
        getLhsIdx,
        getNodeVal,
        getParentIdx,
        getParentNode,
        getRhs,
        getRhsIdx,
        hasLhs,
        hasRhs,
        insertLhs,
        insertLhsNode,
        insertNode,
        insertRhs,
        insertRhsNode,
        isLeaf,
    ]
    imports []

Node a : [Data { val : a, idx : Nat }, Null] where a implements Eq & Inspect
Tree a : List (Node a) where a implements Eq & Inspect

getNodeVal : Tree a, Nat -> Result a [DoesNotExist]
getNodeVal = \tree, idx ->
    when List.get tree idx is
        Ok (Data { val }) -> Ok val
        Ok Null -> Err DoesNotExist
        Err OutOfBounds -> Err DoesNotExist

createRoot : a -> Tree a
createRoot = \val -> [Data { val, idx: 0 }]

createTreeFromStrList : List Str, (Str -> Result a *), [FullList, TruncatedList] -> Tree a
createTreeFromStrList = \strList, strToVal, listType ->
    when listType is
        FullList -> createTreeFromFullStrList strList strToVal
        TruncatedList -> createTreeFromTruncatedStrList strList strToVal

createTreeFromFullStrList : List Str, (Str -> Result a *) -> Tree a
createTreeFromFullStrList = \strList, strToVal ->
    List.mapWithIndex
        strList
        (\strVal, idx ->
            when strToVal strVal is
                Ok val -> Data { val, idx }
                Err _ -> Null
        )

hasLhs : Tree a, Nat -> Bool
hasLhs = \tree, idx ->
    when List.get tree (idx * 2 + 1) is
        Ok (Data _) -> Bool.true
        Ok Null -> Bool.false
        Err OutOfBounds -> Bool.false

hasRhs : Tree a, Nat -> Bool
hasRhs = \tree, idx ->
    when List.get tree (idx * 2 + 2) is
        Ok (Data _) -> Bool.true
        Ok Null -> Bool.false
        Err OutOfBounds -> Bool.false

getLhsIdx : Nat -> Nat
getLhsIdx = \idx -> idx * 2 + 1

getRhsIdx : Nat -> Nat
getRhsIdx = \idx -> idx * 2 + 2

getLhs : Tree a, Nat -> Result (Node a) [DoesNotExist]
getLhs = \tree, parentIdx ->
    when List.get tree (parentIdx * 2 + 1) is
        Ok (Data data) -> Ok (Data data)
        Ok Null -> Err DoesNotExist
        Err OutOfBounds -> Err DoesNotExist

getRhs : Tree a, Nat -> Result (Node a) [DoesNotExist]
getRhs = \tree, parentIdx ->
    when List.get tree (parentIdx * 2 + 2) is
        Ok (Data data) -> Ok (Data data)
        Ok Null -> Err DoesNotExist
        Err OutOfBounds -> Err DoesNotExist

isLeaf : Tree a, Nat -> Bool
isLeaf = \tree, idx ->
    !(hasLhs tree idx) && !(hasRhs tree idx)

insertLhs : Tree a, Nat, a -> Tree a
insertLhs = \tree, idx, val ->
    lhs = Data { val, idx: idx * 2 + 1 }
    when List.get tree (idx * 2 + 1) is
        Ok (Data _) -> List.set tree (idx * 2 + 1) lhs
        Ok Null -> List.set tree (idx * 2 + 1) lhs
        Err OutOfBounds ->
            if List.len tree == idx then
                List.append tree lhs
            else
                addEmptyLevel tree |> List.set (idx * 2 + 1) lhs

insertRhs : Tree a, Nat, a -> Tree a
insertRhs = \tree, idx, val ->
    lhs = Data { val, idx: idx * 2 + 2 }
    when List.get tree (idx * 2 + 2) is
        Ok (Data _) -> List.set tree (idx * 2 + 2) lhs
        Ok Null -> List.set tree (idx * 2 + 2) lhs
        Err OutOfBounds ->
            if List.len tree == idx then
                List.append tree lhs
            else
                addEmptyLevel tree |> List.set (idx * 2 + 2) lhs

insertLhsNode : Tree a, Nat, Node a -> Tree a
insertLhsNode = \tree, idx, node ->
    lhs =
        when node is
            Data data -> Data { val: data.val, idx: idx * 2 + 1 }
            Null -> Null
    when List.get tree (idx * 2 + 1) is
        Ok (Data _) -> List.set tree (idx * 2 + 1) lhs
        Ok Null -> List.set tree (idx * 2 + 1) lhs
        Err OutOfBounds ->
            if List.len tree == idx then
                List.append tree lhs
            else
                addEmptyLevel tree |> List.set (idx * 2 + 1) lhs

insertRhsNode : Tree a, Nat, Node a -> Tree a
insertRhsNode = \tree, idx, node ->
    rhs =
        when node is
            Data data -> Data { val: data.val, idx: idx * 2 + 2 }
            Null -> Null
    when List.get tree (idx * 2 + 2) is
        Ok (Data _) -> List.set tree (idx * 2 + 2) rhs
        Ok Null -> List.set tree (idx * 2 + 2) rhs
        Err OutOfBounds ->
            if List.len tree == idx then
                List.append tree rhs
            else
                addEmptyLevel tree |> List.set (idx * 2 + 2) rhs

deleteNode : Tree a, Nat -> Tree a
deleteNode = \tree, idx ->
    when List.get tree idx is
        Ok (Data _) ->
            treeLhsDeleted = deleteNode tree (getLhsIdx idx)
            treeRhsDeleted = deleteNode treeLhsDeleted (getRhsIdx idx)
            List.set treeRhsDeleted idx Null
        Ok Null -> tree
        Err OutOfBounds -> tree

insertNode : Tree a, Nat, Node a -> Tree a
insertNode = \tree, idx, node ->
    when List.get tree idx is
        Ok (Data _) ->
            when node is
                Data data -> List.set tree idx (Data { val: data.val, idx })
                Null ->
                    treeLhsDeleted = deleteNode tree (getLhsIdx idx)
                    treeRhsDeleted = deleteNode treeLhsDeleted (getRhsIdx idx)
                    List.set treeRhsDeleted idx node
        Ok Null -> List.set tree idx node
        Err OutOfBounds -> tree

## Add space for a new level to the tree
addEmptyLevel : Tree a -> Tree a
addEmptyLevel = \tree ->
    needed = arrLenForTreeWithNLevels (countTreeLevels tree + 1)
    diff = needed - List.len tree
    List.concat tree (List.repeat Null diff)

## Count the number of levels in a tree
countTreeLevels : Tree a -> Nat
countTreeLevels = \tree ->
    countLevelsRecur tree 0
countLevelsRecur : Tree a, Nat -> Nat
countLevelsRecur = \tree, level ->
    if List.len tree == 0 then
        level
    else
        countLevelsRecur (List.dropFirst tree (Num.powInt 2 level)) (level + 1)

arrLenForTreeWithNLevels : Nat -> Nat
arrLenForTreeWithNLevels = \n -> (Num.powInt 2 n) - 1

getParentIdx : Nat -> Nat
getParentIdx = \idx ->
    if idx == 0 then
        0
    else if Num.isOdd idx then
        (idx - 1) // 2
    else
        (idx - 2) // 2

getParentNode : Tree a, Nat -> Node a
getParentNode = \tree, idx ->
    List.get tree (getParentIdx idx)
    |> Result.withDefault Null

getDepthAtIdx : Nat -> Nat
getDepthAtIdx = \idx ->
    if idx == 0 then 0
    else getDepthAtIdx (getParentIdx idx) + 1

getFirstIdxAtDepth : Nat -> Nat
getFirstIdxAtDepth = \depth ->
    Num.powInt 2 depth - 1

getLastIdxAtDepth : Nat -> Nat
getLastIdxAtDepth = \depth ->
    Num.powInt 2 (depth + 1) - 2

getNonNullIndicesAtDepth : Tree a, Nat -> List Nat
getNonNullIndicesAtDepth = \tree, depth ->
    start = getFirstIdxAtDepth depth
    end = getLastIdxAtDepth depth
    len = end - start + 1
    List.sublist tree { start, len }
    |> List.keepIf
        (\node ->
            when node is
                Null -> Bool.false
                Data _ -> Bool.true
        )
    |> List.map
        (\node ->
            when node is
                Data { idx } -> idx
                Null -> crash "getNonNullIndicesAtDepth: Did not expect Null node here."
        )


## Create a tree from a list of strings where the first null in a branch truncates the branch
## Thus the children of a null node are not included in the list.
createTreeFromTruncatedStrList : List Str, (Str -> Result a *) -> Tree a
createTreeFromTruncatedStrList = \strList, strToVal ->
    when List.first strList is
        Ok strVal ->
            when strToVal strVal is
                Ok val ->
                    tree = createRoot val
                    createTreeFromTruncatedStrListRecur (List.dropFirst strList 1) strToVal 1 tree
                Err _ -> [Null]
        Err ListWasEmpty -> [Null]

createTreeFromTruncatedStrListRecur : List Str, (Str -> Result a *), Nat, Tree a -> Tree a
createTreeFromTruncatedStrListRecur = \strList, strToVal, depth, tree ->
    parentIndices = getNonNullIndicesAtDepth tree (depth - 1)
    if List.len parentIndices == 0 then
        tree
    else
        childStrs = List.takeFirst strList (List.len parentIndices * 2)
        newTree = List.walkWithIndex
            childStrs
            tree
            (\walkTree, strVal, i ->
                parentIdx = 
                    when List.get parentIndices (Num.divTrunc i 2) is
                        Ok idx -> idx
                        Err _ -> crash "createTreeFromTruncatedStrListRecur: Expected parent index to exist."
                when strToVal strVal is
                    Ok val ->
                        if Num.isEven i then
                            insertLhs walkTree parentIdx val
                        else
                            insertRhs walkTree parentIdx val
                    Err _ -> 
                        if Num.isEven i then
                            insertLhsNode walkTree parentIdx Null
                        else
                            insertRhsNode walkTree parentIdx Null
            )
        remainingStrs = List.dropFirst strList (List.len parentIndices * 2)
        createTreeFromTruncatedStrListRecur remainingStrs strToVal (depth + 1) newTree





