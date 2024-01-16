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
        getValAtIdx,
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

Node a : [Data { val : a, idx : Nat }, Null]
Tree a : List (Node a)

getNodeVal : Node a -> Result a [DoesNotExist]
getNodeVal = \node ->
    when node is
        Data { val } -> Ok val
        Null -> Err DoesNotExist

getValAtIdx : Tree a, Nat -> Result a [DoesNotExist]
getValAtIdx = \tree, idx ->
    when List.get tree idx is
        Ok node -> getNodeVal node
        Err OutOfBounds -> Err DoesNotExist

createRoot : a -> Tree a
createRoot = \val -> [Data { val, idx: 0 }]

createTreeFromStrList : List Str, (Str -> Result a *) -> Tree a
createTreeFromStrList = \strList, strToVal ->
    List.mapWithIndex strList
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
        Ok Null -> List.set tree idx lhs
        Err OutOfBounds ->
            if List.len tree == idx then
                List.append tree lhs
            else
                addEmptyLevel tree |> List.set (idx * 2 + 1) lhs

insertRhs : Tree a, Nat, a -> Tree a
insertRhs = \tree, idx, val ->
    lhs = Data { val, idx: idx * 2 + 2 }
    when List.get tree (idx * 2 + 1) is
        Ok (Data _) -> List.set tree idx lhs
        Ok Null -> List.set tree (idx * 2 + 1) lhs
        Err OutOfBounds ->
            if List.len tree == idx then
                List.append tree lhs
            else
                addEmptyLevel tree |> List.set (idx * 2 + 2) lhs

insertLhsNode : Tree a, Nat, Node a -> Tree a
insertLhsNode = \tree, idx, node ->
    lhs = when node is
        Data data -> Data { val: data.val, idx: idx * 2 + 1 }
        Null -> Null
    when List.get tree (idx * 2 + 1) is
        Ok (Data _) -> List.set tree (idx * 2 + 1) lhs
        Ok Null -> List.set tree idx lhs
        Err OutOfBounds ->
            if List.len tree == idx then
                List.append tree lhs
            else
                addEmptyLevel tree |> List.set (idx * 2 + 1) lhs

insertRhsNode : Tree a, Nat, Node a -> Tree a
insertRhsNode = \tree, idx, node ->
    rhs = when node is
        Data data -> Data { val: data.val, idx: idx * 2 + 2 }
        Null -> Null
    when List.get tree (idx * 2 + 1) is
        Ok (Data _) -> List.set tree idx rhs
        Ok Null -> List.set tree (idx * 2 + 1) rhs
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
