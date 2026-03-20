open System

// Определение бинарного дерева
type BinaryTree<'T> =
    | Empty
    | Node of 'T * BinaryTree<'T> * BinaryTree<'T>

module TreeTasks =
    
    // Задание 1: Замена каждого символа в строке на следующий по порядку
    let shiftString (s: string) : string =
        s |> Seq.map (fun c -> 
            if c = 'я' then 'а'
            elif c = 'Я' then 'А'
            elif c = 'z' then 'a'
            elif c = 'Z' then 'A'
            else char (int c + 1))
        |> String.Concat
    
    // Функция map для дерева - применяет функцию к каждому узлу
    let rec mapTree (f: 'T -> 'U) (tree: BinaryTree<'T>) : BinaryTree<'U> =
        match tree with
        | Empty -> Empty
        | Node(value, left, right) ->
            Node(f value, mapTree f left, mapTree f right)
    
    // Задание 2: Поиск узлов с двумя листьями через fold с информацией о структуре
    let findNodesWithTwoLeaves (tree: BinaryTree<'T>) : 'T list =
        let rec foldTree f acc tree =
            match tree with
            | Empty -> (acc, true)
            | Node(v, left, right) ->
                // Обрабатываем левое поддерево
                let (leftAcc, leftIsLeaf) = foldTree f acc left
                // Обрабатываем правое поддерево, передавая аккумулятор от левого
                let (rightAcc, rightIsLeaf) = foldTree f leftAcc right
                // Текущий узел обрабатывается с результатами обоих поддеревьев
                let newAcc = f v leftIsLeaf rightIsLeaf rightAcc
                (newAcc, false)
    
        let folder v leftIsLeaf rightIsLeaf acc =
            if leftIsLeaf && rightIsLeaf then
                v :: acc  // Просто добавляем текущий узел к накопленному результату
            else
                acc      // Иначе просто передаем аккумулятор дальше
    
        let (result, _) = foldTree folder [] tree
        result

// Функция для создания дерева из списка (обход в ширину)
let rec buildTreeFromList (list: 'T list) (index: int) : BinaryTree<'T> =
    if index >= list.Length then Empty
    else
        let leftIndex = 2 * index + 1
        let rightIndex = 2 * index + 2
        Node(list.[index], 
            buildTreeFromList list leftIndex,
            buildTreeFromList list rightIndex)

// Функция для ввода дерева с клавиатуры
let inputTree () =
    printfn "Введите строки для узлов через пробел (обход в ширину):"
    let input = Console.ReadLine()
    let values = 
        input.Split(' ')
        |> Array.filter (fun s -> s <> "")
        |> Array.toList
    
    if values.IsEmpty then
        Empty
    else
        buildTreeFromList values 0

// Функция для вывода дерева 
let rec printTree (tree: BinaryTree<string>) (level: int) =
    match tree with
    | Empty -> ()
    | Node(value, left, right) ->
        printTree right (level + 1)
        printfn "%s%s" (String(' ', level * 3)) value
        printTree left (level + 1)

[<EntryPoint>]
let main argv =
    // Задание 1
    printfn "=== ЗАДАНИЕ 1: Замена символов в строках дерева (map) ==="
    printfn "Создайте дерево для первого задания:"
    let stringTree = inputTree()
    
    printfn "\nИсходное дерево:"
    printTree stringTree 0
    
    let shiftedTree = TreeTasks.mapTree TreeTasks.shiftString stringTree
    
    printfn "\nДерево после замены символов на следующие:"
    printTree shiftedTree 0
    printfn ""
    
    // Задание 2
    printfn "=== ЗАДАНИЕ 2: Поиск узлов с двумя листьями (fold) ==="
    printfn "Создайте дерево для второго задания:"
    let testTree = inputTree()
    
    printfn "\nВаше дерево:"
    printTree testTree 0
    
    let nodesWithTwoLeaves = TreeTasks.findNodesWithTwoLeaves testTree
    
    printfn "\nУзлы, у которых оба потомка - листья:"
    if nodesWithTwoLeaves.IsEmpty then
        printfn "Таких узлов не найдено"
    else
        printfn "%A" nodesWithTwoLeaves
    
    0