namespace Busqueda
//namespace Cola
//Representar una cola mediante dos lista
// elementos se insertan al final de una lista
// (frente, trasera)

module Cola = 
    let vacia = [], []
    let enqueue (frente, trasera) n = (frente, n :: trasera)
    let rec dequeue = function 
        | (n :: frente, trasera) -> Some (n, (frente, trasera))
        | ([], n :: trasera) -> 
            dequeue (List.rev (n :: trasera), [])
        | ([], []) -> None