namespace Busqueda 

module VacuumWorld =
    type estado = (int * int) * (int * int)

    type accion = 
        | Left
        | Right
        | Suck
    
    let inicial = ((1, 0), (0, 1))

    let inicial2 = ((0, 1), (1, 1))

    let metas = [((1, 0), (0, 0)); ((0, 0), (1, 0))]

    let meta estado = 
        List.contains estado metas
    
    let costo _ _ _ = 1.0
    
    let sucesores estado =
        match estado with
        | ((1, 1), (0, 1)) -> 
            [Suck, ((1, 0), (0, 1))
             Right, ((0, 1), (1, 1))
            ]
        | ((1, 1),(0, 0)) -> 
            [Suck, ((1, 0), (0, 0))
             Right, ((0, 1), (1, 0))
            ]
        | ((1, 0), (0, 1)) ->
            [Right, ((0, 0), (1, 1))
            ]  
        | ((0, 1), (1, 1)) -> 
            [Suck, ((0, 1), (1, 0))
             Left, ((1, 1), (0, 1))
            ]
        | ((0, 1), (1, 0)) -> 
            [Left, ((1, 1), (0, 0))
            ]
        | ((0, 0), (1, 1)) -> 
            [Suck, ((0, 0), (1, 0))
            ]
        | _ -> []

    let problema inicial = 
        {
            inicial = inicial
            costo = costo
            meta = meta
            sucesores = sucesores
        }