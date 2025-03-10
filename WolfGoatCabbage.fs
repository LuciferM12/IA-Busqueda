namespace Busqueda

module WolfGoatCabbage =
    type estado = int * int * int * int

    type accion = 
        | Lobo
        | Oveja
        | Col
        | Granjero

    
    let inicial = (1, 1, 1, 1)

    let meta estado = 
        estado = (0, 0, 0, 0)

    let costo _ _ _ = 1.0

//(Granjero, Lobo, Oveja, Col)
    let sucesores estado = 
        match estado with 
        | (1, 1, 1, 1) ->
            [
                Oveja, (0, 1, 0, 1)
            ]
        | (0, 1, 0, 1) -> 
            [
                Granjero, (1, 1, 0, 1)
                Oveja, (1, 1, 1, 1)
            ]
        | (1, 1, 0, 1) -> 
            [
                Granjero, (0, 1, 0, 1)
                Lobo, (0, 0, 0, 1)
                Col, (0, 1, 0, 0)
            ]
        | (0, 0, 0, 1) -> 
            [
                Lobo, (1, 1, 0, 1)
                Oveja, (1, 0, 1, 1)
            ]
        | (0, 1, 0, 0) -> 
            [
                Col, (1, 1, 0, 1)
                Oveja, (1, 1, 1, 0)
            ]
        | (1, 0, 1, 1) -> 
            [
                Col, (0, 0, 1, 0)
                Oveja, (0, 0, 0, 1)
            ]
        | (1, 1, 1, 0) -> 
            [
                Oveja, (0, 1, 0, 0)
                Lobo, (0, 0, 1, 0)
            ]
        | (0, 0, 1, 0) -> 
            [
                Granjero, (1, 0, 1, 0)
                Lobo, (1, 1, 1, 0)
                Col, (1, 0, 1, 1)
            ]
        | (1, 0, 1, 0) -> 
            [
                Oveja, (0, 0, 0, 0)
                Granjero, (0, 0, 1, 0)
            ]
        | _ -> []

    let problema inicial = 
        {
            inicial = inicial
            costo = costo
            meta = meta
            sucesores = sucesores
        }