namespace Busqueda

module PuenteTambaleante =
    type estado = int list * int list * int * int

    type accion = 
        | Cruzar of int list


    let inicial = ([1; 2; 5; 8], [], 1, 0)

    let meta (inicio, fin, _, _) = 
        List.isEmpty inicio

    let costo _ _ (_, _, _, tiempoNuevo) = 
        float tiempoNuevo

    let sucesores (inicio, fin, antorcha, tiempo) =

        let (origen, destino) = 
            if antorcha = 1 then (inicio, fin)
            else (fin, inicio)
        

        let individuos = 
            origen |> List.map (fun p -> [p])
        
        let parejas = 
            [for p1 in origen do
                for p2 in origen do
                    if p1 < p2 then yield [p1; p2]]
        
        let candidatos = 
            if List.length origen >= 2 then individuos @ parejas
            else individuos
        
        candidatos
        |> List.map (fun grupo ->
            let nuevoOrigen = origen |> List.filter (fun p -> not (List.contains p grupo))
            let nuevoDestino = destino @ grupo
            
            let tiempoCruce = grupo |> List.max
            

            let nuevoEstado =
                if antorcha = 1 then
                    (nuevoOrigen, nuevoDestino, 0, tiempo + tiempoCruce)
                else
                    (nuevoDestino, nuevoOrigen, 1, tiempo + tiempoCruce)
                    
            (Cruzar grupo, nuevoEstado))

    
    let problema inicial = 
        {
            inicial = inicial
            meta = meta
            costo = costo
            sucesores = sucesores
        }