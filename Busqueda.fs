namespace Busqueda
//variables de tipo
// 's: estado, 'a: accion, 'b: bolsa

// Problema
type problema<'s, 'a> = {
    inicial : 's
    sucesores : 's -> list<'a * 's>
    meta: 's -> bool
    costo: 's -> 'a -> 's -> float
}

// type 'a option = None | Some of 'a
//option<nodo<'s,'a>

//nodo
type nodo<'s, 'a> = {
    profundidad : int
    g : float 
    estado : 's 
    padre : nodo<'s,'a> option
    accion : option<'a>
}

// estrategia

type estrategia<'s, 'a, 'b> = {
    insertar : 'b -> nodo<'s, 'a> -> 'b
    sacar : 'b -> option<nodo<'s, 'a> * 'b>
    vacia : 'b
}

module Capitulo3 = 
    let hacer_nodo estado = // 'a -> nodo<'a, 'b>
        {
            padre = None
            accion = None
            profundidad = 0
            estado = estado
            g = 0.0
        }

    let expand nodo problema =
        problema.sucesores nodo.estado
        |> List.map (fun (accion, estado) -> 
        {
            padre = Some nodo
            accion = Some accion
            estado = estado
            profundidad = nodo.profundidad + 1
            g = nodo.g + problema.costo nodo.estado accion estado

        })
    
    let busqueda_arbol problema estrategia = 
        let bolsa = 
            hacer_nodo problema.inicial
            |> estrategia.insertar estrategia.vacia
        let rec loop bolsa = 
            match estrategia.sacar bolsa with
            | Some (n, bolsa) ->
                if problema.meta n.estado
                then Some n 
                else expand n problema
                    |> List.fold estrategia.insertar bolsa
                    |> loop
            | None -> None
        loop bolsa

    let busqueda_grafo problema estrategia key = 
        let bolsa = 
            hacer_nodo problema.inicial
            |> estrategia.insertar estrategia.vacia
        let rec loop (visitados, bolsa) = 
            match estrategia.sacar bolsa with
            | Some (n, bolsa) ->
                if problema.meta n.estado
                then Some n 
                else if Set.contains (key n) visitados
                     then loop (visitados, bolsa)
                     else
                        expand n problema
                            |> List.fold estrategia.insertar bolsa
                            |> (fun bolsa -> loop (Set.add (key n) visitados, bolsa))
            | None -> None
        loop (Set.empty, bolsa)

    
    let rec acciones n = 
        match n.padre, n.accion with
        | Some p, Some a -> acciones p @ [a]
        | _ -> []