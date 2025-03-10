//Problema
open Busqueda
open Capitulo3
open BFS
//open DFS
//open OchoCasillas
open VacuumWorld

//busqueda_arbol (problema inicial2) estrategia
//busqueda_arbol (problema inicial2) (CostoUniforme.estrategia (fun n -> n.g, n.estado))
//busqueda_arbol (problema inicial2) (DFSL.estrategia 2)
//IDFSL.IDFSL (problema inicial2)

//busqueda_arbol problema estrategia
//busqueda_grafo (problema inicial) estrategia key
//busqueda_grafo (problema inicial) DFS.estrategia DFS.key
//busqueda_grafo (problema inicial) (DFSL.estrategia 23) DFSL.key
//busqueda_grafo (problema inicial) CostoUniforme.estrategia CostoUniforme.key
IDFSL.IDFSL (problema inicial2)

|> Option.map acciones
|> Option.map (fun xs -> List.length xs, xs)
|> printf "%A"
