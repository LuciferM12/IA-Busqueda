namespace Busqueda

module ChangoPlatano =

    type estado = (int * int) * (int * int) * bool * int


    type accion = 
        | MoverMono of int * int      
        | MoverCaja of int * int      
        | SubirCaja                   
        | BajarCaja                   
        | TomarPlatano                


    let inicial = ((1, 1), (3, 1), false, 0)


    let posicionPlatano = (5, 1)


    let meta (_, _, tienePlatano, _) = 
        tienePlatano


    let costo _ _ _ = 1.0


    let sucesores (posicionMono, posicionCaja, tienePlatano, altura) =
        if tienePlatano then
            
            []
        else
            let (monoX, monoY) = posicionMono
            let (cajaX, cajaY) = posicionCaja
            let (platanoX, platanoY) = posicionPlatano
            
            
            let posiblesAcciones = [
                
                if altura = 0 then
                    
                    for dx in [-1..1] do
                        for dy in [-1..1] do
                            if not (dx = 0 && dy = 0) then
                                let nuevoX = monoX + dx
                                let nuevoY = monoY + dy
                                
                                if nuevoX >= 0 && nuevoX <= 10 && nuevoY >= 0 && nuevoY <= 10 then
                                    yield MoverMono(nuevoX, nuevoY), ((nuevoX, nuevoY), posicionCaja, false, 0)
                
                
                if altura = 0 && abs(monoX - cajaX) + abs(monoY - cajaY) = 1 then
                    
                    let difX = cajaX - monoX
                    let difY = cajaY - monoY
                    
                    let nuevaCajaX = cajaX + difX
                    let nuevaCajaY = cajaY + difY
                    
                    if nuevaCajaX >= 0 && nuevaCajaX <= 10 && nuevaCajaY >= 0 && nuevaCajaY <= 10 then
                        yield MoverCaja(nuevaCajaX, nuevaCajaY), ((cajaX, cajaY), (nuevaCajaX, nuevaCajaY), false, 0)
                
                
                if altura = 0 && posicionMono = posicionCaja then
                    yield SubirCaja, (posicionMono, posicionCaja, false, 1)
                
                
                if altura = 1 then
                    yield BajarCaja, (posicionMono, posicionCaja, false, 0)
                
                
                if altura = 1 && posicionMono = posicionCaja && posicionCaja = (platanoX, platanoY) then
                    yield TomarPlatano, (posicionMono, posicionCaja, true, 1)
            ]
            
            posiblesAcciones

    
    let problema inicial = 
        {
            inicial = inicial
            meta = meta
            costo = costo
            sucesores = sucesores
        }