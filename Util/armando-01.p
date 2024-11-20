def temp-table detalle like almmmatg
    field codven like gn-ven.codven
    field nomven like gn-ven.nomven
    field canti  as dec extent 12 format '->>>>,>>>.99'
    index llave01 codcia codmat codven.


for each evtvend no-lock where codcia = 001
    and coddiv = '00000'
    and EvtVend.Codano = 2007,
    first almmmatg of evtvend no-lock,
    first gn-ven of evtvend no-lock:
    find detalle where detalle.codcia = evtvend.codcia
        and detalle.codmat = evtvend.codmat
        and detalle.codven = evtvend.codven
        exclusive-lock no-error.
    if not available detalle then do:
        create detalle.
        buffer-copy almmmatg to detalle
            assign
                detalle.codven = evtvend.codven
                detalle.nomven = gn-ven.nomven.
    end.    
    assign
        detalle.canti[EvtVend.Codmes] = detalle.canti[EvtVend.Codmes] +
                                            EvtVend.CanxMes.
end.        

output to c:\tmp\armando.txt.
for each detalle no-lock:
    display
        detalle.codmat
        detalle.desmat
        detalle.desmar
        detalle.undbas
        detalle.codven
        detalle.nomven
        detalle.canti
        with stream-io no-box width 320.
end.
output close.
