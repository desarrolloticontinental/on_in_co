def temp-table detalle like almmmatg
    field ventas as dec
    field codven like evtvend.codven
    index llave01 as primary codcia codven codmat.

for each evtvend no-lock where EvtVend.CodCia = 001
    and (EvtVend.CodVen = '015' or EvtVend.CodVen = '073')
    and EvtVend.Nrofch >= 200712
    and EvtVend.Nrofch <= 200803,
    first almmmatg of evtvend no-lock:
    find detalle where detalle.codcia = evtvend.codcia
        and detalle.codven = evtvend.codven
        and detalle.codmat = evtvend.codmat
        exclusive-lock no-error.
    if not available detalle then create detalle.
    buffer-copy almmmatg to detalle
        assign detalle.codven = evtvend.codven.
    detalle.ventas = detalle.ventas + EvtVend.VtaxMesMn.
end.    

output to c:\tmp\vendedores.txt.
for each detalle:
    display
        detalle.codven
        detalle.codmat
        detalle.desmat
        detalle.desmar
        detalle.undbas
        detalle.codfam
        detalle.subfam
        detalle.ventas
        with stream-io no-box width 320.
end.
output close.
