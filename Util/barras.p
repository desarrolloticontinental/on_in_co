def temp-table detalle like almmmatg
    field barras13 as char format 'x(15)'
    field barras14 as char format 'x(15)'.
    

for each almmmatg no-lock where codcia = 001 and codbrr <> '':
    create detalle.
    buffer-copy almmmatg to detalle
        assign detalle.barras13 = almmmatg.codbrr.
end.    

for each almmmat1 no-lock where almmmat1.codcia = 001,
        first almmmatg of almmmat1 no-lock:
    find detalle of almmmat1 exclusive-lock no-error.
    if not available detalle then do:
        create detalle.
        buffer-copy almmmatg to detalle.
    end.
    assign
        detalle.barras14 = almmmat1.barras[1].
end.

output to c:\tmp\barras.txt.
for each detalle:
    display 
        detalle.codmat
        detalle.desmat
        detalle.barras13
        detalle.barras14
        with stream-io no-box width 100.
end.
output close.
