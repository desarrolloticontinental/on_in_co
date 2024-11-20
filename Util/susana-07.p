def temp-table detalle like almmmatg.


for each almmmatg where codcia = 001 no-lock:
    find last almstkal where almstkal.codcia = 001
        and almstkal.codmat = almmmatg.codmat
        and codalm = '16'
        and fecha <= 07/31/2006
        no-lock no-error.
    if not available almstkal or stkact = 0 then next.
    create detalle.
    buffer-copy almmmatg to detalle.
end.

def var x-item as int.

output to c:\tmp\resumen.txt.
for each detalle,
        first almsfami of detalle no-lock
        break by detalle.codfam by detalle.subfam by detalle.desmar:
    if first-of(detalle.subfam) then x-item = 0.
    if last-of(detalle.desmar) then do:
        x-item = x-item + 1.
        display
            x-item
            detalle.codfam
            detalle.subfam
            AlmSFami.dessub
            detalle.desmar
            with stream-io no-box width 320.
    end.
    if last-of(detalle.subfam) then down 2.
end.
output close.
