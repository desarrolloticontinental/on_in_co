def var x-fecha as date.
def temp-table detalle like almmmatg
    field stkact like almstkge.stkact
    field ctouni like almstkge.ctouni
    field fchvta as date.
    
x-fecha = date(04,30,2005).
for each almmmatg no-lock where codcia = 001:
    find last almstkge where almstkge.codcia = 001
        and almstkge.codmat = almmmatg.codmat
        and almstkge.fecha <= x-fecha
        no-lock no-error.
    if available almstkge
    then do:
        create detalle.
        buffer-copy almmmatg to detalle
            assign
                detalle.stkact = almstkge.stkact
                detalle.ctouni = almstkge.ctouni.
        find last almdmov use-index almd02 where almdmov.codcia = 001
            and almdmov.tipmov = 'S'
            and almdmov.codmov = 02     /* Ventas */
            and almdmov.codmat = almmmatg.codmat
            and almdmov.fchdoc <= x-fecha
            no-lock no-error.
        if available almdmov then detalle.fchvta = almdmov.fchdoc.
    end.
end.

output to c:\tmp\stock30042005.txt.
for each detalle by detalle.codcia by detalle.codfam by detalle.codmat:
    display detalle.codfam detalle.codmat detalle.desmat detalle.desmar
        detalle.stkact detalle.ctouni detalle.fchvta
        with stream-io no-labels width 320.
end.
output close.
