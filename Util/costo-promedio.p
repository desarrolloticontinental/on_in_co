def var x-procom as dec no-undo.
def temp-table detalle like almmmatg
    field ctopro as dec
    field procom as dec.


for each almmmatg no-lock where codcia = 001
        and lookup(trim(catconta[1]), 'MP,PP,PT') > 0
        and tpoart <> 'D':
    display codmat.
    pause 0.
    create detalle.
    buffer-copy almmmatg to detalle.
    find last almstkge where almstkge.codcia = 001
        and almstkge.codmat = almmmatg.codmat
        no-lock no-error.
    if available almstkge then detalle.ctopro = almstkge.ctouni.
    for each almdmov no-lock where codcia = 001
            and (codalm = '11' or codalm = '85')
            and tipmov = 'i'
            and (codmov = 02 or codmov = 17)
            and codmat = almmmatg.codmat
            and fchdoc >= 12/01/2006:
        if almdmov.codmon = 1
        then x-procom = almdmov.preuni.
        else x-procom = almdmov.preuni * almdmov.tpocmb.
        detalle.procom = maximum(detalle.procom, x-procom).
    end.            
end.        

output to c:\tmp\promedio.txt.
for each detalle:
    display
        detalle.codmat
        detalle.catconta[1]
        detalle.desmat
        detalle.desmar
        detalle.undbas
        detalle.ctopro
        detalle.procom
        with stream-io no-box width 320.
end.
output close.
