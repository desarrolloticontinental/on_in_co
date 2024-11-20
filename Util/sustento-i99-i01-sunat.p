def var x-costo as dec.
def var x-total as dec.

output to c:\tmp\sustento-i99-i01.txt.
for each almdmov no-lock where codcia = 001
    and tipmov = 'i'
    and (codmov = 99 or codmov = 01)
    and fchdoc >= 01/01/2003
    and fchdoc <= 12/31/2003,
    first almmmatg of almdmov no-lock:
    x-costo = 0.
    find last almstkge where almstkge.codcia = 001
        and almstkge.codmat = almdmov.codmat
        and almstkge.fecha <= almdmov.fchdoc
        no-lock no-error.
    if available almstkge then x-costo = AlmStkge.CtoUni.
    x-total = candes * x-costo.
    display 
        codalm
        tipmov 
        codmov
        fchdoc
        nrodoc
        almmmatg.codmat
        almmmatg.desmat
        almmmatg.desmar
        almmmatg.undbas
        almdmov.candes
        x-costo
        x-total
        with stream-io no-labels no-box width 320.
end.
output close.
