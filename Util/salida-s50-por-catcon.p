def var x-costo as dec format '>>>,>>>,>>9.9999' decimals 4 no-undo.
def var x-total as dec format '>>>,>>>,>>9.99' no-undo.

output to c:\tmp\salida-s50-por-catcon.txt.
for each almdmov no-lock where codcia = 001
    and codalm = '12'
    and tipmov = 's'
    and codmov = 50
    and fchdoc >= 01/01/2003
    and fchdoc <= 12/31/2003,
    first almcmov of almdmov no-lock,
    first almmmatg of almdmov no-lock
    break by almdmov.codcia by catconta[1] by desmat by almdmov.fchdoc:
    assign
        x-costo = 0
        x-total = 0.
    find last almstkge where almstkge.codcia = 001
        and almstkge.codmat = almmmatg.codmat
        and almstkge.fecha <= almdmov.fchdoc
        no-lock no-error.
    if available almstkge then x-costo = AlmStkge.CtoUni.
    x-total = x-costo * almdmov.candes.
    accumulate x-total (sub-total by catconta[1]).
    accumulate x-total (total by almdmov.codcia).
    display 
        catconta[1] 
        almdmov.fchdoc 
        almdmov.nrodoc 
        almdmov.codmat
        desmat
        desmar
        undbas
        nrorf1
        candes
        x-costo
        x-total
        with stream-io no-box width 320.
    if last-of(catconta[1]) then do:
        underline
            x-total.
        display
            accum sub-total by catconta[1] x-total @ x-total.
        down 1.
    end.
    if last-of(almdmov.codcia) then do:
        underline
            x-total.
        display
            accum total by almdmov.codcia x-total @ x-total.
        down 1.
    end.
end.
output close.
