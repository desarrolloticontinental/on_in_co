def var x-stkact as dec.
def var x-item as int init 0.

for each almmmatg where codcia = 001 
    and codfam = '008'
    and tpoart <> 'D':
    x-stkact = 0.
    find last almstkge where almstkge.codcia = almmmatg.codcia
        and almstkge.codmat = almmmatg.codmat
        and almstkge.fecha <= date(07,21,2005)
        no-lock no-error.
    if available almstkge then x-stkact = AlmStkge.StkAct.
    if x-stkact > 0 then next.
    find first almdmov use-index almd02 where almdmov.codcia = almmmatg.codcia
        and almdmov.codmat = almmmatg.codmat
        and almdmov.fchdoc > date(05,30,2005)
        no-lock no-error.
    if available almdmov then next.
    x-item = x-item + 1.
    /*lmmmatg.tpoart = 'D'.*/
    /*display almmmatg.codmat codfam desmat with stream-io no-labels.*/
end.
display x-item.    
