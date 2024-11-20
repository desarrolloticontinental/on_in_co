def var x-stkval as dec.
def var x-items as int init 0.

output to c:\tmp\valorxlinea.txt.
for each almmmatg no-lock where codcia = 001
    and tpoart <> 'D'
    break by codfam:
    x-stkval = 0.
    find last almstkge where almstkge.codcia = almmmatg.codcia
        and almstkge.codmat = almmmatg.codmat
        and almstkge.fecha <= date(07,31,2005)
        no-lock no-error.
    if available almstkge then x-stkval = AlmStkge.StkAct * AlmStkge.CtoUni.
    accumulate x-stkval (TOTAL BY codfam).
    x-items = x-items + 1.
    if last-of (codfam)
    then do:
        display codfam x-items accum total by codfam x-stkval format '>>>,>>>,>>9.99'.
        x-items = 0.
    end.
end.
output close.
