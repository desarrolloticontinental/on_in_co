def var x-costos as dec.

output to c:\tmp\compras.txt.
for each almdmov where codcia = 001
    and codalm = '11'
    and tipmov = 'i'
    and (codmov = 02 or codmov = 17 or codmov = 06)
    and fchdoc >= 01/01/2003
    and fchdoc <= 12/31/2003
    no-lock,
    first almmmatg of almdmov no-lock where lookup(trim(catconta[1]), 'mp,e1,s1') > 0:
    x-costos = 0.
    find last almstkge where almstkge.codcia = 001
        and almstkge.codmat = almdmov.codmat
        and almstkge.fecha <= almdmov.fchdoc
        no-lock no-error.
    if available almstkge then x-costos = AlmStkge.CtoUni.
    display almmmatg.codmat desmat desmar tipmov codmov nrodoc fchdoc candes x-costos
        with stream-io no-box no-labels width 320.
end.
output close.
