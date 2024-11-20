def var x-candes as dec.
def var x-unirep as dec.
def var x-unipro as dec.

output to c:\tmp\almacen11.txt.
for each almdmov where codcia = 1 and codalm = '11'
    and tipmov = 's' and codmov = 05
    and fchdoc >= date(12,01,2004) no-lock,
    first almmmatg of almdmov no-lock:
    x-candes = almdmov.candes * almdmov.factor.
    FIND LAST AlmStkGe WHERE almstkge.codcia = 001
        AND almstkge.codmat = almmmatg.codmat
        AND almstkge.fecha <= almdmov.fchdoc
        NO-LOCK NO-ERROR.
    IF AVAILABLE AlmStkGe 
    THEN x-UniPro = almstkge.ctouni.
    ELSE x-UniPro = 0.
    x-UniRep = Almmmatg.CtoLis.
    IF Almmmatg.MonVta = 2
    THEN x-UniRep = x-UniRep * Almmmatg.TpoCmb. 
    display almdmov.fchdoc almdmov.nroser almdmov.nrodoc
        almdmov.codmat almmmatg.desmat almmmatg.desmar
        almmmatg.undbas x-candes x-unirep x-unipro
        with stream-io no-box no-labels width 250.
end.
output close.
