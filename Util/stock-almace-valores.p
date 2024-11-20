def var x-ctopro as dec.
def var x-ctorep as dec.

output to c:\tmp\cissac22.txt.
for each almmmate no-lock where codcia = 001 and codalm = '22' and stkact <> 0,
    first almmmatg of almmmate no-lock:

    x-ctopro = 0.
    FIND LAST AlmStkGe WHERE almstkge.codcia = 001
        AND almstkge.codmat = almmmatg.codmat
        AND almstkge.fecha <= TODAY
        NO-LOCK NO-ERROR.
    IF AVAILABLE AlmStkGe THEN x-ctopro = almstkge.ctouni.

    x-ctorep = Almmmatg.CtoLis.
    IF Almmmatg.MonVta = 2
    THEN x-ctorep = x-ctorep * Almmmatg.TpoCmb. 

    display 
        almmmatg.codmat     column-label 'Codigo'
        almmmatg.desmat     column-label 'Descripcion'
        desmar              column-label 'Marca'
        undbas              column-label 'Unidad'
        codfam              column-label 'Familia'
        subfam              column-label 'Subfamilia'
        x-ctopro            column-label 'Promedio'
        x-ctorep            column-label 'Reposicion'
        with stream-io no-box width 230.

end.
output close.
