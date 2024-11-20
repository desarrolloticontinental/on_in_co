def var x-ctouni as dec no-undo.

output to c:\tmp\movs2005.txt.
for each almacen no-lock where almacen.codcia = 001:
for each almdmov no-lock where codcia = 001
    and codalm = almacen.codalm
    and tipmov = 's'
    and fchdoc >= 01/01/2005
    and fchdoc <= 12/31/2005,
    first almmmatg of almdmov no-lock:
    if tipmov = 's' and codmov = 02 then next.
    if codmov = 03 then next.    
    x-ctouni = 0.
    find last almstkge where almstkge.codcia = 001
        and almstkge.codmat = almdmov.codmat
        and almstkge.fecha <= almdmov.fchdoc
        no-lock no-error.
    if available almstkge then x-ctouni = AlmStkge.CtoUni.
    display 
        almmmatg.codmat     column-label 'codigo'
        almmmatg.desmat     column-label 'descripcion'
        almmmatg.desmar     column-label 'marca'
        almmmatg.undbas     column-label 'unidad'
        almdmov.codalm     column-label 'almacen'
        almdmov.tipmov     column-label 'tipo'
        almdmov.codmov     column-label 'mov'
        almdmov.nrodoc     column-label 'numero'
        almdmov.fchdoc     column-label 'fecha'
        (almdmov.candes * almdmov.factor)        column-label 'cantidad'
        x-ctouni     column-label 'unitario'
        (almdmov.candes * almdmov.factor * x-ctouni)      column-label 'total'
        with stream-io no-box no-underline width 320.
end.
end.
output close.
