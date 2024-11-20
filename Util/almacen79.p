def var x-candes as dec.
def var x-ctouni as dec.
def temp-table detalle like almmmatg
    field candes  as dec
    field impprom as dec
    field imprepo as dec.
    
for each almdmov where codcia = 1 and codalm = '79'
    and tipmov = 'i' and fchdoc >= date(12,01,2004) no-lock,
    first almmmatg of almdmov no-lock:
    find detalle of almmmatg exclusive-lock no-error.
    if not available detalle 
    then do:
        create detalle.
        buffer-copy almmmatg to detalle.
    end.
    assign
        x-candes = almdmov.candes * almdmov.factor
        detalle.candes = detalle.candes + x-candes.
    FIND LAST AlmStkGe WHERE almstkge.codcia = 001
        AND almstkge.codmat = almmmatg.codmat
        AND almstkge.fecha <= almdmov.fchdoc
        NO-LOCK NO-ERROR.
    IF AVAILABLE AlmStkGe 
    THEN x-CtoUni = almstkge.ctouni.
    ELSE x-CtoUni = 0.
    assign
        detalle.impprom = detalle.impprom + x-candes * x-ctouni.
    x-CtoUni = Almmmatg.CtoLis.
    IF Almmmatg.MonVta = 2
    THEN x-CtoUni = x-CtoUni * Almmmatg.TpoCmb. 
    assign
        detalle.imprepo = detalle.imprepo + x-candes * x-ctouni.
end.
output to c:\tmp\almacen79.txt.
for each detalle:
    display detalle.codmat detalle.desmat detalle.desmar detalle.undbas detalle.candes detalle.impprom detalle.imprepo
        with stream-io no-labels no-box width 250.
end.
output close.
