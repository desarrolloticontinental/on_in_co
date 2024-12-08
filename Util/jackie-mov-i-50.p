def var x-ctotot as dec no-undo.
def var x-prevta as dec no-undo.

output to c:\tmp\mi50.txt.
for each almacen where codcia = 001 no-lock:
for each almdmov no-lock where codcia = 001
    and codalm = almacen.codalm
    and tipmov = 'i'
    and codmov = 50
    and fchdoc >= 04/01/2006
    and fchdoc <= 06/30/2006,
    first almmmatg of almdmov no-lock,
    first almcmov of almdmov no-lock:
    x-ctotot = Almmmatg.ctotot.
    x-prevta = almmmatg.prevta[1].
    if x-prevta = 0 then x-prevta = almmmatg.preofi.
    if almmmatg.monvta = 2 then x-ctotot = x-ctotot * almmmatg.tpocmb.
    if almmmatg.monvta = 2 then x-prevta = x-prevta * almmmatg.tpocmb.
    display 
        almdmov.nrodoc 
        almdmov.codalm 
        almdmov.fchdoc 
        nrorf1 
        almdmov.codmat 
        desmat 
        codfam 
        subfam 
        desmar 
        (candes * factor)   column-label 'cantidad'
        undbas 
        almmmatg.pesmat 
        x-ctotot    column-label 'costo unit'
        x-prevta    column-label 'precio unit'
    with stream-io no-box width 320. 
end.
end.
output close.
