def var x-ctotot as dec decimals 4 no-undo.
def var x-prevta as dec no-undo.

output to c:\tmp\ms50.txt.
for each almacen where codcia = 001 no-lock:
for each almdmov no-lock where codcia = 001
    and codalm = almacen.codalm
    and tipmov = 's'
    and codmov = 50
    and fchdoc >= 04/01/2006
    and fchdoc <= 06/30/2006,
    first almmmatg of almdmov no-lock,
    first almcmov of almdmov no-lock:
    x-ctotot = 0.
    find last almstkge where almstkge.codcia = 001
        and almstkge.codmat = almdmov.codmat
        and almstkge.fecha <= almdmov.fchdoc
        no-lock no-error.
    if available almstkge then x-ctotot = almstkge.ctouni.
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
        x-ctotot    column-label 'prom. unit.'  format '->>>,>>9.9999'
    with stream-io no-box width 320. 
end.
end.
output close.
