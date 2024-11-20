def temp-table t-mov like almdmov.
def var x-costo as dec.
def var x-i13 as dec.
def var x-i15 as dec.
def var x-s13 as dec.
def var x-stock as dec.

for each almacen no-lock where almacen.codcia = 001
    and lookup(almacen.codalm, '03,03a,04,04a,05,05a,11,17,83,300') > 0:
    for each almcmov no-lock where codcia = 001
        and codalm = almacen.codalm
        and fchdoc >= 01/01/2003
        and fchdoc <= 12/31/2003
        and tipmov = 'i'
        and (codmov = 13 or codmov = 15),
        each almdmov of almcmov no-lock:
            create t-mov.
            buffer-copy almdmov to t-mov.
    end.
end.

for each almacen no-lock where almacen.codcia = 001
    and lookup(almacen.codalm, '03,03a,04,04a,05,05a,11,17,83,300') > 0:
    for each almcmov no-lock where codcia = 001
        and codalm = almacen.codalm
        and fchdoc >= 01/01/2003
        and fchdoc <= 12/31/2003
        and tipmov = 's'
        and codmov = 13,
        each almdmov of almcmov no-lock:
            create t-mov.
            buffer-copy almdmov to t-mov.
    end.
end.

output to c:\tmp\reclasificacion-sunat.txt.
for each t-mov,
    first almmmatg of t-mov no-lock 
    by t-mov.codalm by t-mov.fchdoc:
    x-costo = 0.
    x-stock = 0.
    find last almstkge where almstkge.codcia = 001
        and almstkge.codmat = t-mov.codmat
        and almstkge.fecha <= t-mov.fchdoc
        no-lock no-error.
    if available almstkge 
    then assign
            x-costo = almstkge.ctouni.
    find last almstkal where almstkal.codcia = 001
        and almstkal.codmat = t-mov.codmat
        and almstkal.codalm = t-mov.codalm
        and almstkal.fecha <= 12/31/2003
        no-lock no-error.
    if available almstkal 
    then assign
            x-stock = AlmStkal.StkAct.
    display 
        t-mov.codalm
        t-mov.fchdoc 
        t-mov.nrodoc 
        t-mov.codmat
        almmmatg.desmat 
        almmmatg.desmar 
        almmmatg.undbas
        x-costo
        t-mov.candes
        t-mov.candes when (t-mov.tipmov = 'i' and t-mov.codmov = 13) @ x-i13
        t-mov.candes when (t-mov.tipmov = 'i' and t-mov.codmov = 15) @ x-i15
        t-mov.candes when (t-mov.tipmov = 's' and t-mov.codmov = 13) @ x-s13
        x-stock
        with stream-io width 320.
end.
output close.
