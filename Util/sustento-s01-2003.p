def var x-s01 as dec.
def var x-i99 as dec.
def var x-i01 as dec.
def var x-totsal as dec.
def var x-toting as dec.
def var x-costo  as dec.

output to c:\tmp\sustento-s01-2006.txt.
for each almacen where almacen.codcia = 001 no-lock:
for each almdmov no-lock where codcia = 001
    and codalm = almacen.codalm
    and fchdoc >= 01/01/2006
    and fchdoc <= 12/31/2006
    and tipmov = 's' 
    and codmov = 01,
    first almmmatg of almdmov no-lock:
    assign
        x-s01 = 0
        x-i01 = 0
        x-i99 = 0
        x-totsal = 0
        x-toting = 0
        x-costo = 0.
    find last almstkge where almstkge.codcia = 001
        and almstkge.codmat = almdmov.codmat
        and almstkge.fecha = fchdoc 
        no-lock no-error.
    if available almstkge then x-costo = AlmStkge.CtoUni.
    if tipmov = 's'
    then assign
            x-totsal = candes * x-costo
            x-s01 = candes.
    else do:
        x-toting = candes * x-costo.
        if codmov = 01 then x-i01 = candes.
        if codmov = 99 then x-i99 = candes.
    end.
    display 
        almdmov.codmat
        desmat
        desmar
        undbas
        codalm
        nrodoc
        fchdoc
        candes
        tipmov 
        codmov
        x-costo
        x-s01
        x-i01 
        x-i99
        x-totsal
        x-toting
        with stream-io no-box width 320.
end.
for each almdmov no-lock where codcia = 001
    and codalm = almacen.codalm
    and fchdoc >= 01/01/2006
    and fchdoc <= 12/31/2006
    and tipmov = 'i' 
    and (codmov = 01 or codmov = 99),
    first almmmatg of almdmov no-lock:
    assign
        x-s01 = 0
        x-i01 = 0
        x-i99 = 0
        x-totsal = 0
        x-toting = 0
        x-costo = 0.
    find last almstkge where almstkge.codcia = 001
        and almstkge.codmat = almdmov.codmat
        and almstkge.fecha = fchdoc 
        no-lock no-error.
    if available almstkge then x-costo = AlmStkge.CtoUni.
    if tipmov = 's'
    then assign
            x-totsal = candes * x-costo
            x-s01 = candes.
    else do:
        x-toting = candes * x-costo.
        if codmov = 01 then x-i01 = candes.
        if codmov = 99 then x-i99 = candes.
    end.
    display 
        almdmov.codmat
        desmat
        desmar
        undbas
        codalm
        nrodoc
        fchdoc
        candes
        tipmov 
        codmov
        x-costo
        x-s01
        x-i01 
        x-i99
        x-totsal
        x-toting
        with stream-io no-box width 320.
end.
end.
output close.
        
