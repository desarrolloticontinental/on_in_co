def var x-s66 as dec.
def var x-i66 as dec.
def var x-totsal as dec.
def var x-toting as dec.
def var x-costo  as dec.

output to c:\tmp\sustento-s66-2003.txt.
for each almacen where almacen.codcia = 001 no-lock:
    for each almdmov no-lock where codcia = 001
        and codalm = almacen.codalm
        and fchdoc >= 01/01/2003
        and fchdoc <= 12/31/2003
        and tipmov = 's' 
        and codmov = 66,
        first almmmatg of almdmov no-lock:
        assign
            x-s66 = 0
            x-i66 = 0
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
                x-s66 = candes.
        else do:
            x-toting = candes * x-costo.
            x-i66 = candes.
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
            x-s66
            x-i66 
            x-totsal
            x-toting
            with stream-io no-box width 320.
    end.
    for each almdmov no-lock where codcia = 001
        and codalm = almacen.codalm
        and fchdoc >= 01/01/2003
        and fchdoc <= 12/31/2003
        and tipmov = 'i' 
        and codmov = 66,
        first almmmatg of almdmov no-lock:
        assign
            x-s66 = 0
            x-i66 = 0
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
                x-s66 = candes.
        else do:
            x-toting = candes * x-costo.
            x-i66 = candes.
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
            x-s66
            x-i66 
            x-totsal
            x-toting
            with stream-io no-box width 320.
    end.
end.

output close.
        
