def var x-ctouni like almstkge.ctouni.

output to c:\tmp\mov-s03.txt.
for each almacen no-lock where codcia = 001:
    for each almdmov no-lock where codcia = 001
            and codalm = almacen.codalm
            and tipmov = 'S'
            and codmov = 03
            and fchdoc >= 01/01/2006
            and fchdoc <= 12/31/2006,
            first almcmov of almdmov no-lock where codref = 'op',
            first almmmatg of almdmov no-lock:
        x-ctouni = 0.
        find last almstkge where almstkge.codcia = almdmov.codcia
            and almstkge.codmat = almdmov.codmat
            and almstkge.fecha <= almdmov.fchdoc
            no-lock no-error.
        if available almstkge then x-ctouni = AlmStkge.CtoUni.
        display
            almdmov.codalm
            almdmov.nroser
            almdmov.nrodoc
            almdmov.fchdoc
            almdmov.codmat
            desmat
            nroref      column-label 'o/p'
            undbas
            (candes * factor)
            catconta[1]
            x-ctouni
            with stream-io no-box width 320.
    end.            
end.
output close.
