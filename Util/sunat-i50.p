def var x-ctouni like almstkge.ctouni.

output to c:\tmp\mov-i50-mayo2006-abril2007.txt.
for each almacen no-lock where codcia = 001:
    for each almdmov no-lock where codcia = 001
            and codalm = almacen.codalm
            and tipmov = 'I'
            and codmov = 50
            and fchdoc >= 05/01/2006
            and fchdoc <= 04/30/2007,
            first almcmov of almdmov no-lock,
            first almmmatg of almdmov no-lock where catconta[1] = 'PT'
                or catconta[1] = 'PP':
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
            nroref  column-label 'o/p'
            undbas
            (candes * factor)
            catconta[1]
            x-ctouni
            with stream-io no-box width 320.
    end.            
end.
output close.
