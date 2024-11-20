def buffer dmov for almdmov.
def buffer cmov for almcmov.
def buffer matg for almmmatg.
def var x-ctouni as dec decimals 4 format '->>>,>>9.9999'.
def var x-toting as dec decimals 4.
def var x-totsal as dec decimals 4.

def frame f-detalle
    almdmov.tipmov
    almdmov.codmov
    almdmov.nrodoc
    almdmov.fchdoc
    almcmov.nrorf1
    almdmov.codmat
    almmmatg.desmat
    almdmov.candes
    almdmov.codund
/*    dmov.tipmov
 *     dmov.codmov
 *     dmov.nrodoc
 *     dmov.fchdoc 
 *     cmov.nrorf1*/
    dmov.candes
    dmov.codund
    x-ctouni
    x-toting
    x-totsal
    with down stream-io no-box width 320.
output to c:\tmp\susana.txt.
for each almdmov no-lock where codcia = 001
    and codalm = '12'
    and tipmov = 'i'
    and codmov = 50
    and fchdoc >= 01/01/2006
    and fchdoc <= 06/30/2006,
    first almcmov of almdmov no-lock,
    first almmmatg of almdmov no-lock
    break by almcmov.nrorf1:
    /* quiebre */
    if first-of(almcmov.nrorf1) 
    then assign 
            x-toting = 0
            x-totsal = 0.
    x-ctouni = 0.
    find last almstkge where almstkge.codcia = 001
        and almstkge.codmat = almdmov.codmat
        and almstkge.fecha <= almdmov.fchdoc
        no-lock no-error.
    if available almstkge then x-ctouni = almstkge.ctouni.
    x-toting = x-toting + (x-ctouni * almdmov.candes).
    display 
        almdmov.tipmov 
        almdmov.codmov 
        almdmov.nrodoc 
        almdmov.fchdoc 
        nrorf1
        almdmov.codmat 
        almmmatg.desmat 
        almdmov.candes 
        almdmov.codund 
        x-ctouni
        (x-ctouni * almdmov.candes) @ x-toting
        with frame f-detalle.
    if last-of(almcmov.nrorf1) then do:
        for each dmov no-lock where dmov.codcia = 001
            and dmov.codalm = '12'
            and dmov.tipmov = 's'
            and dmov.codmov = 50
            and dmov.fchdoc >= 01/01/2006
            and dmov.fchdoc <= 06/30/2006,
            first cmov of dmov no-lock where cmov.nrorf1 = almcmov.nrorf1,
            first matg of dmov no-lock:
            x-ctouni = 0.
            find last almstkge where almstkge.codcia = 001
                and almstkge.codmat = dmov.codmat
                and almstkge.fecha <= dmov.fchdoc
                no-lock no-error.
            if available almstkge then x-ctouni = almstkge.ctouni.
            down 1 with frame f-detalle.
            x-totsal = x-totsal + (x-ctouni * dmov.candes).
            display 
                dmov.tipmov @ almdmov.tipmov
                dmov.codmov @ almdmov.codmov
                dmov.nrodoc @ almdmov.nrodoc
                dmov.fchdoc @ almdmov.fchdoc
                cmov.nrorf1 @ almcmov.nrorf1
                dmov.codmat @ almdmov.codmat 
                matg.desmat @ almmmatg.desmat
                dmov.candes
                dmov.codund
                x-ctouni
                (x-ctouni * dmov.candes) @ x-totsal
                with frame f-detalle.
        end.
    end.
    /* quiebre */
    if last-of(almcmov.nrorf1) then do:
        down 1 with frame f-detalle.
        underline 
            x-toting
            x-totsal
            with frame f-detalle.
        down 1 with frame f-detalle.
        display
            x-toting
            x-totsal
            with frame f-detalle.        
        down 2 with frame f-detalle.
    end.
end.
output close.
