OUTPUT TO c:\tmp\devoluciones.txt.
FOR EACH almcmov NO-LOCK WHERE codcia = 1
    AND codalm = '21'
    AND tipmov = 'i'
    AND codmov = 09
    AND flgest <> 'a'
    AND fchdoc = 12/31/2012,
    EACH almdmov OF almcmov no-lock,
    FIRST almmmatg OF almdmov NO-LOCK:
    DISPLAY
        almcmov.codalm
        almcmov.tipmov
        almcmov.codmov
        almcmov.fchdoc
        almcmov.nroser
        almcmov.nrodoc
        almcmov.codcli
        almcmov.codref
        almcmov.nroref
        almdmov.codmat
        almmmatg.desmat
        almdmov.codund
        almdmov.candes
        almdmov.VctoMn1
        WITH STREAM-IO NO-BOX WIDTH 320.
END.
OUTPUT CLOSE.

