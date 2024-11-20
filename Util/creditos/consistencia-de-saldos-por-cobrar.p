DEF VAR x-sdoact AS DEC NO-UNDO.

OUTPUT TO c:\tmp\revisar.txt.
FOR EACH ccbcdocu NO-LOCK WHERE codcia = 1
    AND LOOKUP(coddoc, 'fac,bol,let,n/d') > 0
    AND fchdoc >= 01/01/09
    AND NOT codcli BEGINS '1111'
    AND flgest <> 'A':
    x-sdoact = imptot.
    FOR EACH ccbdcaja NO-LOCK WHERE ccbdcaja.codcia = 1
        AND ccbdcaja.codref = ccbcdocu.coddoc
        AND ccbdcaja.nroref = ccbcdocu.nrodoc:
        x-sdoact = x-sdoact - ccbdcaja.imptot.
    END.
    IF x-sdoact <> ccbcdocu.sdoact AND x-sdoact > 1 THEN
        DISPLAY 
        ccbcdocu.coddoc
        ccbcdocu.nrodoc
        ccbcdocu.codcli
        ccbcdocu.nomcli
        ccbcdocu.fchdoc
        WITH STREAM-IO NO-BOX WIDTH 320.

END.
OUTPUT CLOSE.
