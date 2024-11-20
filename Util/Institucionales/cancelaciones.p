DEF BUFFER bcdocu FOR ccbcdocu.


OUTPUT TO c:\tmp\cancelaciones.txt.
FOR EACH ccbdcaja NO-LOCK WHERE codcia = 1
    AND codref <> 'LET'
    AND fchdoc >= 01/01/2012,
    FIRST ccbcdocu NO-LOCK WHERE ccbcdocu.codcia = 001
    AND ccbcdocu.coddoc = ccbdcaja.codref
    AND ccbcdocu.nrodoc = ccbdcaja.nroref
    AND (ccbcdocu.coddiv = '00024' OR ccbcdocu.divori = '00024'):
    DISPLAY
        ccbdcaja.coddiv FORMAT 'x(5)'
        ccbdcaja.fchdoc
        ccbdcaja.coddoc
        ccbdcaja.nrodoc
        ccbdcaja.codref
        ccbdcaja.nroref FORMAT 'x(15)'
        ccbdcaja.codmon
        ccbdcaja.imptot
        WITH STREAM-IO NO-BOX WIDTH 320.
END.
OUTPUT CLOSE.

OUTPUT TO c:\tmp\cancelacionletras.txt.
FOR EACH ccbdcaja NO-LOCK WHERE codcia = 1
    AND codref = 'LET'
    AND fchdoc >= 01/01/2012:
    DISPLAY
        ccbdcaja.coddiv FORMAT 'x(5)'
        ccbdcaja.fchdoc
        ccbdcaja.coddoc
        ccbdcaja.nrodoc
        ccbdcaja.codref
        ccbdcaja.nroref FORMAT 'x(15)'
        ccbdcaja.codmon
        ccbdcaja.imptot
        WITH STREAM-IO NO-BOX WIDTH 320.
END.
OUTPUT CLOSE.

