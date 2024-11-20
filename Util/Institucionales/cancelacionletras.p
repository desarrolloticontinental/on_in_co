DEF BUFFER bcdocu FOR ccbcdocu.

OUTPUT TO c:\tmp\letras.txt.
FOR EACH ccbdcaja NO-LOCK WHERE codcia = 1
    AND coddoc = 'cje'
    AND fchdoc >= 01/01/2012,
    FIRST ccbcdocu NO-LOCK WHERE ccbcdocu.codcia = 001
    AND ccbcdocu.coddoc = ccbdcaja.codref
    AND ccbcdocu.nrodoc = ccbdcaja.nroref
    AND ccbcdocu.coddiv = '00024'
    BREAK BY ccbdcaja.nrodoc:
    IF FIRST-OF(ccbdcaja.nrodoc) THEN DO:
        FOR EACH bcdocu NO-LOCK WHERE bcdocu.codcia = 001
            AND bcdocu.coddoc = 'let'
            AND bcdocu.codref = ccbdcaja.coddoc
            AND bcdocu.nroref = ccbdcaja.nrodoc:
            DISPLAY
                bcdocu.coddoc
                bcdocu.nrodoc FORMAT 'x(15)'
                bcdocu.fchdoc
                bcdocu.codcli
                bcdocu.nomcli
                bcdocu.codped
                bcdocu.nroped FORMAT 'x(15)'
                bcdocu.imptot
                bcdocu.codmon
                bcdocu.tpocmb
                bcdocu.codref
                bcdocu.nroref FORMAT 'x(15)'
                bcdocu.fchvto
                bcdocu.coddiv FORMAT 'x(5)'
                bcdocu.fmapgo
                bcdocu.divori
                WITH STREAM-IO NO-BOX WIDTH 320.
            PAUSE 0.
        END.
    END.
END.
