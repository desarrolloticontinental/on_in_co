DEF BUFFER PEDIDO FOR faccpedi.
DEF BUFFER COTIZACION FOR faccpedi.
DEF BUFFER DOCU FOR ccbcdocu.

FOR EACH gn-divi NO-LOCK WHERE codcia = 1:
    FOR EACH ccbcdocu NO-LOCK WHERE ccbcdocu.codcia = 1
        AND ccbcdocu.coddiv = gn-divi.coddiv
        AND ccbcdocu.coddoc = 'bol'
        AND ccbcdocu.fchdoc >= DATE(04/01/2018)
        AND ccbcdocu.codped = "PED",
        FIRST PEDIDO NO-LOCK WHERE PEDIDO.codcia = 1
        AND PEDIDO.coddoc = ccbcdocu.codped
        AND PEDIDO.nroped = ccbcdocu.nroped:
        FIND FIRST COTIZACION WHERE COTIZACION.codcia = PEDIDO.codcia
            AND COTIZACION.coddoc = PEDIDO.codref
            AND COTIZACION.nroped = PEDIDO.nroref
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE COTIZACION THEN NEXT.
        IF ccbcdocu.divori <> cotizacion.coddiv THEN DO:
            DISPLAY ccbcdocu.coddoc ccbcdocu.nrodoc ccbcdocu.divori cotizacion.coddiv
                ccbcdocu.codped ccbcdocu.nroped
                WITH STREAM-IO NO-BOX width 320.
            FIND DOCU WHERE ROWID(DOCU) = ROWID(Ccbcdocu) EXCLUSIVE-LOCK.
            DOCU.divori = cotizacion.coddiv.
        END.
    END.
END.

