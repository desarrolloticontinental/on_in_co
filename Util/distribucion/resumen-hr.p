DEF TEMP-TABLE detalle
    FIELD nrodoc LIKE di-rutac.nrodoc
    FIELD fchdoc LIKE di-rutac.fchdoc
    FIELD imptot AS DEC EXTENT 2
    FIELD impent AS DEC EXTENT 2.

FOR EACH gn-divi NO-LOCK WHERE codcia = 1:
    FOR EACH di-rutac NO-LOCK WHERE codcia = 1
        AND di-rutac.coddiv = gn-divi.coddiv
        AND flgest = 'C':
        CREATE detalle.
        ASSIGN
            detalle.nrodoc = di-rutac.nrodoc
            detalle.fchdoc = di-rutac.fchdoc.
        FOR EACH di-rutad OF di-rutac NO-LOCK WHERE LOOKUP(di-rutad.codref, 'FAC,BOL') > 0,
            FIRST ccbcdocu NO-LOCK WHERE ccbcdocu.codcia = 1
            AND ccbcdocu.coddoc = di-rutad.codref
            AND ccbcdocu.nrodoc = di-rutad.nroref
            AND ccbcdocu.coddiv = '00015':
            IF ccbcdocu.codmon = 1 
            THEN detalle.imptot[1] = detalle.imptot[1] + ccbcdocu.imptot.
            ELSE detalle.imptot[2] = detalle.imptot[2] + ccbcdocu.imptot.
            IF di-rutad.flgest = 'C'        /* ENTREGADO */
            THEN DO:
                IF ccbcdocu.codmon = 1 
                THEN detalle.impent[1] = detalle.impent[1] + ccbcdocu.imptot.
                ELSE detalle.impent[2] = detalle.impent[2] + ccbcdocu.imptot.
            END.
        END.
    END.
END.

OUTPUT TO c:\tmp\hojas.txt.
FOR EACH detalle:
    DISPLAY
        detalle.nrodoc
        detalle.fchdoc
        detalle.imptot
        detalle.impent
        WITH STREAM-IO NO-BOX WIDTH 320.
END.
OUTPUT CLOSE.
