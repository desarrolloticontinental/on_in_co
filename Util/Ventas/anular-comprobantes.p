FOR EACH ccbcdocu WHERE codcia = 1
    AND coddoc = 'BOL'
    AND nrodoc >= '83400004637'
    AND nrodoc <= '83400004637'
    AND flgest <> "A":
    DISPLAY coddoc nrodoc codalm fchvto flgest imptot.
    flgest = 'A'.
    sdoact = 0.
/*     FOR EACH ccbddocu OF ccbcdocu:                                              */
/*         DISPLAY 'COMPROBANTE:' ccbddocu.coddoc ccbddocu.nrodoc ccbddocu.codmat. */
/*     END.                                                                        */
    /* Movimientos de Almacén */
    FIND LAST almcmov WHERE almcmov.codcia = ccbcdocu.codcia
        AND almcmov.tipmov = 's'
        AND almcmov.codmov = 02
        AND almcmov.codref = ccbcdocu.coddoc
        AND almcmov.nroref = ccbcdocu.nrodoc
        AND almcmov.flgest <> "A"
        NO-ERROR.
    IF AVAILABLE almcmov THEN DO:
        almcmov.flgest = 'A'.
        FOR EACH almdmov OF almcmov:
            /*DISPLAY 'ALMACEN:' almdmov.codalm almdmov.codmat almdmov.candes.*/
            DELETE almdmov.
        END.
    END.
    /* Movimientos de Caja (caso facturas manuales en tiendas) */
    FOR EACH ccbdcaja WHERE ccbdcaja.codcia = 1
        AND ccbdcaja.codref = ccbcdocu.coddoc
        AND ccbdcaja.nroref = ccbcdocu.nrodoc,
        FIRST ccbccaja OF ccbdcaja:
        DISPLAY 'CAJA:' ccbdcaja.coddoc ccbdcaja.nrodoc.
        DELETE ccbdcaja.
        ccbccaja.flgest = 'A'.
    END.
END.

