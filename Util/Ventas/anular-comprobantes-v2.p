/* SOLO cancelaciones en caja MOSTRADOR y ya se tenga cierre de caja */
DEF BUFFER guias FOR ccbcdocu.    

/* Para cuando el proceso se reliza en el Servidor Lima */
/* DISABLE TRIGGERS FOR LOAD OF almcmov.  */
/* DISABLE TRIGGERS FOR LOAD OF ccbcdocu. */

FOR EACH ccbcdocu WHERE codcia = 1
    AND coddoc = 'BOL'
    AND nrodoc >= '22300002038'   
    AND nrodoc <= '22300002038' 
    AND flgest = "C":           /* C: Tiene caja cerrad  P:Aun no cierra la caja o esta anulado la I/C */
    DISPLAY coddoc nrodoc codalm fchvto flgest imptot flgest.
    flgest = 'A'.
    sdoact = 0.
    FOR EACH guias EXCLUSIVE-LOCK WHERE guias.codcia = 1
        AND guias.coddoc = 'G/R'
        AND guias.codref = ccbcdocu.coddoc
        AND guias.nroref = ccbcdocu.nrodoc
        AND guias.flgest <> 'A':
        guias.flgest = "A".
    END.
    /* Movimientos de Almacén */
    FIND LAST almcmov WHERE almcmov.codcia = ccbcdocu.codcia
        AND almcmov.tipmov = 's'
        AND almcmov.codmov = 02
        AND almcmov.codref = ccbcdocu.coddoc
        AND almcmov.nroref = ccbcdocu.nrodoc
        AND almcmov.flgest <> 'A'
        NO-ERROR.
    IF AVAILABLE almcmov THEN DO:
        almcmov.flgest = 'A'.
        FOR EACH almdmov OF almcmov:
            DELETE almdmov.
        END.
    END.
    /* Movimientos de Caja (caso facturas manuales en tiendas) */
    FOR EACH ccbdcaja WHERE ccbdcaja.codcia = 1
        AND ccbdcaja.codref = ccbcdocu.coddoc
        AND ccbdcaja.nroref = ccbcdocu.nrodoc,
        FIRST ccbccaja OF ccbdcaja:
        ccbccaja.flgest = 'A'.
    END.
END.
