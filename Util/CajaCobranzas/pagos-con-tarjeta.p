DEF VAR x-implin AS DEC NO-UNDO.

OUTPUT TO c:\tmp\pago-con-tarjetas.txt.
PUT UNFORMATTED
    'DIVISION|I/C|CIERRE|HORA|CAJERO|CLIENTE|IMPORTE S/.|IMPORTE US$|' +
    'TARJETA|BANCO|TIPO|COMPROBANTE|IMPORTE 011'
    SKIP.
GENERAL:
FOR EACH ccbccaja NO-LOCK WHERE codcia = 1
    AND coddoc = 'i/c'
    AND fchdoc >= 01/01/2013
    AND flgcie = 'C'
    AND flgest <> 'A'
    AND (impnac[4] + impusa[4]) > 0:
    FOR EACH ccbdcaja OF ccbccaja NO-LOCK:
        FIND ccbcdocu WHERE ccbcdocu.codcia = 001
            AND ccbcdocu.coddoc = ccbdcaja.codref
            AND ccbcdocu.nrodoc = ccbdcaja.nroref
            AND CAN-FIND(FIRST ccbddocu OF ccbcdocu WHERE
                         CAN-FIND(FIRST Almmmatg OF ccbddocu WHERE
                                  Almmmatg.codfam = '011'
                                  NO-LOCK)
                         NO-LOCK)
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE ccbcdocu THEN NEXT.
        /* acumulamos solo familia 011 */
        x-implin = 0.
        FOR EACH ccbddocu OF ccbcdocu NO-LOCK, FIRST Almmmatg OF ccbddocu
            WHERE Almmmatg.codfam = '011':
            x-implin = x-implin + ccbddocu.implin.
        END.
        PUT UNFORMATTED
            ccbccaja.coddiv '|'
            ccbccaja.coddoc ' '
            ccbccaja.nrodoc '|'
            ccbccaja.fchcie '|'
            ccbccaja.horcie '|'
            ccbccaja.usuario '|'
            ccbccaja.codcli ' '
            ccbccaja.nomcli '|'
            ccbccaja.impnac[4] '|'
            ccbccaja.impusa[4] '|'
            ccbccaja.voucher[4] '|'
            ccbccaja.codbco[4] '|'
            ccbccaja.voucher[9] '|'
            ccbdcaja.codref ' '
            ccbdcaja.nroref '|'
            x-implin
            SKIP.
    END.
END.
OUTPUT CLOSE.

/*
FOR EACH ccbccaja NO-LOCK WHERE codcia = 1
    AND coddoc = 'i/c'
    AND fchdoc >= 01/01/2013
    AND flgcie = 'C'
    AND (impnac[4] + impusa[4]) > 0,
    EACH ccbdcaja OF ccbccaja NO-LOCK,
    FIRST ccbcdocu NO-LOCK WHERE ccbcdocu.codcia = 1
    AND ccbcdocu.coddoc = ccbdcaja.codref
    AND ccbcdocu.nrodoc = ccbdcaja.nroref:
    FIRST ccbddocu OF ccbcdocu NO-LOCK WHERE
    CAN-FIND(FIRST almmmatg OF ccbddocu WHERE almmmatg.codfam = '011' NO-LOCK):
    IF AVAILABLE ccbddocu THEN NEXT.
    PUT UNFORMATTED
        ccbccaja.coddiv '|'
        ccbccaja.coddoc ' '
        ccbccaja.nrodoc '|'
        ccbccaja.fchcie '|'
        ccbccaja.horcie '|'
        ccbccaja.usuario '|'
        ccbccaja.codcli ' '
        ccbccaja.nomcli '|'
        ccbccaja.impnac[4] '|'
        ccbccaja.impusa[4] '|'
        ccbccaja.voucher[4] '|'
        ccbccaja.codbco[4] '|'
        ccbccaja.voucher[9] '|'
        ccbdcaja.codref ' '
        ccbdcaja.nroref
        SKIP.
END.
*/
