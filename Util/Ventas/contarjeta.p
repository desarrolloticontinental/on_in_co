OUTPUT TO d:\tmp\contarjetas.txt.
FOR EACH gn-divi NO-LOCK WHERE codcia = 001
    AND canalventa ='MIN':
    FOR EACH ccbcdocu NO-LOCK WHERE ccbcdocu.codcia = gn-divi.codcia
        AND ccbcdocu.coddoc = 'tck'
        AND ccbcdocu.coddiv = gn-divi.coddiv
        AND ccbcdocu.flgest <> 'A'
        AND ccbcdocu.fchdoc >= 01/01/2015
        AND (ccbcdocu.codcli BEGINS '20'
             OR ccbcdocu.codcli BEGINS '10'),
        FIRST ccbdcaja NO-LOCK WHERE ccbdcaja.codcia = ccbcdocu.codcia
        AND ccbdcaja.coddoc = 'i/c'
        AND ccbdcaja.codref = ccbcdocu.coddoc
        AND ccbdcaja.nroref = ccbcdocu.nrodoc,
        FIRST ccbccaja OF ccbdcaja NO-LOCK WHERE (ccbccaja.impnac[4] +
                                                  ccbccaja.impusa[4]) > 0:
        PUT UNFORMATTED
            ccbcdocu.coddiv '|'
            ccbcdocu.coddoc '|'
            ccbcdocu.nrodoc '|'
            ccbcdocu.fchdoc '|'
            ccbcdocu.codcli '|'
            ccbcdocu.nomcli '|'
            ccbccaja.voucher[4] '|'
            ccbccaja.impnac[4] '|'
            ccbccaja.impusa[4]
            SKIP.

    END.
    
END.
