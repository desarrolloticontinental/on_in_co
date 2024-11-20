
OUTPUT TO c:\tmp\ingresos-a-caja.txt.
PUT UNFORMATTED 
    'DIVISION|FECHA|NUMERO|CLIENTE|NOMBRE|IMPORTE|REF|NRO REF|EMISION'
    SKIP.
FOR EACH ccbccaja NO-LOCK WHERE codcia = 001
    AND coddoc = 'i/c'
    AND fchdoc >= 01/01/2015
    AND fchdoc <= 02/28/2015
    AND flgest <> 'A',
    EACH ccbdcaja OF ccbccaja NO-LOCK,
    FIRST ccbcdocu NO-LOCK WHERE ccbcdocu.codcia = ccbdcaja.codcia
    AND ccbcdocu.coddoc = ccbdcaja.codref
    AND ccbcdocu.nrodoc = ccbdcaja.nroref:
    PUT UNFORMATTED
        ccbccaja.coddiv '|'
        ccbccaja.fchdoc '|'
        ccbccaja.nrodoc '|'
        ccbccaja.codcli '|'
        ccbccaja.nomcli '|'
        ccbdcaja.imptot '|'
        ccbdcaja.codref '|'
        ccbdcaja.nroref '|'
        ccbcdocu.fchdoc 
        SKIP.
    
END.
