OUTPUT TO d:\datos.txt.
PUT UNFORMATTED 'DIVI|MES|DOC|MONTO' SKIP.
FOR EACH ccbccaja NO-LOCK WHERE codcia = 1
    AND coddoc = 'i/c'
    AND fchdoc >= 01/01/2021
    AND fchdoc <= TODAY
    AND flgest <> 'A':
    PUT UNFORMATTED ccbccaja.coddiv '|' MONTH(fchdoc) '|' coddoc '|' (impnac[1] - vuenac) SKIP.
END.
FOR EACH ccbcdocu NO-LOCK WHERE codcia = 1
    AND coddoc = 'bd'
    AND fchdoc >= 01/01/2021
    AND fchdoc <= TODAY
    AND flgest <> 'a':
    PUT UNFORMATTED ccbcdocu.coddiv '|' MONTH(fchdoc) '|' coddoc '|' imptot SKIP.
END.
FOR EACH ccbdcaja NO-LOCK WHERE codcia = 1
    AND coddoc = 'n/b'
    AND fchdoc >= 01/01/2021
    AND fchdoc <= TODAY
    AND codref = 'let':
    PUT UNFORMATTED ccbdcaja.coddiv '|' MONTH(fchdoc) '|' coddoc '|' imptot SKIP.
END.
OUTPUT CLOSE.


