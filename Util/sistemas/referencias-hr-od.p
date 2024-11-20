OUTPUT TO d:\referencias.txt.
FOR EACH di-rutac NO-LOCK WHERE codcia = 1
    AND coddoc = 'h/r'
    AND fchdoc >= 01/01/2019
    AND fchdoc <= DATE(04,30,2023),
    EACH di-rutad OF di-rutac NO-LOCK,
    FIRST ccbcdocu NO-LOCK WHERE ccbcdocu.codcia = 1
    AND ccbcdocu.coddoc = di-rutad.codref
    AND ccbcdocu.nrodoc = di-rutad.nroref:
    PUT UNFORMATTED
        di-rutac.fchdoc ';'
        di-rutad.coddoc ';'
        di-rutad.nrodoc ';'
        ccbcdocu.coddoc ';'
        ccbcdocu.nrodoc ';'
        ccbcdocu.fchdoc ';'
        ccbcdocu.codped ';'
        ccbcdocu.nroped ';'
        ccbcdocu.codref ';'
        ccbcdocu.nroref
        SKIP.
END.


