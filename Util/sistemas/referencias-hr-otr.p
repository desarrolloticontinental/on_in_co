OUTPUT TO d:\referencias-otr.txt.
FOR EACH di-rutac NO-LOCK WHERE codcia = 1
    AND coddoc = 'h/r'
    AND fchdoc >= 01/01/2019
    AND fchdoc <= DATE(04,30,2023),
    EACH di-rutag OF di-rutac NO-LOCK,
    FIRST almcmov NO-LOCK WHERE almcmov.codcia = 1
    AND almcmov.codalm = di-rutag.codalm
    AND almcmov.tipmov = di-rutag.tipmov
    AND almcmov.codmov = di-rutag.codmov
    AND almcmov.nroser = di-rutag.serref
    AND almcmov.nrodoc = di-rutag.nroref
    :
    PUT UNFORMATTED
        di-rutac.fchdoc ';'
        di-rutag.coddoc ';'
        di-rutag.nrodoc ';'
        almcmov.codalm ';'
        almcmov.tipmov ';'
        almcmov.codmov ';'
        almcmov.nroser ';'
        almcmov.nrodoc ';'
        almcmov.fchdoc ';'
        almcmov.codref ';'
        almcmov.nroref
        SKIP.
END.


