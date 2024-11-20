DEF VAR x-estado AS CHAR NO-UNDO.
OUTPUT TO c:\tmp\hojaruta.txt.
PUT UNFORMATTED
    'FECHA|H RUTA|G/R|FAC o BOL|DOC|NUMERO|ESTADO' 
    SKIP.
FOR EACH di-rutac NO-LOCK WHERE di-rutac.codcia = 1
    AND di-rutac.flgest = 'C'
    AND di-rutac.fchdoc >= 09/01/2014,
    EACH di-rutad OF di-rutac NO-LOCK,
    FIRST ccbcdocu NO-LOCK WHERE ccbcdocu.codcia = 001
    AND ccbcdocu.coddoc = di-rutad.codref
    AND ccbcdocu.nrodoc = di-rutad.nroref,
    FIRST faccpedi NO-LOCK WHERE faccpedi.codcia = 001
    AND faccpedi.coddoc = ccbcdocu.codped
    AND faccpedi.nroped = ccbcdocu.nroped
    AND faccpedi.coddiv = '00015'
    AND faccpedi.fchped >= 09/01/2014:
    RUN alm/f-flgrut ("D", Di-RutaD.flgest, OUTPUT x-Estado).
    PUT UNFORMATTED
        di-rutac.fchdoc '|'
        di-rutac.nrodoc '|'
        ccbcdocu.coddoc ' '
        ccbcdocu.nrodoc '|'
        ccbcdocu.codref ' '
        ccbcdocu.nroref '|'
        faccpedi.coddoc '|'
        faccpedi.nroped '|'
        x-estado
        SKIP.
END.


