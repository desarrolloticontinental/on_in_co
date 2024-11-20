OUTPUT TO d:\informacion.txt.
PUT UNFORMATTED 
    'DOC|NUMERO|FECHA|PED|NROPED|CLIENTE|RUC|DNI|COD PLANILLA' SKIP.
FOR EACH ccbcdocu NO-LOCK WHERE codcia = 1
    AND coddiv = '00515'
    AND LOOKUP(coddoc, 'FAC,BOL') > 0
    AND ( (fchdoc >= DATE(12,19,2019) AND fchdoc <= DATE(12,20,2019)) OR
          (fchdoc >= DATE(01,16,2020) AND fchdoc <= DATE(01,17,2020)) ),
    FIRST faccpedi NO-LOCK WHERE faccpedi.codcia = ccbcdocu.codcia
    AND faccpedi.coddoc = ccbcdocu.codped 
    AND faccpedi.nroped = ccbcdocu.nroped:
    PUT UNFORMATTED
        ccbcdocu.coddoc '|'
        ccbcdocu.nrodoc '|'
        ccbcdocu.fchdoc '|'
        faccpedi.coddoc '|'
        faccpedi.nroped '|'
        faccpedi.codcli '|'
        faccpedi.ruccli '|'
        faccpedi.atencion '|'
        faccpedi.libre_c04
        SKIP.
END.
