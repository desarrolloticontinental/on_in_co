DEF TEMP-TABLE detalle
    FIELD coddoc LIKE ccbcdocu.coddoc
    FIELD coddiv LIKE ccbcdocu.coddiv
    FIELD divori LIKE ccbcdocu.divori
    FIELD nromes AS INT
    FIELD periodo AS INT
    FIELD contador AS INT
    INDEX llave AS PRIMARY coddoc coddiv divori nromes periodo.

FOR EACH ccbcdocu NO-LOCK WHERE codcia = 1
    AND LOOKUP(coddoc, 'FAC,BOL,N/C') > 0
    AND fchdoc >= 01/01/2017:
    FIND detalle WHERE detalle.coddoc = ccbcdocu.coddoc
        AND detalle.coddiv = ccbcdocu.coddiv
        AND detalle.divori = ccbcdocu.divori
        AND detalle.nromes = MONTH(ccbcdocu.fchdoc)
        AND detalle.periodo = YEAR(ccbcdocu.fchdoc)
        EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE detalle THEN DO:
        CREATE detalle.
        ASSIGN
            detalle.coddoc = ccbcdocu.coddoc
            detalle.coddiv = ccbcdocu.coddiv
            detalle.divori = ccbcdocu.divori
            detalle.nromes = MONTH(ccbcdocu.fchdoc)
            detalle.periodo = YEAR(ccbcdocu.fchdoc).
    END.
    detalle.contador = detalle.contador + 1.
END.

FOR EACH faccpedi NO-LOCK WHERE faccpedi.codcia = 1
    AND LOOKUP(faccpedi.coddoc, 'COT,P/M,PED,O/M,O/D') > 0
    AND fchped >= 01/01/2017:
    FIND detalle WHERE detalle.coddoc = faccpedi.coddoc
        AND detalle.coddiv = faccpedi.coddiv
        AND detalle.divori = faccpedi.coddiv
        AND detalle.nromes = MONTH(faccpedi.fchped)
        AND detalle.periodo = YEAR(faccpedi.fchped)
        EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE detalle THEN DO:
        CREATE detalle.
        ASSIGN
            detalle.coddoc = faccpedi.coddoc
            detalle.coddiv = faccpedi.coddiv
            detalle.divori = faccpedi.coddiv
            detalle.nromes = MONTH(faccpedi.fchped)
            detalle.periodo = YEAR(faccpedi.fchped).
    END.
    detalle.contador = detalle.contador + 1.
END.

OUTPUT TO d:\tmp\comprobantes.txt.
FOR EACH detalle NO-LOCK:
    DISPLAY 
        detalle.coddoc
        detalle.coddiv
        detalle.divori
        detalle.nromes
        detalle.periodo
        detalle.contador
        WITH STREAM-IO NO-BOX WIDTH 320.
    PAUSE 0.
END.
OUTPUT CLOSE.

