DEF TEMP-TABLE detalle LIKE ccbcdocu
    INDEX llave01 codcia fchdoc.

FOR EACH ccbcdocu NO-LOCK WHERE codcia = 1
    AND coddoc = 'bd'
    AND fchdoc >= 12/01/2010
    AND fchdoc <= 11/30/2011
    AND flgest <> 'A':
    CREATE detalle.
    BUFFER-COPY ccbcdocu TO detalle.
END.

FOR EACH ccbcdocu WHERE codcia = 1
    AND coddoc = 'chc'
    AND fchdoc >= 12/01/2010
    AND fchdoc <= 11/30/2011
    AND flgest <> 'A':
    CREATE detalle.
    BUFFER-COPY ccbcdocu TO detalle.
END.

FOR EACH ccbpendep NO-LOCK WHERE codcia = 1
    AND coddoc = 'bov'
    AND ccbpendep.fchcie >= 12/01/2010
    AND ccbpendep.fchcie <= 11/30/2011
    AND ccbpendep.flgest <> 'A':
    CREATE detalle.
    BUFFER-COPY ccbpendep 
        TO detalle 
        ASSIGN 
            detalle.coddoc = ccbpendep.codref
            detalle.nrodoc = ccbpendep.nroref
            detalle.fchdoc = ccbpendep.fchcie.
    IF ccbpendep.impnac = 0 
        THEN ASSIGN
                detalle.imptot = ccbpendep.impusa
                detalle.codmon = 2.
    ELSE ASSIGN
            detalle.imptot = ccbpendep.impnac
            detalle.codmon = 1.
    FIND CcbCCaja WHERE CcbCCaja.CodCia = detalle.CodCia 
        AND CcbCCaja.CodDoc = detalle.CodDoc 
        AND CcbCCaja.NroDoc = detalle.NroDoc
        NO-LOCK NO-ERROR.
    IF AVAILABLE ccbccaja 
        THEN ASSIGN
                detalle.codcli = ccbccaja.codcli
                detalle.nomcli = ccbccaja.nomcli.
END.

OUTPUT TO c:\tmp\ingresos.txt.
FOR EACH detalle:
    DISPLAY 
        detalle.fchdoc
        detalle.fchate  LABEL 'Fecha depósito'
        detalle.coddoc
        detalle.nrodoc
        detalle.codmon
        detalle.imptot
        detalle.coddiv FORMAT 'X(5)'
        detalle.codcli
        detalle.nomcli
        WITH STREAM-IO NO-BOX WIDTH 300.
END.
OUTPUT CLOSE.


