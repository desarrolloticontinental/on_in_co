DEF TEMP-TABLE detalle
    FIELD codcia LIKE almmmatg.codcia
    FIELD codmat LIKE almmmatg.codmat
    FIELD desmat LIKE almmmatg.desmat
    FIELD undbas LIKE almmmatg.undbas
    FIELD candes LIKE ccbddocu.candes
    FIELD implin LIKE ccbddocu.implin
    FIELD impcto LIKE ccbddocu.impcto
    INDEX LLave01 IS PRIMARY UNIQUE codcia codmat.
DEF VAR x-Factor AS INT NO-UNDO.

FOR EACH gn-divi NO-LOCK WHERE codcia = 001:
    DISPLAY gn-divi.coddiv.
    FOR EACH ccbcdocu NO-LOCK WHERE codcia = 001
            AND coddiv = gn-divi.coddiv
            AND (coddoc = 'FAC' or coddoc = 'BOL' or coddoc = 'TCK' OR coddoc = 'N/C')
            AND fchdoc >= 09/01/09 AND fchdoc <= 09/30/09
            AND flgest <> 'A',
            EACH ccbddocu OF ccbcdocu NO-LOCK WHERE ccbddocu.implin > 0,
            FIRST almmmatg OF ccbddocu NO-LOCK:
        x-Factor = IF ccbcdocu.coddoc = 'N/C' THEN -1 ELSE 1.
        FIND detalle OF ccbddocu NO-ERROR.
        IF NOT AVAILABLE detalle THEN DO:
            CREATE detalle.
            ASSIGN
                detalle.codcia = almmmatg.codcia
                detalle.codmat = almmmatg.codmat
                detalle.desmat = almmmatg.desmat
                detalle.undbas = almmmatg.undbas.
        END.
        detalle.candes = detalle.candes + ccbddocu.candes * ccbddocu.factor * x-factor.
        IF ccbcdocu.codmon = 1 
        THEN detalle.implin = detalle.implin + ccbddocu.implin * x-factor.
        ELSE detalle.implin = detalle.implin + ccbddocu.implin * ccbcdocu.tpocmb * x-factor.
        FIND LAST almstkge WHERE AlmStkge.CodCia = ccbddocu.codcia
            AND AlmStkge.codmat = ccbddocu.codmat
            AND AlmStkge.Fecha <= ccbcdocu.fchdoc
            NO-LOCK NO-ERROR.
        IF AVAILABLE almstkge 
        THEN detalle.impcto = detalle.impcto + ccbddocu.candes * ccbddocu.factor * AlmStkge.CtoUni * x-factor.
    END.
END.

OUTPUT TO c:\tmp\setiembre09.txt.
FOR EACH detalle NO-LOCK:
    DISPLAY
        detalle.codmat
        detalle.desmat
        detalle.undbas
        detalle.candes COLUMN-LABEL 'Cantidad'          FORMAT '->>>,>>>,>>>.99'
        detalle.impcto COLUMN-LABEL 'Costo total S/.'   FORMAT '->>>,>>>,>>>.99'
        detalle.implin COLUMN-LABEL 'Venta total S/.'   FORMAT '->>>,>>>,>>>.99'
        WITH STREAM-IO NO-BOX WIDTH 200.
END.
OUTPUT CLOSE.

