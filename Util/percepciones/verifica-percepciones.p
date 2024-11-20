DEF VAR s-PorPercepcion AS DEC NO-UNDO.
DEF VAR f-ImpPercepcion AS DEC NO-UNDO.
DEF VAR s-ImpPercepcion AS DEC NO-UNDO.
DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR cl-codcia AS INT INIT 000 NO-UNDO.

OUTPUT TO c:\tmp\errores.txt.
FOR EACH ccbcdocu NO-LOCK WHERE codcia = 1
    AND LOOKUP(coddoc, 'fac,bol,tck,n/c') > 0
    AND fchdoc >= 07/01/2013
    AND flgest <> 'A':
    s-Porpercepcion = 0.
    s-ImpPercepcion = 0.
    f-ImpPercepcion = 0.
    PERCEPCION:
    DO:
        /* Veamos si es sujeto a Percepcion */
        FOR EACH Ccbddocu OF Ccbcdocu NO-LOCK,
            FIRST Almmmatg OF Ccbddocu NO-LOCK,
            FIRST Almsfami OF Almmmatg WHERE Almsfami.Libre_C05 = "SI":
            f-ImpPercepcion = f-ImpPercepcion + Ccbddocu.ImpLin.
        END.
        IF Ccbcdocu.coddoc = "BOL" THEN DO:
            IF Ccbcdocu.codmon = 1 AND f-ImpPercepcion <= 700 THEN LEAVE PERCEPCION.
            IF Ccbcdocu.codmon = 2 AND f-ImpPercepcion * Ccbcdocu.tpocmb <= 700 THEN LEAVE PERCEPCION.
        END.
        IF Ccbcdocu.coddoc = "TCK" AND Ccbcdocu.Libre_C04 <> 'FAC' THEN DO:
            IF Ccbcdocu.codmon = 1 AND f-ImpPercepcion <= 700 THEN LEAVE PERCEPCION.
            IF Ccbcdocu.codmon = 2 AND f-ImpPercepcion * Ccbcdocu.tpocmb <= 700 THEN LEAVE PERCEPCION.
        END.
        FIND FIRST Vtatabla WHERE Vtatabla.codcia = s-codcia
            AND Vtatabla.tabla = 'CLNOPER'
            AND VtaTabla.Llave_c1 = CcbCDocu.RucCli
            NO-LOCK NO-ERROR.
        IF AVAILABLE Vtatabla THEN LEAVE PERCEPCION.
        /* ******************************** */
        FIND gn-clie WHERE gn-clie.codcia = cl-codcia
            AND gn-clie.codcli = Ccbcdocu.codcli
            NO-LOCK.
        IF gn-clie.Libre_L01 = YES AND gn-clie.RucOld <> "SI" THEN s-Porpercepcion = 0.5.
        IF gn-clie.Libre_L01 = NO AND gn-clie.RucOld <> "SI" THEN s-Porpercepcion = 2.
        IF Ccbcdocu.coddoc = "BOL" THEN s-Porpercepcion = 2.
        IF Ccbcdocu.coddoc = "TCK" AND Ccbcdocu.Libre_C04 <> 'FAC' THEN s-Porpercepcion = 2.
        IF s-PorPercepcion = 0 THEN LEAVE PERCEPCION.
        FOR EACH ccbddocu OF ccbcdocu NO-LOCK, 
            FIRST Almmmatg OF ccbddocu NO-LOCK,
            FIRST Almsfami OF Almmmatg NO-LOCK WHERE Almsfami.Libre_c05 = "SI":
            ASSIGN
                s-ImpPercepcion = s-ImpPercepcion + ROUND(ccbddocu.implin * s-PorPercepcion / 100, 2).
        END.
        IF s-ImpPercepcion = 0 THEN s-PorPercepcion = 0.
    END.
    IF s-PorPercepcion <> Ccbcdocu.acubon[4] OR s-ImpPercepcion <> Ccbcdocu.acubon[5] THEN
        DISPLAY
        ccbcdocu.fchdoc
        ccbcdocu.coddiv
        ccbcdocu.coddoc
        ccbcdocu.nrodoc
        ccbcdocu.acubon[4]
        s-porpercepcion
        ccbcdocu.acubon[5]
        s-imppercepcion
        WITH STREAM-IO NO-BOX NO-UNDERLINE WIDTH 200.
END.
OUTPUT CLOSE.

