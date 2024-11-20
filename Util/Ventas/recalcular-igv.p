DEFINE VARIABLE F-IGV AS DECIMAL NO-UNDO.
DEFINE VARIABLE F-ISC AS DECIMAL NO-UNDO.
DEFINE VARIABLE f-imp AS DECIMAL NO-UNDO.

FOR EACH ccbcdocu WHERE codcia = 1
    AND coddoc = 'tck'
    AND nrodoc >= '032009100'
    AND nrodoc <= '032009100':
    DISPLAY
        ccbcdocu.coddiv
        ccbcdocu.coddoc
        ccbcdocu.nrodoc
        ccbcdocu.fchdoc
        ccbcdocu.nomcli
        ccbcdocu.fmapgo
        ccbcdocu.codmon
        ccbcdocu.porigv
        ccbcdocu.impbrt 
        ccbcdocu.impdto
        ccbcdocu.impexo
        ccbcdocu.impvta
        ccbcdocu.impigv 
        f-imp
        ccbcdocu.imptot     LABEL 'Total'
        ccbcdocu.imptot2    LABEL 'Adelanto'
        ccbcdocu.sdoact     LABEL 'Saldo'
        ccbcdocu.acubon[4]  LABEL '%Perc'
        ccbcdocu.acubon[5]  LABEL 'Percp'
        CcbcDocu.ImpDto2
        CcbCDocu.Libre_d02
        ccbcdocu.flgest
        WITH 1 COL.
    UPDATE ccbcdocu.porigv.
    FOR EACH ccbddocu OF ccbcdocu, FIRST almmmatg OF ccbddocu NO-LOCK:
        DISPLAY ccbddocu.codmat candes.
        UPDATE preuni implin.
        ccbddocu.aftigv = almmmatg.aftigv = YES.
        IF ccbddocu.AftIgv 
            THEN ccbddocu.ImpIgv = ccbddocu.ImpLin - 
            ROUND( ccbddocu.ImpLin  / ( 1 + (ccbcdocu.PorIgv / 100) ), 4 ).
        f-imp = f-imp + ccbddocu.implin.
    END.

    ASSIGN
        f-Igv = 0
        f-Isc = 0
        Ccbcdocu.ImpDto = 0
        Ccbcdocu.ImpIgv = 0
        Ccbcdocu.ImpIsc = 0
        Ccbcdocu.ImpTot = 0
        Ccbcdocu.ImpExo = 0.
    FOR EACH Ccbddocu OF Ccbcdocu, FIRST Almmmatg OF Ccbddocu NO-LOCK:
        ASSIGN Ccbddocu.aftigv = Almmmatg.aftigv.   /* Comentar esta linea para facturas de servicios*/
        IF Ccbddocu.AftIgv THEN Ccbddocu.ImpIgv = Ccbddocu.ImpLin - ROUND( Ccbddocu.ImpLin  / ( 1 + (Ccbcdocu.PorIgv / 100) ), 4 ).
        ELSE Ccbddocu.ImpIgv = 0.
        ASSIGN
            F-Igv = F-Igv + Ccbddocu.ImpIgv
            F-Isc = F-Isc + Ccbddocu.ImpIsc
            Ccbcdocu.ImpTot = Ccbcdocu.ImpTot + Ccbddocu.ImpLin.
       IF NOT Ccbddocu.AftIgv THEN Ccbcdocu.ImpExo = Ccbcdocu.ImpExo + Ccbddocu.ImpLin.
/*        IF Ccbddocu.AftIgv = YES                                                                          */
/*        THEN Ccbcdocu.ImpDto = Ccbcdocu.ImpDto + ROUND(Ccbddocu.ImpDto / (1 + Ccbcdocu.PorIgv / 100), 2). */
/*        ELSE Ccbcdocu.ImpDto = Ccbcdocu.ImpDto + Ccbddocu.ImpDto.                                         */
    END.
    ASSIGN
        Ccbcdocu.ImpIgv = ROUND(F-IGV,2)
        Ccbcdocu.ImpIsc = ROUND(F-ISC,2)
        Ccbcdocu.ImpVta = Ccbcdocu.ImpTot - Ccbcdocu.ImpExo - Ccbcdocu.ImpIgv.
    ASSIGN
        Ccbcdocu.ImpBrt = Ccbcdocu.ImpVta + Ccbcdocu.ImpIsc + Ccbcdocu.ImpDto + Ccbcdocu.ImpExo
        Ccbcdocu.ImpTot = Ccbcdocu.ImpTot + Ccbcdocu.acubon[5].
    DISPLAY
        f-imp.
    UPDATE
        ccbcdocu.impbrt 
        ccbcdocu.impdto
        ccbcdocu.impexo
        ccbcdocu.impvta
        ccbcdocu.impigv 
        ccbcdocu.imptot 
        ccbcdocu.imptot2
        ccbcdocu.sdoact
        ccbcdocu.acubon[4]  /* Percepcion */
        ccbcdocu.acubon[5]
        CcbcDocu.ImpDto2
        CcbCDocu.Libre_d02
        ccbcdocu.flgest     /* C: cancelado, P: pendiente, A: anulado */
        WITH 1 COL.
END.
