/* COMPROBANTES INAFECTOS */
DEFINE VARIABLE F-IGV AS DECIMAL NO-UNDO.
DEFINE VARIABLE F-ISC AS DECIMAL NO-UNDO.

FOR EACH ccbcdocu WHERE codcia = 1
    AND coddoc = 'fac'
    AND nrodoc >= '015139680'
    AND nrodoc <= '015139680':
    DISPLAY nrodoc fchdoc nomcli.
    ASSIGN
        ccbcdocu.porigv = 0.00.
    FOR EACH ccbddocu OF ccbcdocu:
        ASSIGN
            ccbddocu.aftigv = NO
            ccbddocu.impigv = 0.
        ASSIGN
            ccbddocu.PorDcto_Adelanto[5] = 0
            ccbddocu.ImpDcto_Adelanto[5] = 0.
    END.
    ASSIGN
        f-Igv = 0
        f-Isc = 0
        Ccbcdocu.ImpDto = 0
        Ccbcdocu.ImpIgv = 0
        Ccbcdocu.ImpIsc = 0
        Ccbcdocu.ImpTot = 0
        Ccbcdocu.ImpExo = 0.
    FOR EACH Ccbddocu OF Ccbcdocu NO-LOCK:        
       F-Igv = F-Igv + Ccbddocu.ImpIgv.
       F-Isc = F-Isc + Ccbddocu.ImpIsc.
       Ccbcdocu.ImpTot = Ccbcdocu.ImpTot + Ccbddocu.ImpLin.
       IF NOT Ccbddocu.AftIgv THEN Ccbcdocu.ImpExo = Ccbcdocu.ImpExo + Ccbddocu.ImpLin.
       IF Ccbddocu.AftIgv = YES
       THEN Ccbcdocu.ImpDto = Ccbcdocu.ImpDto + ROUND(Ccbddocu.ImpDto / (1 + Ccbcdocu.PorIgv / 100), 2).
       ELSE Ccbcdocu.ImpDto = Ccbcdocu.ImpDto + Ccbddocu.ImpDto.
            
    END.
    ASSIGN
        Ccbcdocu.ImpIgv = ROUND(F-IGV,2)
        Ccbcdocu.ImpIsc = ROUND(F-ISC,2)
        Ccbcdocu.ImpVta = Ccbcdocu.ImpTot - Ccbcdocu.ImpExo - Ccbcdocu.ImpIgv.
    ASSIGN
        CcbCDocu.ImpVta = CcbCDocu.ImpExo
        CcbCDocu.ImpBrt = CcbCDocu.ImpExo.    
    UPDATE
        ccbcdocu.impbrt
        ccbcdocu.impexo
        ccbcdocu.impdto
        ccbcdocu.impvta
        ccbcdocu.porigv
        ccbcdocu.impigv
        ccbcdocu.imptot
        ccbcdocu.sdoact
        ccbcdocu.acubon[4]  /* Percepcion */
        ccbcdocu.acubon[5]
        WITH 1 COL.
END.
