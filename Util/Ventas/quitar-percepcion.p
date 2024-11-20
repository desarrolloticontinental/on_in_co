DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEFINE VARIABLE F-IGV LIKE Ccbcdocu.ImpIgv NO-UNDO.
DEFINE VARIABLE F-ISC LIKE Ccbcdocu.ImpIsc NO-UNDO.
DEFINE VARIABLE F-ImpDtoAdelanto AS DECIMAL NO-UNDO.
DEFINE VARIABLE F-IgvDtoAdelanto AS DECIMAL NO-UNDO.
DEFINE VARIABLE F-ImpLin LIKE Ccbddocu.ImpLin NO-UNDO.

FOR EACH ccbcdocu WHERE codcia = 1
    AND coddoc = 'FAC'
    AND nrodoc >= '015149379'
    AND nrodoc <= '015149388':
    DISPLAY nrodoc nomcli acubon[5] WITH STREAM-IO NO-BOX.
    FOR EACH ccbddocu OF ccbcdocu:
        ASSIGN
            ccbddocu.pordcto_adelanto[5] = 0
            ccbddocu.impdcto_adelanto[5] = 0.
    END.
    ASSIGN
        ccbcdocu.acubon[4] = 0
        ccbcdocu.acubon[5] = 0.

    ASSIGN
        CcbCDocu.ImpDto = 0
        CcbCDocu.ImpIgv = 0
        CcbCDocu.ImpIsc = 0
        CcbCDocu.ImpTot = 0
        CcbCDocu.ImpExo = 0
        /*CcbCDocu.ImpTot2= 0*/
        F-IGV = 0
        F-ISC = 0
        F-ImpDtoAdelanto = 0
        F-ImpLin = 0.
    /* RHC 14/03/2013 Nuevo cálculo */
    FOR EACH CcbDDocu OF CcbCDocu NO-LOCK:
        ASSIGN
            F-Igv = F-Igv + CcbDDocu.ImpIgv
            F-Isc = F-Isc + CcbDDocu.ImpIsc
            CcbCDocu.ImpTot = CcbCDocu.ImpTot + Ccbddocu.ImpLin.
        /* Importe Inafecto o Exonerado */
        IF CcbDDocu.ImpIgv = 0 THEN CcbCDocu.ImpExo = CcbCDocu.ImpExo + F-ImpLin.
    END.
    ASSIGN
        CcbCDocu.ImpIsc = ROUND(F-ISC,2)
        CcbCDocu.ImpVta = ROUND( (CcbCDocu.ImpTot - CcbCDocu.ImpExo) / (1 + CcbCDocu.PorIgv / 100), 2).
    IF CcbCDocu.ImpExo = 0 THEN CcbCDocu.ImpIgv = CcbCDocu.ImpTot - CcbCDocu.ImpVta.
    ELSE CcbCDocu.ImpIgv = ROUND(CcbCDocu.ImpVta * CcbCDocu.PorIgv / 100, 2).
    ASSIGN
        CcbCDocu.ImpBrt = CcbCDocu.ImpVta /*+ CcbCDocu.ImpIsc*/ + CcbCDocu.ImpDto /*+ CcbCDocu.ImpExo*/
        CcbCDocu.SdoAct = CcbCDocu.ImpTot.

    IF CcbCDocu.PorIgv = 0.00     /* VENTA INAFECTA */
        THEN ASSIGN
            CcbCDocu.ImpVta = CcbCDocu.ImpExo
            CcbCDocu.ImpBrt = CcbCDocu.ImpExo.
END.


