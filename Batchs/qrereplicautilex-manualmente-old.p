
DEF NEW SHARED VAR s-codcia AS INT INIT 001.

DEF VAR x-FchDoc-1 AS DATE NO-UNDO.
DEF VAR x-FchDoc-2 AS DATE NO-UNDO.
DEFINE VARIABLE F-IGV AS DECIMAL NO-UNDO.
DEFINE VARIABLE F-ISC AS DECIMAL NO-UNDO.

/* Debe ser el mes pasado */
ASSIGN
    x-FchDoc-1 = 04/01/2015
    x-FchDoc-2 = 04/30/2015.

/* 1ro barremos comprobantes */
FOR EACH ccbcdocu WHERE ccbcdocu.codcia = s-codcia
    AND ccbcdocu.coddoc = 'TCK'
    AND ccbcdocu.fchdoc >= x-fchdoc-1
    AND ccbcdocu.fchdoc <= x-fchdoc-2:
    DISPLAY ccbcdocu.fchdoc ccbcdocu.coddoc ccbcdocu.nrodoc.
    PAUSE 0.
    ASSIGN
        Ccbcdocu.ImpBrt = 0
        Ccbcdocu.ImpDto = 0
        Ccbcdocu.ImpDto2 = 0
        Ccbcdocu.ImpIgv = 0
        Ccbcdocu.ImpIsc = 0
        Ccbcdocu.ImpTot = 0
        Ccbcdocu.ImpExo = 0
        Ccbcdocu.ImpVta = 0
        Ccbcdocu.Libre_d01 = 0  /* Descuento SIN IGV por Encartes y Otros */
        Ccbcdocu.Libre_d02 = 0  /* Descuento por Linea CON IGV */
        f-igv = 0
        f-isc = 0.
    FOR EACH Ccbddocu OF Ccbcdocu NO-LOCK:        
        Ccbcdocu.ImpTot = Ccbcdocu.ImpTot + (Ccbddocu.ImpLin - Ccbddocu.ImpDto2).
        Ccbcdocu.ImpDto2 = Ccbcdocu.ImpDto2 + Ccbddocu.ImpDto2.
        Ccbcdocu.Libre_d02 = Ccbcdocu.Libre_d02 + Ccbddocu.ImpDto.
        IF Ccbddocu.AftIgv = YES THEN DO:
            f-Igv = f-Igv + (Ccbddocu.ImpLin - Ccbddocu.ImpDto2) / ( 1 + CcbCDocu.PorIgv / 100) * CcbCDocu.PorIgv / 100.
            /*Ccbcdocu.ImpDto = Ccbcdocu.ImpDto + Ccbddocu.ImpDto / ( 1 + CcbCDocu.PorIgv / 100).*/
            Ccbcdocu.Libre_d01 = Ccbcdocu.Libre_d01 + ( Ccbddocu.ImpDto2 / ( 1 + CcbCDocu.PorIgv / 100) ).
        END.
        ELSE DO:
            Ccbcdocu.ImpExo = Ccbcdocu.ImpExo + (Ccbddocu.ImpLin - Ccbddocu.ImpDto2).
            /*Ccbcdocu.ImpDto = Ccbcdocu.ImpDto + Ccbddocu.ImpDto.*/
            Ccbcdocu.Libre_d01 = Ccbcdocu.Libre_d01 + Ccbddocu.ImpDto2.
        END.
    END.
    ASSIGN
        Ccbcdocu.ImpIgv = ROUND(f-Igv, 2)
        Ccbcdocu.ImpDto = ROUND(Ccbcdocu.ImpDto, 2)
        Ccbcdocu.Libre_d01 = ROUND(Ccbcdocu.Libre_d01, 2)
        Ccbcdocu.Libre_d02 = ROUND(Ccbcdocu.Libre_d02, 2)
        CcbCDocu.ImpVta = IF CcbCDocu.ImpIgv > 0 THEN CcbCDocu.ImpTot - Ccbcdocu.ImpExo - CcbCDocu.ImpIgv ELSE 0.
    ASSIGN
        CcbCDocu.ImpBrt = CcbCDocu.ImpVta + (CcbCDocu.ImpDto + Ccbcdocu.Libre_d01) /*+ CcbCDocu.ImpExo*/.
    IF CcbCDocu.PorIgv = 0.00     /* VENTA INAFECTA */
        THEN ASSIGN
            Ccbcdocu.ImpIgv = 0
            CcbCDocu.ImpVta = CcbCDocu.ImpExo
            CcbCDocu.ImpBrt = CcbCDocu.ImpExo.

END.

