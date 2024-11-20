DEF NEW SHARED VAR s-codcia AS INT INIT 001.
DEF NEW SHARED VAR s-coddiv AS CHAR INIT '00032'.
DEF NEW SHARED VAR s-codalm AS CHAR INIT '32'.
    
DEF VAR s-coddoc AS CHAR INIT 'BOL' NO-UNDO.
DEF VAR s-nrodoc AS CHAR INIT '26600017813' NO-UNDO.
DEF VAR pMensaje AS CHAR NO-UNDO.

DO TRANSACTION ON ERROR UNDO, RETURN ON STOP UNDO, RETURN:
    CREATE ccbcdocu.
    ASSIGN
        ccbcdocu.codcia = s-codcia
        ccbcdocu.coddiv = s-coddiv
        ccbcdocu.coddoc = s-coddoc
        ccbcdocu.nrodoc = s-nrodoc
        ccbcdocu.codalm = s-codalm 
        ccbcdocu.fchdoc = DATE(01/12/2018)
        ccbcdocu.fchvto = DATE(01/12/2018)
        ccbcdocu.flgest = "P"
        ccbcdocu.usuario = "AHM-32"
        ccbcdocu.codcli = "11111111111"
        ccbcdocu.codven = "888"
        ccbcdocu.codmon = 1
        ccbcdocu.codref = "P/M"
        ccbcdocu.nroref = "032068958"
        ccbcdocu.fmapgo = "000"
        ccbcdocu.porigv = 18
        .
    UPDATE ccbcdocu WITH 1 COL.
    CREATE ccbddocu.
    BUFFER-COPY ccbcdocu TO ccbddocu.
    UPDATE ccbddocu WITH 1 COL.
    RUN vta2/act_almv2 ( ROWID(Ccbcdocu), OUTPUT pMensaje).
    IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
        MESSAGE pMensaje VIEW-AS ALERT-BOX WARNING.
        UNDO, LEAVE.
    END.
    RUN Graba-Totales.
END.


PROCEDURE Graba-Totales:
/* ******************** */

    DEFINE VARIABLE F-IGV AS DECIMAL NO-UNDO.
    DEFINE VARIABLE F-ISC AS DECIMAL NO-UNDO.

    /* RHC 06/03/2015 CALCULO CORREGIDO DE TICKETS UTILEX */
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
        IF Ccbddocu.AftIgv = YES THEN DO:
            f-Igv = f-Igv + (Ccbddocu.ImpLin - Ccbddocu.ImpDto2) / ( 1 + Ccbcdocu.PorIgv / 100) * Ccbcdocu.PorIgv / 100.
            Ccbcdocu.Libre_d01 = Ccbcdocu.Libre_d01 + ( Ccbddocu.ImpDto2 / ( 1 + Ccbcdocu.PorIgv / 100) ).
        END.
        ELSE DO:
            Ccbcdocu.ImpExo = Ccbcdocu.ImpExo + (Ccbddocu.ImpLin - Ccbddocu.ImpDto2).
            Ccbcdocu.Libre_d01 = Ccbcdocu.Libre_d01 + Ccbddocu.ImpDto2.
        END.
    END.
    ASSIGN
        Ccbcdocu.ImpIgv = ROUND(f-Igv, 2)
        Ccbcdocu.ImpDto = ROUND(Ccbcdocu.ImpDto, 2)
        Ccbcdocu.Libre_d01 = ROUND(Ccbcdocu.Libre_d01, 2)
        Ccbcdocu.ImpVta = IF Ccbcdocu.ImpIgv > 0 THEN Ccbcdocu.ImpTot - Ccbcdocu.ImpExo - Ccbcdocu.ImpIgv ELSE 0.
    ASSIGN
        Ccbcdocu.ImpBrt = Ccbcdocu.ImpVta + (Ccbcdocu.ImpDto + Ccbcdocu.Libre_d01) /*+ Ccbcdocu.ImpExo*/.
    IF Ccbcdocu.PorIgv = 0.00     /* VENTA INAFECTA */
        THEN ASSIGN
            Ccbcdocu.ImpIgv = 0
            Ccbcdocu.ImpVta = Ccbcdocu.ImpExo
            Ccbcdocu.ImpBrt = Ccbcdocu.ImpExo.
    ASSIGN
        Ccbcdocu.SdoAct  = Ccbcdocu.ImpTot.

END PROCEDURE.

