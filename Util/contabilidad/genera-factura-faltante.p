DEF NEW SHARED VAR s-codcia AS INT INIT 001.
DEF NEW SHARED VAR s-coddiv AS CHAR.
DEF NEW SHARED VAR s-codalm AS CHAR.

FIND faccpedi WHERE faccpedi.codcia = s-codcia
    AND faccpedi.coddoc = 'P/M'
    AND faccpedi.nroped = '801048285'
    NO-LOCK.
s-coddiv = Faccpedi.coddiv.
s-codalm = ENTRY(1,Faccpedi.codalm).

DEF VAR s-coddoc AS CHAR INIT 'FAC' NO-UNDO.
DEF VAR s-nrodoc AS CHAR INIT '25300009417' NO-UNDO.
DEF VAR pMensaje AS CHAR NO-UNDO.

FIND FIRST ccbcdocu WHERE ccbcdocu.codcia = s-codcia AND 
                        ccbcdocu.coddoc = s-coddoc AND 
                        ccbcdocu.nrodoc = s-nrodoc NO-LOCK NO-ERROR.

IF AVAILABLE ccbcdocu THEN DO:
    MESSAGE "DOCUMENTO(" + s-coddoc + " " + s-nrodoc + ") YA EXISTE, IMPOSIBLE CREAR".
    RETURN.
END.

DO  TRANSACTION ON ERROR UNDO, LEAVE ON STOP UNDO, LEAVE:
    CREATE ccbcdocu.
    BUFFER-COPY Faccpedi TO Ccbcdocu
        ASSIGN 
        Ccbcdocu.coddoc = s-coddoc
        Ccbcdocu.nrodoc = s-nrodoc
        Ccbcdocu.codalm = s-codalm
        Ccbcdocu.flgest = 'P'.
    FOR EACH Facdpedi OF Faccpedi NO-LOCK:
        CREATE Ccbddocu.
        BUFFER-COPY Facdpedi TO Ccbddocu
            ASSIGN
            Ccbddocu.coddoc = Ccbcdocu.coddoc
            Ccbddocu.nrodoc = Ccbcdocu.nrodoc
            CcbDDocu.CanDes = FacDPedi.CanPed 
            CcbDDocu.impdcto_adelanto[4] = FacDPedi.Libre_d02
            .
    END.
    RUN Graba-Totales.

    RUN vta2/act_almv2 ( ROWID(Ccbcdocu), OUTPUT pMensaje).
    IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
        MESSAGE pMensaje VIEW-AS ALERT-BOX WARNING.
        UNDO, LEAVE.
    END.
END.


PROCEDURE Graba-Totales:


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

