DEF VAR cFlgEst AS CHAR INIT "C".
DEF VAR s-codcia AS INT INIT 001.
DEF VAR cTpoRef AS CHAR INIT "BOV".
DEF VAR moneda AS CHARACTER NO-UNDO.
DEF VAR importe AS DECIMAL NO-UNDO.
DEF VAR TarjCre AS CHARACTER NO-UNDO.
DEF VAR NroTarj AS CHARACTER NO-UNDO.

&SCOPED-DEFINE Condicion ( CcbDMvto.CodCia = S-CodCia ~
AND CcbDMvto.TpoRef = cTpoRef ~
AND CcbDMvto.FlgEst BEGINS cFlgEst ~
AND CcbDMvto.FchEmi >= 01/01/2012 ~
AND CcbDMvto.FchEmi <= 03/31/2012 )

OUTPUT TO c:\tmp\depositosconfirmados.txt.
FOR EACH CcbDMvto WHERE {&Condicion} NO-LOCK:
    IF NUM-ENTRIES(CcbDMvto.NroRef,"|") > 1 THEN DO:
        TarjCre = ENTRY(1,CcbDMvto.NroRef,"|").
        NroTarj = ENTRY(2,CcbDMvto.NroRef,"|").
    END.
    ELSE DO:                               
        TarjCre = CcbDMvto.NroRef.
        NroTarj = "".
    END.
    IF CcbDMvto.DepUsa[1] > 0 THEN DO:
        Moneda = "US$".
        Importe = CcbDMvto.DepUsa[1].
    END.
    ELSE DO:
        Moneda = "S/.".
        Importe = CcbDMvto.DepNac[1].
    END.
    DISPLAY
        CcbDMvto.CodDiv FORMAT 'x(5)' '|'
        coddoc '|'
        nrodoc '|'
        codbco '|'
        codcta '|'
        nrodep '|'
        fchemi '|'
        Moneda '|'
        importe
        WITH STREAM-IO NO-BOX WIDTH 320 NO-LABELS.
    PAUSE 0.
END.
OUTPUT CLOSE.
