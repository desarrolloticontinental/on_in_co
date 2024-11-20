DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR cl-codcia AS INT INIT 000 NO-UNDO.
DEF VAR s-coddoc AS CHAR INIT 'DCO' NO-UNDO.
DEF VAR s-TpoFac AS CHAR INIT 'VALES' NO-UNDO.
DEF VAR s-coddiv AS CHAR INIT '00024' NO-UNDO.
DEF VAR s-user-id AS CHAR INIT 'ADMIN' NO-UNDO.
DEF VAR x-imptot LIKE ccbcdocu.imptot NO-UNDO.
DEF VAR x-fchvto LIKE ccbcdocu.fchvto NO-UNDO.

DEF VAR x-linea AS CHAR NO-UNDO.
DEF BUFFER b-cdocu FOR ccbcdocu.
DEF TEMP-TABLE detalle
    FIELD codcia AS INT
    FIELD coddoc AS CHAR
    FIELD nrodoc AS CHAR FORMAT 'x(12)'
    FIELD codcli AS CHAR FORMAT 'x(12)'
    FIELD imptot AS DEC.

FIND FIRST FacDocum WHERE FacDocum.CodCia = s-codcia
    AND FacDocum.CodDoc = s-coddoc
    NO-LOCK NO-ERROR.
FIND FIRST FacCorre WHERE 
           FacCorre.CodCia = S-CODCIA AND 
           FacCorre.CodDoc = S-CODDOC AND 
           FacCorre.CodDiv = S-CODDIV AND
           FacCorre.FlgEst = YES
           NO-LOCK NO-ERROR.
IF NOT AVAILABLE FacCorre OR NOT AVAILABLE FacDocum THEN DO:
    MESSAGE "Codigo de Documento no configurado:" s-coddoc VIEW-AS ALERT-BOX ERROR.
    RETURN.
END.
FIND FacCfgGn WHERE FacCfgGn.CodCia = S-CODCIA NO-LOCK NO-ERROR.

DEF VAR s-NroSer AS INT NO-UNDO.

s-NroSer = FacCorre.NroSer.

INPUT FROM d:\tmp\vales.prn.
REPEAT :
    IMPORT UNFORMATTED x-linea.
    IF x-linea = '' THEN LEAVE.
    CREATE detalle.
    ASSIGN
        detalle.codcia = s-codcia
        detalle.coddoc = SUBSTRING(x-linea,1,5)
        detalle.nrodoc = SUBSTRING(x-linea,6,15)
        detalle.codcli = SUBSTRING(x-linea,21,15)
        detalle.imptot = DECIMAL(SUBSTRING(x-linea,36)).
END.
INPUT CLOSE.
DEF VAR x-Partes AS INT NO-UNDO.
DEF VAR x-Vencimiento AS DATE EXTENT 3 NO-UNDO.
DEF VAR x-Importe AS DEC EXTENT 3 NO-UNDO.
DEF VAR x-Acumulado AS DEC NO-UNDO.
DEF VAR k AS INT NO-UNDO.
FOR EACH detalle BREAK BY detalle.codcli:
    IF FIRST-OF(detalle.codcli) THEN x-ImpTot = 0.
    x-ImpTot = x-imptot + detalle.imptot.
    FIND B-CDOCU WHERE B-CDOCU.codcia = detalle.codcia
        AND B-CDOCU.coddoc = detalle.coddoc
        AND B-CDOCU.nrodoc = detalle.nrodoc
        EXCLUSIVE-LOCK.
    /* Creamos la cancelación */
    CREATE Ccbdcaja.
    BUFFER-COPY B-CDOCU TO Ccbdcaja
        ASSIGN
        CcbDCaja.FchDoc = TODAY
        CcbDCaja.CodRef = B-CDOCU.CodDoc
        CcbDCaja.NroRef = B-CDOCU.NroDoc
        CcbDCaja.ImpTot = detalle.ImpTot.
    ASSIGN
        B-CDOCU.SdoAct = B-CDOCU.SdoAct - CcbDCaja.ImpTot.
    IF B-CDOCU.SdoAct <= 0 THEN ASSIGN B-CDOCU.FlgEst = "C" B-CDOCU.FchCan = TODAY.
    IF LAST-OF(detalle.codcli) THEN DO:
        /* Por defecto */
        ASSIGN
            x-Partes = 2
            x-Vencimiento[1] = DATE(04,30,2017)
            x-Vencimiento[2] = DATE(05,29,2017)
            x-Vencimiento[3] = ?
            x-Importe[1] = 0
            x-Importe[2] = 0
            x-Importe[3] = 0.
        IF x-ImpTot > 10000 THEN
            ASSIGN
                x-Partes = 3
                x-Vencimiento[1] = DATE(04,30,2017)
                x-Vencimiento[2] = DATE(05,29,2017)
                x-Vencimiento[3] = DATE(06,30,2017).
        /* Repartimos el importe */
        x-Acumulado = 0.
        DO k = 1 TO x-Partes:
            x-Importe[k] = ROUND(x-ImpTot / x-Partes, 2).
            x-Acumulado = x-Acumulado + x-Importe[k].
        END.
        /* Ajustamos ultimo valor */
        x-Importe[x-Partes] = x-Importe[x-Partes] + (x-ImpTot - x-Acumulado).
        /* Creamos los documentos */
        DO k = 1 TO x-Partes:
            {vtagn/i-faccorre-01.i &Codigo = s-coddoc &Serie = s-nroser}
            CREATE Ccbcdocu.
            BUFFER-COPY B-CDOCU TO Ccbcdocu
                ASSIGN 
                Ccbcdocu.CodCia = S-CODCIA
                Ccbcdocu.CodDiv = S-CODDIV
                Ccbcdocu.CodDoc = s-coddoc 
                Ccbcdocu.NroDoc = STRING(FacCorre.NroSer,"999") + STRING(FacCorre.Correlativo,"999999")
                Ccbcdocu.TpoFac = s-TpoFac
                Ccbcdocu.FlgEst = "P"     /* PENDIENTE */
                CcbCDocu.Tipo   = "CREDITO"
                CcbCDocu.TipVta = "2"
                CcbCDocu.usuario = S-USER-ID
                CcbCDocu.HorCie = STRING(TIME, 'HH:MM')
                Ccbcdocu.fchdoc = TODAY
                Ccbcdocu.fchvto = x-Vencimiento[k]
                Ccbcdocu.codven = '020'
                Ccbcdocu.fmapgo = '001'
                Ccbcdocu.imptot = x-Importe[k].
            ASSIGN
                CcbCDocu.TpoCmb = FacCfgGn.TpoCmb[1]
                CcbCDocu.PorIgv = FacCfgGn.PorIgv
                Ccbcdocu.FlgCbd = YES     /* AFECTO */
                Ccbcdocu.ImpDto = 0
                Ccbcdocu.ImpDto2 = 0
                Ccbcdocu.ImpIgv = 0
                Ccbcdocu.ImpIsc = 0
                Ccbcdocu.ImpExo = 0.
            ASSIGN
                Ccbcdocu.ImpVta = ROUND(Ccbcdocu.ImpTot / ( 1 + Ccbcdocu.PorIgv / 100 ), 2)
                Ccbcdocu.ImpIgv = Ccbcdocu.ImpTot - Ccbcdocu.ImpVta
                Ccbcdocu.ImpBrt = Ccbcdocu.ImpVta
                Ccbcdocu.SdoAct = Ccbcdocu.ImpTot.
            ASSIGN
                FacCorre.Correlativo = FacCorre.Correlativo + 1.
        END.
    END.
END.
