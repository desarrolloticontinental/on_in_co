DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR cl-codcia AS INT INIT 000 NO-UNDO.
DEF VAR s-coddoc AS CHAR INIT 'DCO' NO-UNDO.
DEF VAR s-TpoFac AS CHAR INIT 'VALES' NO-UNDO.
DEF VAR s-coddiv AS CHAR INIT '00024' NO-UNDO.
DEF VAR s-user-id AS CHAR INIT 'ADMIN' NO-UNDO.
DEF VAR x-imptot LIKE ccbcdocu.imptot NO-UNDO.
DEF VAR x-fchvto LIKE ccbcdocu.fchvto NO-UNDO.

DEF VAR x-linea AS CHAR NO-UNDO.

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
    FIND gn-clie WHERE gn-clie.codcia = cl-codcia
        AND gn-clie.codcli = SUBSTRING(x-linea,1,20)
        NO-LOCK.
    ASSIGN
        x-fchvto = DATE(SUBSTRING(x-linea,21,20))
        x-imptot = DECIMAL(SUBSTRING(x-linea,41)).

    {vtagn/i-faccorre-01.i &Codigo = s-coddoc &Serie = s-nroser}

    CREATE Ccbcdocu.
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
        CcbCDocu.HorCie = STRING(TIME, 'HH:MM').
    ASSIGN
        Ccbcdocu.codcli = SUBSTRING(x-linea,1,20)
        Ccbcdocu.fchdoc = TODAY
        Ccbcdocu.fchvto = x-fchvto
        Ccbcdocu.codven = '020'
        Ccbcdocu.fmapgo = '001'
        Ccbcdocu.codmon = 1
        Ccbcdocu.imptot = x-imptot.
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
    FIND gn-clie WHERE gn-clie.CodCia = cl-codcia 
        AND gn-clie.CodCli = CcbCDocu.CodCli NO-LOCK NO-ERROR.
    IF AVAILABLE gn-clie  THEN DO:
        ASSIGN 
            Ccbcdocu.ruccli  = gn-clie.ruc
            Ccbcdocu.nomcli  = gn-clie.nomcli
            Ccbcdocu.dircli  = gn-clie.dircli
            CcbCDocu.CodDpto = gn-clie.CodDept 
            CcbCDocu.CodProv = gn-clie.CodProv 
            CcbCDocu.CodDist = gn-clie.CodDist.
    END.

END.
INPUT CLOSE.

