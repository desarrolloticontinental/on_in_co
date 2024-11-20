
DEF NEW SHARED VAR s-FlgSit AS CHAR INIT "YES".     /* Bloqueado x defecto */

/* Local Variable Definitions ---                                       */
DEF NEW SHARED VAR s-coddoc  AS CHAR INITIAL "E/C".
DEF NEW SHARED VAR s-ptovta  AS INT.
DEF NEW SHARED VAR s-tipo    AS CHAR INITIAL "ANTREC".

DEF NEW SHARED VAR s-codcia AS INT INIT 001.
DEF NEW SHARED VAR cl-codcia AS INT INIT 000.
DEF NEW SHARED VAR s-coddiv LIKE gn-divi.coddiv INIT '00000'.
DEF NEW SHARED VAR s-codter LIKE ccbcterm.codter INIT 'ATE04'.
DEF NEW SHARED VAR s-user-id AS CHAR INIT 'GCP-00'.

{ccb\i-ChqUser.i}

FIND CcbDTerm WHERE 
    CcbDTerm.CodCia = s-codcia AND
    CcbDTerm.CodDiv = s-coddiv AND
    CcbDTerm.CodDoc = s-coddoc AND
    CcbDTerm.CodTer = s-codter NO-LOCK NO-ERROR.
IF NOT AVAILABLE ccbdterm THEN DO:
    MESSAGE
        "Egreso de caja no esta configurado en este terminal"
        VIEW-AS ALERT-BOX ERROR.
    RETURN "ADM-ERROR".
END.
  
s-ptovta = ccbdterm.nroser.
  
/* Control de correlativos */
FIND FacCorre WHERE 
    faccorre.codcia = s-codcia AND 
    faccorre.coddoc = s-coddoc AND
    faccorre.nroser = s-ptovta NO-LOCK NO-ERROR.
IF NOT AVAILABLE FacCorre THEN DO:
    MESSAGE
        "No esta definida la serie" s-ptovta SKIP
        "para el ingreso a caja" VIEW-AS ALERT-BOX ERROR.
    RETURN "ADM-ERROR".
END.

DEF BUFFER B-CDocu FOR Ccbcdocu.


DEF VAR cLinea  AS CHAR NO-UNDO.
DEF VAR cCodDoc AS CHAR NO-UNDO.
DEF VAR cNroDoc AS CHAR NO-UNDO.
DEF VAR cGlosa  AS CHAR NO-UNDO.
DEF VAR fFecha  AS DATE NO-UNDO.

fFecha = DATE(10,29,2024).

DEF STREAM Salida.

INPUT FROM d:\egresos.prn.
OUTPUT STREAM Salida TO d:\errores-egresos.txt.
REPEAT:
    IMPORT UNFORMATTED cLinea.
    IF TRUE <> (cLinea > '') THEN LEAVE.
    ASSIGN
        s-Tipo = SUBSTRING(cLinea,1,10)
        cCodDoc = SUBSTRING(cLinea,11,10)
        cNroDoc = SUBSTRING(cLinea,21,15)
        cGlosa = SUBSTRING(cLinea,36).
    RUN Master-Transaction.
    IF RETURN-VALUE = 'ADM-ERROR' 
        THEN PUT STREAM Salida UNFORMATTED s-Tipo ' ' cCodDoc ' ' cNroDoc ' ' cGlosa SKIP.
END.
INPUT CLOSE.
OUTPUT STREAM Salida CLOSE.

/* ************************* */
PROCEDURE Master-Transaction:
/* ************************* */

FIND CcbCDocu WHERE CcbCDocu.CodCia = s-codcia AND
    CcbCDocu.CodDoc = cCodDoc AND 
    CcbCDocu.NroDoc = cNroDoc
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE CcbCDocu THEN DO:
    RETURN 'ADM-ERROR'.
END.

IF Ccbcdocu.flgest <> "P" THEN RETURN 'ADM-ERROR'.

FIND gn-clie WHERE gn-clie.CodCia = cl-codcia AND
    gn-clie.CodCli = Ccbcdocu.codcli
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE gn-clie  THEN DO:
    RETURN 'ADM-ERROR'.
END.

RUN Create-Transaction.

END PROCEDURE.

/* ************************* */
PROCEDURE Create-Transaction:
/* ************************* */

    FIND FacCorre WHERE
        FacCorre.CodCia = s-codcia AND
        FacCorre.CodDoc = s-coddoc AND
        FacCorre.NroSer = s-ptovta
        EXCLUSIVE-LOCK NO-ERROR.
    IF ERROR-STATUS:ERROR THEN UNDO, RETURN 'ADM-ERROR'.

    CREATE CcbCCaja.
    ASSIGN
        CcbCCaja.CodCia  = s-codcia
        CcbCCaja.CodDoc  = s-coddoc
        CcbCCaja.NroDoc  = STRING(faccorre.nroser, "999") + STRING(faccorre.correlativo, "999999")
        CcbCCaja.CodDiv  = S-CODDIV
        CcbCCaja.Tipo    = s-tipo
        CcbCCaja.usuario = s-user-id
        CcbCCaja.CodCaja = S-CODTER
        CcbCCaja.FchDoc  = fFecha            
        CcbCCaja.ImpNac[1] = (IF Ccbcdocu.codmon = 1 THEN Ccbcdocu.sdoact ELSE 0)
        CcbCCaja.ImpUsa[1] = (IF Ccbcdocu.codmon = 2 THEN Ccbcdocu.sdoact ELSE 0)
        CcbcCaja.Flgest    = "C"
        CcbCCaja.Voucher[1] = STRING(cCodDoc,"XXX") + TRIM(cNroDoc)
        CcbCCaja.CodCli  = Ccbcdocu.CodCli
        CcbCCaja.NomCli  = gn-clie.NomCli
        CcbCCaja.Voucher[10] = STRING(TIME,"HH:MM:SS").
    ASSIGN
        CcbCCaja.Glosa = cGlosa
        CcbCCaja.CodCli = Ccbcdocu.codcli
        CcbCCaja.NomCli = Ccbcdocu.nomcli
        .

    FIND LAST gn-tccja WHERE gn-tccja.Fecha <= fFecha NO-LOCK NO-ERROR.
    IF AVAILABLE gn-tccja THEN CcbCCaja.TpoCmb = gn-tccja.Compra.

    ASSIGN
        FacCorre.Correlativo = FacCorre.Correlativo + 1.
    RELEASE faccorre.
    CREATE CcbDCaja.
    ASSIGN
        CcbDCaja.CodCia = CcbCCaja.CodCia   
        CcbDCaja.CodDoc = CcbCCaja.CodDoc
        CcbDCaja.NroDoc = CcbCCaja.NroDoc
        CcbDCaja.CodMon = Ccbcdocu.codmon
        CcbDCaja.CodRef = CcbCCaja.CodDoc
        CcbDCaja.NroRef = CcbCCaja.NroDoc
        CcbDCaja.FchDoc = CcbCCaja.FchDoc
        CcbDCaja.CodDiv = CcbCCaja.CodDiv
        CcbDCaja.TpoCmb = CcbCCaja.TpoCmb.
    IF Ccbcdocu.codmon = 1 THEN DO:
        CcbDCaja.ImpTot = CcbCCaja.ImpNac[1].
    END.
    IF Ccbcdocu.codmon = 2 THEN DO:
        CcbDCaja.ImpTot = CcbCCaja.ImpUsa[1].
    END.
    RUN proc_CancelaDoc.
    IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.

    RETURN 'OK'.

END PROCEDURE.

/* ********************** */
PROCEDURE proc_CancelaDoc:
/* ********************** */

    FIND FIRST B-CDocu WHERE
        B-CDocu.CodCia = s-codcia AND
        B-CDocu.CodDoc = cCodDoc AND
        B-CDocu.NroDoc = cNroDoc
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE B-CDocu THEN RETURN "ADM-ERROR".

    DO TRANSACTION ON ERROR UNDO, RETURN "ADM-ERROR":
        CREATE CCBDMOV.
        ASSIGN
            CCBDMOV.CodCia = CcbCcaja.CodCia
            CCBDMOV.CodDiv = CcbCcaja.CodDiv
            CCBDMOV.CodCli = B-CDocu.CodCli
            CCBDMOV.CodDoc = B-CDocu.CodDoc
            CCBDMOV.NroDoc = B-CDocu.NroDoc
            CCBDMOV.CodMon = B-CDocu.CodMon
            CCBDMOV.CodRef = CcbCcaja.CodDoc
            CCBDMOV.NroRef = CcbCcaja.NroDoc
            CCBDMOV.FchDoc = B-CDocu.FchDoc
            CCBDMOV.TpoCmb = CcbCcaja.TpoCmb
            CCBDMOV.FchMov = fFecha
            CCBDMOV.HraMov = STRING("HH:MM:SS")
            CCBDMOV.usuario = s-User-ID.

        IF B-CDocu.CodMon = 1 THEN DO:
            IF CcbCCaja.ImpNac[1] <> 0 THEN ASSIGN CCBDMOV.ImpTot = CcbCCaja.ImpNac[1].
            ELSE ASSIGN CCBDMOV.ImpTot = CcbCCaja.Impusa[1] * CcbCCaja.TpoCmb.
        END.
        ELSE DO:
            IF CcbCCaja.ImpUsa[1] <> 0 THEN ASSIGN CCBDMOV.ImpTot = CcbCCaja.ImpUsa[1].
            ELSE ASSIGN CCBDMOV.ImpTot = CcbCCaja.ImpNac[1] / CcbCCaja.TpoCmb.
        END.
        ASSIGN CCBDMOV.ImpTot = ROUND(CCBDMOV.ImpTot,2).
        RUN proc_ActualizaDoc
            (
            CCBDMOV.CodDoc,
            CCBDMOV.NroDoc,
            CCBDMOV.ImpTot,
            FALSE
            ).

    END.

    RETURN 'OK'.

END PROCEDURE.

/* ************************* */
PROCEDURE proc_ActualizaDoc:
/* ************************* */

    DEFINE INPUT PARAMETER para_CodDoc AS CHAR.
    DEFINE INPUT PARAMETER para_NroDoc AS CHAR.
    DEFINE INPUT PARAMETER para_ImpTot AS DECIMAL.
    DEFINE INPUT PARAMETER para_SumRes AS LOGICAL.

    DO TRANSACTION ON ERROR UNDO, RETURN ERROR.
        FIND FIRST CcbCDocu WHERE
            CcbCDocu.Codcia = S-CODCIA AND
            CcbCDocu.Coddoc = para_CodDoc AND
            CcbCDocu.Nrodoc = para_NroDoc
            EXCLUSIVE-LOCK NO-ERROR.
        IF AVAILABLE CcbCDocu THEN DO:
            IF para_SumRes THEN CcbCDocu.SdoAct = CcbCDocu.SdoAct + para_ImpTot.
            ELSE CcbCDocu.SdoAct = CcbCDocu.SdoAct - para_ImpTot.
            IF CcbCDocu.SdoAct <= 0 
            THEN ASSIGN CcbCDocu.FlgEst = "C"
                        CcbCDocu.FchCan = fFecha.
            ELSE ASSIGN CcbCDocu.FlgEst = "P".
        END.
        ELSE RETURN ERROR.
        RELEASE CcbCDocu.
    END.


END PROCEDURE.
