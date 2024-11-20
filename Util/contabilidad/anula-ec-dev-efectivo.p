DEF VAR f-CodDoc  AS CHAR INITIAL "A/R".
DEF VAR FILL-IN-NroDoc AS CHAR.
DEF VAR s-codcia AS INT INIT 001.
DEF VAR s-coddiv AS CHAR.

DEF BUFFER B-CDocu FOR Ccbcdocu.
DEF BUFFER b-ccbccaja FOR Ccbccaja.

DISABLE TRIGGERS FOR LOAD OF ccbccaja.
DISABLE TRIGGERS FOR LOAD OF ccbdcaja.
DISABLE TRIGGERS FOR LOAD OF ccbcdocu.
DISABLE TRIGGERS FOR LOAD OF ccbdmov.

FOR EACH ccbccaja EXCLUSIVE-LOCK WHERE codcia = s-codcia
    AND coddoc = 'E/C'
    AND nrodoc >= '013000036'
    AND nrodoc <= '013000073'
    AND LOOKUP(Tipo, 'ANTREC,DEVONC,DEVOBD') > 0:
    s-coddiv = ccbccaja.coddiv.
    f-CodDoc = SUBSTRING(CcbCCaja.Voucher[1],1,3).
    FILL-IN-NroDoc = SUBSTRING(CcbCCaja.Voucher[1],4).
    DISPLAY ccbccaja.coddoc ccbccaja.nrodoc.
    /*CcbCCaja.Voucher[1] = STRING(f-CodDoc,"XXX") + TRIM(FILL-IN-NroDoc)*/
    RUN Anular.
END.

PROCEDURE Anular:
    
    DEFINE VARIABLE monto_aplic LIKE CCBDMOV.ImpTot.

    IF ccbccaja.flgest = "A" THEN DO:
        MESSAGE ccbccaja.coddoc ccbccaja.nrodoc SKIP
            "Registro ya fue Anulado" VIEW-AS ALERT-BOX ERROR.
        RETURN "ADM-ERROR".
    END.

    DO TRANSACTION ON ERROR UNDO, RETURN "ADM-ERROR":

        FIND FIRST B-CDocu WHERE
            B-CDocu.CodCia = s-codcia AND
            B-CDocu.CodDoc = f-CodDoc AND
            B-CDocu.NroDoc = FILL-IN-NroDoc
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE B-CDocu THEN DO:
            MESSAGE f-coddoc fill-in-nrodoc SKIP
                'Documento no registrado'
                VIEW-AS ALERT-BOX ERROR.
            RETURN "ADM-ERROR".
        END.

        /* Elimina Detalle de la Aplicación */
        FOR EACH CCBDMOV WHERE
            CCBDMOV.CodCia = s-CodCia AND
            CCBDMOV.CodDiv = s-CodDiv AND
            CCBDMOV.NroDoc = B-CDocu.NroDoc AND
            CCBDMOV.CodDoc = B-CDocu.CodDoc
            EXCLUSIVE-LOCK:
            /* Referencia E/C */
            IF CCBDMOV.CodRef = CcbCcaja.coddoc AND
                CCBDMOV.NroRef = CcbCcaja.NroDoc THEN
                DELETE CCBDMOV.
        END.

        IF B-CDocu.CodMon = 1 THEN DO:
            IF CcbCCaja.ImpNac[1] <> 0 THEN ASSIGN monto_aplic = CcbCCaja.ImpNac[1].
            ELSE ASSIGN monto_aplic = CcbCCaja.Impusa[1] * CcbCCaja.TpoCmb.
        END.
        ELSE DO:
            IF CcbCCaja.ImpUsa[1] <> 0 THEN ASSIGN monto_aplic = CcbCCaja.ImpUsa[1].
            ELSE ASSIGN monto_aplic = CcbCCaja.ImpNac[1] / CcbCCaja.TpoCmb.
        END.
        ASSIGN monto_aplic = ROUND(monto_aplic,2).
        RUN proc_ActualizaDoc
            (
            B-CDocu.CodDoc,
            B-CDocu.NroDoc,
            monto_aplic,
            TRUE
            ).

        FOR EACH ccbdcaja OF ccbccaja:
            DELETE ccbdcaja.
        END.

        FIND b-ccbccaja WHERE
            ROWID(b-ccbccaja) = ROWID(ccbccaja)
            EXCLUSIVE-LOCK.
        ASSIGN
            b-ccbccaja.flgest = "A" .
    END.

END PROCEDURE.

PROCEDURE proc_ActualizaDoc:

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
                        CcbCDocu.FchCan = TODAY.
            ELSE ASSIGN CcbCDocu.FlgEst = "P".
        END.
        ELSE RETURN ERROR.
    END.

END PROCEDURE.
