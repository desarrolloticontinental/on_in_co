DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR s-coddiv AS CHAR INIT '00000' NO-UNDO.
DEF VAR s-coddoc AS CHAR INIT 'I/C' NO-UNDO.
DEF VAR X-NUMDOC AS CHAR INIT "".
DEF BUFFER b-ccbccaja FOR ccbccaja.

FIND ccbccaja WHERE codcia = s-codcia
    AND coddoc = 'I/C'
    AND nrodoc = '011074918'
    EXCLUSIVE-LOCK NO-ERROR.
IF NOT AVAILABLE ccbccaja OR tipo <> 'CANCELACION' THEN RETURN.
MESSAGE 'Anulamos?' SKIP
    ccbccaja.coddoc ccbccaja.nrodoc SKIP
    ccbccaja.fchdoc ccbccaja.usuario SKIP
    ccbccaja.nomcli SKIP
    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
    UPDATE rpta AS LOG.
IF rpta = no THEN RETURN.

RUN Anula-IC-Liquidacion.    

PROCEDURE Anula-IC-Liquidacion:
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

    IF ccbccaja.flgest = "A" THEN DO:
        MESSAGE
            "Registro ya fue Anulado"
            VIEW-AS ALERT-BOX ERROR.
        RETURN "ADM-ERROR".
    END.

    /* Verifica Cheque Aceptado */
    IF ((CcbCCaja.Voucher[2] <> "" ) AND
        (CcbCCaja.ImpNac[2] + CcbCCaja.ImpUsa[2]) > 0 ) OR
        ((CcbCCaja.Voucher[3] <> "" ) AND
        (CcbCCaja.ImpNac[3] + CcbCCaja.ImpUsa[3]) > 0) THEN DO:

        IF CcbCCaja.Voucher[2] <> "" THEN X-NUMDOC = CcbCCaja.Voucher[2].
        IF CcbCCaja.Voucher[3] <> "" THEN X-NUMDOC = CcbCCaja.Voucher[3].       

        FIND FIRST CcbCDocu WHERE 
            CcbCDocu.CodCia = S-CodCia AND
            CcbCDocu.CodDoc = "CHC" AND
            CcbCDocu.NroDoc = X-NUMDOC
            NO-LOCK NO-ERROR.
        IF AVAILABLE CcbCDocu AND CcbCDocu.FlgEst = "C" THEN DO:
            MESSAGE
                "Ingreso con Cheque Aceptado,"
                "No es posible Anular la Operacion"
                VIEW-AS ALERT-BOX ERROR.
            RETURN "ADM-ERROR".
        END.

    END.

    /* Actualiza la cuenta corriente */
    DO TRANSACTION ON ERROR UNDO, RETURN "ADM-ERROR":

        /* Extorna Saldo de documentos */
        FOR EACH ccbdcaja OF ccbccaja EXCLUSIVE-LOCK:
            FIND FIRST ccbcdocu WHERE
                ccbcdocu.codcia = s-codcia AND
                ccbcdocu.codcli = CcbCCaja.Codcli AND
                ccbcdocu.coddoc = ccbdcaja.codref AND
                ccbcdocu.nrodoc = ccbdcaja.nroref
                EXCLUSIVE-LOCK.
            ASSIGN
                ccbcdocu.sdoact = ccbcdocu.sdoact + ccbdcaja.imptot
                ccbcdocu.fchcan = ?
                ccbcdocu.flgest = "P".
            RELEASE ccbcdocu.
            DELETE ccbdcaja.
        END.

        /* EXTORNOS DE CANCELACIONES */
        /* Cheque */
        IF ((CcbCCaja.Voucher[2] <> "" ) AND
            (CcbCCaja.ImpNac[2] + CcbCCaja.ImpUsa[2]) > 0 ) OR
            ((CcbCCaja.Voucher[3] <> "" ) AND
            (CcbCCaja.ImpNac[3] + CcbCCaja.ImpUsa[3]) > 0) THEN
            RUN Anula-Cheque.

        /* Boleta de Deposito */
        IF CcbCCaja.Voucher[5] <> "" AND
            (CcbCCaja.ImpNac[5] + CcbCCaja.ImpUsa[5]) > 0 THEN DO:
            RUN proc_AnulaBD(
                CcbCCaja.Voucher[5],
                ccbccaja.nrodoc,
                CcbCCaja.ImpNac[5],
                CcbCCaja.ImpUsa[5]
                ).
            IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.
        END.

/*MLR* 07/11/07 ***
        /* N/C */
        IF CcbCCaja.Voucher[6] <> "" AND
            (CcbCCaja.ImpNac[6] + CcbCCaja.ImpUsa[6]) > 0 THEN DO:
            RUN proc_AnulaDoc(
                "N/C",
                CcbCCaja.Voucher[6],
                ccbccaja.nrodoc,
                CcbCCaja.ImpNac[6],
                CcbCCaja.ImpUsa[6]
                ).
            IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.
        END.

        /* Anticipo */
        IF CcbCCaja.Voucher[7] <> "" AND
            (CcbCCaja.ImpNac[7] + CcbCCaja.ImpUsa[7]) > 0 THEN DO:
            RUN proc_AnulaDoc(
                "A/R",
                CcbCCaja.Voucher[7],
                ccbccaja.nrodoc,
                CcbCCaja.ImpNac[7],
                CcbCCaja.ImpUsa[7]
                ).
            IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.
        END.
***/

        /* Elimina Detalle de la Aplicación para N/C y A/R */
        FOR EACH CCBDMOV WHERE
            CCBDMOV.CodCia = ccbccaja.CodCia AND
            CCBDMOV.CodDiv = ccbccaja.CodDiv AND
            CCBDMOV.CodRef = ccbccaja.coddoc AND
            CCBDMOV.NroRef = ccbccaja.nrodoc EXCLUSIVE-LOCK:
            /* Tipo de Documento */
            FIND FacDoc WHERE
                FacDoc.CodCia = CCBDMOV.CodCia AND
                FacDoc.CodDoc = CCBDMOV.CodDoc
                NO-LOCK NO-ERROR.
            IF NOT AVAILABLE FacDoc THEN DO:
                MESSAGE
                    "DOCUMENTO" CCBDMOV.CodDoc 'NO CONFIGURADO'
                    VIEW-AS ALERT-BOX ERROR.
                RETURN "ADM-ERROR".
            END.
            FIND FIRST CcbCDocu WHERE
                CcbCDocu.codcia = CCBDMOV.CodCia AND
                CcbCDocu.coddoc = CCBDMOV.CodDoc AND
                CcbCDocu.nrodoc = CCBDMOV.NroDoc EXCLUSIVE-LOCK.
            IF NOT AVAILABLE ccbcdocu THEN DO:
                MESSAGE
                    "DOCUMENTO" CCBDMOV.CodDoc CCBDMOV.NroDoc "NO REGISTRADO"
                    VIEW-AS ALERT-BOX ERROR.
                RETURN "ADM-ERROR".
            END.
            IF FacDoc.TpoDoc THEN
                ASSIGN CcbCDocu.SdoAct = CcbCDocu.SdoAct - CCBDMOV.Imptot.
            ELSE
                ASSIGN CcbCDocu.SdoAct = CcbCDocu.SdoAct + CCBDMOV.Imptot.
            /* Cancela Documento */
            IF CcbCDocu.SdoAct <> 0 THEN
                ASSIGN
                    CcbCDocu.FlgEst = "P"
                    CcbCDocu.FchCan = ?.
            RELEASE CcbCDocu.
            DELETE CCBDMOV.
        END.

        /* Extorna Retencion */
        FOR EACH CcbCMov WHERE
            CCBCMOV.CodCia = CcbCCaja.CodCia AND
            CCBCMOV.CodRef = CcbCCaja.CodDoc AND
            CCBCMOV.NroRef = CcbCCaja.NroDoc
            EXCLUSIVE-LOCK:
            DELETE CcbCMov.
        END.

        /* Anula Ingreso de Caja */
        FIND b-ccbccaja WHERE
            ROWID(b-ccbccaja) = ROWID(ccbccaja)
            EXCLUSIVE-LOCK.
        ASSIGN b-ccbccaja.flgest = "A".
        RELEASE b-ccbccaja.

    END. /* DO TRANSACTION... */

END PROCEDURE.

PROCEDURE proc_anulabd:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE INPUT PARAMETER para_NroDoc LIKE CCBDMOV.NroDoc.
    DEFINE INPUT PARAMETER para_NroDocCja LIKE CCBDMOV.NroDoc.
    DEFINE INPUT PARAMETER para_ImpNac LIKE CCBDMOV.ImpTot.
    DEFINE INPUT PARAMETER para_ImpUSA LIKE CCBDMOV.ImpTot.

    DO TRANSACTION ON ERROR UNDO, RETURN 'ADM-ERROR':

        FIND ccbboldep WHERE
            ccbboldep.CodCia = s-CodCia AND
            ccbboldep.CodDoc = "BD" AND
            ccbboldep.nrodoc = para_NroDoc
            EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAILABLE CcbBolDep THEN DO:
            MESSAGE
                "BOLETA DE DEPOSITO" para_NroDoc "NO EXISTE"
                VIEW-AS ALERT-BOX ERROR.
            RETURN "ADM-ERROR".
        END.

        /* Elimina Detalle de la Aplicación */
        FOR EACH CCBDMOV WHERE
            CCBDMOV.CodCia = s-CodCia AND
            CCBDMOV.CodDiv = s-CodDiv AND
            CCBDMOV.NroDoc = ccbboldep.NroDoc AND
            CCBDMOV.CodDoc = ccbboldep.CodDoc
            EXCLUSIVE-LOCK:
            /* Referencia I/C */
            IF CCBDMOV.CodRef = s-coddoc AND
                CCBDMOV.NroRef = para_NroDocCja THEN
                DELETE CCBDMOV.
        END.

        IF CcbBolDep.CodMon = 1 THEN
            ASSIGN CcbBolDep.SdoAct = CcbBolDep.SdoAct + para_ImpNac.
        ELSE
            ASSIGN CcbBolDep.SdoAct = CcbBolDep.SdoAct + para_ImpUSA.

        IF CcbBolDep.SdoAct > 0 THEN
            ASSIGN
                CcbBolDep.FchCan = ?
                CcbBolDep.FlgEst = "p".

        RELEASE ccbboldep.

    END. /* DO TRANSACTION... */

END PROCEDURE.

PROCEDURE Anula-Cheque:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    IF ((CcbCCaja.Voucher[2] <> "" ) AND
        (CcbCCaja.ImpNac[2] + CcbCCaja.ImpUsa[2]) > 0 ) OR
        ((CcbCCaja.Voucher[3] <> "" ) AND
        (CcbCCaja.ImpNac[3] + CcbCCaja.ImpUsa[3]) > 0) THEN DO:

        IF CcbCCaja.Voucher[2] <> "" THEN X-NUMDOC = CcbCCaja.Voucher[2].
        IF CcbCCaja.Voucher[3] <> "" THEN X-NUMDOC = CcbCCaja.Voucher[3].

        FIND CcbCDocu WHERE
            CcbCDocu.CodCia = S-CodCia AND
            CcbCDocu.CodDoc = "CHC" AND
            CcbCDocu.NroDoc = X-NUMDOC
            EXCLUSIVE-LOCK NO-ERROR.
        IF AVAILABLE CcbCDocu THEN DELETE CcbCDocu.

    END.

END PROCEDURE.

PROCEDURE proc_AnulaDoc:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE INPUT PARAMETER para_CodDoc LIKE CcbCDocu.CodDoc.
    DEFINE INPUT PARAMETER para_NroDoc LIKE CcbDMov.NroDoc.
    DEFINE INPUT PARAMETER para_NroDocCja LIKE CcbDMov.NroDoc.    
    DEFINE INPUT PARAMETER para_ImpNac LIKE CcbDMov.ImpTot.
    DEFINE INPUT PARAMETER para_ImpUSA LIKE CcbDMov.ImpTot.

    DEFINE VARIABLE x-Monto LIKE ccbcdocu.ImpTot.

    /* Tipo de Documento */
    FIND FacDoc WHERE
        FacDoc.CodCia = s-CodCia AND
        FacDoc.CodDoc = para_CodDoc
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE FacDoc THEN DO:
        MESSAGE
            para_CodDoc 'NO CONFIGURADO'
            VIEW-AS ALERT-BOX ERROR.
        RETURN "ADM-ERROR".
    END.

    DO TRANSACTION ON ERROR UNDO, RETURN 'ADM-ERROR':

        FIND FIRST ccbcdocu WHERE
            ccbcdocu.codcia = s-codcia AND
            ccbcdocu.coddoc = para_CodDoc AND
            ccbcdocu.nrodoc = para_NroDoc
            EXCLUSIVE-LOCK.

        IF NOT AVAILABLE ccbcdocu THEN DO:
            MESSAGE
                "DOCUMENTO" para_CodDoc para_NroDoc "NO REGISTRADO"
                VIEW-AS ALERT-BOX ERROR.
            RETURN "ADM-ERROR".
        END.

        /* Elimina Detalle de la Aplicación */
        FOR EACH CCBDMOV WHERE
            CCBDMOV.CodCia = s-CodCia AND
            CCBDMOV.CodDiv = s-CodDiv AND
            CCBDMOV.NroDoc = ccbcdocu.NroDoc AND
            CCBDMOV.CodDoc = ccbcdocu.CodDoc
            EXCLUSIVE-LOCK:
            /* Referencia I/C */
            IF CCBDMOV.CodRef = s-coddoc AND
                CCBDMOV.NroRef = para_NroDocCja THEN
                DELETE CCBDMOV.
        END.

        IF CcbCDocu.CodMon = 1 THEN ASSIGN x-Monto = para_ImpNac.
        ELSE ASSIGN x-Monto = para_ImpUSA.

        IF FacDoc.TpoDoc THEN
            ASSIGN CcbCDocu.SdoAct = CcbCDocu.SdoAct - x-Monto.
        ELSE
            ASSIGN CcbCDocu.SdoAct = CcbCDocu.SdoAct + x-Monto.

        /* Cancela Documento */
        IF CcbCDocu.SdoAct <> 0 THEN
            ASSIGN
                CcbCDocu.FlgEst = "P"
                CcbCDocu.FchCan = ?.
        RELEASE CcbCDocu.

    END. /* DO TRANSACTION... */

END PROCEDURE.

