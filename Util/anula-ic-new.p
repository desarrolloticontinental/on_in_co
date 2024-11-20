def var s-codcia as int init 001 no-undo.
def var s-user-id as char init 'SYSTEM'.
def buffer b-ccbcdocu for ccbcdocu.
def buffer b-ccbccaja for ccbccaja.
DEFINE VARIABLE x_nrodoc AS CHARACTER NO-UNDO.

find ccbccaja where codcia = s-codcia
and coddoc = 'i/c'
and nrodoc = '011070816'
exclusive-lock no-error.
if not available ccbccaja then return.
message 'anulamos?' ccbccaja.coddoc ccbccaja.nrodoc skip
    ccbccaja.fchdoc ccbccaja.usuario
    view-as alert-box question buttons yes-no
    update rpta as log.
if rpta = no then return.    


    /* Verifica Cheque Aceptado */
    IF ((CcbCCaja.Voucher[2] <> "" ) AND
        (CcbCCaja.ImpNac[2] + CcbCCaja.ImpUsa[2]) > 0 ) OR
        ((CcbCCaja.Voucher[3] <> "" ) AND
        (CcbCCaja.ImpNac[3] + CcbCCaja.ImpUsa[3]) > 0) THEN DO:

        IF CcbCCaja.Voucher[2] <> "" THEN x_nrodoc = CcbCCaja.Voucher[2].
        IF CcbCCaja.Voucher[3] <> "" THEN x_nrodoc = CcbCCaja.Voucher[3].

        FIND FIRST CcbCDocu WHERE 
            CcbCDocu.CodCia = S-CodCia AND
            CcbCDocu.CodDoc = "CHC" AND
            CcbCDocu.NroDoc = x_nrodoc
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
    trloop:
    DO TRANSACTION ON STOP UNDO, RETURN 'ADM-ERROR':
        /* Extorna Saldo de documentos */
        FOR EACH ccbdcaja OF ccbccaja EXCLUSIVE-LOCK:
            FIND FIRST ccbcdocu WHERE
                ccbcdocu.codcia = s-codcia AND
                ccbcdocu.coddoc = ccbdcaja.codref AND
                ccbcdocu.nrodoc = ccbdcaja.nroref
                EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE ccbcdocu THEN DO:
                FIND FIRST b-CcbCDocu WHERE
                    b-CcbCDocu.codcia = CcbCDocu.codcia AND
                    b-CcbCDocu.CodRef = CcbCDocu.CodDoc AND
                    b-CcbCDocu.NroRef = CcbCDocu.NroDoc
                    NO-LOCK NO-ERROR.
                IF AVAILABLE b-CcbCDocu THEN DO:
                    MESSAGE
                        "Factura registra Nota de Credito" SKIP
                        "Nro:" B-CcbCDocu.NroDoc
                        VIEW-AS ALERT-BOX ERROR.
                    UNDO trloop, LEAVE trloop.
                END.
                /* Imprime un Documento similar a la O/D
                   Indicando el motivo de Anulación */
                FIND FIRST CcbDdocu OF CcbCdocu NO-LOCK NO-ERROR.
                IF AVAILABLE CcbDdocu THEN
                    RUN ccb/r-motanu(
                        ROWID(ccbcdocu),
                        CcbDDocu.AlmDes,
                        "Anulación Total").
                /* Pedidos Pendientes */
                FIND Faccpedm WHERE
                    Faccpedm.CodCia = ccbcdocu.codcia AND
                    Faccpedm.CodDiv = ccbcdocu.codDiv AND
                    Faccpedm.CodDoc = ccbcdocu.codPed AND
                    Faccpedm.NroPed = ccbcdocu.NroPed
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Faccpedm THEN Faccpedm.FlgEst = "P".
                RELEASE Faccpedm.
                /* Extorna Salida de Almacen */
                RUN proc_Extorna-Alm(ROWID(CcbCDocu), CcbCCaja.CodCaja).
                IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.
                /* Extorna Orden de Despacho */
                FOR EACH FacCPedi WHERE
                    FacCPedi.CodCia = ccbCdocu.CodCia AND
                    FacCPedi.CodDoc = ccbCdocu.CodRef AND
                    FacCPedi.NroPed = ccbCdocu.NroRef EXCLUSIVE-LOCK:
                    FOR EACH FacDPedi OF FacCPedi EXCLUSIVE-LOCK:
                        DELETE FacDPedi.
                    END.
                    ASSIGN FacCPedi.Flgest = 'A'.
                END.
                /* Elimina Detalle de Documento */
                FOR EACH ccbDdocu OF ccbcdocu EXCLUSIVE-LOCK:
                    DELETE ccbDdocu.
                END.
                ASSIGN
                    ccbCdocu.sdoact = 0
                    ccbcdocu.fchcan = ?
                    ccbCdocu.flgest = "A"
                    ccbCdocu.FchAnu = TODAY
                    ccbCdocu.UsuAnu = S-USER-ID
                    ccbCdocu.codPed = ""
                    ccbCdocu.NroPed = "".
                RELEASE ccbcdocu.
            END.            
            DELETE ccbdcaja.
        END.

        /* EXTORNOS DE CANCELACIONES */
        /* Cheque */
        IF ((CcbCCaja.Voucher[2] <> "" ) AND
            (CcbCCaja.ImpNac[2] + CcbCCaja.ImpUsa[2]) > 0 ) OR
            ((CcbCCaja.Voucher[3] <> "" ) AND
            (CcbCCaja.ImpNac[3] + CcbCCaja.ImpUsa[3]) > 0) THEN DO:
            FIND CcbCDocu WHERE
                CcbCDocu.CodCia = S-CodCia AND
                CcbCDocu.CodDoc = "CHC" AND
                CcbCDocu.NroDoc = x_nrodoc
                EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE CcbCDocu THEN DELETE CcbCDocu.
        END.

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

/*ML3* 07/11/07 ***
        /* Nota de Credito */
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
