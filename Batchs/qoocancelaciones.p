&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 6.58
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR x-NroDoc AS CHAR NO-UNDO.
DEF VAR x-NroRef AS CHAR NO-UNDO.
DEF VAR s-user-id AS CHAR INIT 'SISTEMAS' NO-UNDO.

DISABLE TRIGGERS FOR LOAD OF ccbcdocu.
DISABLE TRIGGERS FOR LOAD OF ccbddocu.
DISABLE TRIGGERS FOR LOAD OF ccbccaja.
DISABLE TRIGGERS FOR LOAD OF ccbdcaja.
DISABLE TRIGGERS FOR LOAD OF ccbdmov.


/* Cabecera y detalle de caja */
DEF TEMP-TABLE T-CCAJA LIKE ccbccaja.
DEF TEMP-TABLE T-DCAJA LIKE ccbdcaja.
/* Control de registros procesados */
DEF TEMP-TABLE T-CANCE LIKE OOCancelaciones
    FIELD OORowid AS ROWID.
/* Nuevos comprobantes */
DEFINE TEMP-TABLE T-CDOCU NO-UNDO LIKE CcbCDocu
    INDEX Llave01 CodCia CodDiv CodDoc NroDoc.
/* Comprobantes aplicados */
DEFINE TEMP-TABLE T-APLIC NO-UNDO LIKE T-CDOCU.

DISPLAY 'Inicio:' TODAY STRING(TIME, 'HH:MM:SS') SKIP
    WITH STREAM-IO NO-BOX NO-LABELS.
PAUSE 0.
RUN Carga-Temporal.

RUN Graba-Registros.

DISPLAY 'Final:' TODAY STRING(TIME, 'HH:MM:SS') SKIP
    WITH STREAM-IO NO-BOX NO-LABELS.
PAUSE 0.

QUIT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-Carga-Aplicaciones) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Carga-Aplicaciones Procedure 
PROCEDURE Carga-Aplicaciones :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    CREATE T-APLIC.
    ASSIGN
        T-APLIC.CodCia = OOCancelaciones.codcia
        T-APLIC.CodDiv = OOCancelaciones.coddiv
        T-APLIC.CodDoc = OOCancelaciones.lincodref
        T-APLIC.NroDoc = x-NroRef
        T-APLIC.CodRef = T-CCAJA.CodDoc
        T-APLIC.NroRef = T-CCAJA.NroDoc
        T-APLIC.FchDoc = OOCancelaciones.fchdoc
        T-APLIC.FchCbd = OOCancelaciones.fchdoc
        T-APLIC.CodCli = OOCancelaciones.codcli
        T-APLIC.NomCli = OOCancelaciones.nomcli
        T-APLIC.RucCli = OOCancelaciones.codcli
        T-APLIC.CodMon = OOCancelaciones.codmon
        T-APLIC.ImpTot = OOCancelaciones.linimptot
        T-APLIC.TpoCmb = OOCancelaciones.tpocmb
        T-APLIC.usuario = OOCancelaciones.usuario.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Carga-Comprobante) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Carga-Comprobante Procedure 
PROCEDURE Carga-Comprobante :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    /* A/R y BD */
    CREATE T-CDOCU.
    ASSIGN
        T-CDOCU.CodCia = OOCancelaciones.codcia
        T-CDOCU.CodDiv = OOCancelaciones.coddiv
        T-CDOCU.CodDoc = OOCancelaciones.lincodref
        T-CDOCU.NroDoc = x-NroRef
        T-CDOCU.CodRef = T-CCAJA.CodDoc
        T-CDOCU.NroRef = T-CCAJA.NroDoc
        T-CDOCU.FchDoc = OOCancelaciones.fchdoc
        T-CDOCU.FmaPgo = "000"                      /* Contado */
        T-CDOCU.FchVto = OOCancelaciones.fchdoc
        T-CDOCU.FchCbd = OOCancelaciones.fchdoc
        T-CDOCU.CodCli = OOCancelaciones.codcli
        T-CDOCU.NomCli = OOCancelaciones.nomcli
        T-CDOCU.RucCli = OOCancelaciones.codcli
        T-CDOCU.CodMon = OOCancelaciones.codmon
        T-CDOCU.ImpTot = OOCancelaciones.linimptot
        T-CDOCU.SdoAct = OOCancelaciones.linimptot
        T-CDOCU.TpoCmb = OOCancelaciones.tpocmb
        T-CDOCU.usuario = OOCancelaciones.usuario
        T-CDOCU.FlgEst = 'P'.
    IF T-CDOCU.CodDoc = "BD" THEN DO:
        ASSIGN
            T-CDOCU.flgsit = 'Autorizada'
            T-CDOCU.tpofac = 'Efectivo'
            T-CDOCU.codage = 'Lima'
            T-CDOCU.FchAte = OOCancelaciones.fchdoc     /* Sirve para BD */
            T-CDOCU.fchubi = T-CDOCU.fchdoc
            T-CDOCU.codcta =  OOCancelaciones.CodBco_1.
        FIND cb-ctas WHERE cb-ctas.codcia = 000
            AND cb-ctas.codcta = T-CDOCU.codcta
            NO-LOCK NO-ERROR.
        IF AVAILABLE cb-ctas THEN T-CDOCU.flgate = cb-ctas.codbco.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Carga-Temporal) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Carga-Temporal Procedure 
PROCEDURE Carga-Temporal :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR NuevoAnticipo AS LOG NO-UNDO.

FOR EACH OOCancelaciones NO-LOCK WHERE OOCancelaciones.FlagMigracion = "N"
    AND OOCancelaciones.CodCia = s-codcia
    AND OOCancelaciones.CodDiv = "00500"
    AND OOCancelaciones.CodDoc = "I/C"
    BREAK BY OOCancelaciones.NroDoc:
    IF FIRST-OF(OOCancelaciones.NroDoc) THEN DO:
        /* CABECERA */
        x-NroDoc = OOCancelaciones.NroDoc.
        x-NroDoc = REPLACE(x-NroDoc, '-', '').
        /*IF LENGTH(x-NroDoc) > 9 THEN x-NroDoc = SUBSTRING(x-NroDoc,1,3) + SUBSTRING(x-NroDoc, 5).*/
        CREATE T-CCAJA.
        ASSIGN
            T-CCAJA.CodCia = OOCancelaciones.CodCia 
            T-CCAJA.CodDiv = OOCancelaciones.CodDiv 
            T-CCAJA.CodDoc = OOCancelaciones.CodDoc
            T-CCAJA.NroDoc = x-NroDoc 
            T-CCAJA.CodCli = OOCancelaciones.CodCli 
            T-CCAJA.CodMon = OOCancelaciones.CodMon 
            T-CCAJA.FchDoc = OOCancelaciones.FchDoc 
            T-CCAJA.FlgEst = OOCancelaciones.FlgEst 
            T-CCAJA.NomCli = OOCancelaciones.NomCli 
            T-CCAJA.TpoCmb = OOCancelaciones.TpoCmb 
            T-CCAJA.Usuario = OOCancelaciones.Usuario
            T-CCAJA.FlgCie = "C"
            T-CCAJA.FchCie = OOCancelaciones.FchDoc 
            T-CCAJA.HorCie = "23:59".
        /* NUEVO ANTICIPO */
        NuevoAnticipo = YES.    /* Por defecto */
    END.
    /* DETALLE */
    x-NroRef = OOCancelaciones.LinNroRef.
    x-NroRef = REPLACE(x-NroRef, '-', '').
    /*IF LENGTH(x-NroRef) > 9 THEN x-NroRef = SUBSTRING(x-NroRef,1,3) + SUBSTRING(x-NroRef, 5).*/
    CASE OOCancelaciones.LinTipo:
        WHEN '1' THEN DO:   /* COMPROBANTES */
            CASE TRUE:
                WHEN LOOKUP(OOCancelaciones.LinCodRef, 'FAC,BOL,TCK,LET') > 0 THEN DO:
                    IF LENGTH(x-NroRef) > 9 THEN x-NroRef = SUBSTRING(x-NroRef,1,3) + SUBSTRING(x-NroRef, 5).
                    CREATE T-DCAJA.
                    BUFFER-COPY T-CCAJA TO T-DCAJA
                        ASSIGN
                        T-DCAJA.CodRef = OOCancelaciones.LinCodRef 
                        T-DCAJA.NroRef = x-NroRef
                        T-DCAJA.ImpTot = OOCancelaciones.LinImpTot.
                    NuevoAnticipo = NO.
                END.
                WHEN OOCancelaciones.LinCodRef = "A/R" THEN DO:
                    T-CCAJA.ImpNac[7] = T-CCAJA.ImpNac[7] + (IF OOCancelaciones.CodMon = 1 THEN OOCancelaciones.LinImpTot ELSE 0).
                    T-CCAJA.ImpUsa[7] = T-CCAJA.ImpUsa[7] + (IF OOCancelaciones.CodMon = 2 THEN OOCancelaciones.LinImpTot ELSE 0).
                    T-CCAJA.Voucher[7] = x-NroRef.
                    RUN Carga-Comprobante.
                    RUN Carga-Aplicaciones.
                END.
                WHEN OOCancelaciones.LinCodRef = "N/C" THEN DO:
                   IF LENGTH(x-NroRef) > 9 THEN x-NroRef = SUBSTRING(x-NroRef,1,3) + SUBSTRING(x-NroRef, 5).
                   T-CCAJA.ImpNac[6] = T-CCAJA.ImpNac[6] + (IF OOCancelaciones.CodMon = 1 THEN OOCancelaciones.LinImpTot ELSE 0).
                   T-CCAJA.ImpUsa[6] = T-CCAJA.ImpUsa[6] + (IF OOCancelaciones.CodMon = 2 THEN OOCancelaciones.LinImpTot ELSE 0).
                   T-CCAJA.Voucher[6] = x-NroRef.
                   /*RUN Carga-Comprobante.*/
                   RUN Carga-Aplicaciones.
                   NuevoAnticipo = NO.
               END.
                OTHERWISE DO:
                    NuevoAnticipo = NO.
                END.
            END CASE.
        END.
        WHEN '2' THEN DO:   /* PAGOS */
            CASE OOCancelaciones.LinCodRef:
                WHEN 'BD' THEN DO:
                    T-CCAJA.ImpNac[5] = T-CCAJA.ImpNac[5] + (IF OOCancelaciones.CodMon = 1 THEN OOCancelaciones.LinImpTot ELSE 0).
                    T-CCAJA.ImpUsa[5] = T-CCAJA.ImpUsa[5] + (IF OOCancelaciones.CodMon = 2 THEN OOCancelaciones.LinImpTot ELSE 0).
                    T-CCAJA.Voucher[5] = x-NroRef.
                    RUN Carga-Comprobante.
                    RUN Carga-Aplicaciones.
                END.
                WHEN 'EF' THEN DO:
                    T-CCAJA.ImpNac[1] = T-CCAJA.ImpNac[1] + (IF OOCancelaciones.CodMon = 1 THEN OOCancelaciones.LinImpTot ELSE 0).
                    T-CCAJA.ImpUsa[1] = T-CCAJA.ImpUsa[1] + (IF OOCancelaciones.CodMon = 2 THEN OOCancelaciones.LinImpTot ELSE 0).
                    NuevoAnticipo = NO.
                END.
                OTHERWISE DO:
                    NuevoAnticipo = NO.
                END.
            END CASE.
        END.
    END CASE.
    /* CONTROL */
    CREATE T-CANCE.
    BUFFER-COPY OOCancelaciones TO T-CANCE ASSIGN T-CANCE.OORowid = ROWID(OOCancelaciones).
    /* CIERRE DE INGRESO A CAJA */
    IF LAST-OF(OOCancelaciones.NroDoc) THEN DO:
        IF NuevoAnticipo = YES THEN DO:
            /* CREACION DE NUEVO ANTICIPO (OJO >>> NO ES UNA CANCELACION) */
            /* Borramos aplicaciones */
            FOR EACH T-APLIC WHERE T-APLIC.CodRef = OOCancelaciones.CodDoc
                AND T-APLIC.NroRef = x-NroDoc:
                DELETE T-APLIC.
            END.
            /* Borramos Ingreso a Caja */
            FOR EACH T-CCAJA WHERE T-CCAJA.CodDoc = OOCancelaciones.CodDoc
                AND T-CCAJA.NroDoc = x-NroDoc:
                T-CCAJA.FlgEst = "A".
/*                 FOR EACH T-DCAJA OF T-CCAJA: */
/*                     DELETE T-DCAJA.          */
/*                 END.                         */
/*                 DELETE T-CCAJA.              */
            END.
        END.
        ELSE DO:
            /* OJO >>> ES UNA CANCELACION */
            /* Borramos comprobantes (A/R) */
/*             FOR EACH T-CDOCU WHERE T-CDOCU.CodRef = OOCancelaciones.CodDoc */
/*                 AND T-CDOCU.NroRef = x-NroDoc:                             */
/*                 DELETE T-CDOCU.                                            */
/*             END.                                                           */
        END.
        /* ********************************************************* */
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Graba-Registros) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Graba-Registros Procedure 
PROCEDURE Graba-Registros :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/* Generamos ingresos a caja */
PRINCIPAL:
FOR EACH T-CCAJA BY T-CCAJA.NroDoc TRANSACTION ON ERROR UNDO, NEXT ON STOP UNDO, NEXT:
    /* Generamos Nuevos Documentos */
    FOR EACH T-CDOCU WHERE T-CDOCU.codref = T-CCAJA.coddoc AND T-CDOCU.nroref = T-CCAJA.nrodoc:
        FIND FIRST Ccbcdocu WHERE Ccbcdocu.codcia = T-CDOCU.codcia
            AND Ccbcdocu.coddiv = T-CDOCU.coddiv
            AND Ccbcdocu.coddoc = T-CDOCU.coddoc
            AND Ccbcdocu.nrodoc = T-CDOCU.nrodoc
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE Ccbcdocu THEN DO:
            CREATE Ccbcdocu.
            BUFFER-COPY T-CDOCU TO Ccbcdocu.
            /* RHC 06/10/2015 Caso de Canje de BD por A/R */
            IF T-CCAJA.FlgEst = "A" AND Ccbcdocu.coddoc = "A/R" THEN DO:
                CREATE ccbdmov.
                ASSIGN
                    CCBDMOV.CodCia = T-CCAJA.codcia
                    CCBDMOV.CodDiv = T-CCAJA.coddiv
                    CCBDMOV.CodDoc = Ccbcdocu.coddoc
                    CCBDMOV.NroDoc = Ccbcdocu.nrodoc
                    CCBDMOV.CodRef = T-CCAJA.coddoc
                    CCBDMOV.NroRef = T-CCAJA.nrodoc
                    CCBDMOV.CodCli = Ccbcdocu.codcli
                    CCBDMOV.FchDoc = T-CCAJA.fchdoc
                    CCBDMOV.FchMov = TODAY
                    CCBDMOV.HraMov = STRING(TIME,'HH:MM:SS')
                    CCBDMOV.ImpTot = Ccbcdocu.sdoact
                    CCBDMOV.CodMon = Ccbcdocu.codmon
                    CCBDMOV.TpoCmb  = T-CCAJA.tpocmb
                    CCBDMOV.usuario = T-CCAJA.usuario.
                ASSIGN
                    Ccbcdocu.sdoact = 0
                    Ccbcdocu.flgest = "C"
                    Ccbcdocu.fchcan = T-CCAJA.fchdoc.
            END.
        END.
    END.
    /* Ingresos a Caja */
    IF T-CCAJA.FlgEst <> "A" THEN DO:
        FIND Ccbccaja WHERE CcbCCaja.CodCia = T-CCAJA.codcia
            AND CcbCCaja.CodDiv = T-CCAJA.coddiv
            AND CcbCCaja.CodDoc = T-CCAJA.coddoc
            AND CcbCCaja.NroDoc = T-CCAJA.nrodoc
            NO-LOCK NO-ERROR.
        IF AVAILABLE Ccbccaja THEN DO:
            DISPLAY 'Existe documento' T-CCAJA.coddoc T-CCAJA.nrodoc
                WITH STREAM-IO NO-BOX.
            PAUSE 0.
            UNDO PRINCIPAL, NEXT PRINCIPAL. 
        END.
        CREATE Ccbccaja.
        BUFFER-COPY T-CCAJA TO Ccbccaja NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DO:
            DISPLAY 'NO se actualizó en documento' T-CCAJA.coddoc T-CCAJA.nrodoc
                WITH STREAM-IO NO-BOX.
            PAUSE 0.
            UNDO PRINCIPAL, NEXT PRINCIPAL. 
        END.

        FOR EACH T-DCAJA OF T-CCAJA:
            CREATE Ccbdcaja.
            BUFFER-COPY T-DCAJA TO Ccbdcaja.
            FIND ccbcdocu WHERE ccbcdocu.codcia = s-codcia 
                AND ccbcdocu.coddoc = ccbdcaja.codref 
                AND ccbcdocu.nrodoc = ccbdcaja.nroref
                EXCLUSIVE-LOCK NO-ERROR.
            IF NOT AVAILABLE Ccbcdocu THEN DO:
                DISPLAY 'NO se actualizó en documento' ccbdcaja.codref ccbdcaja.nroref WITH STREAM-IO NO-BOX.
                PAUSE 0.
                UNDO PRINCIPAL, NEXT PRINCIPAL. 
            END.
            IF AVAILABLE Ccbcdocu THEN DO:
                ASSIGN ccbcdocu.sdoact = ccbcdocu.sdoact - ccbdcaja.imptot.
                IF ccbcdocu.sdoact <= 0 THEN DO:
                    ASSIGN
                        ccbcdocu.fchcan = TODAY
                        ccbcdocu.flgest = "C".
                END.
            END.
        END.
    END.
    ELSE DO:
        /* Cerramos el A/R */
    END.
    /* Aplicamos Documentos */
    FOR EACH T-APLIC WHERE T-APLIC.codref = T-CCAJA.coddoc AND T-APLIC.nroref = T-CCAJA.nrodoc:
        RUN proc_AplicaDoc(
            T-APLIC.coddiv,
            T-APLIC.coddoc,
            T-APLIC.nrodoc,
            T-APLIC.codref,
            T-APLIC.nroref,
            T-APLIC.tpocmb,
            (IF T-APLIC.codmon = 1 THEN T-APLIC.imptot ELSE 0),
            (IF T-APLIC.codmon = 2 THEN T-APLIC.imptot ELSE 0)
            ).
        IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO PRINCIPAL, NEXT PRINCIPAL.
    END.
    /* Cerramos */
    FOR EACH T-CANCE WHERE T-CANCE.coddoc = T-CCAJA.coddoc AND T-CANCE.nrodoc = T-CCAJA.nrodoc:
        FIND OOCancelaciones WHERE ROWID(OOCancelaciones) = T-CANCE.OORowid.
        ASSIGN OOCancelaciones.FlagMigracion = "S".
    END.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-proc_AplicaDoc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE proc_AplicaDoc Procedure 
PROCEDURE proc_AplicaDoc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE INPUT PARAMETER para_CodDiv AS CHAR.                             
    DEFINE INPUT PARAMETER para_CodDoc LIKE CcbCDocu.CodDoc.
    DEFINE INPUT PARAMETER para_NroDoc LIKE CcbDMov.NroDoc.
    DEFINE INPUT PARAMETER para_CodDocCja LIKE CcbDMov.CodDoc.    
    DEFINE INPUT PARAMETER para_NroDocCja LIKE CcbDMov.NroDoc.    
    DEFINE INPUT PARAMETER para_TpoCmb LIKE CCBDMOV.TpoCmb.
    DEFINE INPUT PARAMETER para_ImpNac LIKE CcbDMov.ImpTot.
    DEFINE INPUT PARAMETER para_ImpUSA LIKE CcbDMov.ImpTot.

    DEFINE BUFFER B-CDocu FOR CcbCDocu.

    /* Tipo de Documento */
    FIND FacDocum WHERE
        FacDocum.CodCia = s-CodCia AND
        FacDocum.CodDoc = para_CodDoc
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE FacDocum THEN DO:
        DISPLAY para_CodDoc 'NO CONFIGURADO' WITH STREAM-IO NO-BOX.
        PAUSE 0.
        RETURN "ADM-ERROR".
    END.

    /* Busca Documento */
    FIND FIRST B-CDocu WHERE
        B-CDocu.CodCia = s-codcia AND
        B-CDocu.CodDoc = para_CodDoc AND
        B-CDocu.NroDoc = para_NroDoc
        EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE B-CDocu THEN DO:
        DISPLAY "DOCUMENTO" para_CodDoc para_NroDoc "NO REGISTRADO" SKIP
            WITH STREAM-IO NO-BOX.
        PAUSE 0.
        RETURN "ADM-ERROR".
    END.
    IF AVAILABLE B-CDocu THEN DO:
        /* Crea Detalle de la Aplicación */
        CREATE CCBDMOV.
        ASSIGN
            CCBDMOV.CodCia = s-CodCia
            CCBDMOV.CodDiv = para_CodDiv
            CCBDMOV.NroDoc = B-CDocu.NroDoc
            CCBDMOV.CodDoc = B-CDocu.CodDoc
            CCBDMOV.CodMon = B-CDocu.CodMon
            CCBDMOV.CodRef = para_CodDocCja
            CCBDMOV.NroRef = para_NroDocCja
            CCBDMOV.CodCli = B-CDocu.CodCli
            CCBDMOV.FchDoc = B-CDocu.FchDoc
            CCBDMOV.HraMov = STRING(TIME,"HH:MM:SS")
            CCBDMOV.TpoCmb = para_tpocmb
            CCBDMOV.usuario = s-User-ID.
        IF B-CDocu.CodMon = 1 THEN
            ASSIGN CCBDMOV.ImpTot = para_ImpNac.
        ELSE ASSIGN CCBDMOV.ImpTot = para_ImpUSA.
        IF FacDoc.TpoDoc THEN
            ASSIGN B-CDocu.SdoAct = B-CDocu.SdoAct + CCBDMOV.ImpTot.
        ELSE
            ASSIGN B-CDocu.SdoAct = B-CDocu.SdoAct - CCBDMOV.ImpTot.
        /* Cancela Documento */
        IF B-CDocu.SdoAct <= 0 THEN
            ASSIGN 
                B-CDocu.FlgEst = "C"
                B-CDocu.FchCan = TODAY.
        ELSE
            ASSIGN
                B-CDocu.FlgEst = "P"
                B-CDocu.FchCan = ?.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

