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
DEF NEW SHARED VAR s-codcia AS INTE INIT 001.
DEF NEW SHARED VAR s-coddiv AS CHAR.

/* "clona" una FAC o BOL y actualiza el kardex */

DEF INPUT PARAMETER pRowid AS ROWID NO-UNDO.
DEF OUTPUT PARAMETER pMensaje AS CHAR NO-UNDO.

DEF BUFFER b-ccbcdocu FOR ccbcdocu.
DEF BUFFER b-ccbddocu FOR ccbddocu.

FIND b-ccbcdocu WHERE ROWID(b-ccbcdocu) = pRowid NO-LOCK NO-ERROR.

IF NOT AVAILABLE b-ccbcdocu OR
    LOOKUP(TRIM(b-ccbcdocu.coddoc), 'FAC,BOL') = 0 OR
    b-ccbcdocu.flgest = "A"
    THEN DO:
    pMensaje = "Documento (FAC o BOL) NO encontrado o ya ha sido anulado".
    RETURN.
END.

DEF VAR s-NroSer AS INTE NO-UNDO.
DEF VAR s-CodDoc AS CHAR NO-UNDO.

ASSIGN
    s-NroSer = INTEGER(SUBSTRING(b-ccbcdocu.nrodoc,1,3)).
ASSIGN
    s-CodDoc = b-ccbcdocu.coddoc.
ASSIGN
    s-CodDiv = b-ccbcdocu.coddiv.

DEF VAR x-FormatoFAC  AS CHAR INIT '999-99999999' NO-UNDO.
/* FORMATO DEL COMPROBANTE: XXX-XXXXXXXX    (3-8) */
RUN sunat\p-formato-doc (INPUT s-CodDoc, OUTPUT x-FormatoFAC).

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
         HEIGHT             = 5.85
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

RUN Principal (OUTPUT pMensaje).
IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
    IF TRUE <> (pMensaje > "") THEN pMensaje = "NO se pudo clonar el comprobante".
    RETURN 'ADM-ERROR'.
END.
pMensaje = "".
RETURN 'OK'.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-Principal) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Principal Procedure 
PROCEDURE Principal :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF OUTPUT PARAMETER pMensaje AS CHAR NO-UNDO.

/* Desactivamos Triggers */
DISABLE TRIGGERS FOR LOAD OF Ccbcdocu.
DISABLE TRIGGERS FOR LOAD OF Ccbddocu.

DO TRANSACTION ON ERROR UNDO, RETURN 'ADM-ERROR' ON STOP UNDO, RETURN 'ADM-ERROR':
    /* Bloqueamos Origen */
    FIND CURRENT b-ccbcdocu EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
    IF ERROR-STATUS:ERROR = YES THEN DO:
        pMensaje = "Documento en uso por otro usuario".
        UNDO, RETURN 'ADM-ERROR'.
    END.
    /* Correlativo */
    {lib\lock-genericov3.i ~
        &Tabla="FacCorre" ~
        &Condicion="FacCorre.CodCia = s-CodCia ~
        AND FacCorre.CodDiv = s-CodDiv ~
        AND FacCorre.CodDoc = s-CodDoc ~
        AND FacCorre.NroSer = s-NroSer" ~
        &Bloqueo= "EXCLUSIVE-LOCK" ~
        &Accion="RETRY" ~
        &Mensaje="NO" ~
        &tMensaje="pMensaje" ~
        &TipoError="UNDO, RETURN 'ADM-ERROR'" ~
        }
    CREATE Ccbcdocu.
    BUFFER-COPY b-ccbcdocu TO Ccbcdocu
        ASSIGN
        Ccbcdocu.NroDoc =  STRING(FacCorre.NroSer,ENTRY(1,x-FormatoFAC,'-')) + 
                            STRING(FacCorre.Correlativo,ENTRY(2,x-FormatoFAC,'-')) 
        Ccbcdocu.fchdoc = TODAY
        /*Ccbcdocu.fchvto = TODAY + (b-ccbcdocu.fchvto - b-ccbcdocu.fchdoc)*/
        Ccbcdocu.fchvto = b-ccbcdocu.fchvto
        Ccbcdocu.HorCie = STRING(TIME,'hh:mm')
        .
    ASSIGN
        FacCorre.Correlativo = FacCorre.Correlativo + 1.
    RELEASE FacCorre.

    /* "clonamos" detalle */
    FOR EACH b-ccbddocu OF b-ccbcdocu NO-LOCK:
        CREATE Ccbddocu.
        BUFFER-COPY b-ccbddocu TO Ccbddocu
            ASSIGN
            Ccbddocu.nrodoc = Ccbcdocu.nrodoc
            Ccbddocu.fchdoc = Ccbcdocu.fchdoc
            .
    END.
    RELEASE Ccbddocu.

    /* ****************************************************************************************** */
    /* Importes SUNAT */
    /* 05/08/2022: Pre-calculo para ser usado en APLICACION DE ANTICIPOS */
    /* ****************************************************************************************** */
    DEF VAR hProc AS HANDLE NO-UNDO.

    RUN sunat/sunat-calculo-importes PERSISTENT SET hProc.
    RUN tabla-ccbcdocu IN hProc (INPUT Ccbcdocu.CodDiv,
                                 INPUT Ccbcdocu.CodDoc,
                                 INPUT Ccbcdocu.NroDoc,
                                 OUTPUT pMensaje).
    IF pMensaje = "OK" THEN pMensaje = "".
    IF RETURN-VALUE = "ADM-ERROR" THEN UNDO, RETURN 'ADM-ERROR'.
    DELETE PROCEDURE hProc NO-ERROR.
    /* ****************************************************************************************** */

    /* Campo de RELACION */
    ASSIGN
        b-Ccbcdocu.CodCob = TRIM(Ccbcdocu.coddoc) + "," + TRIM(Ccbcdocu.nrodoc).

    ASSIGN Ccbcdocu.sdoact = b-Ccbcdocu.sdoact
            Ccbcdocu.flgest = b-Ccbcdocu.flgest.

/*         b-Ccbcdocu.FlgEst = "A"      */
/*         b-CcbCDocu.SdoAct = 0        */
/*         b-CcbCDocu.UsuAnu = "SYSTEM" */
        .
END.
RETURN "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

