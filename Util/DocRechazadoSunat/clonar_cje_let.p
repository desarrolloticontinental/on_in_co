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

DEF INPUT PARAMETER pRowidOrigen AS ROWID NO-UNDO.
DEF INPUT PARAMETER pRowidDestino AS ROWID NO-UNDO.
DEF OUTPUT PARAMETER pMensaje AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER pNuevoCJE AS CHAR NO-UNDO.

DEF BUFFER b-origen FOR ccbcdocu.
DEF BUFFER b-ccbddocu FOR ccbddocu.
DEF BUFFER b-destino  FOR ccbcdocu.

FIND b-origen WHERE ROWID(b-origen) = pRowidOrigen NO-LOCK NO-ERROR.

IF NOT AVAILABLE b-origen OR LOOKUP(TRIM(b-origen.coddoc), 'FAC,BOL') = 0 OR b-origen.flgest = "A" THEN DO:
    pMensaje = "Documento (FAC o BOL) NO encontrado o ya ha sido anulado".
    RETURN.
END.

FIND b-destino WHERE ROWID(b-destino) = pRowidDestino NO-LOCK NO-ERROR.
IF NOT AVAILABLE b-destino OR LOOKUP(TRIM(b-destino.coddoc), 'FAC,BOL') = 0 THEN DO:
    pMensaje = "Documento (FAC o BOL) NO encontrado".
    RETURN.
END.

DEF TEMP-TABLE t-ccbcmvto NO-UNDO LIKE ccbcmvto.

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
         HEIGHT             = 5.46
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

DEFINE OUTPUT PARAMETER pMensaje AS CHAR NO-UNDO.

DEF VAR s-coddoc AS CHAR INITIAL "CJE".
DEF VAR s-nroser AS INT.

pNuevoCJE = "".

/* Desabiliamos Triggers */
DISABLE TRIGGERS FOR LOAD OF Ccbcmvto.
DISABLE TRIGGERS FOR LOAD OF Ccbdcaja.
DISABLE TRIGGERS FOR LOAD OF Ccbcdocu.

/* Buscamos todos los CJE relacionados a la FAC */
FOR EACH ccbdcaja NO-LOCK WHERE ccbdcaja.codcia = s-codcia AND
    ccbdcaja.coddoc = s-coddoc AND
    ccbdcaja.codref = b-origen.coddoc AND
    ccbdcaja.nroref = b-origen.nrodoc,
    FIRST ccbcmvto NO-LOCK WHERE ccbcmvto.codcia = s-codcia AND
    ccbcmvto.coddoc = ccbdcaja.coddoc AND
    ccbcmvto.nrodoc = ccbdcaja.nrodoc AND
    ccbcmvto.flgest <> "A":
    FIND FIRST t-ccbcmvto WHERE t-ccbcmvto.codcia = ccbdcaja.codcia AND
        t-ccbcmvto.coddoc = ccbdcaja.coddoc AND
        t-ccbcmvto.nrodoc = ccbdcaja.nrodoc
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE t-ccbcmvto THEN DO:
        CREATE t-ccbcmvto.
        BUFFER-COPY ccbcmvto TO t-ccbcmvto.
    END.
END.

DEF BUFFER b-ccbcmvto FOR ccbcmvto.
DEF BUFFER b-ccbdmvto FOR ccbdmvto.
DEF BUFFER b-ccbdcaja FOR ccbdcaja.

RLOOP:
DO TRANSACTION ON ERROR UNDO, RETURN 'ADM-ERROR' ON STOP UNDO, RETURN 'ADM-ERROR':
    FOR EACH t-ccbcmvto NO-LOCK, 
        FIRST b-ccbcmvto EXCLUSIVE-LOCK WHERE b-ccbcmvto.codcia = t-ccbcmvto.codcia AND
        b-ccbcmvto.coddoc = t-ccbcmvto.coddoc AND
        b-ccbcmvto.nrodoc = t-ccbcmvto.nrodoc ON ERROR UNDO, THROW:
        /* Creamos Cabecera */
        s-nroser = INTEGER(SUBSTRING(b-ccbcmvto.nrodoc,1,3)).
        FIND FacCorre WHERE FacCorre.CodCia = s-codcia AND
            FacCorre.CodDoc = s-coddoc AND
            FacCorre.NroSer = s-nroser
            EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
        IF ERROR-STATUS:ERROR = YES THEN DO:
            pMensaje = "NO se pudo bloquear el correlativo".
            UNDO RLOOP, RETURN 'ADM-ERROR'.
        END.
        CREATE Ccbcmvto.
        BUFFER-COPY b-ccbcmvto TO Ccbcmvto
            ASSIGN
            Ccbcmvto.nrodoc = STRING(FacCorre.NroSer,"999") + STRING(FacCorre.Correlativo,"999999")
            Ccbcmvto.fchdoc = TODAY
            Ccbcmvto.usuario = "SYSTEM"
            .
        ASSIGN
            FacCorre.Correlativo = FacCorre.Correlativo + 1.
        RELEASE FacCorre.
        /* "Clonamos" detalle */
        FOR EACH b-ccbdmvto NO-LOCK WHERE b-ccbdmvto.codcia = s-codcia AND
            b-ccbdmvto.coddoc = b-ccbcmvto.coddoc AND
            b-ccbdmvto.nrodoc = b-ccbcmvto.nrodoc:
            CREATE Ccbdmvto.
            BUFFER-COPY b-ccbdmvto TO Ccbdmvto
                ASSIGN
                Ccbdmvto.nrodoc = Ccbcmvto.nrodoc
                .
            IF Ccbdmvto.codref = b-origen.coddoc AND Ccbdmvto.nroref = b-origen.nrodoc
                THEN ASSIGN
                Ccbdmvto.codref = b-destino.coddoc
                Ccbdmvto.nroref = b-destino.nrodoc
                .
        END.
        /* Actualizamos LETRA */
        REPEAT:
            FIND FIRST Ccbcdocu WHERE Ccbcdocu.codcia = s-codcia AND
                Ccbcdocu.coddoc = "LET" AND
                Ccbcdocu.codref = b-ccbcmvto.coddoc AND
                Ccbcdocu.nroref = b-ccbcmvto.nrodoc
                EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
            IF ERROR-STATUS:ERROR = YES THEN LEAVE.
            ASSIGN
                Ccbcdocu.codref = Ccbcmvto.coddoc
                Ccbcdocu.nroref = Ccbcmvto.nrodoc
                .
        END.
        /* Actualizamos DETALLE DEL CANJE APROBADO */
        FOR EACH b-Ccbdcaja NO-LOCK WHERE b-Ccbdcaja.codcia = s-codcia AND
            b-Ccbdcaja.coddoc = b-ccbcmvto.coddoc AND
            b-Ccbdcaja.nrodoc = b-ccbcmvto.nrodoc:
            CREATE Ccbdcaja.
            BUFFER-COPY b-ccbdcaja TO Ccbdcaja
                ASSIGN
                Ccbdcaja.nrodoc = Ccbcmvto.nrodoc
                Ccbdcaja.fchdoc = Ccbcmvto.fchdoc
                .
            IF Ccbdcaja.codref = b-origen.coddoc AND Ccbdcaja.nroref = b-origen.nrodoc 
                THEN ASSIGN
                Ccbdcaja.codref = b-destino.coddoc
                Ccbdcaja.nroref = b-destino.nrodoc
                .
        END.

        /* Anulamos Cabecera */
        ASSIGN
            b-ccbcmvto.flgest = "A"
            b-ccbcmvto.libre_chr[10] = "Anulado x SYSTEM " + STRING(TODAY,"99/99/9999") + " " + STRING(TIME,"HH:MM:SS").

        IF NOT (TRUE <> (pNuevoCJE > "")) THEN pNuevoCJE = pNuevoCJE + ", ".
        pNuevoCJE = pNuevoCJE + ccbcmvto.nrodoc.
    END.
END.

RETURN 'OK'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

