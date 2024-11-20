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
DEF OUTPUT PARAMETER pNuevoIC AS CHAR NO-UNDO.

DEF BUFFER b-origen FOR ccbcdocu.
DEF BUFFER b-ccbddocu FOR ccbddocu.

DEF BUFFER b-destino  FOR ccbcdocu.

FIND b-origen WHERE ROWID(b-origen) = pRowidOrigen NO-LOCK NO-ERROR.

IF NOT AVAILABLE b-origen OR LOOKUP(TRIM(b-origen.coddoc), 'FAC,BOL') = 0 OR b-origen.flgest = "A" THEN DO:
    pMensaje = "Documento (FAC o BOL) NO encontrado o ya ha sido anulado".
    /*RETURN "ADM-ERROR".*/
END.

FIND b-destino WHERE ROWID(b-destino) = pRowidDestino NO-LOCK NO-ERROR.
IF NOT AVAILABLE b-destino OR LOOKUP(TRIM(b-destino.coddoc), 'FAC,BOL') = 0 THEN DO:
    pMensaje = "Documento (FAC o BOL) NO encontrado".
    RETURN "ADM-ERROR".
END.

pMensaje = "".

DEF TEMP-TABLE t-ccbccaja NO-UNDO LIKE ccbccaja.

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

DEF VAR s-coddoc AS CHAR INITIAL "I/C".
DEF VAR s-ptovta AS INT.

pNuevoIC = "".

/* Desabiliamos Triggers */
DISABLE TRIGGERS FOR LOAD OF Ccbccaja.
DISABLE TRIGGERS FOR LOAD OF Ccbdcaja.
DISABLE TRIGGERS FOR LOAD OF Ccbdmov.

/* Buscamos todos los I/C relacionados a la FAC */
FOR EACH ccbdcaja NO-LOCK WHERE ccbdcaja.codcia = s-codcia AND
    ccbdcaja.coddoc = s-coddoc AND
    ccbdcaja.codref = b-origen.coddoc AND
    ccbdcaja.nroref = b-origen.nrodoc,
    FIRST ccbccaja NO-LOCK WHERE ccbccaja.codcia = ccbdcaja.codcia AND
    ccbccaja.coddoc = ccbdcaja.coddoc AND
    ccbccaja.nrodoc = ccbdcaja.nrodoc AND
    ccbccaja.flgest <> "A":
    FIND FIRST t-ccbccaja WHERE t-ccbccaja.codcia = ccbccaja.codcia AND
        t-ccbccaja.coddoc = ccbccaja.coddoc AND
        t-ccbccaja.nrodoc = ccbccaja.nrodoc
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE t-ccbccaja THEN DO:
        CREATE t-ccbccaja.
        BUFFER-COPY ccbccaja TO t-ccbccaja.
    END.
END.

DEF BUFFER b-ccbccaja FOR ccbccaja.
DEF BUFFER b-ccbdcaja FOR ccbdcaja.
DEF BUFFER b-ccbdmov  FOR ccbdmov.

RLOOP:
DO TRANSACTION ON ERROR UNDO, RETURN 'ADM-ERROR' ON STOP UNDO, RETURN 'ADM-ERROR':
    FOR EACH t-ccbccaja NO-LOCK, 
        FIRST b-ccbccaja EXCLUSIVE-LOCK WHERE b-ccbccaja.codcia = t-ccbccaja.codcia AND
        b-ccbccaja.coddoc = t-ccbccaja.coddoc AND
        b-ccbccaja.nrodoc = t-ccbccaja.nrodoc ON ERROR UNDO, THROW:
        /* Creamos Cabecera */
        s-ptovta = INTEGER(SUBSTRING(b-ccbccaja.nrodoc,1,3)).
        FIND FacCorre WHERE FacCorre.CodCia = s-codcia AND
            FacCorre.CodDoc = s-coddoc AND
            FacCorre.NroSer = s-ptovta
            EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
        IF ERROR-STATUS:ERROR = YES THEN DO:
            pMensaje = "NO se pudo bloquear el correlativo".
            UNDO RLOOP, RETURN 'ADM-ERROR'.
        END.
        CREATE Ccbccaja.
        BUFFER-COPY b-ccbccaja TO Ccbccaja
            ASSIGN
            Ccbccaja.nrodoc = STRING(faccorre.nroser, "999") +
                                STRING(faccorre.correlativo, "999999")
            Ccbccaja.fchdoc = TODAY
            Ccbccaja.fchcie = TODAY
            Ccbccaja.usuario = "SYSTEM".
        ASSIGN
            FacCorre.Correlativo = FacCorre.Correlativo + 1.
        RELEASE FacCorre.

        /* "Clonamos" detalle */
        FOR EACH b-ccbdcaja OF b-ccbccaja NO-LOCK:
            CREATE Ccbdcaja.
            BUFFER-COPY b-ccbdcaja TO ccbdcaja
                ASSIGN
                Ccbdcaja.nrodoc = Ccbccaja.nrodoc
                Ccbdcaja.fchdoc = Ccbccaja.fchdoc.
            /* "Clonamos" */
            IF Ccbdcaja.codref = b-origen.coddoc AND 
                Ccbdcaja.nroref = b-origen.nrodoc
                THEN
                ASSIGN
                Ccbdcaja.codref = b-destino.coddoc
                Ccbdcaja.nroref = b-destino.nrodoc.
        END.

        /* "Clonamos" aplicación */
        FOR EACH CCBDMOV EXCLUSIVE-LOCK WHERE CCBDMOV.CodCia = b-ccbccaja.CodCia AND
            CCBDMOV.CodDiv = b-ccbccaja.CodDiv AND
            CCBDMOV.CodRef = b-ccbccaja.coddoc AND
            CCBDMOV.NroRef = b-ccbccaja.nrodoc:
            CREATE b-ccbdmov.
            BUFFER-COPY ccbdmov TO b-ccbdmov
                ASSIGN
                b-ccbdmov.nroref = Ccbccaja.nrodoc
                b-ccbdmov.fchdoc = Ccbccaja.fchdoc
                b-ccbdmov.hramov = STRING(TIME,"HH:MM:SS")
                .
            /* DELETE Ccbdmov.*/
            ASSIGN Ccbdmov.codref = Ccbdmov.codref + "YY".
        END.
        
        /* Anualmos Cabecera */
        ASSIGN
            b-ccbccaja.flgest = "A"
            b-ccbccaja.usranu = "SYSTEM"
            b-ccbccaja.fchanu = TODAY
            b-ccbccaja.horanu = STRING(TIME, 'HH:MM:SS').
        
        IF NOT (TRUE <> (pNuevoIC > "")) THEN pNuevoIC = pNuevoIC + ", ".
        pNuevoIC = pNuevoIC + Ccbccaja.nrodoc.
        
    END.
END.

RETURN 'OK'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

