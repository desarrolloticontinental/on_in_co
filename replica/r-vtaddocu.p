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
         HEIGHT             = 3.35
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
DEFINE INPUT PARAMETER pKeyValue AS CHAR.
DEFINE INPUT PARAMETER pEvent AS CHAR.
DEFINE INPUT PARAMETER pDataRecord AS RAW.
DEFINE OUTPUT PARAMETER L-FLG AS LOGICAL NO-UNDO.

DISABLE TRIGGERS FOR LOAD OF base1.vtaddocu.
DEFINE VAR iErrores AS INT INIT 0 NO-UNDO.
L-FLG = NO.
RLOOP:
DO TRANSACTION ON ERROR UNDO, LEAVE ON STOP UNDO, LEAVE:
    DEF VAR LocalCounter AS INTEGER NO-UNDO.
    LocalCounter = 0.
    GetLock:
    REPEAT ON ERROR UNDO GetLock, LEAVE GetLock:
        FIND base1.vtaddocu WHERE base1.vtaddocu.codcia = INTEGER(SUBSTRING(pKeyValue,1,3)) AND ~
         base1.vtaddocu.codped = SUBSTRING(pKeyValue,4,3) AND ~
         base1.vtaddocu.nroped = SUBSTRING(pKeyValue,7,12) AND ~
         base1.vtaddocu.codmat = SUBSTRING(pKeyValue,19,6) AND ~
         base1.vtaddocu.almdes = SUBSTRING(pKeyValue,25) ~
            EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
        /* Encontrado */
        IF AVAILABLE base1.vtaddocu THEN LEAVE GetLock.
        /* Duplicado */
        IF AMBIGUOUS base1.vtaddocu THEN LEAVE GetLock.
        /* NO existe el registro */
        IF NOT AVAILABLE base1.vtaddocu AND NOT LOCKED base1.vtaddocu THEN LEAVE GetLock.
        LocalCounter = LocalCounter + 1.
        PAUSE 2 NO-MESSAGE.
        IF LocalCounter >= 10 THEN LEAVE GetLock.
    END.
    IF NOT AVAILABLE base1.vtaddocu AND LOCKED base1.vtaddocu THEN RETURN. 
    CASE pEvent:
        WHEN 'DELETE' THEN DO:
            IF AVAILABLE base1.vtaddocu THEN DELETE base1.vtaddocu.
            L-FLG = YES.    
        END.
        WHEN 'WRITE' THEN DO:
            IF NOT AVAILABLE base1.vtaddocu THEN CREATE base1.vtaddocu.
            RAW-TRANSFER pDataRecord TO base1.vtaddocu.
            L-FLG = YES.
        END.
    END CASE.
    CATCH eError AS PROGRESS.Lang.Error:
        DISPLAY eError:GetMessage(1) WITH STREAM-IO NO-BOX WIDTH 320.
        PAUSE 0.
        DELETE OBJECT eError.
        UNDO, LEAVE RLOOP.
    END CATCH.
END.
IF AVAILABLE(base1.vtaddocu) THEN RELEASE base1.vtaddocu.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

