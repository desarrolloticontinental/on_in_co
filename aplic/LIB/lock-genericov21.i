&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
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



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 3.81
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Include 


/* ***************************  Main Block  *************************** */
/*
  Tabla: "Almacen"
  Alcance: [{"FIRST" | "LAST" | "CURRENT" }]   Valor opcional
  Condicion: "Almacen.codcia = s-codcia AND Almacen.codalm = s-codalm"
  Bloqueo: { "NO-LOCK" | "SHARED-LOCK" | "EXCLUSIVE-LOCK" }
  Accion: { "RETRY" | "LEAVE" }
  Mensaje: { "YES" | "NO" }
  TipoError: { "[UNDO,] RETURN {'ADM-ERROR' | ERROR | }" | "NEXT" | "LEAVE" }
  &Intentos="5"
*/

DEF VAR LocalCounter AS INTEGER INITIAL 0 NO-UNDO.
DEF VAR cMsgs AS CHARACTER NO-UNDO.
DEF VAR ix AS INTEGER NO-UNDO.

LocalCounter = -1.
GetLock:
REPEAT ON STOP UNDO, {&Accion} ON ERROR UNDO, {&Accion}:
    &IF DEFINED(Intentos) &THEN
            LocalCounter = LocalCounter + 1.
            IF LocalCounter >= {&Intentos} THEN LEAVE GetLock.    /* 5 intentos m�ximo */
    &ELSE 
            LocalCounter = LocalCounter + 1.
            IF LocalCounter >= 5 THEN LEAVE GetLock.    /* 5 intentos m�ximo */
    &ENDIF
    &IF DEFINED(Alcance) &THEN
        FIND {&Alcance} {&Tabla} WHERE {&Condicion} {&Bloqueo} NO-ERROR.
    &ELSE
        FIND {&Tabla} WHERE {&Condicion} {&Bloqueo} NO-ERROR.
    &ENDIF
    IF AVAILABLE {&Tabla} THEN LEAVE.
    IF AMBIGUOUS {&Tabla} THEN DO:      /* Llave Duplicada o No existe*/
        IF {&Mensaje} = YES THEN DO:
            IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
                cMsgs = ERROR-STATUS:GET-MESSAGE(1).
                DO ix = 2 TO ERROR-STATUS:NUM-MESSAGES:
                    cMsgs = cMsgs + CHR(10) + ERROR-STATUS:GET-MESSAGE(ix).
                END.
                MESSAGE cMsgs VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            END.
        END.
        LEAVE GetLock.
    END.
    PAUSE 2 NO-MESSAGE.
END.
&IF DEFINED(Intentos) &THEN
    IF LocalCounter >= {&Intentos} OR NOT AVAILABLE {&Tabla} THEN {&TipoError}.
&ELSE
    IF LocalCounter >= 5 OR NOT AVAILABLE {&Tabla} THEN {&TipoError}.
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


