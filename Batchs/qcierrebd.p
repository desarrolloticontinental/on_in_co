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

/* Cierre de BOletas de Deposito */

DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR s-user-id AS CHAR INIT 'SISTEMAS' NO-UNDO.

DISABLE TRIGGERS FOR LOAD OF ccbcdocu.
DISPLAY 'INICIO:' DATETIME(TODAY,MTIME). PAUSE 0.
FOR EACH ccbcdocu WHERE ccbcdocu.codcia = s-codcia
    AND ccbcdocu.coddoc = "BD"
    AND LOOKUP(ccbcdocu.flgest, "E,R") > 0      /* Emitidos y Rechazados */
    AND ccbcdocu.fchdoc < TODAY - 7:    /* 7 dias */
    ASSIGN
      CcbCDocu.UsuAnu = s-user-id
      CcbCDocu.FchAnu = TODAY
      Ccbcdocu.FlgEst = 'A'
      Ccbcdocu.SdoAct = 0.
END.
DISPLAY 'FIN:' DATETIME(TODAY,MTIME). PAUSE 0.
QUIT.

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
         HEIGHT             = 3.92
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


