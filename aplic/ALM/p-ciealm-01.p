&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*--------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
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
         HEIGHT             = 2
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
DEF INPUT PARAMETER xFchDoc AS DATE.
DEF INPUT PARAMETER xCodAlm AS CHAR.

DEF SHARED VAR s-codcia AS INT.

FIND AlmCieAl WHERE
     AlmCieAl.CodCia = S-CODCIA AND
     AlmCieAl.CodAlm = xCodAlm  AND
     AlmCieAl.FchCie = xFchDoc 
     NO-LOCK NO-ERROR.
IF AVAILABLE AlmCieAl AND AlmCieAl.FlgCie THEN DO:
   MESSAGE "Este dia " xFchDoc " se encuentra cerrado" SKIP 
           "Almacen:" xCodAlm SKIP
           "Consulte con sistemas " VIEW-AS ALERT-BOX WARNING.
   RETURN "ADM-ERROR".
END.
RETURN "OK".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


