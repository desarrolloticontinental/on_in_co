&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12
/* Procedure Description
"Definición de Variables compartidas generales"
*/
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
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
DEFINE SHARED VARIABLE s-admin     AS LOGICAL   FORMAT "Si/No".
DEFINE SHARED VARIABLE s-codcia    AS INTEGER   FORMAT "999".
DEFINE SHARED VARIABLE s-ruccia    AS INTEGER   FORMAT "99999999999".
DEFINE SHARED VARIABLE s-nomcia    AS CHARACTER FORMAT "X(50)".
DEFINE SHARED VARIABLE s-dircia    AS CHARACTER FORMAT "X(50)".
DEFINE SHARED VARIABLE s-user-id   AS CHARACTER FORMAT "X(16)".
DEFINE SHARED VARIABLE s-aplic-id  AS CHARACTER FORMAT "X(3)".
DEFINE SHARED VARIABLE s-seguridad AS CHARACTER FORMAT "X(20)".
DEFINE SHARED VARIABLE s-nomcia1   AS CHARACTER FORMAT "X(60)".
DEFINE SHARED VARIABLE s-local     AS CHARACTER FORMAT "X(5)".

DEFINE SHARED VARIABLE cb-codcia AS INT.
DEFINE SHARED VARIABLE cl-codcia AS INT.
DEFINE SHARED VARIABLE pv-codcia AS INT.
DEFINE SHARED VARIABLE s-OpSys   AS CHAR.

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
         HEIGHT             = 2
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Include 


/* ***************************  Main Block  *************************** */

FIND FIRST Empresas WHERE Empresas.codcia = s-codcia NO-LOCK NO-ERROR.
IF AVAILABLE Empresas THEN DO:
    IF NOT Empresas.Campo-CodCbd THEN cb-codcia = s-codcia.
    IF NOT Empresas.Campo-CodCli THEN cl-codcia = s-codcia.
    IF NOT Empresas.Campo-CodPro THEN pv-codcia = s-codcia.
END.

GET-KEY-VALUE SECTION 'Startup' KEY 'OpSysVersion' VALUE s-OpSys.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


