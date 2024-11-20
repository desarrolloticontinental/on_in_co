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

DEFINE SHARED VAR pRCID AS INT.
DEFINE NEW GLOBAL SHARED VARIABLE s-user-id AS CHARACTER FORMAT "x(16)".
DEFINE NEW GLOBAL SHARED VARIABLE hSocket AS HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE hWebService AS HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE hPortType AS HANDLE NO-UNDO.

DEFINE VARIABLE botones     AS WIDGET-HANDLE EXTENT 50 NO-UNDO.
DEFINE VARIABLE s-fil       AS DECIMAL   NO-UNDO.
DEFINE VARIABLE s-col       AS DECIMAL   NO-UNDO.
DEFINE VARIABLE fila        AS DECIMAL   NO-UNDO.
DEFINE VARIABLE columna     AS DECIMAL   NO-UNDO.
DEFINE VARIABLE fila-ini    AS DECIMAL   NO-UNDO.
DEFINE VARIABLE columna-ini AS DECIMAL   NO-UNDO.
DEFINE VARIABLE i           AS INTEGER   NO-UNDO.
DEFINE VARIABLE max-app     AS INTEGER   NO-UNDO.
DEFINE VARIABLE x-codcia    AS INTEGER   NO-UNDO.
DEFINE VARIABLE lista-cia   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lista-cod   AS CHARACTER NO-UNDO.
DEFINE VARIABLE Ok          AS LOGICAL   NO-UNDO.

/* CONFIGURACIONES DE ENTORNO POR DEFECTO */
SESSION:DATA-ENTRY-RETURN = TRUE.    /* << OJO actua como si fuera TAB */
SESSION:APPL-ALERT-BOXES  = NO.     /* << OJO si es TRUE los mensajes son alert-box */
SESSION:SYSTEM-ALERT-BOXES  = NO.     /* << OJO si es TRUE los mensajes son alert-box */
SESSION:SUPPRESS-WARNINGS  = YES.     /* << OJO si es TRUE los mensajes son alert-box */
SESSION:TIME-SOURCE = "integral".   /* Toma la fecha y hora del servidor de 'integral' */

ASSIGN
    Ok        = SESSION:SET-WAIT-STATE("GENERAL")
    s-user-id = USERID("integral").

RUN bin/_registr.w("YES").

ASSIGN
    Ok        = SESSION:SET-WAIT-STATE("").

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
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

    /* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
    
    RUN bin/_medio-rapido.p( 001, "PDA", "pda/w-ped-mostrador-mayorista.w").

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


