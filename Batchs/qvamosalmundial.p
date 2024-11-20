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

DEFINE VAR hProc AS HANDLE NO-UNDO.
DEFINE VAR pCodError AS CHAR.

/* Cargamos en memoria las librerias */
RUN vtagn/puntos-vamos-mundial.p PERSISTENT SET hProc NO-ERROR.
IF ERROR-STATUS:ERROR = YES THEN DO:
    pCodError = "ERROR en las librerias de la Promoción VAMOS AL MUNDIAL" + CHR(10) +
        "Salir del Sistema, volver a entrar y repetir el proceso".
    DISPLAY pCodError WITH STREAM-IO NO-BOX WIDTH 320.
    RETURN "ADM-ERROR".
END.

/* AQUI VAN LAS RUTINAS PROPIAS DEL PROGRAMA */
DEF VAR pFchDoc AS DATE NO-UNDO.

pFchDoc = TODAY - 1.
IF pFchDoc >= DATE(05,14,2018) AND pFchDoc <= DATE(07,15,2018) THEN DO:
    DISPLAY 'INICIO:' NOW WITH STREAM-IO DOWN.
    RUN carga-puntos-por-cliente IN hProc (001, pFchDoc).
    DISPLAY 'FIN:' NOW WITH STREAM-IO DOWN.
END.
/* ***************************************** */

/* Borramos de la memoria las librerias antes cargadas */
DELETE PROCEDURE hProc.

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
         HEIGHT             = 4.5
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


