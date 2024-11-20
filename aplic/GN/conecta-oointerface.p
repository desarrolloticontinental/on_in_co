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

/* CONECTA LA BASE DE DATOS CISSAC */
/* Copiamos a CISSAC */
DEF VAR pProfile AS CHAR INIT 'oointerface.pf' NO-UNDO.

IF NOT CONNECTED("oointerface") THEN DO:
    /*
    pProfile = SEARCH(pProfile) NO-ERROR.
    IF pProfile <> ? THEN DO:
        MESSAGE "dd2".
        CONNECT -pf VALUE(pProfile) NO-ERROR.
        MESSAGE "dd3".
        IF ERROR-STATUS:ERROR THEN DO:
            MESSAGE "No se pudo CONECTAR a las base de OOINTERFACE".
            RETURN ERROR.
        END.            
    END.
    ELSE DO:
        MESSAGE "No se pudo el leer profile()".
        RETURN ERROR.
    END.
    */
    CONNECT -db oointerface -ld oointerface -N TCP -S 65110 -H 192.168.100.210 NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        MESSAGE "No se pudo CONECTAR a las base de OOINTERFACE".
        RETURN ERROR.
    END.            

END.

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
         HEIGHT             = 3.65
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


