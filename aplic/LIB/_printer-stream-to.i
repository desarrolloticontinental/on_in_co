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
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Include 


/* ***************************  Main Block  *************************** */

/* DEF VAR x-OpSys AS CHAR NO-UNDO.                                  */
/* GET-KEY-VALUE SECTION 'Startup' KEY 'OpSysVersion' VALUE x-OpSys. */

IF "{3}" = 'PAGED' THEN DO:
    IF s-OpSys = 'WinVista' OR s-OpSys = 'WinXP'
    THEN OUTPUT STREAM {2} TO PRINTER VALUE(s-port-name) PAGED PAGE-SIZE {1}.
    ELSE OUTPUT STREAM {2} TO VALUE(s-port-name) PAGED PAGE-SIZE {1}.
END.
ELSE DO:
    IF s-OpSys = 'WinVista' OR s-OpSys = 'WinXP'
    THEN OUTPUT STREAM {2} TO PRINTER VALUE(s-port-name) PAGE-SIZE {1}.
    ELSE OUTPUT STREAM {2} TO VALUE(s-port-name) PAGE-SIZE {1}.
END.
  /*OUTPUT STREAM Reporte TO VALUE( s-port-name ) PAGED PAGE-SIZE 30.  */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


