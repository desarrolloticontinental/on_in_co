&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r11
/* Procedure Description
"Borra el contenido de una tabla"
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
                                                                        */
&ANALYZE-RESUME
 



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Include 


/* ***************************  Main Block  *************************** */

    DEFINE VARIABLE ARCHIVO  AS CHARACTER FORMAT "X(30)" NO-UNDO.
    DEFINE VARIABLE borrando AS CHARACTER FORMAT "X(30)" NO-UNDO.
    
    DEFINE VARIABLE I       AS DECIMAL   NO-UNDO.
    DEFINE FRAME F-MSG-5
        SKIP(1)
        SPACE(2)
        ARCHIVO  FORMAT "X(30)"
        SKIP
        bORRANDO FORMAT "X(30)"
        SPACE(2)
        SKIP(1)
        WITH VIEW-AS DIALOG-BOX CENTERED NO-LABEL 
            TITLE "Procesando .." FONT 4.
    VIEW FRAME F-MSG-5.        
    ARCHIVO = "Tabla : {1}".
    ARCHIVO:SCREEN-VALUE IN FRAME F-MSG-5 = ARCHIVO.

I = 0.
FOR EACH DICTDB.{1}:
    IF I MODULO 100 = 0
    THEN DO:       
        Borrando:screen-value = STRING( I, ">>>,>>>,>>>,>>9") + " Registros Borrados".
        PROCESS EVENTS.
    END.
    DELETE DICTDB.{1}.
END.
Borrando:screen-value = STRING( I, ">>>,>>>,>>>,>>9") + " Registros Borrados".

PAUSE 1.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


