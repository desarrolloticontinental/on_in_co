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

DISABLE TRIGGERS FOR LOAD OF LG-COCMP.
CIERRE:
FOR EACH LG-COCmp EXCLUSIVE-LOCK WHERE 
    LG-COCmp.CodCia = 001 AND
    LG-COCmp.FlgSit = "P" AND 
    LG-COCmp.FchVto < TODAY - 30  
    ON ERROR UNDO , RETURN "ADM-ERROR":
    /* RHC 12.04.04 NO SE CIERRA SI TIENE ATENCIONES PARCIALES */
    FOR EACH Lg-Docmp OF Lg-Cocmp NO-LOCK:
        IF lg-docmp.canaten > 0 THEN NEXT CIERRE.
    END.
    DISPLAY lg-cocmp.nrodoc lg-cocmp.fchdoc lg-cocmp.fchvto 
        WITH STREAM-IO NO-BOX. 
    PAUSE 0.
    ASSIGN LG-COCmp.FlgSit = "V". 
END.

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
         HEIGHT             = 3.65
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


