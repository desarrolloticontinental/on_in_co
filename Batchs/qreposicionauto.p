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

/* CIERRE Y GENERACION DE REPOSICIONES AUTOMATICAS */
DISABLE TRIGGERS FOR LOAD OF almcrepo.

DEF VAR s-codcia AS INT INIT 001 NO-UNDO.

/* CIERRE MANUAL DE REPOSICIONES VENCIDAS */
FOR EACH Almacen NO-LOCK WHERE Almacen.codcia = s-codcia,
    EACH Almcrepo WHERE Almcrepo.codcia = s-codcia
        AND Almcrepo.codalm = Almacen.codalm
        AND Almcrepo.TipMov = 'A'
        AND Almcrepo.flgest = 'P'
        AND Almcrepo.FlgSit = 'A'
        AND Almcrepo.fchvto < TODAY:
    DISPLAY
        almcrepo.codalm
        almcrepo.nrodoc
        almcrepo.fchdoc
        almcrepo.fchvto
        SKIP
        WITH STREAM-IO NO-BOX.
    PAUSE 0.
    ASSIGN
        almcrepo.FchApr = TODAY
        almcrepo.FlgSit = 'M'
        almcrepo.HorApr = STRING(TIME, 'HH:MM')
        almcrepo.UsrApr = 'AUTO'.
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
         HEIGHT             = 3.88
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


