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

  FIND {&Tabla} WHERE {&Tabla}.codcia = s-codcia
      AND {&Tabla}.coddoc = x-coddoc
      AND {&Tabla}.nroped = VtaTrack04.NroPed:SCREEN-VALUE IN BROWSE {&browse-name}
      AND {&Tabla}.codalm = s-codalm
      NO-LOCK NO-ERROR.
  IF NOT AVAILABLE {&Tabla} THEN DO:
      MESSAGE 'Documento no registrado o pertenece a otro almac�n'
          VIEW-AS ALERT-BOX ERROR.
      APPLY 'ENTRY':U TO VtaTrack04.NroPed IN BROWSE {&browse-name}.
      RETURN 'ADM-ERROR'.
  END.
  IF LOOKUP({&Tabla}.flgest, 'C,P,F') = 0 THEN DO:
      MESSAGE 'Pedido no se encuentra aprobado' VIEW-AS ALERT-BOX ERROR.
      APPLY 'ENTRY':U TO VtaTrack04.NroPed IN BROWSE {&browse-name}.
      RETURN 'ADM-ERROR'.
  END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


