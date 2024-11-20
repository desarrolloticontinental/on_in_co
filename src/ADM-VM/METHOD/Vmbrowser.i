&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Method-Library 
/*--------------------------------------------------------------------------
    Library     : 
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
   Type: Method-Library
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Method-Library ASSIGN
         HEIGHT             = 1.12
         WIDTH              = 46.86.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME
 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Method-Library 
/* ************************* Included-Libraries *********************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Method-Library 


/* ***************************  Main Block  *************************** */
/*STATUS INPUT "[INS]-Add [CTRL+G]-Delete [F10]-Mod./Grabar [CTRL+U]-Cancelar".
 * 
 * /* Add */
 * ON "INSERT":U OF FRAME {&FRAME-NAME} ANYWHERE DO:
 *   RUN Procesa-Handle IN lh_Handle ('update-Btn-Add').
 *   APPLY "ENTRY" TO {&BROWSE-NAME}.
 *   RETURN NO-APPLY.
 * END.
 * 
 * /* Delete */
 * ON "CTRL-G":U OF FRAME {&FRAME-NAME} ANYWHERE DO:
 *   RUN Procesa-Handle IN lh_Handle ('update-Btn-Delete').
 *   APPLY "ENTRY" TO {&BROWSE-NAME}.
 *   RETURN NO-APPLY.
 * END.
 * 
 * /* Save */
 * ON "F10":U OF FRAME {&FRAME-NAME} ANYWHERE DO:
 *   RUN Procesa-Handle IN lh_Handle ('update-Btn-Save').
 *   APPLY "ENTRY" TO {&BROWSE-NAME}.
 *   RETURN NO-APPLY.
 * END.
 * 
 * /* Cancel */
 * ON "CTRL-U":U OF FRAME {&FRAME-NAME} ANYWHERE DO:
 *   RUN Procesa-Handle IN lh_Handle ('update-Btn-Cancel').
 *   APPLY "ENTRY" TO {&BROWSE-NAME}.
 *   RETURN NO-APPLY.
 * END.
 * 
 * 
 * ON ENTRY OF FRAME {&FRAME-NAME} ANYWHERE DO:
 *     {&ENTRY_FIELD}
 * END.*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


