&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          integral         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File:

  Description: from VIEWER.W - Template for SmartViewer Objects

  Input Parameters:
      <none>

  Output Parameters:
      <none>

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEF SHARED VAR s-codcia AS INT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES CcbCDocu
&Scoped-define FIRST-EXTERNAL-TABLE CcbCDocu


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR CcbCDocu.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-66 Btn-Otros 
&Scoped-Define DISPLAYED-FIELDS CcbCDocu.ImpBrt CcbCDocu.ImpExo ~
CcbCDocu.ImpDto CcbCDocu.ImpVta CcbCDocu.ImpIgv CcbCDocu.ImpIsc ~
CcbCDocu.TotalMontoICBPER CcbCDocu.ImpTot 
&Scoped-define DISPLAYED-TABLES CcbCDocu
&Scoped-define FIRST-DISPLAYED-TABLE CcbCDocu


/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "",
     Keys-Supplied = ""':U).
/**************************
</EXECUTING-CODE> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn-Otros 
     LABEL "Redondeo" 
     SIZE 9 BY .85.

DEFINE RECTANGLE RECT-66
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 108 BY 1.92.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     CcbCDocu.ImpBrt AT ROW 1.77 COL 6 NO-LABEL FORMAT "->>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 8.57 BY .81
          BGCOLOR 15 
     CcbCDocu.ImpExo AT ROW 1.77 COL 14.72 COLON-ALIGNED NO-LABEL FORMAT "->>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 8.57 BY .81
          BGCOLOR 15 
     CcbCDocu.ImpDto AT ROW 1.77 COL 25.14 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 8.57 BY .81
          BGCOLOR 15 
     CcbCDocu.ImpVta AT ROW 1.77 COL 35.86 COLON-ALIGNED NO-LABEL FORMAT "->>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 8.57 BY .81
          BGCOLOR 15 
     CcbCDocu.ImpIgv AT ROW 1.81 COL 45.86 COLON-ALIGNED NO-LABEL FORMAT "->>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 8.57 BY .81
          BGCOLOR 15 
     CcbCDocu.ImpIsc AT ROW 1.81 COL 56.72 COLON-ALIGNED NO-LABEL FORMAT "->>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 8.57 BY .81
          BGCOLOR 15 
     CcbCDocu.TotalMontoICBPER AT ROW 1.81 COL 67 COLON-ALIGNED NO-LABEL WIDGET-ID 14 FORMAT "->>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 8 BY .81
          BGCOLOR 15 FGCOLOR 0 
     CcbCDocu.ImpTot AT ROW 1.81 COL 76 COLON-ALIGNED NO-LABEL FORMAT "->>>>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 8.57 BY .81
          BGCOLOR 15 
     Btn-Otros AT ROW 1.81 COL 88
     "ICBPER" VIEW-AS TEXT
          SIZE 6 BY .5 AT ROW 1.27 COL 69 WIDGET-ID 16
     "I.S.C." VIEW-AS TEXT
          SIZE 8.57 BY .5 AT ROW 1.19 COL 59.14
     "I.G.V." VIEW-AS TEXT
          SIZE 8.57 BY .5 AT ROW 1.19 COL 48.86
     "Exonerado" VIEW-AS TEXT
          SIZE 8.57 BY .5 AT ROW 1.19 COL 16.72
     "Imp.Bruto" VIEW-AS TEXT
          SIZE 8.57 BY .5 AT ROW 1.19 COL 6
     "Imp.Total" VIEW-AS TEXT
          SIZE 8.57 BY .5 AT ROW 1.27 COL 78
     "Imp.Dcto." VIEW-AS TEXT
          SIZE 8.57 BY .5 AT ROW 1.19 COL 27.14
     "Valor Venta" VIEW-AS TEXT
          SIZE 8.57 BY .5 AT ROW 1.19 COL 37.86
     RECT-66 AT ROW 1 COL 1 WIDGET-ID 12
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 4.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: integral.CcbCDocu
   Allow: Basic,DB-Fields
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY COMPILE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW V-table-Win ASSIGN
         HEIGHT             = 1.92
         WIDTH              = 109.14.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm-vm/method/vmviewer.i}
{src/adm/method/viewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit                                              */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN CcbCDocu.ImpBrt IN FRAME F-Main
   NO-ENABLE ALIGN-L EXP-FORMAT                                         */
/* SETTINGS FOR FILL-IN CcbCDocu.ImpDto IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN CcbCDocu.ImpExo IN FRAME F-Main
   NO-ENABLE EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN CcbCDocu.ImpIgv IN FRAME F-Main
   NO-ENABLE EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN CcbCDocu.ImpIsc IN FRAME F-Main
   NO-ENABLE EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN CcbCDocu.ImpTot IN FRAME F-Main
   NO-ENABLE EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN CcbCDocu.ImpVta IN FRAME F-Main
   NO-ENABLE EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN CcbCDocu.TotalMontoICBPER IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Btn-Otros
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Otros V-table-Win
ON CHOOSE OF Btn-Otros IN FRAME F-Main /* Redondeo */
DO:
/*  DEF VAR RPTA AS CHAR INIT "".
 *   FIND FIRST FacCfgGn WHERE FacCfgGn.CodCia = Ccbcdocu.Codcia NO-LOCK NO-ERROR.
 *   RUN ALM/D-CLAVE(FacCfgGn.Cla_Venta,OUTPUT RPTA). 
 *   IF RPTA = "ERROR" THEN RETURN "ADM-ERROR".*/
  
  /* Code placed here will execute PRIOR to standard behavior. */
  IF CcbCDocu.FlgEst = "A" THEN DO:
     MESSAGE 'El documento se enuentra Anulado...' VIEW-AS ALERT-BOX.
     RETURN 'ADM-ERROR'.
  END.
  IF CcbCDocu.SdoAct <> CcbCDocu.ImpTot  THEN DO:
     MESSAGE 'El documento registra amortizaciones...' VIEW-AS ALERT-BOX.
     RETURN 'ADM-ERROR'.
  END.

  /* Valida Clave de Anulación */
  DEF VAR s-coddoc AS CHAR INIT 'RNC' NO-UNDO.
  {adm/i-DocPssw.i s-CodCia s-CodDoc ""UPD""}

  IF CCbcDocu.Sdoact <= 0  THEN DO:
      MESSAGE  "Documento Cancelado "
               "Imposible Efectuar Modificaciones " VIEW-AS ALERT-BOX TITLE "Documento Cancelado".
                RETURN NO-APPLY.
  END.
  RUN ccb/d-rndn-a (ROWID(CcbCDocu)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         
  
  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available V-table-Win  _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Create a list of all the tables that we need to get.            */
  {src/adm/template/row-list.i "CcbCDocu"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "CcbCDocu"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI V-table-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME F-Main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  RUN valida.
  IF RETURN-VALUE = "ADM-ERROR" THEN RETURN "ADM-ERROR".
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE procesa-parametros V-table-Win 
PROCEDURE procesa-parametros :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    /*
    Variables a usar:
    output-var-1 como ROWID
    output-var-2 como CHARACTER
    output-var-3 como CHARACTER.
    */

    CASE HANDLE-CAMPO:name:
        WHEN "" THEN .
    END CASE.
    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE recoge-parametros V-table-Win 
PROCEDURE recoge-parametros :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    /*
    Variables a usar:
    input-var-1 como CHARACTER
    input-var-2 como CHARACTER
    input-var-3 como CHARACTER.
    */

    CASE HANDLE-CAMPO:name:
        WHEN "" THEN .
        /*
            ASSIGN
                input-para-1 = ""
                input-para-2 = ""
                input-para-3 = "".
         */      
    END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records V-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "CcbCDocu"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed V-table-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

  IF p-state = 'update-begin':U THEN DO:
     RUN valida-update.
     IF RETURN-VALUE = "ADM-ERROR" THEN RETURN.
  END.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
      {src/adm/template/vstates.i}
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valida V-table-Win 
PROCEDURE valida :
/*------------------------------------------------------------------------------
  Purpose:     Validacion de datos
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME} :
   /* IF CAMPO:SCREEN-VALUE = "" THEN DO:
         MESSAGE "Campo no debe ser blanco"
         VIEW-AS ALERT-BOX ERROR.
         APPLY "ENTRY" TO CAMPO.
         RETURN "ADM-ERROR".   
   
      END.
   */

END.

RETURN "OK".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valida-update V-table-Win 
PROCEDURE valida-update :
/*------------------------------------------------------------------------------
  Purpose:     Rutina de validacion en caso de modificacion
  Parameters:  Regresar "ADM-ERROR" si no se quiere modificar
  Notes:       
------------------------------------------------------------------------------*/

RETURN "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

