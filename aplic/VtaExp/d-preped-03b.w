&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          integral         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME gDialog
{adecomm/appserv.i}


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE CDOCU NO-UNDO LIKE CcbCDocu.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS gDialog 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrdlg.w - ADM2 SmartDialog Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
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

{src/adm2/widgetprto.i}

DEF INPUT PARAMETER pCodCli AS CHAR.

DEF SHARED VAR s-codcia AS INT.
DEF SHARED VAR s-coddiv AS CHAR.

DEF VAR x-moneda AS CHAR FORMAT 'x(3)'.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME gDialog
&Scoped-define BROWSE-NAME BROWSE-2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES CDOCU

/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 CDOCU.FchDoc CDOCU.CodDoc ~
CDOCU.NroDoc _Moneda() @ x-Moneda CDOCU.ImpTot CDOCU.SdoAct 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2 
&Scoped-define QUERY-STRING-BROWSE-2 FOR EACH CDOCU NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-2 OPEN QUERY BROWSE-2 FOR EACH CDOCU NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-2 CDOCU
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 CDOCU


/* Definitions for DIALOG-BOX gDialog                                   */
&Scoped-define OPEN-BROWSERS-IN-QUERY-gDialog ~
    ~{&OPEN-QUERY-BROWSE-2}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-2 Btn_Cancel 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "SmartDialogCues" gDialog _INLINE
/* Actions: adecomm/_so-cue.w ? adecomm/_so-cued.p ? adecomm/_so-cuew.p */
/* SmartDialog,ab,49267
Destroy on next read */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD _Moneda gDialog 
FUNCTION _Moneda RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "SALIR" 
     SIZE 15 BY 1.15.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-2 FOR 
      CDOCU SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 gDialog _STRUCTURED
  QUERY BROWSE-2 NO-LOCK DISPLAY
      CDOCU.FchDoc FORMAT "99/99/9999":U
      CDOCU.CodDoc FORMAT "x(3)":U
      CDOCU.NroDoc FORMAT "X(13)":U WIDTH 13
      _Moneda() @ x-Moneda COLUMN-LABEL "Moneda" FORMAT "x(3)":U
      CDOCU.ImpTot FORMAT "->>,>>>,>>9.99":U
      CDOCU.SdoAct COLUMN-LABEL "Saldo Actual" FORMAT "->>,>>>,>>9.99":U
            WIDTH 9.43
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 59 BY 9.42
         FONT 4 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME gDialog
     BROWSE-2 AT ROW 1.27 COL 2 WIDGET-ID 200
     Btn_Cancel AT ROW 10.96 COL 2
     SPACE(48.13) SKIP(0.17)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 4
         TITLE "SALDO DE COBRANZAS Y PEDIDOS EN TRAMITE"
         CANCEL-BUTTON Btn_Cancel WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target
   Other Settings: COMPILE APPSERVER
   Temp-Tables and Buffers:
      TABLE: CDOCU T "?" NO-UNDO INTEGRAL CcbCDocu
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB gDialog 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX gDialog
   FRAME-NAME                                                           */
/* BROWSE-TAB BROWSE-2 1 gDialog */
ASSIGN 
       FRAME gDialog:SCROLLABLE       = FALSE
       FRAME gDialog:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-2
/* Query rebuild information for BROWSE BROWSE-2
     _TblList          = "Temp-Tables.CDOCU"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   = Temp-Tables.CDOCU.FchDoc
     _FldNameList[2]   = Temp-Tables.CDOCU.CodDoc
     _FldNameList[3]   > Temp-Tables.CDOCU.NroDoc
"NroDoc" ? "X(13)" "character" ? ? ? ? ? ? no ? no no "13" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > "_<CALC>"
"_Moneda() @ x-Moneda" "Moneda" "x(3)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   = Temp-Tables.CDOCU.ImpTot
     _FldNameList[6]   > Temp-Tables.CDOCU.SdoAct
"SdoAct" "Saldo Actual" ? "decimal" ? ? ? ? ? ? no ? no no "9.43" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BROWSE-2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX gDialog
/* Query rebuild information for DIALOG-BOX gDialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX gDialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME gDialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gDialog gDialog
ON WINDOW-CLOSE OF FRAME gDialog /* SALDO DE COBRANZAS Y PEDIDOS EN TRAMITE */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-2
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK gDialog 


/* ***************************  Main Block  *************************** */

{src/adm2/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects gDialog  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Carga-Temporal gDialog 
PROCEDURE Carga-Temporal :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FOR EACH Ccbcdocu NO-LOCK WHERE Ccbcdocu.codcia = s-codcia
    AND Ccbcdocu.coddiv = s-coddiv
    AND Ccbcdocu.codcli = pCodCli
    AND Ccbcdocu.flgest = 'P'
    AND LOOKUP(Ccbcdocu.coddoc, 'FAC,BOL,LET') > 0:
    CREATE CDOCU.
    BUFFER-COPY Ccbcdocu TO CDOCU.
END.


   /* PEDIDOS PENDIENTES */
   FOR EACH Faccpedi NO-LOCK WHERE Faccpedi.codcia = s-codcia
       AND Faccpedi.coddiv = s-coddiv
       AND Faccpedi.coddoc = 'PED'
       AND Faccpedi.codcli = pCodCli
       AND Faccpedi.flgest = 'P':
       CREATE CDOCU.
       ASSIGN
           CDOCU.CodDoc = Faccpedi.coddoc
           CDOCU.FchDoc = Faccpedi.fchped
           CDOCU.ImpTot = Faccpedi.imptot
           CDOCU.NroDoc = Faccpedi.nroped
           CDOCU.SdoAct = Faccpedi.imptot.
   END.
    /* PRE-PEDIDOS PENDIENTES */
   FOR EACH Faccpedi NO-LOCK WHERE Faccpedi.codcia = s-codcia
       AND Faccpedi.coddiv = s-coddiv
       AND Faccpedi.coddoc = 'PPD'
       AND Faccpedi.codcli = pCodCli
       AND Faccpedi.flgest = 'P':
       CREATE CDOCU.
       ASSIGN
           CDOCU.CodDoc = Faccpedi.coddoc
           CDOCU.FchDoc = Faccpedi.fchped
           CDOCU.ImpTot = Faccpedi.imptot
           CDOCU.NroDoc = Faccpedi.nroped
           CDOCU.SdoAct = Faccpedi.imptot.
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI gDialog  _DEFAULT-DISABLE
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
  HIDE FRAME gDialog.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI gDialog  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  ENABLE BROWSE-2 Btn_Cancel 
      WITH FRAME gDialog.
  VIEW FRAME gDialog.
  {&OPEN-BROWSERS-IN-QUERY-gDialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject gDialog 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  RUN Carga-Temporal.

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION _Moneda gDialog 
FUNCTION _Moneda RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

IF cdocu.codmon = 1 THEN RETURN 'S/.'.
IF cdocu.codmon = 2 THEN RETURN 'US$'.

  RETURN "�?".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
