&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          integral         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrdlg.w - ADM SmartDialog Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
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
DEFINE INPUT PARAMETER C-CODDOC AS CHAR.
DEFINE INPUT PARAMETER C-NRODOC AS CHAR.
DEFINE INPUT PARAMETER C-NROPED AS CHAR.
DEFINE INPUT PARAMETER C-SDOACT AS DECIMAL.
DEFINE INPUT PARAMETER C-IMPTOT AS DECIMAL.
DEFINE INPUT PARAMETER C-NROGUI AS CHAR.

/* --- Local Variable Definitions ---   */
/*DEFINE NEW SHARED TEMP-TABLE ITEM LIKE FacDPedm.
DEFINE NEW SHARED VARIABLE S-CODDOC   AS CHAR.*/
DEFINE NEW SHARED VARIABLE S-CODCLI   AS CHAR.
DEFINE NEW SHARED VARIABLE lh_Handle  AS HANDLE.
DEFINE NEW SHARED VARIABLE S-CODMON   AS INTEGER INITIAL 1.
DEFINE NEW SHARED VARIABLE S-CODALM   AS CHAR.
DEFINE     SHARED VARIABLE S-CODCIA   AS INTEGER.
DEFINE     SHARED VARIABLE S-CODDIV   AS CHAR.
DEFINE NEW SHARED VARIABLE S-CODVEN   AS CHAR.
DEFINE     SHARED VARIABLE S-USER-ID  AS CHAR.
DEFINE VAR X-MON AS CHAR INIT "".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME D-Dialog
&Scoped-define BROWSE-NAME BROWSE-3

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES CcbDCaja

/* Definitions for BROWSE BROWSE-3                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-3 CcbDCaja.CodDoc CcbDCaja.NroDoc ~
CcbDCaja.CodRef CcbDCaja.NroRef CcbDCaja.FchDoc X-MON @ X-MON ~
CcbDCaja.ImpTot 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-3 
&Scoped-define FIELD-PAIRS-IN-QUERY-BROWSE-3
&Scoped-define OPEN-QUERY-BROWSE-3 OPEN QUERY BROWSE-3 FOR EACH CcbDCaja ~
      WHERE CcbDCaja.CodCia = S-CODCIA ~
 AND CcbDCaja.CodRef = C-CODDOC ~
 AND CcbDCaja.NroRef = C-NRODOC NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BROWSE-3 CcbDCaja
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-3 CcbDCaja


/* Definitions for DIALOG-BOX D-Dialog                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-D-Dialog ~
    ~{&OPEN-QUERY-BROWSE-3}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-20 BROWSE-3 EDITOR-2 
&Scoped-Define DISPLAYED-OBJECTS w-total w-acuen w-saldo w-pedido EDITOR-2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE VARIABLE EDITOR-2 AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 31 BY 2.04
     BGCOLOR 15 FGCOLOR 4  NO-UNDO.

DEFINE VARIABLE w-acuen AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 9.57 BY .62 NO-UNDO.

DEFINE VARIABLE w-pedido AS CHARACTER FORMAT "XXX-XXXXXXXX":U 
     VIEW-AS FILL-IN 
     SIZE 11.14 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE w-saldo AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 9.57 BY .62 NO-UNDO.

DEFINE VARIABLE w-total AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 9.57 BY .62 NO-UNDO.

DEFINE RECTANGLE RECT-20
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 55.29 BY 3.19.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-3 FOR 
      CcbDCaja SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-3 D-Dialog _STRUCTURED
  QUERY BROWSE-3 DISPLAY
      CcbDCaja.CodDoc COLUMN-LABEL "Tipo" FORMAT "x(5)"
      CcbDCaja.NroDoc COLUMN-LABEL "Recibo No." FORMAT "XXX-XXXXXXXX"
      CcbDCaja.CodRef COLUMN-LABEL "Doc." FORMAT "x(5)"
      CcbDCaja.NroRef COLUMN-LABEL "Docmto No." FORMAT "XXX-XXXXXXXX"
      CcbDCaja.FchDoc COLUMN-LABEL "     Fecha     !    Emisi�n"
      X-MON @ X-MON COLUMN-LABEL "Mon." FORMAT "XXX"
      CcbDCaja.ImpTot
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 54.86 BY 5.19
         BGCOLOR 15 FONT 4
         TITLE BGCOLOR 15 "==>  Ingreso a Caja".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     BROWSE-3 AT ROW 4.31 COL 1
     w-total AT ROW 1.27 COL 20 RIGHT-ALIGNED NO-LABEL
     w-acuen AT ROW 1.96 COL 20 RIGHT-ALIGNED NO-LABEL
     w-saldo AT ROW 2.62 COL 20 RIGHT-ALIGNED NO-LABEL
     w-pedido AT ROW 3.42 COL 9.43 COLON-ALIGNED NO-LABEL
     EDITOR-2 AT ROW 2.04 COL 24.29 NO-LABEL
     "Saldo" VIEW-AS TEXT
          SIZE 5.57 BY .54 AT ROW 2.69 COL 2.43
          FONT 6
     "Gu�as No." VIEW-AS TEXT
          SIZE 8.86 BY .62 AT ROW 1.35 COL 24.72
          FONT 6
     RECT-20 AT ROW 1.04 COL 1
     "Total" VIEW-AS TEXT
          SIZE 5.57 BY .5 AT ROW 1.31 COL 2
          FONT 6
     "A Cuenta" VIEW-AS TEXT
          SIZE 8 BY .5 AT ROW 2 COL 2.29
          FONT 6
     "Pedido No." VIEW-AS TEXT
          SIZE 9.57 BY .65 AT ROW 3.38 COL 1.72
          FONT 6
     SPACE(45.00) SKIP(5.54)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 4
         TITLE "==>  Detalle Saldos".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX D-Dialog
                                                                        */
/* BROWSE-TAB BROWSE-3 TEXT-4 D-Dialog */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

ASSIGN 
       EDITOR-2:READ-ONLY IN FRAME D-Dialog        = TRUE.

/* SETTINGS FOR FILL-IN w-acuen IN FRAME D-Dialog
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN w-pedido IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN w-saldo IN FRAME D-Dialog
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN w-total IN FRAME D-Dialog
   NO-ENABLE ALIGN-R                                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-3
/* Query rebuild information for BROWSE BROWSE-3
     _TblList          = "integral.CcbDCaja"
     _Where[1]         = "integral.CcbDCaja.CodCia = S-CODCIA
 AND integral.CcbDCaja.CodRef = C-CODDOC
 AND integral.CcbDCaja.NroRef = C-NRODOC"
     _FldNameList[1]   > integral.CcbDCaja.CodDoc
"CcbDCaja.CodDoc" "Tipo" "x(5)" "character" ? ? ? ? ? ? no ?
     _FldNameList[2]   > integral.CcbDCaja.NroDoc
"CcbDCaja.NroDoc" "Recibo No." "XXX-XXXXXXXX" "character" ? ? ? ? ? ? no ?
     _FldNameList[3]   > integral.CcbDCaja.CodRef
"CcbDCaja.CodRef" "Doc." "x(5)" "character" ? ? ? ? ? ? no ?
     _FldNameList[4]   > integral.CcbDCaja.NroRef
"CcbDCaja.NroRef" "Docmto No." "XXX-XXXXXXXX" "character" ? ? ? ? ? ? no ?
     _FldNameList[5]   > integral.CcbDCaja.FchDoc
"CcbDCaja.FchDoc" "     Fecha     !    Emisi�n" ? "date" ? ? ? ? ? ? no ?
     _FldNameList[6]   > "_<CALC>"
"X-MON @ X-MON" "Mon." "XXX" ? ? ? ? ? ? ? no ?
     _FldNameList[7]   = integral.CcbDCaja.ImpTot
     _Query            is OPENED
*/  /* BROWSE BROWSE-3 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX D-Dialog */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB D-Dialog 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* ==>  Detalle Saldos */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-3
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */

ASSIGN
     w-total   = C-IMPTOT
     w-acuen   = C-IMPTOT - C-SDOACT
     w-saldo   = C-SDOACT
     w-pedido  = C-NROPED
     EDITOR-2  = C-NROGUI.

ON FIND OF CcbDCaja
DO:
    IF CcbDCaja.CodMon = 1 THEN
        ASSIGN
            X-MON = "S/." .
    ELSE
        ASSIGN
            X-MON = "US$" .
END.            
{src/adm/template/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects D-Dialog _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available D-Dialog _ADM-ROW-AVAILABLE
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

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI D-Dialog _DEFAULT-DISABLE
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
  HIDE FRAME D-Dialog.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI D-Dialog _DEFAULT-ENABLE
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
  DISPLAY w-total w-acuen w-saldo w-pedido EDITOR-2 
      WITH FRAME D-Dialog.
  ENABLE RECT-20 BROWSE-3 EDITOR-2 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records D-Dialog _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "CcbDCaja"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed D-Dialog 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


