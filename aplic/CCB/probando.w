&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
/*********************************************************************
* Copyright (C) 2000 by Progress Software Corporation ("PSC"),       *
* 14 Oak Park, Bedford, MA 01730, and other contributors as listed   *
* below.  All Rights Reserved.                                       *
*                                                                    *
* The Initial Developer of the Original Code is PSC.  The Original   *
* Code is Progress IDE code released to open source December 1, 2000.*
*                                                                    *
* The contents of this file are subject to the Possenet Public       *
* License Version 1.0 (the "License"); you may not use this file     *
* except in compliance with the License.  A copy of the License is   *
* available as of the date of this notice at                         *
* http://www.possenet.org/license.html                               *
*                                                                    *
* Software distributed under the License is distributed on an "AS IS"*
* basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. You*
* should refer to the License for the specific language governing    *
* rights and limitations under the License.                          *
*                                                                    *
* Contributors:                                                      *
*                                                                    *
*********************************************************************/
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: 
          
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
DEF SHARED VAR s-codcia AS INT.
DEF SHARED VAR s-coddiv AS CHAR.
DEF SHARED VAR cl-codcia AS INT.


/* Local Variable Definitions ---                                       */

DEFINE TEMP-TABLE tabla
    FIELDS t-codcli LIKE gn-clie.codcli
    FIELDS t-coddoc LIKE faccpedi.coddoc
    FIELDS t-nrocot LIKE faccpedi.nroped
    FIELDS t-totcot AS DECIMAL FORMAT "->>>>>>>>>9.99"
    FIELDS t-codfac LIKE ccbcdocu.coddoc
    FIELDS t-nrofac LIKE ccbcdocu.nrodoc
    FIELDS t-totfac AS DECIMAL FORMAT "->>>>>>>>>9.99"
    FIELDS t-totlet AS DECIMAL FORMAT "->>>>>>>>>9.99".

DEFINE BUFFER b-faccpedi FOR faccpedi.
DEFINE VARIABLE cDivi AS CHARACTER NO-UNDO INIT "00000".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-27 BUTTON-6 f-Desde f-Hasta BUTTON-1 ~
BUTTON-5 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-Division f-Desde f-Hasta txt-msj 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-1 
     IMAGE-UP FILE "img/excel.bmp":U
     LABEL "Button 1" 
     SIZE 7 BY 1.5.

DEFINE BUTTON BUTTON-5 
     IMAGE-UP FILE "img/exit.ico":U
     LABEL "Button 5" 
     SIZE 7 BY 1.5.

DEFINE BUTTON BUTTON-6 
     LABEL "..." 
     SIZE 3 BY .77.

DEFINE VARIABLE f-Desde AS DATE FORMAT "99/99/9999":U 
     LABEL "Desde" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .81 NO-UNDO.

DEFINE VARIABLE f-Hasta AS DATE FORMAT "99/99/9999":U 
     LABEL "Hasta" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .81 NO-UNDO.

DEFINE VARIABLE FILL-IN-Division AS CHARACTER FORMAT "X(60)":U INITIAL "00000" 
     LABEL "Divisi�n" 
     VIEW-AS FILL-IN 
     SIZE 43 BY .81
     BGCOLOR 15 FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE txt-msj AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 64 BY .81
     BGCOLOR 1 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-27
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 66 BY 6.19.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     BUTTON-6 AT ROW 1.81 COL 55 WIDGET-ID 14
     FILL-IN-Division AT ROW 1.88 COL 9 COLON-ALIGNED WIDGET-ID 16
     f-Desde AT ROW 2.88 COL 9 COLON-ALIGNED WIDGET-ID 4
     f-Hasta AT ROW 2.88 COL 25 COLON-ALIGNED WIDGET-ID 6
     BUTTON-1 AT ROW 4.77 COL 53 WIDGET-ID 8
     BUTTON-5 AT ROW 4.77 COL 60 WIDGET-ID 10
     txt-msj AT ROW 6.38 COL 2.86 NO-LABEL WIDGET-ID 12
     RECT-27 AT ROW 1.27 COL 2 WIDGET-ID 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 68.57 BY 6.77
         FONT 1 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Letras Emitidas por Cliente"
         HEIGHT             = 6.77
         WIDTH              = 68.57
         MAX-HEIGHT         = 6.77
         MAX-WIDTH          = 68.57
         VIRTUAL-HEIGHT     = 6.77
         VIRTUAL-WIDTH      = 68.57
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN FILL-IN-Division IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN txt-msj IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Letras Emitidas por Cliente */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Letras Emitidas por Cliente */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 W-Win
ON CHOOSE OF BUTTON-1 IN FRAME F-Main /* Button 1 */
DO:
  ASSIGN f-Desde f-Hasta.
  DISPLAY 'Cargando Informaci�n....' @ txt-msj WITH FRAME {&FRAME-NAME}.
  RUN Excel.
  DISPLAY '' @ txt-msj WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-5 W-Win
ON CHOOSE OF BUTTON-5 IN FRAME F-Main /* Button 5 */
DO:
    APPLY "CLOSE":U TO THIS-PROCEDURE.
    RETURN NO-APPLY.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-6 W-Win
ON CHOOSE OF BUTTON-6 IN FRAME F-Main /* ... */
DO:

    DEF VAR x-Divisiones AS CHAR.
    x-Divisiones = FILL-IN-Division:SCREEN-VALUE.    
    RUN vta/d-repo06 (INPUT-OUTPUT x-Divisiones).
    FILL-IN-Division:SCREEN-VALUE = x-Divisiones.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-Division
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-Division W-Win
ON LEAVE OF FILL-IN-Division IN FRAME F-Main /* Divisi�n */
DO:
    ASSIGN FILL-IN-Division.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects W-Win  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available W-Win  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Carga-Data W-Win 
PROCEDURE Carga-Data :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FOR EACH ccbcdocu WHERE ccbcdocu.codcia = s-codcia
    AND LOOKUP(ccbcdocu.coddiv,"00000,00015") > 0
    AND ccbcdocu.coddoc = "LET"
    AND ccbcdocu.fchdoc >= f-Desde
    AND ccbcdocu.fchdoc <= f-Hasta
    AND ccbcdocu.flgest = 'P' NO-LOCK:
    FIND FIRST tabla WHERE t-codcli = ccbcdocu.codcli NO-LOCK NO-ERROR.
    IF NOT AVAIL tabla THEN DO:
        CREATE tabla.
        ASSIGN t-codcli = ccbcdocu.codcli.
    END.
    t-totlet = t-totlet + ccbcdocu.imptot.
END.

FOR EACH tabla NO-LOCK:
    /*Carga Cotizacion y Facturado*/
    FOR EACH gn-divi WHERE gn-divi.codcia = s-codcia
        AND LOOKUP(gn-divi.CodDiv,cDivi) > 0 NO-LOCK:
        FOR EACH faccpedi WHERE faccpedi.codcia = s-codcia
            AND faccpedi.coddiv = gn-divi.coddiv
            AND faccpedi.coddoc = "COT"
            AND faccpedi.codcli = t-codcli
            AND faccpedi.fchped >= 11/01/09        
            AND faccpedi.fchped <= f-hasta        
            AND LOOKUP(TRIM(facCPedi.FmaPgo),"102,103,104") > 0
            AND faccpedi.flgest <> 'A'
            AND lookup(trim(faccpedi.codven),'015,173') > 0 NO-LOCK:

            ASSIGN t-totcot = t-totcot + faccpedi.imptot.
            /*Busca Pedidos*/
            FOR EACH b-faccpedi WHERE b-faccpedi.codcia = faccpedi.codcia
                AND b-faccpedi.coddiv = faccpedi.coddiv
                AND b-faccpedi.coddoc = "PED"
                AND b-faccpedi.flgest <> 'A'
                AND b-faccpedi.nroref = faccpedi.nroped NO-LOCK:
                /*Busca Facturas*/
                FOR EACH ccbcdocu WHERE ccbcdocu.codcia = faccpedi.codcia
                    AND ccbcdocu.coddiv = faccpedi.coddiv
                    AND LOOKUP(ccbcdocu.coddoc,'FAC,BOL,TCK,N/C') > 0
                    AND ccbcdocu.fchdoc >= 11/01/2009
                    AND ccbcdocu.fchdoc <= f-hasta
                    AND ccbcdocu.flgest <> 'A'
                    AND CcbCDocu.Nroped = b-faccpedi.nroped NO-LOCK:
                    ASSIGN t-totfac = t-totfac + ccbcdocu.imptot.
                END.
            END.
        END.    
    END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
  THEN DELETE WIDGET W-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-Win  _DEFAULT-ENABLE
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
  DISPLAY FILL-IN-Division f-Desde f-Hasta txt-msj 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE RECT-27 BUTTON-6 f-Desde f-Hasta BUTTON-1 BUTTON-5 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Excel W-Win 
PROCEDURE Excel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE VARIABLE chExcelApplication      AS COM-HANDLE.
DEFINE VARIABLE chWorkbook              AS COM-HANDLE.
DEFINE VARIABLE chWorksheet             AS COM-HANDLE.
DEFINE VARIABLE chChart                 AS COM-HANDLE.
DEFINE VARIABLE chWorksheetRange        AS COM-HANDLE.
DEFINE VARIABLE iCount                  AS INTEGER init 1.
DEFINE VARIABLE iIndex                  AS INTEGER.
DEFINE VARIABLE cColumn                 AS CHARACTER.
DEFINE VARIABLE cRange                  AS CHARACTER.
DEFINE VARIABLE t-Column                AS INTEGER INIT 15.
DEFINE VARIABLE i-Column                AS INTEGER NO-UNDO.
DEFINE VARIABLE j-Column                AS INTEGER NO-UNDO.

/* create a new Excel Application object */
CREATE "Excel.Application" chExcelApplication.

/* create a new Workbook */
chWorkbook = chExcelApplication:Workbooks:Add("C:\PRG\Templates\q fue\prueba.xlt").

/* get the active Worksheet */
chWorkSheet = chExcelApplication:Sheets:Item(1).

/* set the column names for the Worksheet */
chWorkSheet:Range("A13"):Value = "LETRAS POR CLIENTE".

/*Formato*/
chWorkSheet:Columns("A"):NumberFormat = "@".

t-Column = t-Column + 1.
cColumn = STRING(t-Column).
cRange = "A" + cColumn.
chWorkSheet:Range(cRange):Value = "Codigo".
cRange = "B" + cColumn.
chWorkSheet:Range(cRange):Value = "Apellidos y Nombres".
cRange = "C" + cColumn.
chWorkSheet:Range(cRange):Value = "Total Cotizado".
cRange = "D" + cColumn.
chWorkSheet:Range(cRange):Value = "Total Facturado".
cRange = "E" + cColumn.
chWorkSheet:Range(cRange):Value = "Tot Letras".

cDivi = FILL-IN-Division:SCREEN-VALUE IN FRAME {&FRAME-NAME}.
RUN Carga-Data.

FOR EACH tabla NO-LOCK:
    FIND FIRST gn-clie WHERE gn-clie.codcia = cl-codcia
        AND gn-clie.codcli = t-codcli NO-LOCK NO-ERROR.

    t-Column = t-Column + 1.
    cColumn = STRING(t-Column).
    cRange = "A" + cColumn.
    chWorkSheet:Range(cRange):Value = t-codcli.
    IF AVAIL gn-clie THEN DO:
        cRange = "B" + cColumn.
        chWorkSheet:Range(cRange):Value = gn-clie.nomcli.
    END.
    ELSE DO:
        cRange = "B" + cColumn.
        chWorkSheet:Range(cRange):Value = ''.
    END.
    cRange = "C" + cColumn.
    chWorkSheet:Range(cRange):Value = t-totcot.
    cRange = "D" + cColumn.
    chWorkSheet:Range(cRange):Value = t-totfac.
    cRange = "E" + cColumn.
    chWorkSheet:Range(cRange):Value = t-totlet.

END.

MESSAGE 'Proceso Terminado!!!'.

/* launch Excel so it is visible to the user */
 chExcelApplication:Visible = TRUE.

/* release com-handles */
RELEASE OBJECT chExcelApplication.      
RELEASE OBJECT chWorkbook.
RELEASE OBJECT chWorksheet.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit W-Win 
PROCEDURE local-exit :
/* -----------------------------------------------------------
  Purpose:  Starts an "exit" by APPLYing CLOSE event, which starts "destroy".
  Parameters:  <none>
  Notes:    If activated, should APPLY CLOSE, *not* dispatch adm-exit.   
-------------------------------------------------------------*/
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   
   RETURN.
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize W-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
    ASSIGN 
        f-Desde = TODAY
        f-Hasta = TODAY.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records W-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartWindow, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed W-Win 
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
