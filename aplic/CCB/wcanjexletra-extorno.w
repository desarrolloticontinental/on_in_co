&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          integral         PROGRESS
*/
&Scoped-define WINDOW-NAME W-Win


/* Temp-Table and Buffer definitions                                    */
DEFINE NEW SHARED TEMP-TABLE DOCU LIKE CcbCDocu.
DEFINE NEW SHARED TEMP-TABLE MVTO LIKE CcbDMvto.



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

/* Local Variable Definitions ---                                       */

DEF NEW SHARED VAR s-coddiv AS CHAR.
DEFINE NEW SHARED VARIABLE lh_handle  AS HANDLE.
DEFINE NEW SHARED VAR s-coddoc AS CHAR.
DEFINE NEW SHARED VAR s-nroser AS INT.

/* Local Variable Definitions ---                                       */
DEF SHARED VAR s-codcia AS INT.

s-CodDoc = "CJE".

DEFINE NEW SHARED VARIABLE S-CODCLI   AS CHAR. 
DEFINE NEW SHARED VARIABLE S-CODMON   AS INTEGER.
DEFINE NEW SHARED VARIABLE S-TPOCMB   AS DECIMAL.
DEFINE NEW SHARED VARIABLE S-CONDIC   AS CHAR.

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
&Scoped-Define ENABLED-OBJECTS RECT-25 COMBO-NroSer BUTTON-Extornar 
&Scoped-Define DISPLAYED-OBJECTS COMBO-NroSer 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_b-division-canje AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bcanjexletra-01 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bcanjexletra-02 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-navico AS HANDLE NO-UNDO.
DEFINE VARIABLE h_qcanjexletra-extorno AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vcanjexletra AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-Extornar 
     LABEL "EXTORNAR CANJE APROBADO" 
     SIZE 25 BY 1.12.

DEFINE VARIABLE COMBO-NroSer AS CHARACTER FORMAT "X(3)":U INITIAL "0" 
     LABEL "Serie" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     DROP-DOWN-LIST
     SIZE 6.72 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-25
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 104 BY 3.65.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     COMBO-NroSer AT ROW 6.65 COL 64.71 WIDGET-ID 6
     BUTTON-Extornar AT ROW 11.77 COL 88 WIDGET-ID 8
     RECT-25 AT ROW 7.92 COL 2 WIDGET-ID 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 114 BY 25.46
         FONT 4 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: DOCU T "NEW SHARED" ? INTEGRAL CcbCDocu
      TABLE: MVTO T "NEW SHARED" ? INTEGRAL CcbDMvto
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "EXTORNO CANJE POR LETRA"
         HEIGHT             = 25.46
         WIDTH              = 114
         MAX-HEIGHT         = 29.54
         MAX-WIDTH          = 144.29
         VIRTUAL-HEIGHT     = 29.54
         VIRTUAL-WIDTH      = 144.29
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
/* SETTINGS FOR COMBO-BOX COMBO-NroSer IN FRAME F-Main
   ALIGN-L                                                              */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* EXTORNO CANJE POR LETRA */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* EXTORNO CANJE POR LETRA */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Extornar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Extornar W-Win
ON CHOOSE OF BUTTON-Extornar IN FRAME F-Main /* EXTORNAR CANJE APROBADO */
DO:
  RUN Extorno-de-canje IN h_vcanjexletra.
  RUN dispatch IN h_qcanjexletra-extorno ('open-query':U).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME COMBO-NroSer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL COMBO-NroSer W-Win
ON VALUE-CHANGED OF COMBO-NroSer IN FRAME F-Main /* Serie */
DO:
    ASSIGN COMBO-NroSer.
    s-NroSer = INTEGER(COMBO-NroSer).
    RUN dispatch IN h_qcanjexletra-extorno ('open-query':U).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */
lh_handle = THIS-PROCEDURE.

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
  DEFINE VARIABLE adm-current-page  AS INTEGER NO-UNDO.

  RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
  ASSIGN adm-current-page = INTEGER(RETURN-VALUE).

  CASE adm-current-page: 

    WHEN 0 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'aplic/ccb/b-division-canje.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-division-canje ).
       RUN set-position IN h_b-division-canje ( 1.27 , 2.00 ) NO-ERROR.
       RUN set-size IN h_b-division-canje ( 5.38 , 66.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'src/adm/objects/p-navico.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Right':U ,
             OUTPUT h_p-navico ).
       RUN set-position IN h_p-navico ( 6.12 , 88.00 ) NO-ERROR.
       RUN set-size IN h_p-navico ( 1.77 , 18.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'aplic/ccb/vcanjexletra.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_vcanjexletra ).
       RUN set-position IN h_vcanjexletra ( 8.00 , 3.00 ) NO-ERROR.
       /* Size in UIB:  ( 3.31 , 102.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'aplic/ccb/bcanjexletra-01.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_bcanjexletra-01 ).
       RUN set-position IN h_bcanjexletra-01 ( 11.77 , 2.00 ) NO-ERROR.
       RUN set-size IN h_bcanjexletra-01 ( 6.38 , 85.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'aplic/ccb/bcanjexletra-02.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_bcanjexletra-02 ).
       RUN set-position IN h_bcanjexletra-02 ( 18.23 , 2.00 ) NO-ERROR.
       RUN set-size IN h_bcanjexletra-02 ( 7.92 , 62.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'aplic/ccb/qcanjexletra-extorno.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'SortBy-Case = NroDoc':U ,
             OUTPUT h_qcanjexletra-extorno ).
       RUN set-position IN h_qcanjexletra-extorno ( 6.38 , 76.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.35 , 12.00 ) */

       /* Links to SmartViewer h_vcanjexletra. */
       RUN add-link IN adm-broker-hdl ( h_qcanjexletra-extorno , 'Record':U , h_vcanjexletra ).

       /* Links to SmartBrowser h_bcanjexletra-01. */
       RUN add-link IN adm-broker-hdl ( h_qcanjexletra-extorno , 'Record':U , h_bcanjexletra-01 ).

       /* Links to SmartBrowser h_bcanjexletra-02. */
       RUN add-link IN adm-broker-hdl ( h_qcanjexletra-extorno , 'Record':U , h_bcanjexletra-02 ).

       /* Links to SmartQuery h_qcanjexletra-extorno. */
       RUN add-link IN adm-broker-hdl ( h_b-division-canje , 'Record':U , h_qcanjexletra-extorno ).
       RUN add-link IN adm-broker-hdl ( h_p-navico , 'Navigation':U , h_qcanjexletra-extorno ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-division-canje ,
             COMBO-NroSer:HANDLE IN FRAME F-Main , 'BEFORE':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-navico ,
             h_b-division-canje , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_vcanjexletra ,
             COMBO-NroSer:HANDLE IN FRAME F-Main , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_bcanjexletra-01 ,
             h_vcanjexletra , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_bcanjexletra-02 ,
             BUTTON-Extornar:HANDLE IN FRAME F-Main , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_qcanjexletra-extorno ,
             h_bcanjexletra-02 , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.

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
  DISPLAY COMBO-NroSer 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE RECT-25 COMBO-NroSer BUTTON-Extornar 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW W-Win.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Numero-Serie W-Win 
PROCEDURE Numero-Serie :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE VARIABLE cListItems AS CHARACTER NO-UNDO.

  FIND FacCfgGn WHERE FacCfgGn.CodCia = s-CodCia NO-LOCK NO-ERROR.
  FOR EACH FacCorre NO-LOCK WHERE 
      FacCorre.CodCia = s-CodCia AND
      FacCorre.CodDoc = s-CodDoc AND
      FacCorre.CodDiv = s-CodDiv:
      IF cListItems = "" THEN cListItems = STRING(FacCorre.NroSer,"999").
      ELSE cListItems = cListItems + "," + STRING(FacCorre.NroSer,"999").
  END.
  DO WITH FRAME {&FRAME-NAME}:
      COMBO-NroSer:LIST-ITEMS = cListItems.
      COMBO-NroSer = ENTRY(1,COMBO-NroSer:LIST-ITEMS).
      s-NroSer = INTEGER(COMBO-NroSer).
      DISPLAY COMBO-NroSer.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Procesa-Handle W-Win 
PROCEDURE Procesa-Handle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER L-Handle AS CHAR.

CASE L-Handle:
    WHEN "Pide-Serie" THEN DO:
         RUN Numero-Serie.
         RUN dispatch IN h_qcanjexletra-extorno ('open-query':U).
    END.
END CASE.

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
