&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          integral         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW


/* Temp-Table and Buffer definitions                                    */
DEFINE SHARED TEMP-TABLE ITEM LIKE FacDPedi.
DEFINE SHARED TEMP-TABLE T-DMatriz NO-UNDO LIKE VtaDMatriz.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
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

  Description: from BROWSER.W - Basic SmartBrowser Object Template

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
DEF SHARED VAR s-CodDiv AS CHAR.
DEF SHARED VAR s-CodCli AS CHAR.
DEF SHARED VAR s-CodMon AS INT.
DEF SHARED VAR s-TpoCmb AS DEC.
DEF SHARED VAR s-FlgSit AS CHAR.
DEF SHARED VAR s-NroDec AS INT.
DEF SHARED VAR s-CodAlm AS CHAR.
DEF SHARED VAR s-PorIgv AS DEC.

/* PRODUCTOS POR ROTACION */
DEFINE SHARED VAR s-FlgRotacion LIKE gn-divi.flgrotacion.
DEFINE SHARED VAR s-FlgEmpaque  LIKE GN-DIVI.FlgEmpaque.
DEFINE SHARED VAR s-FlgMinVenta LIKE GN-DIVI.FlgMinVenta.
DEFINE SHARED VAR s-VentaMayorista LIKE GN-DIVI.VentaMayorista.
DEFINE SHARED VAR s-FlgTipoVenta LIKE GN-DIVI.FlgPreVta.

DEFINE SHARED VARIABLE s-adm-new-record AS CHAR.
DEFINE SHARED VARIABLE S-CODDOC  AS CHAR.
DEFINE SHARED VARIABLE S-NROPED  AS CHAR.

DEFINE BUFFER B-DMatriz FOR T-DMatriz.

DEFINE VARIABLE x-Cantidad AS DEC NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br_table

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES VtaCMatriz
&Scoped-define FIRST-EXTERNAL-TABLE VtaCMatriz


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR VtaCMatriz.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES T-DMatriz

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table T-DMatriz.LabelFila ~
T-DMatriz.PreUni[1] T-DMatriz.PreUni[2] T-DMatriz.PreUni[3] ~
T-DMatriz.PreUni[4] T-DMatriz.PreUni[5] T-DMatriz.PreUni[6] ~
T-DMatriz.PreUni[7] T-DMatriz.PreUni[8] T-DMatriz.PreUni[9] ~
T-DMatriz.PreUni[10] T-DMatriz.PreUni[11] T-DMatriz.PreUni[12] 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table 
&Scoped-define QUERY-STRING-br_table FOR EACH T-DMatriz OF VtaCMatriz WHERE ~{&KEY-PHRASE} NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH T-DMatriz OF VtaCMatriz WHERE ~{&KEY-PHRASE} NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-br_table T-DMatriz
&Scoped-define FIRST-TABLE-IN-QUERY-br_table T-DMatriz


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br_table 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" B-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Advanced Query Options" B-table-Win _INLINE
/* Actions: ? adm/support/advqedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
</KEY-OBJECT>
<SORTBY-OPTIONS>
</SORTBY-OPTIONS> 
<SORTBY-RUN-CODE>
************************
* Set attributes related to SORTBY-OPTIONS */
RUN set-attribute-list (
    'SortBy-Options = ""':U).
/************************
</SORTBY-RUN-CODE> 
<FILTER-ATTRIBUTES>
</FILTER-ATTRIBUTES> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fCantidad B-table-Win 
FUNCTION fCantidad RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      T-DMatriz SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      T-DMatriz.LabelFila FORMAT "x(60)":U WIDTH 22.43
      T-DMatriz.PreUni[1] COLUMN-LABEL "1" FORMAT ">,>>9.9999":U
            WIDTH 9
      T-DMatriz.PreUni[2] COLUMN-LABEL "2" FORMAT ">,>>9.9999":U
            WIDTH 9
      T-DMatriz.PreUni[3] COLUMN-LABEL "3" FORMAT ">,>>9.9999":U
            WIDTH 9
      T-DMatriz.PreUni[4] COLUMN-LABEL "4" FORMAT ">,>>9.9999":U
            WIDTH 9
      T-DMatriz.PreUni[5] COLUMN-LABEL "5" FORMAT ">,>>9.9999":U
            WIDTH 9
      T-DMatriz.PreUni[6] COLUMN-LABEL "6" FORMAT ">,>>9.9999":U
            WIDTH 9
      T-DMatriz.PreUni[7] COLUMN-LABEL "7" FORMAT ">,>>9.9999":U
            WIDTH 9
      T-DMatriz.PreUni[8] COLUMN-LABEL "8" FORMAT ">,>>9.9999":U
            WIDTH 9
      T-DMatriz.PreUni[9] COLUMN-LABEL "9" FORMAT ">,>>9.9999":U
            WIDTH 9.57
      T-DMatriz.PreUni[10] COLUMN-LABEL "10" FORMAT ">,>>9.9999":U
            WIDTH 9
      T-DMatriz.PreUni[11] COLUMN-LABEL "1" FORMAT ">,>>9.9999":U
            WIDTH 9
      T-DMatriz.PreUni[12] COLUMN-LABEL "12" FORMAT ">,>>9.9999":U
            WIDTH 9
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 142 BY 13.27
         FONT 4.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br_table AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 4 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
   External Tables: INTEGRAL.VtaCMatriz
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY COMPILE
   Temp-Tables and Buffers:
      TABLE: ITEM T "SHARED" ? INTEGRAL FacDPedi
      TABLE: T-DMatriz T "SHARED" NO-UNDO INTEGRAL VtaDMatriz
   END-TABLES.
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
  CREATE WINDOW B-table-Win ASSIGN
         HEIGHT             = 16.31
         WIDTH              = 142.57.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm-vm/method/vmviewer.i}
{src/adm-vm/method/vmbrowser.i}
{src/adm/method/browser.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
/* BROWSE-TAB br_table 1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _TblList          = "Temp-Tables.T-DMatriz OF INTEGRAL.VtaCMatriz"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _FldNameList[1]   > Temp-Tables.T-DMatriz.LabelFila
"T-DMatriz.LabelFila" ? ? "character" ? ? ? ? ? ? no ? no no "22.43" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.T-DMatriz.PreUni[1]
"T-DMatriz.PreUni[1]" "1" ">,>>9.9999" "decimal" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.T-DMatriz.PreUni[2]
"T-DMatriz.PreUni[2]" "2" ">,>>9.9999" "decimal" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.T-DMatriz.PreUni[3]
"T-DMatriz.PreUni[3]" "3" ">,>>9.9999" "decimal" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.T-DMatriz.PreUni[4]
"T-DMatriz.PreUni[4]" "4" ">,>>9.9999" "decimal" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.T-DMatriz.PreUni[5]
"T-DMatriz.PreUni[5]" "5" ">,>>9.9999" "decimal" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > Temp-Tables.T-DMatriz.PreUni[6]
"T-DMatriz.PreUni[6]" "6" ">,>>9.9999" "decimal" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > Temp-Tables.T-DMatriz.PreUni[7]
"T-DMatriz.PreUni[7]" "7" ">,>>9.9999" "decimal" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > Temp-Tables.T-DMatriz.PreUni[8]
"T-DMatriz.PreUni[8]" "8" ">,>>9.9999" "decimal" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > Temp-Tables.T-DMatriz.PreUni[9]
"T-DMatriz.PreUni[9]" "9" ">,>>9.9999" "decimal" ? ? ? ? ? ? no ? no no "9.57" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > Temp-Tables.T-DMatriz.PreUni[10]
"T-DMatriz.PreUni[10]" "10" ">,>>9.9999" "decimal" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > Temp-Tables.T-DMatriz.PreUni[11]
"T-DMatriz.PreUni[11]" "1" ">,>>9.9999" "decimal" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > Temp-Tables.T-DMatriz.PreUni[12]
"T-DMatriz.PreUni[12]" "12" ">,>>9.9999" "decimal" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE br_table */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME br_table
&Scoped-define SELF-NAME br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-DISPLAY OF br_table IN FRAME F-Main
DO:
    IF T-DMatriz.CodMat[1] = "" THEN T-DMatriz.PreUni[1]:BGCOLOR IN BROWSE {&browse-name} = 8.
    IF T-DMatriz.CodMat[2] = "" THEN T-DMatriz.PreUni[2]:BGCOLOR IN BROWSE {&browse-name} = 8.
    IF T-DMatriz.CodMat[3] = "" THEN T-DMatriz.PreUni[3]:BGCOLOR IN BROWSE {&browse-name} = 8.
    IF T-DMatriz.CodMat[4] = "" THEN T-DMatriz.PreUni[4]:BGCOLOR IN BROWSE {&browse-name} = 8.
    IF T-DMatriz.CodMat[5] = "" THEN T-DMatriz.PreUni[5]:BGCOLOR IN BROWSE {&browse-name} = 8.
    IF T-DMatriz.CodMat[6] = "" THEN T-DMatriz.PreUni[6]:BGCOLOR IN BROWSE {&browse-name} = 8.
    IF T-DMatriz.CodMat[7] = "" THEN T-DMatriz.PreUni[7]:BGCOLOR IN BROWSE {&browse-name} = 8.
    IF T-DMatriz.CodMat[8] = "" THEN T-DMatriz.PreUni[8]:BGCOLOR IN BROWSE {&browse-name} = 8.
    IF T-DMatriz.CodMat[9] = "" THEN T-DMatriz.PreUni[9]:BGCOLOR IN BROWSE {&browse-name} = 8.
    IF T-DMatriz.CodMat[10] = "" THEN T-DMatriz.PreUni[10]:BGCOLOR IN BROWSE {&browse-name} = 8.
    IF T-DMatriz.CodMat[11] = "" THEN T-DMatriz.PreUni[11]:BGCOLOR IN BROWSE {&browse-name} = 8.
    IF T-DMatriz.CodMat[12] = "" THEN T-DMatriz.PreUni[12]:BGCOLOR IN BROWSE {&browse-name} = 8.
    IF T-DMatriz.CodMat[1] = "" THEN T-DMatriz.PreUni[1]:FGCOLOR IN BROWSE {&browse-name} = 8.
    IF T-DMatriz.CodMat[2] = "" THEN T-DMatriz.PreUni[2]:FGCOLOR IN BROWSE {&browse-name} = 8.
    IF T-DMatriz.CodMat[3] = "" THEN T-DMatriz.PreUni[3]:FGCOLOR IN BROWSE {&browse-name} = 8.
    IF T-DMatriz.CodMat[4] = "" THEN T-DMatriz.PreUni[4]:FGCOLOR IN BROWSE {&browse-name} = 8.
    IF T-DMatriz.CodMat[5] = "" THEN T-DMatriz.PreUni[5]:FGCOLOR IN BROWSE {&browse-name} = 8.
    IF T-DMatriz.CodMat[6] = "" THEN T-DMatriz.PreUni[6]:FGCOLOR IN BROWSE {&browse-name} = 8.
    IF T-DMatriz.CodMat[7] = "" THEN T-DMatriz.PreUni[7]:FGCOLOR IN BROWSE {&browse-name} = 8.
    IF T-DMatriz.CodMat[8] = "" THEN T-DMatriz.PreUni[8]:FGCOLOR IN BROWSE {&browse-name} = 8.
    IF T-DMatriz.CodMat[9] = "" THEN T-DMatriz.PreUni[9]:FGCOLOR IN BROWSE {&browse-name} = 8.
    IF T-DMatriz.CodMat[10] = "" THEN T-DMatriz.PreUni[10]:FGCOLOR IN BROWSE {&browse-name} = 8.
    IF T-DMatriz.CodMat[11] = "" THEN T-DMatriz.PreUni[11]:FGCOLOR IN BROWSE {&browse-name} = 8.
    IF T-DMatriz.CodMat[12] = "" THEN T-DMatriz.PreUni[12]:FGCOLOR IN BROWSE {&browse-name} = 8.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-ENTRY OF br_table IN FRAME F-Main
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-LEAVE OF br_table IN FRAME F-Main
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
   {src/adm/template/brsleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON VALUE-CHANGED OF br_table IN FRAME F-Main
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available B-table-Win  _ADM-ROW-AVAILABLE
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
  {src/adm/template/row-list.i "VtaCMatriz"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "VtaCMatriz"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Crea-Item B-table-Win 
PROCEDURE Crea-Item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEF INPUT PARAMETER pCodMat AS CHAR.
    DEF INPUT PARAMETER pCantidad AS DEC.
    DEF INPUT PARAMETER pUndVta AS CHAR.

    IF pCodMat = "" THEN RETURN.
    IF pCantidad <= 0 THEN RETURN.

    DEF VAR x-NroItm AS INT NO-UNDO.

    x-NroItm = 1.
    FOR EACH ITEM BY ITEM.NroItm:
        x-NroItm = ITEM.NroItm + 1.
    END.
    FIND ITEM WHERE ITEM.codmat = pCodMat NO-ERROR.
    IF NOT AVAILABLE ITEM THEN CREATE ITEM. ELSE x-NroItm = ITEM.NroItm.
    ASSIGN
        ITEM.codcia = s-codcia
        ITEM.almdes = ENTRY(1, s-codalm)
        ITEM.CodMat = pCodMat
        ITEM.CanPed = pCantidad
        ITEM.Factor = 1
        ITEM.UndVta = pUndVta
        ITEM.NroItm = x-NroItm.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI B-table-Win  _DEFAULT-DISABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Graba-Registros B-table-Win 
PROCEDURE Graba-Registros :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR pCodMat AS CHAR NO-UNDO.
DEF VAR f-Factor AS DEC NO-UNDO.
DEF VAR f-PreBas AS DEC NO-UNDO.
DEF VAR f-PreVta AS DEC NO-UNDO.
DEF VAR f-Dsctos AS DEC NO-UNDO.
DEF VAR y-Dsctos AS DEC NO-UNDO.
DEF VAR z-Dsctos AS DEC NO-UNDO.
DEF VAR x-TipDto AS CHAR NO-UNDO.

FOR EACH T-DMatriz:
    RUN Crea-Item (T-DMatriz.CodMat[1], T-DMatriz.Cantidad[1], T-DMatriz.UndVta[1]).
    RUN Crea-Item (T-DMatriz.CodMat[2], T-DMatriz.Cantidad[2], T-DMatriz.UndVta[2]).
    RUN Crea-Item (T-DMatriz.CodMat[3], T-DMatriz.Cantidad[3], T-DMatriz.UndVta[3]).
    RUN Crea-Item (T-DMatriz.CodMat[4], T-DMatriz.Cantidad[4], T-DMatriz.UndVta[4]).
    RUN Crea-Item (T-DMatriz.CodMat[5], T-DMatriz.Cantidad[5], T-DMatriz.UndVta[5]).
    RUN Crea-Item (T-DMatriz.CodMat[6], T-DMatriz.Cantidad[6], T-DMatriz.UndVta[6]).
    RUN Crea-Item (T-DMatriz.CodMat[7], T-DMatriz.Cantidad[7], T-DMatriz.UndVta[7]).
    RUN Crea-Item (T-DMatriz.CodMat[8], T-DMatriz.Cantidad[8], T-DMatriz.UndVta[8]).
    RUN Crea-Item (T-DMatriz.CodMat[9], T-DMatriz.Cantidad[9], T-DMatriz.UndVta[9]).
    RUN Crea-Item (T-DMatriz.CodMat[10], T-DMatriz.Cantidad[10], T-DMatriz.UndVta[10]).
    RUN Crea-Item (T-DMatriz.CodMat[11], T-DMatriz.Cantidad[11], T-DMatriz.UndVta[11]).
    RUN Crea-Item (T-DMatriz.CodMat[12], T-DMatriz.Cantidad[12], T-DMatriz.UndVta[12]).
END.

END PROCEDURE.

/*
DEF VAR pCodMat AS CHAR NO-UNDO.
DEF VAR f-Factor AS DEC NO-UNDO.
DEF VAR f-PreBas AS DEC NO-UNDO.
DEF VAR f-PreVta AS DEC NO-UNDO.
DEF VAR f-Dsctos AS DEC NO-UNDO.
DEF VAR y-Dsctos AS DEC NO-UNDO.
DEF VAR z-Dsctos AS DEC NO-UNDO.
DEF VAR x-TipDto AS CHAR NO-UNDO.

FOR EACH T-DMatriz:
    IF T-DMatriz.Cantidad[1] <> 0 THEN DO:
        CREATE ITEM.
        ASSIGN
            ITEM.codcia = s-codcia
            ITEM.almdes = ENTRY(1, s-codalm)
            ITEM.CodMat = T-DMatriz.CodMat[1]
            ITEM.CanPed = T-DMatriz.Cantidad[1]
            ITEM.Factor = 1.
        pCodMat = ITEM.codmat.
        RUN vta2/p-codigo-producto (INPUT-OUTPUT pCodMat, YES).
        IF pCodMat = '' THEN UNDO, NEXT.
        FIND Almmmatg WHERE Almmmatg.codcia = s-codcia
            AND Almmmatg.codmat = ITEM.codmat
            NO-LOCK.
        IF Almmmatg.Chr__01 = "" THEN DO:
           MESSAGE "Articulo" ITEM.codmat "no tiene unidad de Oficina" VIEW-AS ALERT-BOX ERROR.
           UNDO, NEXT.
        END.
        ITEM.UndVta = Almmmatg.UndA.
        RUN vta2/PrecioMayorista-Cont (s-CodCia,
                                       s-CodDiv,
                                       s-CodCli,
                                       s-CodMon,
                                       s-TpoCmb,
                                       OUTPUT f-Factor,
                                       ITEM.codmat,
                                       s-FlgSit,
                                       ITEM.undvta,
                                       ITEM.CanPed,
                                       s-NroDec,
                                       ITEM.almdes,   /* Necesario para REMATES */
                                       OUTPUT f-PreBas,
                                       OUTPUT f-PreVta,
                                       OUTPUT f-Dsctos,
                                       OUTPUT y-Dsctos,
                                       OUTPUT x-TipDto
                                       ).
        IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, NEXT.
        ASSIGN
            ITEM.PreUni = F-PREVTA
            ITEM.Por_Dsctos[2] = z-Dsctos
            ITEM.Por_Dsctos[3] = y-Dsctos.

        ASSIGN 
            ITEM.Factor = F-FACTOR
            /*ITEM.NroItm = I-NroItm*/
            ITEM.PorDto = f-Dsctos
            ITEM.PreBas = F-PreBas 
            ITEM.AftIgv = Almmmatg.AftIgv
            ITEM.AftIsc = Almmmatg.AftIsc
            ITEM.Libre_c04 = x-TipDto.
        ASSIGN
            ITEM.ImpLin = ROUND ( ITEM.CanPed * ITEM.PreUni * 
                          ( 1 - ITEM.Por_Dsctos[1] / 100 ) *
                          ( 1 - ITEM.Por_Dsctos[2] / 100 ) *
                          ( 1 - ITEM.Por_Dsctos[3] / 100 ), 2 ).
        IF ITEM.Por_Dsctos[1] = 0 AND ITEM.Por_Dsctos[2] = 0 AND ITEM.Por_Dsctos[3] = 0 
            THEN ITEM.ImpDto = 0.
            ELSE ITEM.ImpDto = ITEM.CanPed * ITEM.PreUni - ITEM.ImpLin.
        ASSIGN
            ITEM.ImpLin = ROUND(ITEM.ImpLin, 2)
            ITEM.ImpDto = ROUND(ITEM.ImpDto, 2).
        IF ITEM.AftIsc 
        THEN ITEM.ImpIsc = ROUND(ITEM.PreBas * ITEM.CanPed * (Almmmatg.PorIsc / 100),4).
        ELSE ITEM.ImpIsc = 0.
        IF ITEM.AftIgv 
        THEN ITEM.ImpIgv = ITEM.ImpLin - ROUND( ITEM.ImpLin  / ( 1 + (s-PorIgv / 100) ), 4 ).
        ELSE ITEM.ImpIgv = 0.
    END.
END.
*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Importe-Item B-table-Win 
PROCEDURE Importe-Item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF INPUT PARAMETER pCodMat AS CHAR.
DEF INPUT PARAMETER pUndVta AS CHAR.
DEF INPUT PARAMETER pCanPed AS DEC.
DEF INPUT PARAMETER pAlmDes AS CHAR.
DEF OUTPUT PARAMETER pImpLin AS DEC.

pImpLin = 0.
IF pCodMat = "" THEN RETURN.

DEFINE VARIABLE f-Factor LIKE Facdpedi.factor NO-UNDO.
DEFINE VARIABLE F-PREBAS LIKE Almmmatg.PreBas NO-UNDO.
DEFINE VARIABLE F-PREVTA AS DECI NO-UNDO.
DEFINE VARIABLE F-DSCTOS LIKE Almmmatg.PorMax NO-UNDO.
DEFINE VARIABLE Y-DSCTOS AS DECI NO-UNDO.
DEFINE VARIABLE x-TipDto AS CHAR NO-UNDO.

FIND Almmmatg WHERE Almmmatg.codcia = s-codcia
    AND Almmmatg.codmat = pCodMat
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE Almmmatg THEN RETURN.
RUN vta2/PrecioMayorista-Cont (s-CodCia,
                               s-CodDiv,
                               s-CodCli,
                               s-CodMon,
                               s-TpoCmb,
                               OUTPUT f-Factor,
                               pCodMat,
                               s-FlgSit,
                               pUndVta,
                               pCanPed,
                               s-NroDec,
                               pAlmDes,
                               OUTPUT f-PreBas,
                               OUTPUT f-PreVta,
                               OUTPUT f-Dsctos,
                               OUTPUT y-Dsctos,
                               OUTPUT x-TipDto
                               ).
IF RETURN-VALUE = 'ADM-ERROR' THEN RETURN.
ASSIGN
    pImpLin = ROUND ( pCanPed * f-PreVta * ( 1 - y-Dsctos / 100 ), 2 ).
ASSIGN
    pImpLin = ROUND(pImpLin, 2).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-busca B-table-Win 
PROCEDURE local-busca :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE OK-WAIT-STATE AS LOGICAL NO-UNDO.
  ASSIGN  input-var-1 = ""
          input-var-2 = ""
          input-var-3 = ""
          output-var-1 = ?
          OK-WAIT-STATE = SESSION:SET-WAIT-STATE("GENERAL").

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'busca':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO ON ERROR UNDO, LEAVE ON STOP UNDO, LEAVE ON ENDKEY UNDO, LEAVE:
    /*RUN PL/C-XXX.W("").*/
    IF OUTPUT-VAR-1 <> ? THEN DO:
         FIND {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} WHERE
              ROWID({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}) = OUTPUT-VAR-1
              NO-LOCK NO-ERROR.
         IF AVAIL {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} THEN DO:
            REPOSITION {&BROWSE-NAME}  TO ROWID OUTPUT-VAR-1.
         END.
    END.
  END.
  OK-WAIT-STATE = SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-open-query B-table-Win 
PROCEDURE local-open-query :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  ASSIGN
      T-DMatriz.PreUni[1]:LABEL IN BROWSE {&browse-name} = VtaCMatriz.LabelColumna[1]
      T-DMatriz.PreUni[2]:LABEL IN BROWSE {&browse-name} = VtaCMatriz.LabelColumna[2]
      T-DMatriz.PreUni[3]:LABEL IN BROWSE {&browse-name} = VtaCMatriz.LabelColumna[3]
      T-DMatriz.PreUni[4]:LABEL IN BROWSE {&browse-name} = VtaCMatriz.LabelColumna[4]
      T-DMatriz.PreUni[5]:LABEL IN BROWSE {&browse-name} = VtaCMatriz.LabelColumna[5]
      T-DMatriz.PreUni[6]:LABEL IN BROWSE {&browse-name} = VtaCMatriz.LabelColumna[6]
      T-DMatriz.PreUni[7]:LABEL IN BROWSE {&browse-name} = VtaCMatriz.LabelColumna[7]
      T-DMatriz.PreUni[8]:LABEL IN BROWSE {&browse-name} = VtaCMatriz.LabelColumna[8]
      T-DMatriz.PreUni[9]:LABEL IN BROWSE {&browse-name} = VtaCMatriz.LabelColumna[9]
      T-DMatriz.PreUni[10]:LABEL IN BROWSE {&browse-name} = VtaCMatriz.LabelColumna[10]
      T-DMatriz.PreUni[11]:LABEL IN BROWSE {&browse-name} = VtaCMatriz.LabelColumna[11]
      T-DMatriz.PreUni[12]:LABEL IN BROWSE {&browse-name} = VtaCMatriz.LabelColumna[12].

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .

  /* Code placed here will execute AFTER standard behavior.    */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record B-table-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE procesa-parametros B-table-Win 
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
        WHEN "" THEN.
    END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE recoge-parametros B-table-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records B-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "VtaCMatriz"}
  {src/adm/template/snd-list.i "T-DMatriz"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed B-table-Win 
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
      {src/adm/template/bstates.i}
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valida B-table-Win 
PROCEDURE valida :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

RETURN "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Valida-por-Item B-table-Win 
PROCEDURE Valida-por-Item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF INPUT PARAMETER pCodMat AS CHAR.
DEF INPUT PARAMETER pAlmDes AS CHAR.
DEF INPUT PARAMETER pUndVta AS CHAR.
DEF INPUT PARAMETER pCanPed AS DEC.

DEF VAR pPreUni AS DEC NO-UNDO.
DEF VAR pPor_Dsctos1 AS DEC NO-UNDO.
DEF VAR pPor_Dsctos2 AS DEC NO-UNDO.
DEF VAR pPor_Dsctos3 AS DEC NO-UNDO.

DEFINE VARIABLE F-FACTOR AS DECI NO-UNDO.
DEFINE VARIABLE F-PREBAS LIKE Almmmatg.PreBas NO-UNDO.
DEFINE VARIABLE F-DSCTOS LIKE Almmmatg.PorMax NO-UNDO.
DEFINE VARIABLE X-CANPED AS DECI NO-UNDO.
DEFINE VARIABLE F-PREVTA AS DECI NO-UNDO.
DEFINE VARIABLE Y-DSCTOS AS DECI NO-UNDO.
DEFINE VARIABLE Z-DSCTOS AS DECI NO-UNDO.
DEFINE VARIABLE x-TipDto AS CHAR NO-UNDO.

/* PRODUCTO */  
FIND Almmmatg WHERE Almmmatg.codcia = s-codcia AND Almmmatg.codmat = pCodMat
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE Almmmatg THEN DO:
    MESSAGE 'C�digo de producto' pCodMat 'NO registrado' VIEW-AS ALERT-BOX ERROR.
    RETURN "ADM-ERROR".
END.
IF Almmmatg.TpoArt = "D" THEN DO:
    MESSAGE 'Producto' Almmmatg.desmat 'DESACTIVADO' VIEW-AS ALERT-BOX ERROR.
    RETURN "ADM-ERROR".
END.
FIND Almmmate WHERE Almmmate.CodCia = S-CODCIA 
    AND Almmmate.CodAlm = pAlmDes
    AND Almmmate.codmat = pCodMat
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE Almmmate THEN DO:
    MESSAGE "Articulo" Almmmatg.desmat "no asignado al almac�n" pAlmDes
        VIEW-AS ALERT-BOX ERROR.
    RETURN "ADM-ERROR".
END.
/* FAMILIA DE VENTAS */
FIND Almtfami OF Almmmatg NO-LOCK NO-ERROR.
IF AVAILABLE Almtfami AND Almtfami.SwComercial = NO THEN DO:
    MESSAGE 'Producto' Almmmatg.desmat SKIP
        'L�nea' Almmmatg.codfam 'NO autorizada para ventas' VIEW-AS ALERT-BOX ERROR.
    RETURN "ADM-ERROR".
END.
FIND Almsfami OF Almmmatg NO-LOCK NO-ERROR.
IF AVAILABLE Almsfami 
    AND AlmSFami.SwDigesa = YES 
    AND Almmmatg.VtoDigesa <> ? 
    AND  Almmmatg.VtoDigesa < TODAY THEN DO:
    MESSAGE 'Producto' Almmmatg.desmat 'con autorizaci�n de DIGESA VENCIDA' VIEW-AS ALERT-BOX ERROR.
    RETURN "ADM-ERROR".
END.
/* RHC 21/08/2012 CONTROL POR TIPO DE PRODUCTO */
IF Almmmatg.TpoMrg = "1" AND s-FlgTipoVenta = NO THEN DO:
    MESSAGE "No se puede vender el producto" Almmmatg.desmat "al por menor"
        VIEW-AS ALERT-BOX ERROR.
    RETURN 'ADM-ERROR'.
END.
IF Almmmatg.TpoMrg = "2" AND s-FlgTipoVenta = YES THEN DO:
    MESSAGE "No se puede vender el producto" Almmmatg.desmat "al por mayor"
        VIEW-AS ALERT-BOX ERROR.
    RETURN 'ADM-ERROR'.
END.
/* ********************************************* */
/* UNIDAD */
IF pUndVta = "" THEN DO:
    MESSAGE 'Producto' Almmmatg.desmat "NO tiene registrado la unidad de venta" VIEW-AS ALERT-BOX ERROR.
    RETURN "ADM-ERROR".
END.
/* CANTIDAD */
IF pCanPed < 0.125 THEN DO:
    MESSAGE "Cantidad del producto" Almmmatg.desmat "debe ser mayor o igual a 0.125" VIEW-AS ALERT-BOX ERROR.
    RETURN "ADM-ERROR".
END.
IF pUndVta = "UNI" AND pCanPed - TRUNCATE(pCanPed, 0) <> 0 THEN DO:
    MESSAGE "NO se permiten ventas fraccionadas en el producto" Almmmatg.desmat VIEW-AS ALERT-BOX ERROR.
    RETURN "ADM-ERROR".
END.
/* FACTOR DE EQUIVALENCIA Y PRECIO UNITARIO*/
RUN vta2/PrecioMayorista-Cont (s-CodCia,
                               s-CodDiv,
                               s-CodCli,
                               s-CodMon,
                               s-TpoCmb,
                               OUTPUT f-Factor,
                               pCodMat,
                               s-FlgSit,
                               pUndVta,
                               pCanPed,
                               s-NroDec,
                               pAlmDes,
                               OUTPUT f-PreBas,
                               OUTPUT f-PreVta,
                               OUTPUT f-Dsctos,
                               OUTPUT y-Dsctos,
                               OUTPUT x-TipDto
                               ).
IF RETURN-VALUE = 'ADM-ERROR' THEN RETURN "ADM-ERROR".
ASSIGN
    pPreUni = f-PreVta
    pPor_Dsctos2 = f-Dsctos
    pPor_Dsctos3 = y-Dsctos.

/* EMPAQUE */
DEF VAR f-Canped AS DEC NO-UNDO.
IF s-FlgEmpaque = YES THEN DO:
  f-CanPed = pCanPed * f-Factor.
  IF s-VentaMayorista = 2 THEN DO:  /* LISTA POR DIVISION */
      FIND FIRST VtaListaMay OF Almmmatg WHERE Vtalistamay.coddiv = s-coddiv NO-LOCK NO-ERROR.
      IF AVAILABLE VtaListaMay AND Vtalistamay.CanEmp > 0 THEN DO:
          f-CanPed = (TRUNCATE((f-CanPed / Vtalistamay.CanEmp),0) * Vtalistamay.CanEmp).
          IF f-CanPed <> pCanPed * f-Factor THEN DO:
              MESSAGE 'Para el producto' Almmmatg.desmat SKIP
                  'Solo puede vender en empaques de' Vtalistamay.CanEmp Almmmatg.UndBas
                  VIEW-AS ALERT-BOX ERROR.
              RETURN "ADM-ERROR".
          END.
      END.
  END.
  ELSE DO:      /* LISTA GENERAL */
      IF Almmmatg.DEC__03 > 0 THEN DO:
          f-CanPed = (TRUNCATE((f-CanPed / Almmmatg.DEC__03),0) * Almmmatg.DEC__03).
          IF f-CanPed <> pCanPed * f-Factor THEN DO:
              MESSAGE 'Para el producto' Almmmatg.desmat SKIP
                  'Solo puede vender en empaques de' Almmmatg.DEC__03 Almmmatg.UndBas
                  VIEW-AS ALERT-BOX ERROR.
              RETURN "ADM-ERROR".
          END.
      END.
  END.
END.
/* MINIMO DE VENTA */
IF s-FlgMinVenta = YES THEN DO:
  f-CanPed = pCanPed * f-Factor.
  IF s-VentaMayorista = 2 THEN DO:  /* LISTA POR DIVISION */
      FIND FIRST VtaListaMay OF Almmmatg WHERE Vtalistamay.coddiv = s-coddiv NO-LOCK NO-ERROR.
      IF AVAILABLE VtaListaMay AND Vtalistamay.CanEmp > 0 THEN DO:
          IF f-CanPed < Vtalistamay.CanEmp THEN DO:
              MESSAGE 'Para el producto' Almmmatg.desmat SKIP
                  'Solo puede vender como m�nimo' Vtalistamay.CanEmp Almmmatg.UndBas
                  VIEW-AS ALERT-BOX ERROR.
              RETURN "ADM-ERROR".
          END.
      END.
  END.
  ELSE DO:      /* LISTA GENERAL */
      IF Almmmatg.DEC__03 > 0 THEN DO:
          IF f-CanPed < Almmmatg.DEC__03 THEN DO:
              MESSAGE 'Para el producto' Almmmatg.desmat SKIP
                  'Solo puede vender como m�nimo' Almmmatg.DEC__03 Almmmatg.UndBas
                  VIEW-AS ALERT-BOX ERROR.
              RETURN "ADM-ERROR".
          END.
      END.
  END.
END.

/* STOCK COMPROMETIDO */
DEF VAR s-StkComprometido AS DEC NO-UNDO.
DEF VAR x-StkAct AS DEC NO-UNDO.

RUN vta2/Stock-Comprometido (pCodMat, 
                             pAlmDes, 
                             OUTPUT s-StkComprometido).
IF s-adm-new-record = 'NO' THEN DO:
    FIND Facdpedi WHERE Facdpedi.codcia = s-codcia
        AND Facdpedi.coddoc = s-coddoc
        AND Facdpedi.nroped = s-nroped
        AND Facdpedi.codmat = pCodMat
        NO-LOCK NO-ERROR.
    IF AVAILABLE Facdpedi THEN s-StkComprometido = s-StkComprometido - ( Facdpedi.CanPed * Facdpedi.Factor ).
END.
ASSIGN
    x-CanPed = pCanPed * f-Factor
    x-StkAct = Almmmate.StkAct.
IF (x-StkAct - s-StkComprometido) < x-CanPed
  THEN DO:
    MESSAGE "Producto" Almmmatg.desmat SKIP
        "No hay STOCK suficiente" SKIP(1)
        "       STOCK ACTUAL : " x-StkAct Almmmatg.undbas SKIP
        "  STOCK COMPROMETIDO: " s-StkComprometido Almmmatg.undbas SKIP
        "   STOCK DISPONIBLE : " (x-StkAct - s-StkComprometido) Almmmatg.undbas SKIP
        VIEW-AS ALERT-BOX ERROR.
    RETURN "ADM-ERROR".
END.

/* ************************************** */
/* RHC 13.12.2010 Margen de Utilidad */
/* RHC menos para productos de remate */
FIND Almacen WHERE Almacen.codcia = s-codcia AND Almacen.codalm = pAlmDes NO-LOCK.
IF Almacen.Campo-C[3] <> "Si" THEN DO:    /* NO para almacenes de remate */
    DEF VAR pError AS CHAR NO-UNDO.
    DEF VAR X-MARGEN AS DEC NO-UNDO.
    DEF VAR X-LIMITE AS DEC NO-UNDO.
    DEF VAR x-PreUni AS DEC NO-UNDO.

    x-PreUni = pPreUni *
        ( 1 - pPor_Dsctos1 / 100 ) *
        ( 1 - pPor_Dsctos2 / 100 ) *
        ( 1 - pPor_Dsctos3 / 100 ) .
    RUN vtagn/p-margen-utilidad (
        pCodMat,      /* Producto */
        x-PreUni,  /* Precio de venta unitario */
        pUndVta,
        s-CodMon,       /* Moneda de venta */
        s-TpoCmb,       /* Tipo de cambio */
        YES,            /* Muestra el error */
        "",
        OUTPUT x-Margen,        /* Margen de utilidad */
        OUTPUT x-Limite,        /* Margen m�nimo de utilidad */
        OUTPUT pError           /* Control de errores: "OK" "ADM-ERROR" */
        ).
    IF pError = "ADM-ERROR" THEN RETURN "ADM-ERROR".
END.
RETURN "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valida-update B-table-Win 
PROCEDURE valida-update :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RETURN "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fCantidad B-table-Win 
FUNCTION fCantidad RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN T-DMatriz.Cantidad[1] + T-DMatriz.Cantidad[2] + T-DMatriz.Cantidad[3] + 
    T-DMatriz.Cantidad[4] + T-DMatriz.Cantidad[5] + T-DMatriz.Cantidad[6] + 
    T-DMatriz.Cantidad[7] + T-DMatriz.Cantidad[8] + T-DMatriz.Cantidad[9] + 
    T-DMatriz.Cantidad[10] + T-DMatriz.Cantidad[11] + T-DMatriz.Cantidad[12].

  RETURN 0.00.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

