&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          integral         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
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
DEFINE SHARED VARIABLE lh_Handle  AS HANDLE.
/* Local Variable Definitions ---                                       */

DEFINE SHARED VAR s-codcia AS INT.
DEFINE SHARED VAR pv-codcia AS INT.
DEFINE SHARED VAR s-codpro AS CHAR.
DEFINE SHARED VAR s-coddiv AS CHAR.

DEFINE BUFFER b-almcatvtac FOR almcatvtac.

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
&Scoped-define EXTERNAL-TABLES gn-prov
&Scoped-define FIRST-EXTERNAL-TABLE gn-prov


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR gn-prov.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES AlmCatVtaC

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table AlmCatVtaC.NroPag AlmCatVtaC.DesPag 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table AlmCatVtaC.NroPag ~
AlmCatVtaC.DesPag 
&Scoped-define ENABLED-TABLES-IN-QUERY-br_table AlmCatVtaC
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br_table AlmCatVtaC
&Scoped-define QUERY-STRING-br_table FOR EACH AlmCatVtaC WHERE AlmCatVtaC.CodPro = gn-prov.CodPro ~
      AND AlmCatVtaC.CodCia = s-codcia ~
 AND AlmCatVtaC.CodDiv = s-coddiv NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH AlmCatVtaC WHERE AlmCatVtaC.CodPro = gn-prov.CodPro ~
      AND AlmCatVtaC.CodCia = s-codcia ~
 AND AlmCatVtaC.CodDiv = s-coddiv NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-br_table AlmCatVtaC
&Scoped-define FIRST-TABLE-IN-QUERY-br_table AlmCatVtaC


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-60 br_table BUTTON-5 

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fMov B-table-Win 
FUNCTION fMov RETURNS LOGICAL
  ( INPUT pCodPro AS CHAR,
    INPUT pNroPag AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-5 
     IMAGE-UP FILE "img/excel.bmp":U
     LABEL "Button 5" 
     SIZE 10 BY 1.62.

DEFINE RECTANGLE RECT-60
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 76.43 BY 6.58.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      AlmCatVtaC SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      AlmCatVtaC.NroPag FORMAT "9999":U
      AlmCatVtaC.DesPag FORMAT "x(50)":U WIDTH 60
  ENABLE
      AlmCatVtaC.NroPag
      AlmCatVtaC.DesPag
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 71 BY 6.19
         FONT 4
         TITLE "Ingreso P�ginas".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br_table AT ROW 1.27 COL 4
     BUTTON-5 AT ROW 6.08 COL 79.14 WIDGET-ID 4
     RECT-60 AT ROW 1.15 COL 1.57 WIDGET-ID 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
   External Tables: INTEGRAL.gn-prov
   Allow: Basic,Browse
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
  CREATE WINDOW B-table-Win ASSIGN
         HEIGHT             = 6.96
         WIDTH              = 91.
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
/* BROWSE-TAB br_table RECT-60 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _TblList          = "INTEGRAL.AlmCatVtaC WHERE INTEGRAL.gn-prov ..."
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _JoinCode[1]      = "AlmCatVtaC.CodPro = gn-prov.CodPro"
     _Where[1]         = "AlmCatVtaC.CodCia = s-codcia
 AND AlmCatVtaC.CodDiv = s-coddiv"
     _FldNameList[1]   > INTEGRAL.AlmCatVtaC.NroPag
"AlmCatVtaC.NroPag" ? ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > INTEGRAL.AlmCatVtaC.DesPag
"AlmCatVtaC.DesPag" ? "x(50)" "character" ? ? ? ? ? ? yes ? no no "60" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
ON MOUSE-SELECT-DBLCLICK OF br_table IN FRAME F-Main /* Ingreso P�ginas */
DO:
      RUN Procesa-Handle IN lh_handle ('Open-Query-2':U).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-ENTRY OF br_table IN FRAME F-Main /* Ingreso P�ginas */
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-LEAVE OF br_table IN FRAME F-Main /* Ingreso P�ginas */
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
   {src/adm/template/brsleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON VALUE-CHANGED OF br_table IN FRAME F-Main /* Ingreso P�ginas */
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME AlmCatVtaC.NroPag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL AlmCatVtaC.NroPag br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF AlmCatVtaC.NroPag IN BROWSE br_table /* Pagina */
DO:
    /*Pagina no puede ser 0*/
    IF INT(AlmCatVtaC.NroPag:SCREEN-VALUE IN BROWSE {&BROWSE-name}) = 0 THEN DO:
        MESSAGE 'La pagina no puede ser cero (0)'
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        APPLY 'ENTRY' TO AlmCatVtaC.NroPag.
        RETURN NO-APPLY.
    END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-5 B-table-Win
ON CHOOSE OF BUTTON-5 IN FRAME F-Main /* Button 5 */
DO:  
    RUN Genera_Excel.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

ON RETURN OF almcatvtac.nropag, AlmCatVtaC.DesPag DO:
    APPLY 'TAB'.
    RETURN NO-APPLY.
END.

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
  {src/adm/template/row-list.i "gn-prov"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "gn-prov"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Genera_Excel B-table-Win 
PROCEDURE Genera_Excel :
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
DEFINE VARIABLE t-Column                AS INTEGER INIT 5.
DEFINE VARIABLE i-Column                AS INTEGER NO-UNDO.
DEFINE VARIABLE j-Column                AS INTEGER NO-UNDO.

/* create a new Excel Application object */
CREATE "Excel.Application" chExcelApplication.

/* create a new Workbook */
chWorkbook = chExcelApplication:Workbooks:Add().

/* get the active Worksheet */
chWorkSheet = chExcelApplication:Sheets:Item(1).

/*Header del Excel */
cRange = "D" + '2'.
chWorkSheet:Range('D2'):Font:Bold = TRUE.
chWorkSheet:Range(cRange):Value = "CATALOGO DE PRODUCTOS PARA EL PROVEEDOR " + GN-PROV.NOMPRO.
cRange = "K" + '2'.
chWorkSheet:Range(cRange):Value = TODAY.


/*Formato*/
chWorkSheet:Columns("A"):NumberFormat = "@".
chWorkSheet:Columns("B"):NumberFormat = "@".
chWorkSheet:Columns("C"):NumberFormat = "@".

FOR EACH almcatvtac WHERE almcatvtac.codcia = s-codcia
    AND almcatvtac.coddiv = s-coddiv
    AND almcatvtac.codpro = gn-prov.codpro NO-LOCK:
    t-Column = t-Column + 1.
    cColumn = STRING(t-Column).
    cRange = "A" + cColumn.    
    chWorkSheet:Range(cRange):Font:Bold = TRUE.
    chWorkSheet:Range(cRange):Value = STRING(almcatvtac.nropag,'999').
    cRange = "B" + cColumn.
    chWorkSheet:Range(cRange):Font:Bold = TRUE.
    chWorkSheet:Range(cRange):Value = AlmCatVtaC.DesPag.  

    /* set the column names for the Worksheet */
    t-Column = t-Column + 1.
    cColumn = STRING(t-Column).
    cRange = "A" + cColumn.
    chWorkSheet:Range(cRange):Font:Bold = TRUE.
    chWorkSheet:Range(cRange):Value = "Nro.Pag".
    cRange = "B" + cColumn.
    chWorkSheet:Range(cRange):Font:Bold = TRUE.
    chWorkSheet:Range(cRange):Value = "Nro.Sec".
    cRange = "C" + cColumn.
    chWorkSheet:Range(cRange):Font:Bold = TRUE.
    chWorkSheet:Range(cRange):Value = "Codigo".
    cRange = "D" + cColumn.
    chWorkSheet:Range(cRange):Font:Bold = TRUE.
    chWorkSheet:Range(cRange):Value = "Descripci�n".
    cRange = "E" + cColumn.
    chWorkSheet:Range(cRange):Font:Bold = TRUE.
    chWorkSheet:Range(cRange):Value = "Marca".
    cRange = "F" + cColumn.
    chWorkSheet:Range(cRange):Font:Bold = TRUE.
    chWorkSheet:Range(cRange):Value = "Unidades".

    cRange = "G" + cColumn.
    chWorkSheet:Range(cRange):Font:Bold = TRUE.
    chWorkSheet:Range(cRange):Value = "Moneda".
    cRange = "H" + cColumn.
    chWorkSheet:Range(cRange):Font:Bold = TRUE.
    chWorkSheet:Range(cRange):Value = "Tpo Cambio".
    cRange = "I" + cColumn.
    chWorkSheet:Range(cRange):Font:Bold = TRUE.
    chWorkSheet:Range(cRange):Value = "Precio".
    cRange = "J" + cColumn.
    chWorkSheet:Range(cRange):Font:Bold = TRUE.
    chWorkSheet:Range(cRange):Value = "Empaque".
    cRange = "K" + cColumn.
    chWorkSheet:Range(cRange):Font:Bold = TRUE.
    chWorkSheet:Range(cRange):Value = "Cantidad".

    FOR EACH almcatvtad WHERE almcatvtad.codcia = s-codcia
        AND almcatvtad.coddiv = s-coddiv
        AND almcatvtad.codpro = gn-prov.codpro
        AND almcatvtad.nropag = almcatvtac.nropag NO-LOCK,
        FIRST almmmatg WHERE almmmatg.codcia = s-codcia
        AND almmmatg.codmat = almcatvtad.codmat NO-LOCK
        BREAK BY almcatvtad.nropag:

        FIND FIRST VtaListaMay WHERE VtaListaMay.CodCia = s-codcia
            AND VtaListaMay.CodDiv = s-coddiv
            AND VtaListaMay.CodMat = almcatvtad.codmat 
            NO-LOCK NO-ERROR.

        IF AVAIL VtaListaMay THEN DO:
            t-Column = t-Column + 1.
            cColumn = STRING(t-Column).
            cRange = "A" + cColumn.
            chWorkSheet:Range(cRange):Value = STRING(almcatvtad.nropag,'999').
            cRange = "B" + cColumn.
            chWorkSheet:Range(cRange):Value = STRING(almcatvtad.nrosec,'999').
            cRange = "C" + cColumn.
            chWorkSheet:Range(cRange):Value = almcatvtad.codmat.
            cRange = "D" + cColumn.
            chWorkSheet:Range(cRange):Value = almcatvtad.DesMat.
            cRange = "E" + cColumn.
            chWorkSheet:Range(cRange):Value = almmmatg.desmar.
            cRange = "F" + cColumn.
            chWorkSheet:Range(cRange):Value = almmmatg.undbas.
            cRange = "G" + cColumn.
            chWorkSheet:Range(cRange):Value = VtaListaMay.MonVta.    
            cRange = "H" + cColumn.
            chWorkSheet:Range(cRange):Value = VtaListaMay.TpoCmb.    
            cRange = "I" + cColumn.
            chWorkSheet:Range(cRange):Value = VtaListaMay.PreOfi.    
            cRange = "J" + cColumn.
            chWorkSheet:Range(cRange):Value = AlmCatVtaD.CanEmp.
            cRange = "K" + cColumn.
            chWorkSheet:Range(cRange):Value = AlmCatVtaD.Libre_d05.                    
        END.
    END.
    t-Column = t-Column + 3.
END.


HIDE FRAME F-Proceso.

/* launch Excel so it is visible to the user */
 chExcelApplication:Visible = TRUE.

/* release com-handles */
RELEASE OBJECT chExcelApplication.      
RELEASE OBJECT chWorkbook.
RELEASE OBJECT chWorksheet.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-add-record B-table-Win 
PROCEDURE local-add-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  
  
                       
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record B-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */ 
  RUN GET-ATTRIBUTE('ADM-NEW-RECORD').
  IF RETURN-VALUE = 'YES' THEN DO:      
      ASSIGN 
          almcatvtac.coddiv = s-coddiv
          almcatvtac.codcia = s-codcia
          almcatvtac.codpro = s-codpro.
  END.

  RUN adm-row-changed.  

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record B-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  
  IF fMov(almcatvtac.codpro, almcatvtac.nropag) THEN DO: 
      MESSAGE 'Esta p�gina tiene registros asociados' SKIP
              '      No se puede eliminar           '
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN 'ADM-ERROR'.
  END.
  
  FOR EACH almcatvtad WHERE almcatvtad.codcia = s-codcia
      AND almcatvtad.codpro = almcatvtac.codpro
      AND almcatvtad.nropag = almcatvtac.nropag :
      DELETE almcatvtad.
  END.

  /* Dispatch standard ADM method.                             */
  
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .
  
  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable-fields B-table-Win 
PROCEDURE local-enable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  /*
  RUN GET-ATTRIBUTE('ADM-NEW-RECORD').
  IF RETURN-VALUE = 'YES' 
  THEN DO:
      AlmCatVtaC.NroPag:READ-ONLY IN BROWSE {&BROWSE-NAME} = NO.
      APPLY 'ENTRY':U TO AlmCatVtaC.NroPag.
  END.
  ELSE DO:
      AlmCatVtaC.NroPag:READ-ONLY IN BROWSE {&BROWSE-NAME} = YES.
      APPLY 'ENTRY':U TO AlmCatVtaC.DesPag.
  END.
  */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-row-changed B-table-Win 
PROCEDURE local-row-changed :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'row-changed':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  APPLY 'value-changed' TO {&browse-name} IN FRAME {&FRAME-NAME}.

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
  {src/adm/template/snd-list.i "gn-prov"}
  {src/adm/template/snd-list.i "AlmCatVtaC"}

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
    DO WITH FRAME {&FRAME-NAME}:        
        IF AlmCatVtaC.DesPag:SCREEN-VALUE IN BROWSE {&BROWSE-name} = '' THEN DO:
            MESSAGE 'Debe asignar un nombre a la p�gina'
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            APPLY 'ENTRY' TO AlmCatVtaC.DesPag.
            RETURN 'ADM-ERROR'.
        END.

        RUN GET-ATTRIBUTE('ADM-NEW-RECORD').
        IF RETURN-VALUE = 'YES' THEN DO:
            FIND FIRST b-almcatvtac WHERE b-almcatvtac.codcia = s-codcia
                AND b-almcatvtac.codpro = s-codpro
                AND b-almcatvtac.coddiv = s-coddiv
                AND b-almcatvtac.nropag = INT(almcatvtac.nropag:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) NO-LOCK NO-ERROR.
            IF AVAIL b-almcatvtac THEN DO:
                MESSAGE "N�mero de P�gina Repetido"
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
                APPLY "ENTRY" TO almcatvtac.nropag.
                RETURN "adm-error".
            END.
        END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fMov B-table-Win 
FUNCTION fMov RETURNS LOGICAL
  ( INPUT pCodPro AS CHAR,
    INPUT pNroPag AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lMov AS LOGICAL     NO-UNDO.
  FIND FIRST vtaddocu WHERE vtaddocu.codcia = s-codcia
      AND vtaddocu.libre_c03 = pCodPro
      AND vtaddocu.libre_c01 = STRING(pNroPag,'9999') NO-LOCK NO-ERROR.
  IF AVAIL vtaddocu THEN lMov = YES.
  ELSE lMov = NO.

  RETURN lMov.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

