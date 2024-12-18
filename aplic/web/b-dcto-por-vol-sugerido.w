&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          integral         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE ITEM LIKE FacDPedi.
DEFINE TEMP-TABLE PEDI LIKE FacDPedi
       FIELD CanSugerida AS DECI
       FIELD PorSugerido AS DECI
       FIELD ImpSugerido AS DECI.



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

/*&SCOPED-DEFINE precio-venta-general pri/PrecioVentaMayorCredito.p*/
/*&SCOPED-DEFINE precio-venta-general pri/PrecioVentaMayorCreditoFlash.p*/
&SCOPED-DEFINE precio-venta-general web/PrecioFinalCreditoMayorista.p

/* Parameters Definitions ---                                           */
DEF SHARED VAR s-codcia AS INTE.

/* Local Variable Definitions ---                                       */

DEF VAR x-CanSugerida AS DECI NO-UNDO.
DEF VAR x-PorSugerido AS DECI NO-UNDO.
DEF VAR x-ImpSugerido AS DECI NO-UNDO.

/*DEF VAR x-CodDiv AS CHAR NO-UNDO.*/


DEF SHARED VAR s-TpoPed AS CHAR.
DEF SHARED VAR pCodDiv AS CHAR.
DEF SHARED VAR s-CodCli AS CHAR.
DEF SHARED VAR s-CodMon AS INTE.
DEF SHARED VAR s-FmaPgo AS CHAR.
DEF SHARED VAR s-NroDec AS INTE.

DEF SHARED VAR lh_handle AS HANDLE.

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

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES PEDI Almmmatg VtaListaMay

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table PEDI.codmat Almmmatg.DesMat ~
PEDI.UndVta PEDI.CanPed PEDI.CanSugerida @ x-CanSugerida ~
PEDI.PorSugerido @ x-PorSugerido ~
PEDI.ImpSugerido * (1 - PEDI.PorSugerido / 100) @ x-ImpSugerido 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table PEDI.CanPed 
&Scoped-define ENABLED-TABLES-IN-QUERY-br_table PEDI
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br_table PEDI
&Scoped-define QUERY-STRING-br_table FOR EACH PEDI WHERE ~{&KEY-PHRASE} NO-LOCK, ~
      FIRST Almmmatg OF PEDI NO-LOCK, ~
      FIRST VtaListaMay WHERE VtaListaMay.CodCia = PEDI.CodCia ~
  AND VtaListaMay.codmat = PEDI.codmat ~
      AND VtaListaMay.CodDiv = pCodDiv OUTER-JOIN NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH PEDI WHERE ~{&KEY-PHRASE} NO-LOCK, ~
      FIRST Almmmatg OF PEDI NO-LOCK, ~
      FIRST VtaListaMay WHERE VtaListaMay.CodCia = PEDI.CodCia ~
  AND VtaListaMay.codmat = PEDI.codmat ~
      AND VtaListaMay.CodDiv = pCodDiv OUTER-JOIN NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-br_table PEDI Almmmatg VtaListaMay
&Scoped-define FIRST-TABLE-IN-QUERY-br_table PEDI
&Scoped-define SECOND-TABLE-IN-QUERY-br_table Almmmatg
&Scoped-define THIRD-TABLE-IN-QUERY-br_table VtaListaMay


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


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      PEDI, 
      Almmmatg, 
      VtaListaMay SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      PEDI.codmat COLUMN-LABEL "Articulo" FORMAT "X(6)":U
      Almmmatg.DesMat FORMAT "X(60)":U WIDTH 60.86
      PEDI.UndVta FORMAT "x(8)":U
      PEDI.CanPed FORMAT ">,>>>,>>9.9999":U COLUMN-FGCOLOR 0 COLUMN-BGCOLOR 11
      PEDI.CanSugerida @ x-CanSugerida COLUMN-LABEL "Cantidad!Sugerida"
            WIDTH 8.72 COLUMN-FGCOLOR 0 COLUMN-BGCOLOR 14
      PEDI.PorSugerido @ x-PorSugerido COLUMN-LABEL "% Dcto. Vol."
            WIDTH 9.43 COLUMN-FGCOLOR 0 COLUMN-BGCOLOR 14
      PEDI.ImpSugerido * (1 - PEDI.PorSugerido / 100) @ x-ImpSugerido COLUMN-LABEL "Unitario"
            WIDTH 2.72 COLUMN-FGCOLOR 0 COLUMN-BGCOLOR 14
  ENABLE
      PEDI.CanPed
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 117 BY 13.19
         FONT 4 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br_table AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY COMPILE
   Temp-Tables and Buffers:
      TABLE: ITEM T "?" ? INTEGRAL FacDPedi
      TABLE: PEDI T "?" ? INTEGRAL FacDPedi
      ADDITIONAL-FIELDS:
          FIELD CanSugerida AS DECI
          FIELD PorSugerido AS DECI
          FIELD ImpSugerido AS DECI
      END-FIELDS.
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
         HEIGHT             = 13.58
         WIDTH              = 119.43.
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
     _TblList          = "Temp-Tables.PEDI,INTEGRAL.Almmmatg OF Temp-Tables.PEDI,INTEGRAL.VtaListaMay WHERE Temp-Tables.PEDI ..."
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = ", FIRST, FIRST OUTER"
     _JoinCode[3]      = "INTEGRAL.VtaListaMay.CodCia = Temp-Tables.PEDI.CodCia
  AND INTEGRAL.VtaListaMay.codmat = Temp-Tables.PEDI.codmat"
     _Where[3]         = "INTEGRAL.VtaListaMay.CodDiv = pCodDiv"
     _FldNameList[1]   > Temp-Tables.PEDI.codmat
"PEDI.codmat" "Articulo" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > INTEGRAL.Almmmatg.DesMat
"Almmmatg.DesMat" ? "X(60)" "character" ? ? ? ? ? ? no ? no no "60.86" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   = Temp-Tables.PEDI.UndVta
     _FldNameList[4]   > Temp-Tables.PEDI.CanPed
"PEDI.CanPed" ? ? "decimal" 11 0 ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > "_<CALC>"
"PEDI.CanSugerida @ x-CanSugerida" "Cantidad!Sugerida" ? ? 14 0 ? ? ? ? no ? no no "8.72" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > "_<CALC>"
"PEDI.PorSugerido @ x-PorSugerido" "% Dcto. Vol." ? ? 14 0 ? ? ? ? no ? no no "9.43" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > "_<CALC>"
"PEDI.ImpSugerido * (1 - PEDI.PorSugerido / 100) @ x-ImpSugerido" "Unitario" ? ? 14 0 ? ? ? ? no ? no no "2.72" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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

ON 'RETURN':U OF  PEDI.CanPed
DO:
    APPLY 'TAB':U.
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

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Captura-Temporal B-table-Win 
PROCEDURE Captura-Temporal :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF INPUT PARAMETER pCodDiv AS CHAR.
DEF INPUT PARAMETER TABLE FOR ITEM.    /* Digitado por el vendedor */

/*x-CodDiv = pCodDiv.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Carga-Temporal B-table-Win 
PROCEDURE Carga-Temporal :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

EMPTY TEMP-TABLE PEDI.

DEF VAR x-Sumatoria AS DECI NO-UNDO.
DEF VAR k AS INTE NO-UNDO.
DEF VAR x-CanSugerida AS DECI NO-UNDO.
DEF VAR x-PorSugerido AS DECI NO-UNDO.
DEF VAR x-ImpSugerido AS DECI NO-UNDO.

FOR EACH ITEM NO-LOCK, FIRST Almmmatg OF ITEM NO-LOCK:
    RUN Sugerido (ITEM.CodMat,
                  ITEM.CanPed * ITEM.Factor,    /* En unidades b�sicas */
                  OUTPUT x-CanSugerida,         /* En unidades b�sicas */
                  OUTPUT x-PorSugerido,
                  OUTPUT x-ImpSugerido).
    IF x-CanSugerida > 0 THEN DO:
        CREATE PEDI.
        BUFFER-COPY ITEM TO PEDI.
        ASSIGN
            PEDI.CanSugerida = x-CanSugerida / ITEM.Factor
            PEDI.PorSugerido = x-PorSugerido
            PEDI.ImpSugerido = x-ImpSugerido.
    END.
END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Graba-Temporal B-table-Win 
PROCEDURE Graba-Temporal :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAMETER TABLE FOR ITEM.

  FOR EACH PEDI, FIRST ITEM WHERE ITEM.CodMat = PEDI.CodMat EXCLUSIVE-LOCK:
      ITEM.CanPed = PEDI.CanPed.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-statement B-table-Win 
PROCEDURE local-assign-statement :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-statement':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN Sugerido (PEDI.CodMat,
                PEDI.CanPed * PEDI.Factor,    /* En unidades b�sicas */
                OUTPUT x-CanSugerida,         /* En unidades b�sicas */
                OUTPUT x-PorSugerido,
                OUTPUT x-ImpSugerido).
  ASSIGN
      PEDI.CanSugerida = x-CanSugerida / PEDI.Factor
      PEDI.PorSugerido = x-PorSugerido
      PEDI.ImpSugerido = x-ImpSugerido.
  RUN dispatch IN THIS-PROCEDURE ('display-fields':U).

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-disable-fields B-table-Win 
PROCEDURE local-disable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'disable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN Procesa-Handle IN lh_handle ('Enable-Header').

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
  RUN Procesa-Handle IN lh_handle ('Disable-Header').

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
  RUN Carga-Temporal.

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
  {src/adm/template/snd-list.i "PEDI"}
  {src/adm/template/snd-list.i "Almmmatg"}
  {src/adm/template/snd-list.i "VtaListaMay"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Sugerido B-table-Win 
PROCEDURE Sugerido :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF INPUT PARAMETER pCodMat AS CHAR.
DEF INPUT PARAMETER pCanPed AS DECI.
DEF OUTPUT PARAMETER pCanSugerida AS DECI.
DEF OUTPUT PARAMETER pPorDescuento AS DECI.
DEF OUTPUT PARAMETER pImpDescuento AS DECI.

pCanSugerida = 0.
pPorDescuento = 0.
pImpDescuento = 0.

DEF VAR k AS INTE NO-UNDO.

FIND gn-divi WHERE gn-divi.codcia = s-codcia AND
    gn-divi.coddiv = pCodDiv /*x-CodDiv*/ NO-LOCK.

CASE GN-DIVI.VentaMayorista:
    WHEN 1 THEN DO:
        /* Barremos las escalas de descuento */
        DO k = 1 TO 10:
            IF pCanPed < Almmmatg.DtoVolR[k] THEN DO:
                pCanSugerida = Almmmatg.DtoVolR[k].
                pPorDescuento = Almmmatg.DtoVolD[k].
                LEAVE.
            END.
        END.
    END.
    WHEN 2 THEN DO:
        FIND VtaListaMay WHERE VtaListaMay.CodCia = s-codcia AND
            VtaListaMay.CodDiv = pCodDiv /*x-CodDiv*/ AND
            VtaListaMay.codmat = pCodMat NO-LOCK NO-ERROR.
        IF AVAILABLE VtaListaMay THEN DO:
            /* Barremos las escalas de descuento */
            DO k = 1 TO 10:
                IF pCanPed < VtaListaMay.DtoVolR[k] THEN DO:
                    pCanSugerida = VtaListaMay.DtoVolR[k].
                    pPorDescuento = VtaListaMay.DtoVolD[k].
                    LEAVE.
                END.
            END.
        END.
    END.
END CASE.

IF pCanSugerida > 0 AND pPorDescuento > 0 THEN DO:
    /* Confirmamos con la lista de precios */
    DEF VAR s-UndVta AS CHAR NO-UNDO.
    DEF VAR f-Factor AS DECI NO-UNDO.
    DEF VAR x-CanPed AS DECI NO-UNDO.
    DEF VAR f-PreBas AS DECI NO-UNDO.
    DEF VAR f-PreVta AS DECI NO-UNDO.
    DEF VAR f-Dsctos AS DECI NO-UNDO.
    DEF VAR y-Dsctos AS DECI NO-UNDO.
    DEF VAR z-Dsctos AS DECI NO-UNDO.
    DEF VAR x-TipDto AS CHAR NO-UNDO.
    DEF VAR f-FleteUnitario AS DECI NO-UNDO.

    DEF VAR pMensaje AS CHAR NO-UNDO.

    s-UndVta = Almmmatg.CHR__01.
    f-Factor = 1.
    x-CanPed = pCanSugerida.

    RUN {&precio-venta-general} (
        s-TpoPed,
        pCodDiv,
        s-CodCli,
        s-CodMon,
        INPUT-OUTPUT s-UndVta,
        OUTPUT f-Factor,
        Almmmatg.CodMat,
        s-FmaPgo,
        x-CanPed,
        s-NroDec,
        OUTPUT f-PreBas,
        OUTPUT f-PreVta,
        OUTPUT f-Dsctos,
        OUTPUT y-Dsctos,
        OUTPUT z-Dsctos,
        OUTPUT x-TipDto,
        "",     /* ClfCli: lo ingresamos solo si se quiere forzar la clasificacion */
        OUTPUT f-FleteUnitario,
        "",
        NO,
        OUTPUT pMensaje).
    IF RETURN-VALUE = 'ADM-ERROR' OR x-TipDto <> "VOL" THEN DO:
        ASSIGN
            pCanSugerida = 0 
            pPorDescuento = 0
            pImpDescuento = 0.
    END.
    ELSE pImpDescuento = f-PreBas.
END.

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

IF DECIMAL(PEDI.CanPed:SCREEN-VALUE IN BROWSE {&browse-name}) <= 0 THEN DO:
    MESSAGE 'Debe ingresar la cantidad' VIEW-AS ALERT-BOX ERROR.
    RETURN 'ADM-ERROR'.
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

