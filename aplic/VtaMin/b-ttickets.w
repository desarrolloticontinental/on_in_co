&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          integral         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW


/* Temp-Table and Buffer definitions                                    */
DEFINE SHARED TEMP-TABLE T-Tickets NO-UNDO LIKE VtaDTickets.



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

DEFINE SHARED VARIABLE lh_Handle  AS HANDLE.

DEFINE SHARED VAR p-FlgSit AS CHAR.
DEFINE SHARED VAR p-CodPro AS CHAR.


DEFINE VAR x-ImpNac AS DEC NO-UNDO.
DEFINE VAR x-ImpUsa AS DEC NO-UNDO.

DEF BUFFER B-Tickets FOR T-Tickets.

DEFINE VAR s-TicketErrado AS LOG NO-UNDO.
DEFINE VAR pImporte AS DEC NO-UNDO.

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
&Scoped-define INTERNAL-TABLES T-Tickets

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table T-Tickets.NroTck T-Tickets.Valor 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table T-Tickets.NroTck 
&Scoped-define ENABLED-TABLES-IN-QUERY-br_table T-Tickets
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br_table T-Tickets
&Scoped-define QUERY-STRING-br_table FOR EACH T-Tickets WHERE ~{&KEY-PHRASE} NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH T-Tickets WHERE ~{&KEY-PHRASE} NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-br_table T-Tickets
&Scoped-define FIRST-TABLE-IN-QUERY-br_table T-Tickets


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br_table 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-ImpNac FILL-IN-Vales 

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
DEFINE VARIABLE FILL-IN-ImpNac AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 2 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FILL-IN-Vales AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 12 FGCOLOR 15  NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      T-Tickets SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      T-Tickets.NroTck FORMAT "x(30)":U WIDTH 26.43
      T-Tickets.Valor FORMAT "->>,>>9.99":U WIDTH 10.43
  ENABLE
      T-Tickets.NroTck
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 43 BY 16.42
         FONT 4.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br_table AT ROW 1.12 COL 1
     FILL-IN-ImpNac AT ROW 17.69 COL 9 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     FILL-IN-Vales AT ROW 17.69 COL 28 COLON-ALIGNED NO-LABEL WIDGET-ID 2
     "v2" VIEW-AS TEXT
          SIZE 3 BY .62 AT ROW 18.31 COL 1 WIDGET-ID 8
          FONT 4
     "...." VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 17.73 COL 2 WIDGET-ID 6
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
      TABLE: T-Tickets T "SHARED" NO-UNDO INTEGRAL VtaDTickets
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
         HEIGHT             = 17.96
         WIDTH              = 45.86.
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
/* BROWSE-TAB br_table TEXT-2 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-ImpNac IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-Vales IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _TblList          = "Temp-Tables.T-Tickets"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _FldNameList[1]   > Temp-Tables.T-Tickets.NroTck
"T-Tickets.NroTck" ? "x(30)" "character" ? ? ? ? ? ? yes ? no no "26.43" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.T-Tickets.Valor
"T-Tickets.Valor" ? ? "decimal" ? ? ? ? ? ? no ? no no "10.43" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Calcula-Totales B-table-Win 
PROCEDURE Calcula-Totales :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
ASSIGN
    FILL-IN-Vales = 0.
FOR EACH B-Tickets:
    FILL-IN-Vales = FILL-IN-Vales + B-Tickets.Valor.
END.
DISPLAY
    FILL-IN-Vales WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Captura-Valores B-table-Win 
PROCEDURE Captura-Valores :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF INPUT PARAMETER pImpNac AS DEC.
DEF INPUT PARAMETER pImpUsa AS DEC.

ASSIGN
    x-Impnac = pImpNac
    x-ImpUsa = pImpUsa
    FILL-IN-ImpNac = pImpNac.
DISPLAY
    FILL-IN-ImpNac WITH FRAME {&FRAME-NAME}.


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-add-record B-table-Win 
PROCEDURE local-add-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  ASSIGN
      s-TicketErrado = NO
      pImporte = 0.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN Cambia-Estado IN lh_handle ('2').

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
  IF AVAILABLE VtaCTickets THEN BUFFER-COPY VtaCTickets TO T-Tickets.
  ASSIGN
      T-Tickets.NroTck = T-Tickets.NroTck:SCREEN-VALUE IN BROWSE {&browse-name}
      T-Tickets.Valor = DECIMAL(T-Tickets.Valor:SCREEN-VALUE IN BROWSE {&browse-name}).
  RUN Calcula-Totales.
  RUN Cambia-Estado IN lh_handle ('1').

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-cancel-record B-table-Win 
PROCEDURE local-cancel-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN Cambia-Estado IN lh_handle ('1').

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

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN Calcula-Totales.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize B-table-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  ASSIGN
      FILL-IN-ImpNac = x-ImpNac
      FILL-IN-Vales = 0.
  FOR EACH T-Tickets:
    FILL-IN-Vales = FILL-IN-Vales + T-Tickets.Valor.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

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
  {src/adm/template/snd-list.i "T-Tickets"}

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

/* VERIFICAMOS EL VALE */
DEF VAR x-Ok AS LOG INIT NO NO-UNDO.
DEF VAR x-CodBarra AS CHAR NO-UNDO.
DEF VAR x-Producto AS CHAR NO-UNDO.
DEF VAR x-Ano AS CHAR NO-UNDO.
DEF VAR x-Mes AS CHAR NO-UNDO.
DEF VAR x-Dia AS CHAR NO-UNDO.
DEF VAR x-IniMes AS DATE NO-UNDO.
DEF VAR x-FinMes AS DATE NO-UNDO.
DEF VAR x-FchVto AS DATE FORMAT '99/99/9999' NO-UNDO.
DEF VAR x-NroTck AS CHAR NO-UNDO.
DEF VAR x-NroTckExtraviado AS INT NO-UNDO.  /* Ic 20Feb2013*/
DEF VAR x-Valor AS DEC NO-UNDO.
DEF VAR x-Digito AS INT NO-UNDO.

DEFINE VAR x-tckok AS LOG.

/* Ic - 01Feb206 - Validar la Exitencia del Vales */
DEFINE VAR lValidarVale AS CHAR.

/*Peruana de Vales */
DEF VAR lEmisor AS CHAR.
DEF VAR lTipoVale AS CHAR.

x-CodBarra = T-Tickets.NroTck:SCREEN-VALUE IN BROWSE {&browse-name}.

x-tckok = NO.
x-Ok = NO.

/* Ic - 31Ago2017, Para Wabu */
IF LENGTH(x-CodBarra) = 16 THEN DO:    
    DEFINE VAR lNroTck AS CHAR.
    DEFINE VAR lValorTck AS CHAR.

    lNroTck = SUBSTRIN(x-codbarra,1,9).
    lValorTck = SUBSTRIN(x-codbarra,10,7).
    
    DEFINE BUFFER w-vtatabla FOR vtatabla.

    FIND FIRST w-vtatabla WHERE w-vtatabla.codcia = s-codcia AND 
                                w-vtatabla.tabla = 'VUTILEXTCK' AND 
                                w-vtatabla.llave_c5 = lNroTck AND 
                                w-vtatabla.llave_c4 = lValorTck NO-LOCK NO-ERROR.
    IF AVAILABLE w-vtatabla THEN x-CodBarra = w-vtatabla.llave_c1.

    RELEASE w-vtatabla.
    /*MESSAGE  x-CodBarra.*/
END.

/*MESSAGE p-CodPro.*/
/* Barremos todos los productos del proveedor */
FOR EACH Vtactickets NO-LOCK WHERE Vtactickets.codcia = s-codcia
    AND VtaCTickets.CodPro = p-CodPro 
    AND TODAY >= VtaCTickets.FchIni
    AND TODAY <= VtaCTickets.FchFin:
    /*MESSAGE "Vtactickes".*/
    /* Determinamos cual es el producto */
    IF VtaCTickets.Pos_Producto = '9999' THEN NEXT.    
    IF LENGTH(x-CodBarra) <> VtaCTickets.Longitud THEN NEXT.    

    x-Producto = SUBSTRING(x-CodBarra, INTEGER(SUBSTRING(VtaCTickets.Pos_Producto,1,2)), INTEGER(SUBSTRING(VtaCTickets.Pos_Producto,3,2))).
    IF VtaCTickets.Producto <> x-Producto THEN NEXT.
    /*MESSAGE "Valida".*/
    /*MESSAGE 'Emisor 33333333'VIEW-AS ALERT-BOX ERROR.*/
    /* Fecha de vencimiento */
    IF VtaCTickets.Pos_DayVcto = '9999' THEN x-Dia = STRING(DAY(TODAY), '99').
    ELSE x-Dia = SUBSTRING(x-CodBarra, INTEGER(SUBSTRING(VtaCTickets.Pos_DayVcto,1,2)), INTEGER(SUBSTRING(VtaCTickets.Pos_DayVcto,3,2))).
    IF VtaCTickets.Pos_MonthVcto = '9999' THEN x-Mes = STRING(MONTH(TODAY), '99').
    ELSE x-Mes = SUBSTRING(x-CodBarra, INTEGER(SUBSTRING(VtaCTickets.Pos_MonthVcto,1,2)), INTEGER(SUBSTRING(VtaCTickets.Pos_MonthVcto,3,2))).
    IF VtaCTickets.Pos_YearVcto = '9999' THEN x-Ano = STRING(YEAR(TODAY), '9999').
    ELSE x-Ano = SUBSTRING(x-CodBarra, INTEGER(SUBSTRING(VtaCTickets.Pos_YearVcto,1,2)), INTEGER(SUBSTRING(VtaCTickets.Pos_YearVcto,3,2))).
    IF LENGTH(x-Ano) = 2 THEN x-Ano = '20' + x-Ano.
    RUN src/bin/_dateif (INTEGER(x-Mes), INTEGER(x-Ano), OUTPUT x-IniMes, OUTPUT x-FinMes).
    IF INTEGER(x-Dia) > DAY(x-FinMes) THEN x-Dia = STRING (DAY(x-FinMes), '99').
    x-FchVto = DATE (INTEGER(x-Mes), INTEGER(x-Dia), INTEGER(x-Ano)).
    IF TODAY > x-FchVto THEN DO:
        MESSAGE 'El Ticket venci� el' x-FchVto VIEW-AS ALERT-BOX ERROR.
        RETURN 'ADM-ERROR'.
    END.
    
    /* Ic 28Feb2013, valida codigo de barra para PERUANA DE VALES */
    IF p-CodPro="50725898" THEN DO:        
        lEmisor = substring(x-CodBarra,1,2).
        lTipoVale = substring(x-CodBarra,3,2).
        IF lEmisor <> '02' THEN DO:
            MESSAGE 'Emisor INCORRECTO' VIEW-AS ALERT-BOX ERROR.
            RETURN 'ADM-ERROR'.
        END.
        IF lEmisor <> '02' THEN DO:
            MESSAGE 'Tipo de Vale INCORRECTO' VIEW-AS ALERT-BOX ERROR.
            RETURN 'ADM-ERROR'.
        END.
    END.
    /* Ic 28Feb2013 - FIN */
    /* Digito verificador */
    IF VtaCTickets.Pos_Verif1 <> '9999' AND VtaCTickets.Prog_Verif1 <> '' THEN DO:
        RUN VALUE (VtaCTickets.Prog_Verif1) (x-CodBarra, OUTPUT x-Digito).
        
        IF x-Digito = -1 THEN DO:
            MESSAGE 'Error en el c�digo de barra, volver a pasar el ticket'
                VIEW-AS ALERT-BOX ERROR.
            APPLY 'entry' TO T-Tickets.NroTck.
            RETURN 'ADM-ERROR'.
        END.
        IF INTEGER ( SUBSTRING(x-CodBarra, INTEGER(SUBSTRING(VtaCTickets.Pos_Verif1,1,2)), INTEGER(SUBSTRING(VtaCTickets.Pos_Verif1,3,2))) )
            <> x-Digito THEN DO:
            MESSAGE 'Error en el d�gito verificador'
                VIEW-AS ALERT-BOX ERROR.
            APPLY 'entry' TO T-Tickets.NroTck.
            RETURN 'ADM-ERROR'.
        END.
    END.
    /* RHC 14/01/2016 Segundo digito verificador */
    IF VtaCTickets.Pos_Verif2 <> '9999' AND VtaCTickets.Prog_Verif2 <> '' THEN DO:
        /* Tomamos TODOS los valores justo antes del digito verificado */
        RUN VALUE (VtaCTickets.Prog_Verif2) (SUBSTRING(x-CodBarra, 1, INTEGER(SUBSTRING(VtaCTickets.Pos_Verif2,1,2)) - 1 ), OUTPUT x-Digito).
        
        IF x-Digito = -1 THEN DO:
            MESSAGE 'Error en el c�digo de barra, volver a pasar el ticket'
                VIEW-AS ALERT-BOX ERROR.
            APPLY 'entry' TO T-Tickets.NroTck.
            RETURN 'ADM-ERROR'.
        END.
        IF INTEGER ( SUBSTRING(x-CodBarra, INTEGER(SUBSTRING(VtaCTickets.Pos_Verif2,1,2)), INTEGER(SUBSTRING(VtaCTickets.Pos_Verif2,3,2))) )
            <> x-Digito THEN DO:
            MESSAGE 'Error en el d�gito verificador 2'
                VIEW-AS ALERT-BOX ERROR.
            APPLY 'entry' TO T-Tickets.NroTck.
            RETURN 'ADM-ERROR'.
        END.
    END.

    /* Ic - 01Feb2016, valida la existencia del vale */
    IF Vtactickets.libre_c02 = 'SI' THEN DO:
        DEFINE BUFFER h-vtatabla FOR vtatabla.
 
        FIND FIRST h-vtatabla WHERE h-vtatabla.codcia  = s-codcia AND 
                                    h-vtatabla.tabla = 'VUTILEXTCK' AND 
                                    h-vtatabla.llave_c1 = x-CodBarra NO-LOCK NO-ERROR.

        IF NOT AVAILABLE h-vtatabla THEN DO:
            MESSAGE 'Ticket no existe'
                VIEW-AS ALERT-BOX ERROR.
            APPLY 'entry' TO T-Tickets.NroTck.
            RELEASE h-vtatabla.
            RETURN 'ADM-ERROR'.
        END.
        IF h-vtatabla.libre_c01 = '' OR h-vtatabla.libre_c01 = ? THEN DO:
            MESSAGE 'Ticket aun no esta activado'
                VIEW-AS ALERT-BOX ERROR.
            APPLY 'entry' TO T-Tickets.NroTck.
            RELEASE h-vtatabla.
            RETURN 'ADM-ERROR'.
        END.

        RELEASE h-vtatabla.
    END.

    /* capturamos los valores */
    ASSIGN
        x-NroTck = SUBSTRING(x-CodBarra, INTEGER(SUBSTRING(VtaCTickets.Pos_NroTck,1,2)), INTEGER(SUBSTRING(VtaCTickets.Pos_NroTck,3,2)))
        x-Valor = DECIMAL (SUBSTRING(x-CodBarra, INTEGER(SUBSTRING(VtaCTickets.Pos_Valor,1,2)), INTEGER(SUBSTRING(VtaCTickets.Pos_Valor,3,2))) ) / 100.

    /* Ic 20Feb2013 - Validar que Tickets extraviados no pasen */
    x-NroTckExtraviado = INTEGER(x-NroTck).
    IF x-NroTckExtraviado = 112322 OR x-NroTckExtraviado = 112323 OR x-NroTckExtraviado = 112324 
        OR x-NroTckExtraviado = 112261 OR x-NroTckExtraviado = 112262 OR x-NroTckExtraviado = 112415  
        OR x-NroTckExtraviado = 111301 OR x-NroTckExtraviado = 111302 OR x-NroTckExtraviado = 111303
        OR x-NroTckExtraviado = 111304
        THEN DO:
        MESSAGE 'El ticket esta registrado como EXTRAVIADO...Tenga CUIDADO'
            VIEW-AS ALERT-BOX ERROR.
        APPLY 'entry' TO T-Tickets.NroTck.
        RETURN 'ADM-ERROR'.
    END.
    /* Ic 20Feb2013 */

    /* Ic - 09Mar2015 Vales etraviados para rechazar */
    DEFINE BUFFER b-vtatabla FOR vtatabla.    

    FIND FIRST b-vtatabla WHERE b-vtatabla.codcia = s-codcia AND b-vtatabla.tabla = 'VUTILEX-EXTRAVIADOS' AND
            b-vtatabla.llave_c1 = x-NroTck NO-LOCK NO-ERROR.
    IF AVAILABLE b-vtatabla THEN DO:
        RELEASE b-vtatabla.
        MESSAGE 'El ticket esta registrado como EXTRAVIADO...Tenga CUIDADO'
            VIEW-AS ALERT-BOX ERROR.
        APPLY 'entry' TO T-Tickets.NroTck.
        RETURN 'ADM-ERROR'.
    END.

    RELEASE b-vtatabla.
    /* Ic - 09Mar2015*/

    ASSIGN
        T-Tickets.NroTck:SCREEN-VALUE IN BROWSE {&browse-name} = x-NroTck
        T-Tickets.Valor:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(x-Valor).
    /* NO repetido */
    IF CAN-FIND(FIRST T-Tickets WHERE T-Tickets.NroTck = x-NroTck NO-LOCK) THEN DO:
        MESSAGE 'Ticket repetido' VIEW-AS ALERT-BOX ERROR.
        APPLY 'entry' TO T-Tickets.NroTck.
        RETURN 'ADM-ERROR'.
    END.
    /* Que no est� registrado */
    FIND Vtadtickets OF Vtactickets WHERE VTadtickets.nrotck = x-NroTck
        NO-LOCK NO-ERROR.
    IF AVAILABLE vtadtickets THEN DO:
        MESSAGE 'Ticket' x-NroTck 'YA ha sido consumido anteriormente'
            VIEW-AS ALERT-BOX ERROR.
        APPLY 'entry' TO T-Tickets.NroTck.
        RETURN 'ADM-ERROR'.
    END.

    /* Ic - 16Jun2017, no aceptar vales con valor CERO */
    IF x-valor <= 0 THEN DO:
        MESSAGE 'Ticket' x-NroTck 'NO debe tener importe CERO'
            VIEW-AS ALERT-BOX ERROR.
        APPLY 'entry' TO T-Tickets.NroTck.
        RETURN 'ADM-ERROR'.

    END.

    x-Ok = YES.     /* <<< OJO: Ticket CORRECTO */
    LEAVE.          /* Salir de la Tabla  */
END.

IF x-Ok = NO AND p-CodPro="10003814" THEN DO:
    /* Ic - 09Mar2015 Vales etraviados para rechazar */
    DEFINE BUFFER c-vtatabla FOR vtatabla.    

    FIND FIRST c-vtatabla WHERE c-vtatabla.codcia = s-codcia AND c-vtatabla.tabla = 'VUTILEX-EXTRAVIADOS' AND
                c-vtatabla.llave_c1 = x-CodBarra NO-LOCK NO-ERROR.

    IF AVAILABLE c-vtatabla THEN DO:
        RELEASE c-vtatabla.
        MESSAGE 'El ticket esta registrado como EXTRAVIADO...Tenga CUIDADO(**)'
            VIEW-AS ALERT-BOX ERROR.
        APPLY 'entry' TO T-Tickets.NroTck.
        RETURN 'ADM-ERROR'.
    END.

    RELEASE c-vtatabla.
    /* Ic - 09Mar2015*/

    /* SI POR ALGUN MOTIVO HAY UN ERROR EN LAS BARRAS TRATA DE INGRESARLO MANUALMENTE Solo para CONTINENTAL */
    RUN vta2/d-valeerrado (x-CodBarra, OUTPUT pImporte).
    IF pImporte = 0 THEN DO:
        MESSAGE 'Ticket NO v�lido....' VIEW-AS ALERT-BOX ERROR.
        APPLY 'ENTRY' TO T-Tickets.NroTck.
        RETURN "ADM-ERROR".
    END.
    /* Asignamos el valor del ticket y continuamos con la rutina */
    ASSIGN
        s-TicketErrado = YES
        T-Tickets.Valor:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(pImporte).
END.

/* Que no supere el importe */
DEF VAR x-Total AS DEC NO-UNDO.

/*x-Total = DECIMAL(T-Tickets.Valor:SCREEN-VALUE IN BROWSE {&browse-name}).*/
x-valor = DECIMAL(T-Tickets.Valor:SCREEN-VALUE IN BROWSE {&browse-name}).

/* Ic - 16Jun2017, no aceptar vales con valor CERO */
IF x-valor <= 0 THEN DO:
    MESSAGE 'Ticket' x-NroTck 'NO debe tener importe CERO'
        VIEW-AS ALERT-BOX ERROR.
    APPLY 'entry' TO T-Tickets.NroTck.
    RETURN 'ADM-ERROR'.
END.

/* Totales */
x-total = 0.
FOR EACH B-Tickets NO-LOCK:
    x-Total = x-Total + B-Tickets.Valor.
END.
IF x-Total > x-ImpNac THEN DO:
    MESSAGE 'El importe de los vales supera la venta'
        VIEW-AS ALERT-BOX ERROR.
    APPLY 'entry' TO T-Tickets.NroTck.
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
RETURN "ADM-ERROR".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

