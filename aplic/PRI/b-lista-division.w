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

/* Local Variable Definitions ---                                       */
DEF SHARED VAR s-codcia AS INT.

DEF SHARED VAR s-Tabla AS CHAR.
DEF SHARED VAR lh_handle AS HANDLE.

/*&SCOPED-DEFINE Condicion (GN-DIVI.Campo-Log[1] = NO AND LOOKUP(GN-DIVI.Campo-Char[1], 'A,L') > 0)*/

&SCOPED-DEFINE Condicion (GN-DIVI.Campo-Log[1] = NO)

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
&Scoped-define EXTERNAL-TABLES Vtamcanal
&Scoped-define FIRST-EXTERNAL-TABLE Vtamcanal


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR Vtamcanal.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES GN-DIVI VtaCTabla

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table GN-DIVI.CodDiv GN-DIVI.DesDiv ~
GN-DIVI.Campo-Date[1] GN-DIVI.Campo-Date[2] GN-DIVI.FlgDtoCndVta ~
GN-DIVI.FlgDtoClfCli GN-DIVI.FlgDtoVol GN-DIVI.FlgDtoProm GN-DIVI.Libre_c01 ~
GN-DIVI.FlgEmpaque GN-DIVI.FlgMinVenta VtaCTabla.Libre_l01 ~
VtaCTabla.Libre_l02 GN-DIVI.VentaMayorista GN-DIVI.Campo-Log[6] 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table GN-DIVI.Campo-Date[1] ~
GN-DIVI.Campo-Date[2] GN-DIVI.FlgDtoCndVta GN-DIVI.FlgDtoClfCli ~
GN-DIVI.FlgDtoVol GN-DIVI.FlgDtoProm GN-DIVI.Libre_c01 GN-DIVI.FlgEmpaque ~
GN-DIVI.FlgMinVenta VtaCTabla.Libre_l01 VtaCTabla.Libre_l02 ~
GN-DIVI.VentaMayorista GN-DIVI.Campo-Log[6] 
&Scoped-define ENABLED-TABLES-IN-QUERY-br_table GN-DIVI VtaCTabla
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br_table GN-DIVI
&Scoped-define SECOND-ENABLED-TABLE-IN-QUERY-br_table VtaCTabla
&Scoped-define QUERY-STRING-br_table FOR EACH GN-DIVI OF Vtamcanal WHERE ~{&KEY-PHRASE} ~
      AND {&Condicion} NO-LOCK, ~
      FIRST VtaCTabla WHERE VtaCTabla.CodCia = GN-DIVI.CodCia ~
  AND VtaCTabla.Llave = GN-DIVI.CodDiv ~
      AND VtaCTabla.Tabla = s-Tabla NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH GN-DIVI OF Vtamcanal WHERE ~{&KEY-PHRASE} ~
      AND {&Condicion} NO-LOCK, ~
      FIRST VtaCTabla WHERE VtaCTabla.CodCia = GN-DIVI.CodCia ~
  AND VtaCTabla.Llave = GN-DIVI.CodDiv ~
      AND VtaCTabla.Tabla = s-Tabla NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-br_table GN-DIVI VtaCTabla
&Scoped-define FIRST-TABLE-IN-QUERY-br_table GN-DIVI
&Scoped-define SECOND-TABLE-IN-QUERY-br_table VtaCTabla


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
      GN-DIVI, 
      VtaCTabla SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      GN-DIVI.CodDiv COLUMN-LABEL "Divisi�n" FORMAT "x(5)":U
      GN-DIVI.DesDiv FORMAT "X(40)":U WIDTH 25
      GN-DIVI.Campo-Date[1] COLUMN-LABEL "Vig. Desde" FORMAT "99/99/9999":U
      GN-DIVI.Campo-Date[2] COLUMN-LABEL "Vig. Hasta" FORMAT "99/99/9999":U
      GN-DIVI.FlgDtoCndVta COLUMN-LABEL "Descuento!xCond.Vta" FORMAT "yes/no":U
            WIDTH 8.72 COLUMN-FGCOLOR 0 COLUMN-BGCOLOR 11 LABEL-FGCOLOR 0 LABEL-BGCOLOR 11 VIEW-AS TOGGLE-BOX
      GN-DIVI.FlgDtoClfCli COLUMN-LABEL "Descuento!xClsf.Cliente" FORMAT "yes/no":U
            WIDTH 8.43 COLUMN-FGCOLOR 0 COLUMN-BGCOLOR 11 LABEL-FGCOLOR 0 LABEL-BGCOLOR 11 VIEW-AS TOGGLE-BOX
      GN-DIVI.FlgDtoVol COLUMN-LABEL "Descuento!Dcto.xVol" FORMAT "yes/no":U
            WIDTH 7.43 COLUMN-FGCOLOR 0 COLUMN-BGCOLOR 14 VIEW-AS TOGGLE-BOX
      GN-DIVI.FlgDtoProm COLUMN-LABEL "Descuento!Prom." FORMAT "yes/no":U
            WIDTH 7.43 COLUMN-FGCOLOR 0 COLUMN-BGCOLOR 14 VIEW-AS TOGGLE-BOX
      GN-DIVI.Libre_c01 COLUMN-LABEL "Tipo C�lculo" FORMAT "x(20)":U
            VIEW-AS COMBO-BOX INNER-LINES 5
                      LIST-ITEM-PAIRS "EXCLUYENTES","",
                                      "ACUMULADOS","A"
                      DROP-DOWN-LIST 
      GN-DIVI.FlgEmpaque FORMAT "yes/no":U VIEW-AS TOGGLE-BOX
      GN-DIVI.FlgMinVenta COLUMN-LABEL "Min.Venta" FORMAT "Yes/No":U
            VIEW-AS TOGGLE-BOX
      VtaCTabla.Libre_l01 COLUMN-LABEL "Dcto. x Vol.!x Linea y SubLin" FORMAT "yes/no":U
            VIEW-AS TOGGLE-BOX
      VtaCTabla.Libre_l02 COLUMN-LABEL "Dcto. x Vol.!x Saldo" FORMAT "yes/no":U
            VIEW-AS TOGGLE-BOX
      GN-DIVI.VentaMayorista COLUMN-LABEL "Tipo Lista" FORMAT "9":U
            WIDTH 14.43 VIEW-AS COMBO-BOX INNER-LINES 5
                      LIST-ITEM-PAIRS "LISTA UNICA",1,
                                      "LISTA INDIVIDUAL",2
                      DROP-DOWN-LIST 
      GN-DIVI.Campo-Log[6] COLUMN-LABEL "Solo!Invitados" FORMAT "yes/no":U
            VIEW-AS TOGGLE-BOX
  ENABLE
      GN-DIVI.Campo-Date[1]
      GN-DIVI.Campo-Date[2]
      GN-DIVI.FlgDtoCndVta
      GN-DIVI.FlgDtoClfCli
      GN-DIVI.FlgDtoVol
      GN-DIVI.FlgDtoProm
      GN-DIVI.Libre_c01
      GN-DIVI.FlgEmpaque
      GN-DIVI.FlgMinVenta
      VtaCTabla.Libre_l01
      VtaCTabla.Libre_l02
      GN-DIVI.VentaMayorista
      GN-DIVI.Campo-Log[6]
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 159 BY 8.88
         FONT 4
         TITLE "DIVISIONES / LISTAS DE PRECIOS" FIT-LAST-COLUMN.


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
   External Tables: INTEGRAL.Vtamcanal
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
         HEIGHT             = 9.88
         WIDTH              = 159.29.
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
     _TblList          = "INTEGRAL.GN-DIVI OF INTEGRAL.Vtamcanal,INTEGRAL.VtaCTabla WHERE INTEGRAL.GN-DIVI ..."
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = ", FIRST"
     _Where[1]         = "{&Condicion}"
     _JoinCode[2]      = "VtaCTabla.CodCia = GN-DIVI.CodCia
  AND VtaCTabla.Llave = GN-DIVI.CodDiv"
     _Where[2]         = "VtaCTabla.Tabla = s-Tabla"
     _FldNameList[1]   > INTEGRAL.GN-DIVI.CodDiv
"GN-DIVI.CodDiv" "Divisi�n" "x(5)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > INTEGRAL.GN-DIVI.DesDiv
"GN-DIVI.DesDiv" ? ? "character" ? ? ? ? ? ? no ? no no "25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > INTEGRAL.GN-DIVI.Campo-Date[1]
"GN-DIVI.Campo-Date[1]" "Vig. Desde" ? "date" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > INTEGRAL.GN-DIVI.Campo-Date[2]
"GN-DIVI.Campo-Date[2]" "Vig. Hasta" ? "date" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > INTEGRAL.GN-DIVI.FlgDtoCndVta
"GN-DIVI.FlgDtoCndVta" "Descuento!xCond.Vta" ? "logical" 11 0 ? 11 0 ? yes ? no no "8.72" yes no no "U" "" "" "TOGGLE-BOX" "," ? ? 5 no 0 no no
     _FldNameList[6]   > INTEGRAL.GN-DIVI.FlgDtoClfCli
"GN-DIVI.FlgDtoClfCli" "Descuento!xClsf.Cliente" ? "logical" 11 0 ? 11 0 ? yes ? no no "8.43" yes no no "U" "" "" "TOGGLE-BOX" "," ? ? 5 no 0 no no
     _FldNameList[7]   > INTEGRAL.GN-DIVI.FlgDtoVol
"GN-DIVI.FlgDtoVol" "Descuento!Dcto.xVol" ? "logical" 14 0 ? ? ? ? yes ? no no "7.43" yes no no "U" "" "" "TOGGLE-BOX" "," ? ? 5 no 0 no no
     _FldNameList[8]   > INTEGRAL.GN-DIVI.FlgDtoProm
"GN-DIVI.FlgDtoProm" "Descuento!Prom." ? "logical" 14 0 ? ? ? ? yes ? no no "7.43" yes no no "U" "" "" "TOGGLE-BOX" "," ? ? 5 no 0 no no
     _FldNameList[9]   > INTEGRAL.GN-DIVI.Libre_c01
"GN-DIVI.Libre_c01" "Tipo C�lculo" "x(20)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "DROP-DOWN-LIST" "," ? "EXCLUYENTES,,ACUMULADOS,A" 5 no 0 no no
     _FldNameList[10]   > INTEGRAL.GN-DIVI.FlgEmpaque
"GN-DIVI.FlgEmpaque" ? ? "logical" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "TOGGLE-BOX" "," ? ? 5 no 0 no no
     _FldNameList[11]   > INTEGRAL.GN-DIVI.FlgMinVenta
"GN-DIVI.FlgMinVenta" "Min.Venta" "Yes/No" "logical" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "TOGGLE-BOX" "," ? ? 5 no 0 no no
     _FldNameList[12]   > INTEGRAL.VtaCTabla.Libre_l01
"VtaCTabla.Libre_l01" "Dcto. x Vol.!x Linea y SubLin" ? "logical" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "TOGGLE-BOX" "," ? ? 5 no 0 no no
     _FldNameList[13]   > INTEGRAL.VtaCTabla.Libre_l02
"VtaCTabla.Libre_l02" "Dcto. x Vol.!x Saldo" ? "logical" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "TOGGLE-BOX" "," ? ? 5 no 0 no no
     _FldNameList[14]   > INTEGRAL.GN-DIVI.VentaMayorista
"GN-DIVI.VentaMayorista" "Tipo Lista" ? "integer" ? ? ? ? ? ? yes ? no no "14.43" yes no no "U" "" "" "DROP-DOWN-LIST" "," ? "LISTA UNICA,1,LISTA INDIVIDUAL,2" 5 no 0 no no
     _FldNameList[15]   > INTEGRAL.GN-DIVI.Campo-Log[6]
"GN-DIVI.Campo-Log[6]" "Solo!Invitados" ? "logical" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "TOGGLE-BOX" "," ? ? 5 no 0 no no
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
ON ROW-ENTRY OF br_table IN FRAME F-Main /* DIVISIONES / LISTAS DE PRECIOS */
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-LEAVE OF br_table IN FRAME F-Main /* DIVISIONES / LISTAS DE PRECIOS */
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
   {src/adm/template/brsleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON VALUE-CHANGED OF br_table IN FRAME F-Main /* DIVISIONES / LISTAS DE PRECIOS */
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
  {src/adm/template/row-list.i "Vtamcanal"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "Vtamcanal"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Carga-Tabla B-table-Win 
PROCEDURE Carga-Tabla :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  FOR EACH gn-divi OF Vtamcanal NO-LOCK WHERE {&Condicion}:
      FIND VtaCTabla WHERE VtaCTabla.CodCia = s-CodCia AND
          VtaCTabla.Tabla = s-Tabla AND
          VtaCTabla.Llave = gn-divi.coddiv
          NO-LOCK NO-ERROR.
      IF NOT AVAILABLE VtaCTabla THEN DO:
          CREATE VtaCTabla.
          ASSIGN
              VtaCTabla.CodCia = s-CodCia 
              VtaCTabla.Tabla = s-Tabla
              VtaCTabla.Llave = gn-divi.coddiv.
      END.
  END.
  RELEASE VtaCTabla.

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
  RUN Procesa-Handle IN lh_handle ('Enable-Detail').

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
  RUN Procesa-Handle IN lh_handle ('Disable-Detail').

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
  RUN Carga-Tabla.

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
  {src/adm/template/snd-list.i "Vtamcanal"}
  {src/adm/template/snd-list.i "GN-DIVI"}
  {src/adm/template/snd-list.i "VtaCTabla"}

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

IF LOGICAL(GN-DIVI.FlgEmpaque:SCREEN-VALUE IN BROWSE {&browse-name}) = YES AND 
    LOGICAL(GN-DIVI.FlgMinVenta:SCREEN-VALUE IN BROWSE {&browse-name}) = YES THEN DO:
    MESSAGE 'Debe seleccionar o EMPAQUE o MIN. VENTA pero no ambos'
        VIEW-AS ALERT-BOX ERROR.
    APPLY 'ENTRY':U TO GN-DIVI.FlgEmpaque.
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

