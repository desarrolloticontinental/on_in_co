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
DEF SHARED VAR s-codcia AS INT.
DEF SHARED VAR s-coddiv AS CHAR.

/* Local Variable Definitions ---                                       */
DEF VAR s-coddoc AS CHAR INIT 'COT' NO-UNDO.
DEF VAR s-flgest AS CHAR INIT 'P' NO-UNDO.
/* DEF VAR FILL-IN-CodCli AS CHAR NO-UNDO. */
/* DEF VAR FILL-IN-NomCli AS CHAR NO-UNDO. */
/* DEF VAR txtOrdenCompra AS CHAR NO-UNDO. */

&SCOPED-DEFINE Condicion ( FacCPedi.CodCia = s-codcia ~
AND FacCPedi.CodDoc = s-coddoc ~
AND FacCPedi.CodDiv = s-coddiv ~
AND FacCPedi.FlgEst = s-flgest ~
AND (TRUE <> (FILL-IN-CodCli > '') OR FacCPedi.CodCli = FILL-IN-CodCli) ~
AND (TRUE <> (FILL-IN-NomCli > '') OR INDEX(FacCPedi.NomCli, FILL-IN-NomCli) > 0) ~
AND (TRUE <> (txtOrdenCompra > '') OR Faccpedi.ordcmp BEGINS txtOrdenCompra) ~
)

    /*AND FacCPedi.FchVen >= ( TODAY - 120 ) ~*/

DEFINE BUFFER x-factabla FOR factabla.

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
&Scoped-define INTERNAL-TABLES FacCPedi

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table FacCPedi.CodDoc FacCPedi.NroPed ~
FacCPedi.FchPed FacCPedi.fchven FacCPedi.FchEnt FacCPedi.CodCli ~
FacCPedi.NomCli FacCPedi.ImpTot FacCPedi.FmaPgo 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table 
&Scoped-define QUERY-STRING-br_table FOR EACH FacCPedi WHERE ~{&KEY-PHRASE} ~
      AND {&Condicion} NO-LOCK ~
    BY FacCPedi.FchPed DESCENDING ~
       BY FacCPedi.NroPed DESCENDING
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH FacCPedi WHERE ~{&KEY-PHRASE} ~
      AND {&Condicion} NO-LOCK ~
    BY FacCPedi.FchPed DESCENDING ~
       BY FacCPedi.NroPed DESCENDING.
&Scoped-define TABLES-IN-QUERY-br_table FacCPedi
&Scoped-define FIRST-TABLE-IN-QUERY-br_table FacCPedi


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-56 txtOrdenCompra BUTTON-11 ~
FILL-IN-CodCli FILL-IN-NomCli br_table 
&Scoped-Define DISPLAYED-OBJECTS txtOrdenCompra FILL-IN-CodCli ~
FILL-IN-NomCli 

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
DEFINE BUTTON BUTTON-11 
     LABEL "Aplicar Filtro" 
     SIZE 15 BY 1.12.

DEFINE VARIABLE FILL-IN-CodCli AS CHARACTER FORMAT "X(11)":U 
     LABEL "Filtrar por C�digo del cliente" 
     VIEW-AS FILL-IN 
     SIZE 11.72 BY .81 NO-UNDO.

DEFINE VARIABLE FILL-IN-NomCli AS CHARACTER FORMAT "X(256)":U 
     LABEL "Filtrar por Nombre del Cliente" 
     VIEW-AS FILL-IN 
     SIZE 54 BY .81 NO-UNDO.

DEFINE VARIABLE txtOrdenCompra AS CHARACTER FORMAT "X(15)":U 
     LABEL "Filtrar por O/C Sup.Mercados Peruanos" 
     VIEW-AS FILL-IN 
     SIZE 13.86 BY .81 NO-UNDO.

DEFINE RECTANGLE RECT-56
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 128 BY 2.42
     BGCOLOR 8 FGCOLOR 0 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      FacCPedi SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      FacCPedi.CodDoc COLUMN-LABEL "Doc" FORMAT "x(3)":U
      FacCPedi.NroPed COLUMN-LABEL "Numero" FORMAT "X(12)":U WIDTH 10
      FacCPedi.FchPed COLUMN-LABEL "Fecha de!Emisi�n" FORMAT "99/99/9999":U
      FacCPedi.fchven COLUMN-LABEL "Fecha de!Vencimiento" FORMAT "99/99/9999":U
      FacCPedi.FchEnt COLUMN-LABEL "Fecha de!Entrega" FORMAT "99/99/9999":U
      FacCPedi.CodCli COLUMN-LABEL "Cliente" FORMAT "x(11)":U WIDTH 12.14
      FacCPedi.NomCli FORMAT "x(100)":U WIDTH 50.57
      FacCPedi.ImpTot FORMAT "->>,>>>,>>9.99":U
      FacCPedi.FmaPgo COLUMN-LABEL "Condicion de!Venta" FORMAT "X(8)":U
            WIDTH 9.43
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 128 BY 19.65
         FONT 4 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     txtOrdenCompra AT ROW 1.54 COL 66.14 COLON-ALIGNED WIDGET-ID 24
     BUTTON-11 AT ROW 1.54 COL 83 WIDGET-ID 28
     FILL-IN-CodCli AT ROW 1.62 COL 26 COLON-ALIGNED WIDGET-ID 20
     FILL-IN-NomCli AT ROW 2.42 COL 26 COLON-ALIGNED WIDGET-ID 22
     br_table AT ROW 3.73 COL 2
     RECT-56 AT ROW 1.27 COL 2 WIDGET-ID 26
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 4 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
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
         HEIGHT             = 23.23
         WIDTH              = 139.14.
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
/* BROWSE-TAB br_table FILL-IN-NomCli F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _TblList          = "INTEGRAL.FacCPedi"
     _Options          = "NO-LOCK KEY-PHRASE"
     _OrdList          = "INTEGRAL.FacCPedi.FchPed|no,INTEGRAL.FacCPedi.NroPed|no"
     _Where[1]         = "{&Condicion}"
     _FldNameList[1]   > INTEGRAL.FacCPedi.CodDoc
"FacCPedi.CodDoc" "Doc" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > INTEGRAL.FacCPedi.NroPed
"FacCPedi.NroPed" "Numero" ? "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > INTEGRAL.FacCPedi.FchPed
"FacCPedi.FchPed" "Fecha de!Emisi�n" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > INTEGRAL.FacCPedi.fchven
"FacCPedi.fchven" "Fecha de!Vencimiento" "99/99/9999" "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > INTEGRAL.FacCPedi.FchEnt
"FacCPedi.FchEnt" "Fecha de!Entrega" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > INTEGRAL.FacCPedi.CodCli
"FacCPedi.CodCli" "Cliente" ? "character" ? ? ? ? ? ? no ? no no "12.14" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > INTEGRAL.FacCPedi.NomCli
"FacCPedi.NomCli" ? ? "character" ? ? ? ? ? ? no ? no no "50.57" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   = INTEGRAL.FacCPedi.ImpTot
     _FldNameList[9]   > INTEGRAL.FacCPedi.FmaPgo
"FacCPedi.FmaPgo" "Condicion de!Venta" ? "character" ? ? ? ? ? ? no ? no no "9.43" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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


&Scoped-define SELF-NAME BUTTON-11
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-11 B-table-Win
ON CHOOSE OF BUTTON-11 IN FRAME F-Main /* Aplicar Filtro */
DO:
    ASSIGN
        FILL-IN-CodCli FILL-IN-NomCli txtOrdenCompra.
    RUN dispatch IN THIS-PROCEDURE ('open-query':U).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Captura-Cotizacion B-table-Win 
PROCEDURE Captura-Cotizacion :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF OUTPUT PARAMETER pCodDoc AS CHAR.
DEF OUTPUT PARAMETER pNroPed AS CHAR.

DEFINE VAR timeout AS INT.
DEFINE VAR timenow AS INT.

ASSIGN
    pCodDoc = ''
    pNroPed = ''.
IF AVAILABLE Faccpedi THEN DO:
    /*
        11Nov2019, indicacion de Daniel Llican a pedido de Lucy Mesia y Max Ramos
    */
    FIND FIRST x-factabla WHERE x-factabla.codcia = s-codcia AND
                                x-factabla.tabla = 'EVENTOS_LISTAS' AND
                                x-factabla.codigo = Faccpedi.libre_c01 NO-LOCK NO-ERROR.
    IF AVAILABLE x-factabla THEN DO:
        MESSAGE 'Cotizaci�n' Faccpedi.nroped ' pertenece a evento, ' SKIP 
                    'por favor comunicarse con el area de Abastecimiento' VIEW-AS ALERT-BOX INFORMATION.
    END.
    ELSE DO:
        ASSIGN pCodDoc = Faccpedi.coddoc 
            pNroPed = Faccpedi.nroped.
    END.

    /* Ic - 23Abr2020, para casos de ventas x Whatsapp tiene un limite de tiempo */
    DEF VAR TimeLimit AS CHAR NO-UNDO.
    FIND FIRST x-factabla WHERE x-factabla.codcia = s-codcia AND
        x-factabla.tabla = 'GN-DIVI' AND
        x-factabla.codigo = faccpedi.coddiv AND 
        x-factabla.campo-l[2] = YES 
        NO-LOCK NO-ERROR.
    IF AVAILABLE x-factabla THEN DO:
        /* RHC 21/05/2020 Ahora tiene horas y/o hora tope */
        /* Pasada esa hora NO vale la Cotizaci�n */
        TimeLimit = ''.
        IF x-FacTabla.campo-c[1] > '' AND x-FacTabla.campo-c[1] > '0000' THEN DO:
            TimeLimit = STRING(x-FacTabla.campo-c[1], 'XX:XX').
            IF STRING(TIME, 'HH:MM') > TimeLimit THEN DO:
                MESSAGE 'Cotizaci�n ' Faccpedi.nroped ' , ya pasaron las' TimeLimit 'horas' SKIP 
                    'por favor comunicarse con el area de ventas' VIEW-AS ALERT-BOX INFORMATION.
                ASSIGN
                    pCodDoc = ''
                    pNroPed = ''.
                RETURN.
            END.
        END.

        TimeOut = (x-FacTabla.Valor[1] * 3600).       /* En segundos */
        TimeNow = (TODAY - Faccpedi.FchPed) * 24 * 3600.
        TimeNow = TimeNow + TIME - ( (INTEGER(SUBSTRING(faccpedi.Hora, 1, 2)) * 3600) +
                  (INTEGER(SUBSTRING(faccpedi.Hora, 4, 2)) * 60) ).
        IF TimeOut > 0 THEN DO:
            ASSIGN
                pCodDoc = ''
                pNroPed = ''.

            IF TimeNow <= TimeOut THEN DO: /* Dentro de la valides */
                ASSIGN pCodDoc = Faccpedi.coddoc pNroPed = Faccpedi.nroped.
            END.
            ELSE DO:
                MESSAGE 'Cotizaci�n ' Faccpedi.nroped ' , ya vencio su tiempo de espera' SKIP 
                            'por favor comunicarse con el area de ventas' VIEW-AS ALERT-BOX INFORMATION.

            END.
        END.
    END.
    /*
    ELSE DO:
        ASSIGN pCodDoc = Faccpedi.coddoc 
            pNroPed = Faccpedi.nroped.
    END.
    */
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
  {src/adm/template/snd-list.i "FacCPedi"}

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

