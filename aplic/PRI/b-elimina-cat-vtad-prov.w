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
DEFINE SHARED VAR s-codcia AS INT.

DEFINE VARIABLE iNroSec AS INT.

DEFINE BUFFER b-almcatvtad FOR almcatvtad.
DEFINE TEMP-TABLE tt-detalle LIKE almcatvtad.

DEFINE VAR x-nrosec AS INT NO-UNDO.

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
&Scoped-define EXTERNAL-TABLES AlmCatVtaC
&Scoped-define FIRST-EXTERNAL-TABLE AlmCatVtaC


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR AlmCatVtaC.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES AlmCatVtaD

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table AlmCatVtaD.NroSec AlmCatVtaD.codmat ~
AlmCatVtaD.DesMat AlmCatVtaD.Libre_d02 AlmCatVtaD.Libre_d03 ~
AlmCatVtaD.Libre_c04 AlmCatVtaD.Libre_c05 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table 
&Scoped-define QUERY-STRING-br_table FOR EACH AlmCatVtaD OF AlmCatVtaC WHERE ~{&KEY-PHRASE} NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH AlmCatVtaD OF AlmCatVtaC WHERE ~{&KEY-PHRASE} NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-br_table AlmCatVtaD
&Scoped-define FIRST-TABLE-IN-QUERY-br_table AlmCatVtaD


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


/* Menu Definitions                                                     */
DEFINE MENU POPUP-MENU-br_table 
       MENU-ITEM m_Asigna_Secuencia LABEL "Asigna Secuencia".


/* Definitions of the field level widgets                               */
/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      AlmCatVtaD SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      AlmCatVtaD.NroSec COLUMN-LABEL "Sec" FORMAT "9999":U WIDTH 5.43
      AlmCatVtaD.codmat COLUMN-LABEL "Codigo" FORMAT "X(6)":U WIDTH 7.43
      AlmCatVtaD.DesMat FORMAT "X(60)":U WIDTH 36.43
      AlmCatVtaD.Libre_d02 COLUMN-LABEL "M�n. Venta" FORMAT ">>>,>>9.99":U
            WIDTH 8.43
      AlmCatVtaD.Libre_d03 COLUMN-LABEL "Empaque" FORMAT ">>>,>>9.99":U
            WIDTH 6.43
      AlmCatVtaD.Libre_c04 COLUMN-LABEL "CodGrupo" FORMAT "x(10)":U
            WIDTH 7
      AlmCatVtaD.Libre_c05 COLUMN-LABEL "Descripcion Grupo" FORMAT "x(60)":U
            WIDTH 29.72
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 109.57 BY 10.15
         FONT 4
         TITLE "Lista de Art�culos" FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br_table AT ROW 1.08 COL 1.43
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
   External Tables: INTEGRAL.AlmCatVtaC
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
         HEIGHT             = 10.81
         WIDTH              = 116.
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

ASSIGN 
       br_table:POPUP-MENU IN FRAME F-Main             = MENU POPUP-MENU-br_table:HANDLE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _TblList          = "INTEGRAL.AlmCatVtaD OF INTEGRAL.AlmCatVtaC"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _FldNameList[1]   > INTEGRAL.AlmCatVtaD.NroSec
"AlmCatVtaD.NroSec" "Sec" ? "integer" ? ? ? ? ? ? no ? no no "5.43" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > INTEGRAL.AlmCatVtaD.codmat
"AlmCatVtaD.codmat" "Codigo" ? "character" ? ? ? ? ? ? no ? no no "7.43" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > INTEGRAL.AlmCatVtaD.DesMat
"AlmCatVtaD.DesMat" ? "X(60)" "character" ? ? ? ? ? ? no ? no no "36.43" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > INTEGRAL.AlmCatVtaD.Libre_d02
"AlmCatVtaD.Libre_d02" "M�n. Venta" ">>>,>>9.99" "decimal" ? ? ? ? ? ? no ? no no "8.43" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > INTEGRAL.AlmCatVtaD.Libre_d03
"AlmCatVtaD.Libre_d03" "Empaque" ">>>,>>9.99" "decimal" ? ? ? ? ? ? no ? no no "6.43" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > INTEGRAL.AlmCatVtaD.Libre_c04
"AlmCatVtaD.Libre_c04" "CodGrupo" "x(10)" "character" ? ? ? ? ? ? no ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > INTEGRAL.AlmCatVtaD.Libre_c05
"AlmCatVtaD.Libre_c05" "Descripcion Grupo" ? "character" ? ? ? ? ? ? no ? no no "29.72" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
ON ROW-ENTRY OF br_table IN FRAME F-Main /* Lista de Art�culos */
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-LEAVE OF br_table IN FRAME F-Main /* Lista de Art�culos */
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
   {src/adm/template/brsleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON VALUE-CHANGED OF br_table IN FRAME F-Main /* Lista de Art�culos */
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME AlmCatVtaD.codmat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL AlmCatVtaD.codmat br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF AlmCatVtaD.codmat IN BROWSE br_table /* Codigo */
DO:
    DO WITH FRAME {&FRAME-NAME}:
        IF SELF:SCREEN-VALUE = "" THEN RETURN.
        IF LENGTH(SELF:SCREEN-VALUE) <= 6 THEN DO: 
            ASSIGN
                SELF:SCREEN-VALUE = STRING(INTEGER(SELF:SCREEN-VALUE),"999999")
                NO-ERROR.
        END.
        FIND almmmatg WHERE almmmatg.codcia = s-codcia
            AND almmmatg.codmat = almcatvtad.codmat:SCREEN-VALUE IN BROWSE {&BROWSE-name}
            NO-LOCK NO-ERROR.
        IF AVAIL almmmatg THEN DO: 
            DISPLAY almmmatg.desmat @ almcatvtad.desmat
                WITH BROWSE {&BROWSE-NAME}.
        END.
        ELSE DO:
            MESSAGE 'Articulo no registrado'
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            APPLY 'ENTRY' TO AlmCatVtaD.CodMat.
            RETURN NO-APPLY.
        END.            
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Asigna_Secuencia
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Asigna_Secuencia B-table-Win
ON CHOOSE OF MENU-ITEM m_Asigna_Secuencia /* Asigna Secuencia */
DO:
    RUN vtaexp\i-nrosec(OUTPUT x-nrosec).    
    FIND FIRST b-almcatvtad 
        WHERE ROWID(b-almcatvtad) = ROWID(almcatvtad) NO-ERROR.
    IF AVAIL b-almcatvtad THEN DO:
        
        IF /*b-almcatvtad.nrosec = (x-nrosec - 1) 
            OR*/ b-almcatvtad.nrosec < x-nrosec THEN
            ASSIGN b-almcatvtad.libre_d04 = (x-nrosec * 10) + 5.
        ELSE ASSIGN b-almcatvtad.libre_d04 = (x-nrosec - 1) * 10 + 5.        
        
    END.     
    RUN Actualiza-Secuencia.    
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

ON RETURN OF almcatvtad.codmat, almcatvtad.desmat, almcatvtad.libre_c04, almcatvtad.libre_c05  DO:
    APPLY 'TAB'.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Actualiza-Secuencia B-table-Win 
PROCEDURE Actualiza-Secuencia :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE VARIABLE iint AS INTEGER INIT 0 NO-UNDO.

    SESSION:SET-WAIT-STATE('GENERAL').
    
    EMPTY TEMP-TABLE tt-detalle.
    FOR EACH almcatvtad OF almcatvtac:
        CREATE tt-detalle.
        BUFFER-COPY Almcatvtad TO tt-detalle.
        DELETE Almcatvtad.
    END.
    FOR EACH tt-detalle BY tt-detalle.libre_d04:
        iint = iint + 1.
        CREATE Almcatvtad.
        BUFFER-COPY tt-detalle
            TO Almcatvtad
            ASSIGN 
                Almcatvtad.NroSec = iint.
    END.
    FOR EACH Almcatvtad OF Almcatvtac:
        Almcatvtad.libre_d04 = Almcatvtad.nrosec * 10.
    END.
    SESSION:SET-WAIT-STATE('').

    RUN dispatch IN THIS-PROCEDURE ('open-query':U).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  {src/adm/template/row-list.i "AlmCatVtaC"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "AlmCatVtaC"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-statement B-table-Win 
PROCEDURE local-assign-statement :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  DEFINE VARIABLE cDesMat AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cUndVta AS CHARACTER   NO-UNDO.
  DEF VAR pError AS CHAR NO-UNDO.
  DEF VAR k AS INT NO-UNDO.

  /* Actualizamos el cat�logo */
/*   FIND Almmmatg OF Almcatvtad EXCLUSIVE-LOCK NO-ERROR.                 */
/*   IF NOT AVAILABLE Almmmatg THEN DO:                                   */
/*       {lib/mensaje-de-error.i &CuentaError="k" &MensajeError="pError"} */
/*       MESSAGE pError VIEW-AS ALERT-BOX ERROR.                          */
/*       UNDO, RETURN 'ADM-ERROR'.                                        */
/*   END.                                                                 */
/*   /* Actualizamos el Catalogo */                                       */
/*   ASSIGN                                                               */
/*       Almmmatg.StkMax =  0                                             */
/*       Almmmatg.Libre_d03 = 0.                                          */
/*   RELEASE Almmmatg.                                                    */

  FIND LAST b-almcatvtad OF Almcatvtac NO-LOCK NO-ERROR.
  IF AVAIL b-almcatvtad THEN iNroSec = b-almcatvtad.nrosec + 1.
  ELSE iNroSec = 1.  
  FIND almmmatg WHERE almmmatg.codcia = s-codcia
      AND almmmatg.codmat = almcatvtad.codmat:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
      NO-LOCK NO-ERROR.
  IF AVAIL almmmatg THEN 
      ASSIGN 
        cDesMat = almmmatg.desmat
        cUndVta = almmmatg.undbas.
  ELSE 
      ASSIGN 
          cDesMat = ""
          cUndVta = ''.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-statement':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN GET-ATTRIBUTE('ADM-NEW-RECORD').
  IF RETURN-VALUE = 'YES' THEN DO:
      ASSIGN 
          almcatvtad.codcia = almcatvtac.codcia
          almcatvtad.coddiv = almcatvtac.coddiv
          almcatvtad.codpro = almcatvtac.codpro
          almcatvtad.nropag = almcatvtac.nropag
          almcatvtad.nrosec = iNroSec
          almcatvtad.desmat = cDesMat
          almcatvtad.libre_c01 = cUndVta
          almcatvtad.libre_d04 = iNroSec * 10.
      IF almcatvtad.codmat = '' THEN almcatvtad.codmat = almcatvtad.codmat:SCREEN-VALUE IN BROWSE {&BROWSE-name}.
  END.
  ELSE DO:
      FIND FIRST b-almcatvtad WHERE b-almcatvtad.codcia = s-codcia
          AND b-almcatvtad.coddiv = almcatvtad.coddiv
          AND b-almcatvtad.codpro = almcatvtad.codpro
          AND b-almcatvtad.codmat = almcatvtad.codmat:SCREEN-VALUE IN BROWSE {&BROWSE-name} NO-ERROR.

      IF AVAIL b-almcatvtad THEN DO:
          ASSIGN b-almcatvtad.libre_c04 = almcatvtad.libre_c04:SCREEN-VALUE IN BROWSE {&BROWSE-name}
                b-almcatvtad.libre_c05 = almcatvtad.libre_c05:SCREEN-VALUE IN BROWSE {&BROWSE-name}.
      END.

      /*IF AVAIL b-almcatvtad THEN ASSIGN b-almcatvtad.flgqui = almcatvtad.flgqui:SCREEN-VALUE IN BROWSE {&BROWSE-name}.*/

  END.
  /* NO REPETIR EL CODIGO */
  IF CAN-FIND(FIRST b-almcatvtad WHERE b-almcatvtad.codcia = almcatvtac.codcia
              AND b-almcatvtad.coddiv = almcatvtac.coddiv
              AND b-almcatvtad.codmat = almcatvtad.codmat
              AND ROWID(b-almcatvtad) <> ROWID(almcatvtad)
              NO-LOCK)
      THEN DO:
      MESSAGE "Art�culo Repetido con el proveedor " + almcatvtac.codpro SKIP
              "         en la p�gina " + STRING(b-almcatvtad.nropag,"9999")
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY 'ENTRY' TO almcatvtad.codmat.
      UNDO, RETURN 'ADM-ERROR'.
  END.
  /* Actualizamos el Cat�logo General */
  FIND Almmmatg OF Almcatvtad EXCLUSIVE-LOCK NO-ERROR.
  IF NOT AVAILABLE Almmmatg THEN DO:
      {lib/mensaje-de-error.i &MensajeError="pError"}
      MESSAGE pError VIEW-AS ALERT-BOX ERROR.
      UNDO, RETURN 'ADM-ERROR'.
  END.
  /* Actualizamos el Catalogo */
  ASSIGN
      Almmmatg.StkMax =  AlmCatVtaD.Libre_d02 
      Almmmatg.Libre_d03 = AlmCatVtaD.Libre_d03.
  RELEASE Almmmatg.

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
  DEF VAR pError AS CHAR NO-UNDO.

  IF AVAILABLE Almcatvtad THEN DO:
      FIND Almmmatg OF Almcatvtad EXCLUSIVE-LOCK NO-ERROR.
      IF NOT AVAILABLE Almmmatg THEN DO:
          {lib/mensaje-de-error.i &MensajeError="pError"}
          MESSAGE pError VIEW-AS ALERT-BOX ERROR.
          UNDO, RETURN 'ADM-ERROR'.
      END.
      /* Actualizamos el Catalogo */
      ASSIGN
          Almmmatg.StkMax =  0
          Almmmatg.Libre_d03 = 0.
      RELEASE Almmmatg.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

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
  IF RETURN-VALUE = "ADM-ERROR" THEN RETURN "ADM-ERROR".

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
  {src/adm/template/snd-list.i "AlmCatVtaC"}
  {src/adm/template/snd-list.i "AlmCatVtaD"}

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

