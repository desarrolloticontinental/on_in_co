&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          integral         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
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
DEFINE SHARED VARIABLE lh_Handle  AS HANDLE.
DEFINE VAR X-MARGEN AS DECI.
DEFINE SHARED VAR s-codcia AS int.
DEFINE NEW SHARED VAR lCodMat AS CHAR.
DEFINE NEW SHARED VAR lNroOC AS INT.


DEF VAR X-equival AS DECI INIT 0.
DEF VAR X-UNDMIN AS CHAR INIT "".
DEF VAR X-precon AS DECI INIT 0.
DEF VAR X-atender AS DEC INIT 0.

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
&Scoped-define EXTERNAL-TABLES LG-COCmp
&Scoped-define FIRST-EXTERNAL-TABLE LG-COCmp


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR LG-COCmp.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES LG-DOCmp Almmmatg

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table LG-DOCmp.Codmat Almmmatg.DesMat ~
Almmmatg.DesMar Almmmatg.TipArt LG-DOCmp.UndCmp LG-DOCmp.CanPedi ~
LG-DOCmp.CanAten ~
(IF LG-DOCmp.CanPedi - LG-DOCmp.CanAten > 0 THEN LG-DOCmp.CanPedi - LG-DOCmp.CanAten ELSE 0)  @ X-atender ~
LG-DOCmp.PreUni LG-DOCmp.ImpTot LG-DOCmp.Dsctos[1] LG-DOCmp.Dsctos[2] ~
LG-DOCmp.Dsctos[3] LG-DOCmp.IgvMat X-MARGEN @ X-MARGEN x-undmin @ x-undmin 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table 
&Scoped-define QUERY-STRING-br_table FOR EACH LG-DOCmp WHERE LG-DOCmp.CodCia = LG-COCmp.CodCia ~
  AND LG-DOCmp.TpoDoc = LG-COCmp.TpoDoc ~
  AND LG-DOCmp.NroDoc = LG-COCmp.NroDoc ~
  AND LG-DOCmp.CodDiv = LG-COCmp.CodDiv NO-LOCK, ~
      EACH Almmmatg OF LG-DOCmp NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH LG-DOCmp WHERE LG-DOCmp.CodCia = LG-COCmp.CodCia ~
  AND LG-DOCmp.TpoDoc = LG-COCmp.TpoDoc ~
  AND LG-DOCmp.NroDoc = LG-COCmp.NroDoc ~
  AND LG-DOCmp.CodDiv = LG-COCmp.CodDiv NO-LOCK, ~
      EACH Almmmatg OF LG-DOCmp NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-br_table LG-DOCmp Almmmatg
&Scoped-define FIRST-TABLE-IN-QUERY-br_table LG-DOCmp
&Scoped-define SECOND-TABLE-IN-QUERY-br_table Almmmatg


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br_table RECT-26 
&Scoped-Define DISPLAYED-OBJECTS F-ImpBrt F-ImpDes F-ValVta F-ImpIgv ~
F-ImpIsc F-ImpExo F-ImpTot F-ImpAtendido 

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
DEFINE VARIABLE F-ImpAtendido AS DECIMAL FORMAT "Z,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY .69
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE F-ImpBrt AS DECIMAL FORMAT "Z,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY .69
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE F-ImpDes AS DECIMAL FORMAT "Z,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY .69
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE F-ImpExo AS DECIMAL FORMAT "Z,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY .69
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE F-ImpIgv AS DECIMAL FORMAT "Z,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY .69
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE F-ImpIsc AS DECIMAL FORMAT "Z,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY .69
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE F-ImpTot AS DECIMAL FORMAT "Z,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY .69
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE F-ValVta AS DECIMAL FORMAT "Z,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY .69
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE RECTANGLE RECT-26
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 85 BY 1.42.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      LG-DOCmp, 
      Almmmatg SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      LG-DOCmp.Codmat FORMAT "x(8)":U
      Almmmatg.DesMat FORMAT "X(50)":U
      Almmmatg.DesMar COLUMN-LABEL "Marca" FORMAT "X(15)":U
      Almmmatg.TipArt COLUMN-LABEL "Rotacion" FORMAT "X(1)":U
      LG-DOCmp.UndCmp COLUMN-LABEL "Unid!Cmp." FORMAT "X(4)":U
      LG-DOCmp.CanPedi FORMAT "Z,ZZZ,ZZ9.99":U
      LG-DOCmp.CanAten FORMAT "Z,ZZZ,ZZ9.99":U
      (IF LG-DOCmp.CanPedi - LG-DOCmp.CanAten > 0 THEN LG-DOCmp.CanPedi - LG-DOCmp.CanAten ELSE 0)  @ X-atender COLUMN-LABEL "Cantidad! x Atender" FORMAT "Z,ZZZ,ZZ9.99":U
      LG-DOCmp.PreUni FORMAT ">>>,>>9.9999":U
      LG-DOCmp.ImpTot FORMAT ">>,>>>,>>9.99":U
      LG-DOCmp.Dsctos[1] COLUMN-LABEL "Dscto1" FORMAT ">>9.99":U
      LG-DOCmp.Dsctos[2] COLUMN-LABEL "Dscto2" FORMAT ">>9.99":U
      LG-DOCmp.Dsctos[3] COLUMN-LABEL "Dscto3" FORMAT ">>9.99":U
      LG-DOCmp.IgvMat FORMAT ">>9.99":U
      X-MARGEN @ X-MARGEN COLUMN-LABEL "Margen" FORMAT "->>9.99%":U
      x-undmin @ x-undmin COLUMN-LABEL "Unidad" FORMAT "X(5)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 114 BY 9.23
         FONT 4.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br_table AT ROW 1 COL 1
     F-ImpBrt AT ROW 11 COL 1.43 NO-LABEL
     F-ImpDes AT ROW 11 COL 9.72 COLON-ALIGNED NO-LABEL
     F-ValVta AT ROW 11 COL 20 COLON-ALIGNED NO-LABEL
     F-ImpIgv AT ROW 11 COL 30.29 COLON-ALIGNED NO-LABEL
     F-ImpIsc AT ROW 11 COL 40.72 COLON-ALIGNED NO-LABEL
     F-ImpExo AT ROW 11 COL 53 NO-LABEL
     F-ImpTot AT ROW 11 COL 61.29 COLON-ALIGNED NO-LABEL
     F-ImpAtendido AT ROW 11 COL 72 COLON-ALIGNED NO-LABEL WIDGET-ID 2
     "Total Bruto" VIEW-AS TEXT
          SIZE 8 BY .5 AT ROW 10.5 COL 3
     "Total Atendido" VIEW-AS TEXT
          SIZE 9.29 BY .5 AT ROW 10.5 COL 74.57 WIDGET-ID 4
     "T.Exonerado" VIEW-AS TEXT
          SIZE 9.43 BY .5 AT ROW 10.5 COL 53.57
     "Total Importe" VIEW-AS TEXT
          SIZE 9.29 BY .5 AT ROW 10.5 COL 63.86
     "I.G.V." VIEW-AS TEXT
          SIZE 5.14 BY .5 AT ROW 10.5 COL 37
     "I.S.C." VIEW-AS TEXT
          SIZE 4.57 BY .5 AT ROW 10.5 COL 48
     "Valor Venta" VIEW-AS TEXT
          SIZE 8 BY .5 AT ROW 10.5 COL 24
     "T.Descuento" VIEW-AS TEXT
          SIZE 9.43 BY .5 AT ROW 10.5 COL 12.57
     RECT-26 AT ROW 10.42 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 4.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
   External Tables: integral.LG-COCmp
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
         HEIGHT             = 10.96
         WIDTH              = 114.72.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}
{src/adm-vm/method/vmviewer.i}

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
       br_table:NUM-LOCKED-COLUMNS IN FRAME F-Main     = 3.

/* SETTINGS FOR FILL-IN F-ImpAtendido IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F-ImpBrt IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN F-ImpDes IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F-ImpExo IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN F-ImpIgv IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F-ImpIsc IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F-ImpTot IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F-ValVta IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _TblList          = "integral.LG-DOCmp WHERE integral.LG-COCmp <external> ...,integral.Almmmatg OF integral.LG-DOCmp"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _JoinCode[1]      = "LG-DOCmp.CodCia = LG-COCmp.CodCia
  AND LG-DOCmp.TpoDoc = LG-COCmp.TpoDoc
  AND LG-DOCmp.NroDoc = LG-COCmp.NroDoc
  AND LG-DOCmp.CodDiv = LG-COCmp.CodDiv"
     _FldNameList[1]   = integral.LG-DOCmp.Codmat
     _FldNameList[2]   > integral.Almmmatg.DesMat
"Almmmatg.DesMat" ? "X(50)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > integral.Almmmatg.DesMar
"Almmmatg.DesMar" "Marca" "X(15)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > integral.Almmmatg.TipArt
"Almmmatg.TipArt" "Rotacion" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > integral.LG-DOCmp.UndCmp
"LG-DOCmp.UndCmp" "Unid!Cmp." "X(4)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   = integral.LG-DOCmp.CanPedi
     _FldNameList[7]   = integral.LG-DOCmp.CanAten
     _FldNameList[8]   > "_<CALC>"
"(IF LG-DOCmp.CanPedi - LG-DOCmp.CanAten > 0 THEN LG-DOCmp.CanPedi - LG-DOCmp.CanAten ELSE 0)  @ X-atender" "Cantidad! x Atender" "Z,ZZZ,ZZ9.99" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   = integral.LG-DOCmp.PreUni
     _FldNameList[10]   = integral.LG-DOCmp.ImpTot
     _FldNameList[11]   > integral.LG-DOCmp.Dsctos[1]
"LG-DOCmp.Dsctos[1]" "Dscto1" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > integral.LG-DOCmp.Dsctos[2]
"LG-DOCmp.Dsctos[2]" "Dscto2" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > integral.LG-DOCmp.Dsctos[3]
"LG-DOCmp.Dsctos[3]" "Dscto3" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   = integral.LG-DOCmp.IgvMat
     _FldNameList[15]   > "_<CALC>"
"X-MARGEN @ X-MARGEN" "Margen" "->>9.99%" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > "_<CALC>"
"x-undmin @ x-undmin" "Unidad" "X(5)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
ON MOUSE-SELECT-DBLCLICK OF br_table IN FRAME F-Main
DO:

  lCodmat = lg-docmp.codmat.
  lNroOc = lg-docmp.nrodoc.

  RUN lgc\w-atenciones-oc.r.
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


on find of LG-DOCmp do:
find almmmatg where almmmatg.codcia = s-codcia and
                    almmmatg.codmat = LG-DOCmp.codmat no-lock no-error. 
x-margen = 0.
if almmmatg.undbas ne ""  then do:
  if almmmatg.undA ne "" then do:
        find almtconv where almtconv.codunid = almmmatg.undbas and almtconv.codalter = almmmatg.undA no-lock no-error.
        x-equival = almtconv.equival.
        x-undmin = almmmatg.undA.
        X-precon = prevta[2].              
                    
    if almmmatg.undB ne "" then  do:                   
        find almtconv where almtconv.codunid = almmmatg.undbas 
                        and almtconv.codalter = almmmatg.undB 
                        no-lock no-error.
        if avail almtconv and almtconv.equival > x-equival then do:
                        x-equival = almtconv.equival.
                        x-undmin = almmmatg.undB.
                        X-precon = prevta[3].
        end.
    end.                                    

    if almmmatg.undC ne "" then do:                    
        find almtconv where almtconv.codunid = almmmatg.undbas 
                        and almtconv.codalter = almmmatg.undC 
                        no-lock no-error.
        if avail almtconv and  almtconv.equival > x-equival then do:
                        x-equival = almtconv.equival.
                        x-undmin = almmmatg.undC.
                        X-precon = prevta[4].                                   
        end.
    end.
  END.    

    If x-equival ne 0 and x-undmin ne "" then do:
        find almtconv where almtconv.codunid = almmmatg.undbas 
                        and almtconv.codalter = x-undmin 
                        no-lock no-error.
        if almmmatg.tpocmb ne 0 then do:
            if lg-cocmp.codmon ne almmmatg.monvta then do:
                if lg-cocmp.codmon = 1 and almmmatg.monvta = 2 then X-precon = X-precon * Almmmatg.tpocmb.
                if lg-cocmp.codmon = 2 and almmmatg.monvta = 1 then X-precon = X-precon / Almmmatg.tpocmb.
            end.
            
            if avail almtconv then x-margen = ( round( ( X-precon / x-equival ) / ( LG-DOCmp.ImpTot / LG-DOCmp.CanPedi ) ,4 ) - 1 ) * 100.
        end.    
    end.
END.
End.

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
  {src/adm/template/row-list.i "LG-COCmp"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "LG-COCmp"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Importe-Total B-table-Win 
PROCEDURE Importe-Total :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF AVAILABLE LG-COCmp THEN DO WITH FRAME {&FRAME-NAME} :  
   ASSIGN 
       F-ImpDes = LG-COCmp.ImpDto
       F-ImpExo = LG-COCmp.ImpExo 
       F-ImpIgv = LG-COCmp.ImpIgv
       F-ImpIsc = 0
       F-ImpTot = LG-COCmp.ImpTot
       F-ImpBrt = LG-COCmp.ImpBrt
       F-ValVta = LG-COCmp.ImpNet
       f-ImpAtendido = 0.
   FOR EACH lg-docmp OF lg-cocmp NO-LOCK:
       f-ImpAtendido = f-ImpAtendido +  LG-DOCmp.CanAten * (LG-DOCmp.ImpTot / LG-DOCmp.CanPedi).
   END.
   DISPLAY F-ImpDes  F-ImpExo  F-ImpIgv  F-ImpIsc  F-ImpTot  F-ImpBrt  F-ValVta f-ImpAtendido.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Ingresos-Parciales B-table-Win 
PROCEDURE Ingresos-Parciales :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

RUN lgc/w-conord-1 (lg-docmp.codmat, lg-docmp.nrodoc, LG-COCmp.TpoDoc, Lg-cocmp.codalm).

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

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .
  /* Code placed here will execute AFTER standard behavior.    */
  
  RUN Importe-Total.

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
  {src/adm/template/snd-list.i "LG-COCmp"}
  {src/adm/template/snd-list.i "LG-DOCmp"}
  {src/adm/template/snd-list.i "Almmmatg"}

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
  Notes:  EN CASO DE ERROR RETORNAR : RETURN "ADM-ERROR"
------------------------------------------------------------------------------*/
RETURN "OK".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
