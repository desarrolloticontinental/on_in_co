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

/* Shared Variable Definitions ---                                       */
DEFINE SHARED TEMP-TABLE PEDI LIKE FacDPedi.
DEFINE SHARED VARIABLE S-CODCIA  AS INTEGER.
DEFINE SHARED VARIABLE CL-CODCIA AS INTEGER.
DEFINE SHARED VARIABLE S-USER-ID AS CHAR.
DEFINE SHARED VARIABLE S-CODDOC  AS CHAR.
DEFINE SHARED VARIABLE S-CODDIV  AS CHAR.
DEFINE SHARED VARIABLE S-CODCLI  AS CHAR.
DEFINE SHARED VARIABLE S-CODMON  AS INTEGER.
DEFINE SHARED VARIABLE S-CODALM  AS CHAR.
DEFINE SHARED VARIABLE lh_Handle  AS HANDLE.

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE F-FACTOR     AS DECIMAL   NO-UNDO.
DEFINE VARIABLE S-UNDBAS     AS CHARACTER NO-UNDO.
DEFINE VARIABLE I-ListPr     AS INTEGER   NO-UNDO.
DEFINE VARIABLE S-OK         AS LOGICAL   NO-UNDO.
DEFINE VARIABLE I-NroItm     AS INTEGER   NO-UNDO.

DEFINE VARIABLE F-PREVTA LIKE Almmmatg.PreBas NO-UNDO.
DEFINE VARIABLE F-PREBAS LIKE Almmmatg.PreBas NO-UNDO.
DEFINE VARIABLE F-DSCTOS LIKE Almmmatg.PorMax NO-UNDO.
DEFINE VARIABLE F-PorImp LIKE Almmmatg.PreBas NO-UNDO.
DEFINE VARIABLE F-MRGUTI LIKE Almmmatg.MrgUti NO-UNDO.
DEFINE VARIABLE F-DCTPRM LIKE Almmmatg.PorMax NO-UNDO.

DEFINE BUFFER B-PEDI FOR PEDI.

FIND FacCfgGn WHERE FacCfgGn.CodCia = S-CODCIA NO-LOCK NO-ERROR.

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
&Scoped-define INTERNAL-TABLES PEDI Almmmatg

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table PEDI.NroItm PEDI.codmat Almmmatg.DesMat PEDI.UndVta PEDI.CanPed PEDI.PreUni PEDI.PorDto PEDI.PorDto2 PEDI.ImpLin   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table PEDI.codmat ~
PEDI.UndVta ~
PEDI.CanPed ~
PEDI.PorDto ~
PEDI.PorDto2   
&Scoped-define ENABLED-TABLES-IN-QUERY-br_table PEDI
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br_table PEDI
&Scoped-define SELF-NAME br_table
&Scoped-define QUERY-STRING-br_table FOR EACH PEDI  NO-LOCK, ~
             FIRST Almmmatg OF PEDI NO-LOCK BY PEDI.NroItm
&Scoped-define OPEN-QUERY-br_table OPEN QUERY {&SELF-NAME} FOR EACH PEDI  NO-LOCK, ~
             FIRST Almmmatg OF PEDI NO-LOCK BY PEDI.NroItm .
&Scoped-define TABLES-IN-QUERY-br_table PEDI Almmmatg
&Scoped-define FIRST-TABLE-IN-QUERY-br_table PEDI
&Scoped-define SECOND-TABLE-IN-QUERY-br_table Almmmatg


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br_table RECT-11 
&Scoped-Define DISPLAYED-OBJECTS F-TotBrt F-ImpExo F-ImpDes F-ValVta ~
F-ImpIsc F-ImpIgv F-ImpTot 

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
DEFINE VARIABLE F-ImpDes AS DECIMAL FORMAT "Z,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 11 BY .69
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE F-ImpExo AS DECIMAL FORMAT "Z,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 11 BY .69
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE F-ImpIgv AS DECIMAL FORMAT "Z,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 11 BY .69
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE F-ImpIsc AS DECIMAL FORMAT "Z,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 11 BY .69
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE F-ImpTot AS DECIMAL FORMAT "Z,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 11 BY .69
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE F-TotBrt AS DECIMAL FORMAT "Z,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 11 BY .69
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE F-ValVta AS DECIMAL FORMAT "Z,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 11 BY .69
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 87.72 BY 1.42.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      PEDI, 
      Almmmatg SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _FREEFORM
  QUERY br_table NO-LOCK DISPLAY
      PEDI.NroItm  COLUMN-LABEL "No" FORMAT "Z9"
      PEDI.codmat  COLUMN-LABEL "Articulo"
      Almmmatg.DesMat FORMAT "X(47)"
      PEDI.UndVta  COLUMN-LABEL "Unidad"
      PEDI.CanPed  COLUMN-LABEL "Cantidad" FORMAT ">>>,>>>.99"
      PEDI.PreUni  COLUMN-LABEL "Precio!Unitario" FORMAT ">>,>>>.99"
      PEDI.PorDto  COLUMN-LABEL "(1)    !%Dcto."
      PEDI.PorDto2 COLUMN-LABEL "(2)    !%Dcto."
      PEDI.ImpLin  COLUMN-LABEL "Importe" FORMAT ">>>,>>>.99"
   ENABLE
      PEDI.codmat
      PEDI.UndVta
      PEDI.CanPed
      PEDI.PorDto
      PEDI.PorDto2
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 87.72 BY 7.38
         FONT 4.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br_table AT ROW 1 COL 1
     F-TotBrt AT ROW 9.46 COL 2.14 NO-LABEL
     F-ImpExo AT ROW 9.46 COL 14.72 NO-LABEL
     F-ImpDes AT ROW 9.46 COL 26.14 COLON-ALIGNED NO-LABEL
     F-ValVta AT ROW 9.46 COL 38.14 COLON-ALIGNED NO-LABEL
     F-ImpIsc AT ROW 9.46 COL 50.14 COLON-ALIGNED NO-LABEL
     F-ImpIgv AT ROW 9.46 COL 62 COLON-ALIGNED NO-LABEL
     F-ImpTot AT ROW 9.46 COL 74.29 COLON-ALIGNED NO-LABEL
     "Total Importe" VIEW-AS TEXT
          SIZE 9.29 BY .5 AT ROW 8.96 COL 77
     "I.G.V." VIEW-AS TEXT
          SIZE 5.14 BY .42 AT ROW 8.96 COL 66.57
     "I.S.C." VIEW-AS TEXT
          SIZE 4.57 BY .5 AT ROW 8.96 COL 55
     "Valor Venta" VIEW-AS TEXT
          SIZE 8 BY .5 AT ROW 8.96 COL 41.43
     "T.Descuento" VIEW-AS TEXT
          SIZE 9.43 BY .5 AT ROW 8.96 COL 29.14
     "T.Exonerado" VIEW-AS TEXT
          SIZE 9.43 BY .5 AT ROW 8.96 COL 15.57
     "Total Bruto" VIEW-AS TEXT
          SIZE 8 BY .5 AT ROW 8.96 COL 4
     RECT-11 AT ROW 8.88 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 4.


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
         HEIGHT             = 9.31
         WIDTH              = 87.72.
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
/* SETTINGS FOR FILL-IN F-TotBrt IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN F-ValVta IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH PEDI  NO-LOCK,
      FIRST Almmmatg OF PEDI NO-LOCK BY PEDI.NroItm .
     _END_FREEFORM
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
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
ON ANY-PRINTABLE OF br_table IN FRAME F-Main
DO:
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-ENTRY OF br_table IN FRAME F-Main
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}
  DISPLAY I-NroItm @ PEDI.NroItm WITH BROWSE {&BROWSE-NAME}.
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


ON "RETURN":U OF PEDI.codmat,PEDI.UndVta ,PEDI.CanPed,PEDI.Pordto,PEDI.Pordto2
DO:
   APPLY "TAB":U.
   RETURN NO-APPLY.
END.

ON "MOUSE-SELECT-DBLCLICK":U OF PEDI.Codmat
DO:
    input-var-1 = STRING(I-ListPr).
    input-var-2 = PEDI.Codmat:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}.
    FIND Almtfami WHERE Almtfami.codcia = S-CODCIA AND
         Almtfami.codfam = input-var-2  NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Almtfami THEN input-var-2 = ''.
    RUN lkup\c-listpr.r ('Consulta de Articulos').
    IF output-var-1 <> ? THEN
       PEDI.Codmat:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = output-var-2.
END.

ON "F8":U OF PEDI.Codmat
DO:
    input-var-1 = STRING(I-ListPr).
    input-var-2 = PEDI.Codmat:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}.
    FIND Almtfami WHERE Almtfami.codcia = S-CODCIA AND
         Almtfami.codfam = input-var-2  NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Almtfami THEN input-var-2 = ''.
    RUN lkup\c-listpr.r ('Consulta de Articulos').
    IF output-var-1 <> ? THEN
       PEDI.Codmat:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = output-var-2.
END.

ON "LEAVE":U OF PEDI.CodMat
DO:
   IF SELF:SCREEN-VALUE = "" THEN RETURN.
   SELF:SCREEN-VALUE = STRING(INTEGER(SELF:SCREEN-VALUE),"999999").
   /* Valida Maestro Productos */
   FIND Almmmatg WHERE Almmmatg.CodCia = S-CODCIA AND
        Almmmatg.codmat = SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
   IF NOT AVAILABLE Almmmatg THEN DO:
      MESSAGE "Codigo de Articulo no Existe" VIEW-AS ALERT-BOX ERROR.
      RETURN NO-APPLY.
   END.
   /* Valida Maestro Productos x Almacen */
   FIND Almmmate WHERE Almmmate.CodCia = S-CODCIA AND
        Almmmate.CodAlm = S-CODALM AND
        Almmmate.CodMat = SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
   IF NOT AVAILABLE Almmmate THEN DO:
      MESSAGE "Articulo no esta asignado al" SKIP
              "    ALMACEN : " S-CODALM VIEW-AS ALERT-BOX ERROR.
      RETURN NO-APPLY.   
   END.
   S-UNDBAS = Almmmatg.UndBas.
   F-FACTOR = 1.
   RUN Calculo-Precios.
   DISPLAY Almmmatg.DesMat @ Almmmatg.DesMat 
           Almmmatg.UndStk @ PEDI.UndVta 
           F-PREVTA @ PEDI.PreUni WITH BROWSE {&BROWSE-NAME}.
END.

ON "LEAVE":U OF PEDI.UndVta
DO:
   IF SELF:SCREEN-VALUE = "" THEN RETURN.
   FIND Almmmatg WHERE Almmmatg.CodCia = S-CODCIA AND
        Almmmatg.codmat = PEDI.codmat:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} 
        NO-LOCK NO-ERROR.
   IF AVAILABLE Almmmatg THEN DO:
      FIND Almtconv WHERE Almtconv.CodUnid = Almmmatg.UndBas AND
           Almtconv.Codalter = SELF:SCREEN-VALUE  NO-LOCK NO-ERROR.
      IF NOT AVAILABLE Almtconv THEN DO:
         MESSAGE "Codigo de unidad no exixte" VIEW-AS ALERT-BOX ERROR.
         RETURN NO-APPLY.
      END.
      F-FACTOR = Almtconv.Equival / Almmmatg.FacEqu.
      RUN Calculo-Precios.
      DISPLAY F-PREVTA  @ PEDI.PreUni WITH BROWSE {&BROWSE-NAME}.
   END.
END.

ON "LEAVE":U OF PEDI.CanPed
DO:
   IF PEDI.UndVta:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = "Und" THEN DO:
      IF DECIMAL(PEDI.CanPed:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) <> INT(PEDI.CanPed:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) THEN DO:
         MESSAGE " Venta en Unidades, Cantidad " SKIP
                 " no debe Tener Decimales. " VIEW-AS ALERT-BOX ERROR.
         RETURN NO-APPLY.
      END.   
   END. 
   RUN Recalculo-Precios.
END.

ON "LEAVE":U OF PEDI.Pordto
DO:
   IF DECIMAL(PEDI.PorDto:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) > 0 AND 
      DECIMAL(PEDI.PorDto:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) > F-DSCTOS THEN DO:
      MESSAGE " Porcentaje de descuento no puede ser mayor a " + STRING(F-DSCTOS, '>>9.99') + '%' VIEW-AS ALERT-BOX ERROR.
      RETURN NO-APPLY.
   END. 
   RUN Recalculo-Precios.
END.

ON "LEAVE":U OF PEDI.Pordto2
DO:
   IF DECIMAL(PEDI.PorDto2:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) > 0 AND 
      DECIMAL(PEDI.PorDto2:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) > F-DCTPRM THEN DO:
      MESSAGE " Porcentaje de descuento no puede ser mayor a " + STRING(F-DCTPRM, '>>9.99') + '%' VIEW-AS ALERT-BOX ERROR.
      RETURN NO-APPLY.
   END. 
   RUN Recalculo-Precios.
END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Asignar-Articulos B-table-Win 
PROCEDURE Asignar-Articulos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE I AS INTEGER INIT 1 NO-UNDO.
   DEFINE VARIABLE C-CODIGO AS CHAR NO-UNDO.
   DEFINE VARIABLE N-ITMS AS INTEGER INIT 0 NO-UNDO.
   DEFINE VARIABLE I-NITM  AS INTEGER INIT 0 NO-UNDO.
   
   I-NroItm = 0.
   FOR EACH B-PEDI BY B-PEDI.NroItm:
       N-ITMS = N-ITMS + 1.
       I-NroItm = B-PEDI.NroItm.
   END.
   IF (N-ITMS + 1) > FacCfgGn.Items_Pedido THEN RETURN "ADM-ERROR".
   
   ASSIGN input-var-1 = STRING(I-ListPr).
   
   RUN LKUP\C-AsgArt.r("ASIGNACION DE ARTICULOS").
   
   IF output-var-2 <> "" AND output-var-2 <> ? THEN DO:
      I-NITM = MAXIMUM(NUM-ENTRIES(output-var-2),(FacCfgGn.Items_Pedido - N-ITMS)).
/*****DO I = 1 TO I-NITM:
         C-CODIGO = ENTRY(I,output-var-2).******/
      DO I = 1 TO NUM-ENTRIES(output-var-2) WHILE (N-ITMS + NUM-ENTRIES(output-var-2)) <= FacCfgGn.Items_Pedido :
         C-CODIGO = ENTRY(I,output-var-2).
         FIND B-PEDI WHERE B-PEDI.CODCIA = S-CODCIA AND
              B-PEDI.CodMat = C-CODIGO NO-LOCK NO-ERROR.
         IF NOT AVAILABLE B-PEDI THEN DO:
            FIND Almmmatg WHERE Almmmatg.CodCia = S-CODCIA AND
                 Almmmatg.codmat = C-CODIGO NO-LOCK NO-ERROR.
            S-UNDBAS = Almmmatg.UndBas.
            F-FACTOR = 1.
            I-NroItm = I-NroItm + 1.
            RUN Calculo-Precios.
            CREATE B-PEDI.
            ASSIGN B-PEDI.CodCia = S-CODCIA
                   B-PEDI.codmat = C-CODIGO
                   B-PEDI.NroItm = I-NroItm
                   B-PEDI.UndVta = Almmmatg.UndStk
                   B-PEDI.Factor = F-FACTOR
                   B-PEDI.PreBas = F-PreBas 
                   B-PEDI.AftIgv = Almmmatg.AftIgv 
                   B-PEDI.AftIsc = Almmmatg.AftIsc
                   B-PEDI.PreUni = F-PREVTA
                   B-PEDI.PorDto = F-DSCTOS
                   B-PEDI.CanPed = 1
                   B-PEDI.ImpDto = ROUND( B-PEDI.PreUni * (F-DSCTOS / 100) * B-PEDI.CanPed , 2 ).
                   B-PEDI.ImpLin = ROUND( B-PEDI.PreUni * B-PEDI.CanPed , 2 ) - B-PEDI.ImpDto.
            IF B-PEDI.AftIsc THEN 
               B-PEDI.ImpIsc = ROUND(B-PEDI.PreBas * B-PEDI.CanPed * (Almmmatg.PorIsc / 100),4).
            IF B-PEDI.AftIgv THEN  
               B-PEDI.ImpIgv = B-PEDI.ImpLin - ROUND(B-PEDI.ImpLin  / (1 + (FacCfgGn.PorIgv / 100)),4).
         END.
      END.
      RUN dispatch IN THIS-PROCEDURE ('open-query':U).
   END.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Calculo-Precios B-table-Win 
PROCEDURE Calculo-Precios :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   F-PorImp = 1.
   IF Almmmatg.AftIsc THEN F-PorImp = (1 + Almmmatg.PorIsc / 100).
   IF Almmmatg.AftIgv THEN F-PorImp = F-PorImp * (1 + FacCfgGn.PorIgv / 100).

   IF S-CODMON = 1 THEN DO:
      IF Almmmatg.MonVta = 1 THEN ASSIGN F-PREBAS = Almmmatg.PreBas * F-FACTOR.
      ELSE ASSIGN F-PREBAS = Almmmatg.PreBas * FacCfgGn.Tpocmb[1] * F-FACTOR.
   END.
   IF S-CODMON = 2 THEN DO:
      IF Almmmatg.MonVta = 2 THEN ASSIGN F-PREBAS = Almmmatg.PreBas * F-FACTOR.
      ELSE ASSIGN F-PREBAS = (Almmmatg.PreBas / FacCfgGn.Tpocmb[1]) * F-FACTOR.
   END.

   F-DSCTOS = Almmmatg.PorMax.
   F-DCTPRM = IF TODAY >= Almmmatg.FchPrmD AND TODAY <= Almmmatg.FchPrmH THEN Almmmatg.Porvta[5] ELSE 0.
   F-PREVTA = ROUND(F-PREBAS * F-PorImp, 2).
      
END PROCEDURE.







/***********

   F-PorImp = 1.
   IF Almmmatg.AftIsc THEN F-PorImp = (1 + Almmmatg.PorIsc / 100).
   IF Almmmatg.AftIgv THEN F-PorImp = F-PorImp * (1 + FacCfgGn.PorIgv / 100).
   IF S-CODMON = 1 THEN DO:
      IF Almmmatg.MonVta = 1 THEN ASSIGN F-PREBAS = Almmmatg.PreBas * F-FACTOR.
      ELSE ASSIGN F-PREBAS = Almmmatg.PreBas * FacCfgGn.Tpocmb[1] * F-FACTOR.
   END.
   IF S-CODMON = 2 THEN DO:
      IF Almmmatg.MonVta = 2 THEN ASSIGN F-PREBAS = Almmmatg.PreBas * F-FACTOR.
      ELSE ASSIGN F-PREBAS = (Almmmatg.PreBas / FacCfgGn.Tpocmb[1]) * F-FACTOR.
   END.
   CASE I-ListPr:
        WHEN 2 THEN F-DSCTOS = Almmmatg.PorVta[1].
        WHEN 3 THEN F-DSCTOS = Almmmatg.PorVta[2].
        WHEN 4 THEN DO:
               F-DSCTOS = 0.
               F-PREBAS = F-PREBAS * (1 + (Almmmatg.PorVta[4] / 100)).
          END.
   END CASE. 
   F-PREVTA = ROUND(F-PREBAS * F-PorImp , 2).
   
*********/

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imp-Total B-table-Win 
PROCEDURE Imp-Total :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE F-IGV AS DECIMAL INIT 0 NO-UNDO.
DEFINE VARIABLE F-ISC AS DECIMAL INIT 0 NO-UNDO.
ASSIGN F-ImpDes = 0
       F-ImpExo = 0
       F-ImpIgv = 0
       F-ImpIsc = 0
       F-ImpTot = 0
       F-TotBrt = 0
       F-ValVta = 0.
FOR EACH B-PEDI :
    F-ImpTot = F-ImpTot + B-PEDI.ImpLin.
    F-Igv = F-Igv + B-PEDI.ImpIgv.
    F-Isc = F-Isc + B-PEDI.ImpIsc.
    F-ImpDes = F-ImpDes + B-PEDI.ImpDto.
    IF NOT B-PEDI.AftIgv THEN F-ImpExo = F-ImpExo + B-PEDI.ImpLin.
END.
F-ImpIgv = ROUND(F-Igv,2).
F-ImpIsc = ROUND(F-Isc,2).
F-TotBrt = F-ImpTot - F-ImpIgv - F-ImpIsc + F-ImpDes - F-ImpExo.
F-ValVta = F-TotBrt -  F-ImpDes.
DISPLAY F-ImpDes
        F-ImpExo
        F-ImpIgv
        F-ImpIsc
        F-ImpTot
        F-TotBrt
        F-ValVta WITH FRAME {&FRAME-NAME}.
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
  FIND gn-clie WHERE gn-clie.CodCia = cl-codcia AND
       gn-clie.CodCli = S-CODCLI NO-LOCK NO-ERROR.
  I-ListPr = 0.     
  IF AVAILABLE gn-clie THEN I-ListPr = INTEGER(gn-clie.TpoCli).

  I-NroITm = 0.  
  DEFINE VARIABLE N-ITMS AS INTEGER INIT 0 NO-UNDO.
  FOR EACH B-PEDI BY B-PEDI.NroItm:
      N-ITMS = N-ITMS + 1.
      I-NroItm = B-PEDI.NroItm.
  END.
  IF (N-ITMS + 1) > FacCfgGn.Items_Pedido THEN DO:
     MESSAGE "El numero de items no puede ser mayor a "  FacCfgGn.Items_Pedido 
             VIEW-AS ALERT-BOX ERROR.
     RETURN "ADM-ERROR".
  END.
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .
  I-NroItm = I-NroItm + 1.
  
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
  DEFINE VARIABLE F-PRECIO AS DECIMAL NO-UNDO.
  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-statement':U ) .
  /* Code placed here will execute AFTER standard behavior.    */
  
  ASSIGN PEDI.CodCia = S-CODCIA
         PEDI.Factor = F-FACTOR
         PEDI.NroItm = I-NroItm.

  FIND Almmmatg WHERE Almmmatg.CodCia = S-CODCIA AND
       Almmmatg.codmat = PEDI.codmat NO-LOCK NO-ERROR.
  IF AVAILABLE Almmmatg THEN DO:
     F-DCTPRM = DECIMAL(PEDI.PorDto2:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}).
     F-DSCTOS = DECIMAL(PEDI.PorDto:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}).
     ASSIGN PEDI.PreBas = F-PreBas 
            PEDI.AftIgv = Almmmatg.AftIgv 
            PEDI.AftIsc = Almmmatg.AftIsc 
            PEDI.PreUni = F-PREVTA
/*            PEDI.Pordto  = F-DSCTOS
            PEDI.Pordto2 = F-DCTPRM.*/
            PEDI.ImpDto  = ROUND( PEDI.PreUni * PEDI.CanPed * (PEDI.PorDto / 100), 2)
            PEDI.ImpLin  = ROUND( PEDI.PreUni * PEDI.CanPed , 2 ) - PEDI.ImpDto
            PEDI.ImpDto  = PEDI.ImpDto + ROUND( PEDI.Implin * (PEDI.PorDto2 / 100), 2)
            PEDI.ImpLin  = ROUND( PEDI.PreUni * PEDI.CanPed , 2 ) - PEDI.ImpDto.
     IF PEDI.AftIsc THEN 
        PEDI.ImpIsc = ROUND(PEDI.PreBas * PEDI.CanPed * (Almmmatg.PorIsc / 100),4).
     IF PEDI.AftIgv THEN  
        PEDI.ImpIgv = PEDI.ImpLin - ROUND(PEDI.ImpLin  / (1 + (FacCfgGn.PorIgv / 100)),4).
  END.
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

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .
  /* Code placed here will execute AFTER standard behavior.    */
  
  FOR EACH B-PEDI:
      I-NroItm = I-NroItm - 1.
      I-NroItm = B-PEDI.NroItm.
  END.

  DEFINE VARIABLE N-ITMS AS INTEGER INIT 0 NO-UNDO.
  FOR EACH B-PEDI BY B-PEDI.NroItm:
      N-ITMS = N-ITMS + 1.
      I-NroItm = N-ITMS.
  END.
  
  RUN Imp-Total.
  
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
  
  RUN Imp-Total.

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
  
  RUN Imp-Total.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Recalcular-Precios B-table-Win 
PROCEDURE Recalcular-Precios :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FOR EACH B-PEDI:
    F-FACTOR = B-PEDI.Factor.
    FIND Almmmatg WHERE Almmmatg.CodCia = S-CODCIA AND
         Almmmatg.codmat = B-PEDI.codmat NO-LOCK NO-ERROR.
    IF AVAILABLE Almmmatg THEN DO:
       RUN Calculo-Precios.
       ASSIGN B-PEDI.PreBas = F-PreBas 
              B-PEDI.AftIgv = Almmmatg.AftIgv 
              B-PEDI.AftIsc = Almmmatg.AftIsc
              B-PEDI.PreUni = F-PREVTA
              B-PEDI.PorDto = F-DSCTOS
              B-PEDI.ImpDto = ROUND( B-PEDI.PreUni * (F-DSCTOS / 100) * B-PEDI.CanPed , 2).
              B-PEDI.ImpLin = ROUND( B-PEDI.PreUni * B-PEDI.CanPed , 2 ) - B-PEDI.ImpDto.
       IF B-PEDI.AftIsc THEN 
          B-PEDI.ImpIsc = ROUND(B-PEDI.PreBas * B-PEDI.CanPed * (Almmmatg.PorIsc / 100),4).
       IF B-PEDI.AftIgv THEN  
          B-PEDI.ImpIgv = B-PEDI.ImpLin - ROUND(B-PEDI.ImpLin  / (1 + (FacCfgGn.PorIgv / 100)),4).
    END.
END.
RUN dispatch IN THIS-PROCEDURE ('open-query':U).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Recalculo-Precios B-table-Win 
PROCEDURE Recalculo-Precios :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   F-PorImp = 1.
   IF Almmmatg.AftIsc THEN F-PorImp = (1 + Almmmatg.PorIsc / 100).
   IF Almmmatg.AftIgv THEN F-PorImp = F-PorImp * (1 + FacCfgGn.PorIgv / 100).

   IF S-CODMON = 1 THEN DO:
      IF Almmmatg.MonVta = 1 THEN ASSIGN F-PREBAS = Almmmatg.PreBas * F-FACTOR.
      ELSE ASSIGN F-PREBAS = Almmmatg.PreBas * FacCfgGn.Tpocmb[1] * F-FACTOR.
   END.
   IF S-CODMON = 2 THEN DO:
      IF Almmmatg.MonVta = 2 THEN ASSIGN F-PREBAS = Almmmatg.PreBas * F-FACTOR.
      ELSE ASSIGN F-PREBAS = (Almmmatg.PreBas / FacCfgGn.Tpocmb[1]) * F-FACTOR.
   END.

   F-PREVTA = ROUND(F-PREBAS * F-PorImp, 2).
   
   DISPLAY
      ROUND(F-PREVTA * DECIMAL(PEDI.Canped:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) *
      (1 - DECIMAL(PEDI.Pordto:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) / 100) * 
      (1 - DECIMAL(PEDI.Pordto2:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) / 100), 2) @ PEDI.Implin WITH BROWSE {&BROWSE-NAME}.

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
        WHEN "codmat" THEN ASSIGN input-var-1 = STRING(I-ListPr + 1).
        WHEN "UndVta" THEN ASSIGN input-var-1 = S-UNDBAS.
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
  Notes:  EN CASO DE ERROR RETORNAR : RETURN "ADM-ERROR"
------------------------------------------------------------------------------*/
DEFINE VARIABLE F-STKRET AS DECIMAL NO-UNDO.
IF INTEGER(PEDI.CodMat:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) = 0 THEN DO:
   MESSAGE "Codigo de Articulo no puede ser blanco" VIEW-AS ALERT-BOX ERROR.
   RETURN "ADM-ERROR".
END.
IF DECIMAL(PEDI.CanPed:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) = 0 THEN DO:
   MESSAGE "Cantidad debe ser mayor a cero" VIEW-AS ALERT-BOX ERROR.
   RETURN "ADM-ERROR".
END.
IF DECIMAL(PEDI.PreUni:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) = 0 THEN DO:
   MESSAGE "Precio debe ser mayor a cero" VIEW-AS ALERT-BOX ERROR.
   RETURN "ADM-ERROR".
END.
IF PEDI.UndVta:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = "" THEN DO:
   MESSAGE "Codigo de Unidad no puede ser blanco" VIEW-AS ALERT-BOX ERROR.
   RETURN "ADM-ERROR".
END.
FIND Almmmatg WHERE Almmmatg.CodCia = S-CODCIA AND
     Almmmatg.codmat = PEDI.CodMat:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
     NO-LOCK NO-ERROR.
IF NOT AVAILABLE Almmmatg THEN DO:
    MESSAGE "Codigo de articulo no existe" VIEW-AS ALERT-BOX ERROR.
    RETURN "ADM-ERROR".
END.
FIND Almmmate WHERE Almmmate.CodCia = S-CODCIA AND
     Almmmate.CodAlm = S-CODALM AND
     Almmmate.codmat = PEDI.CodMat:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
     NO-LOCK NO-ERROR.
IF NOT AVAILABLE Almmmate THEN DO:
   MESSAGE "Articulo no asignado al almacen " S-CODALM VIEW-AS ALERT-BOX ERROR.
   RETURN "ADM-ERROR".
END.
FIND B-PEDI WHERE B-PEDI.CODCIA = S-CODCIA AND
     B-PEDI.CodMat = PEDI.CodMat:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} NO-LOCK NO-ERROR.
IF AVAILABLE  B-PEDI AND ROWID(B-PEDI) <> ROWID(PEDI) THEN DO:
   MESSAGE "Codigo de Articulo repetido" VIEW-AS ALERT-BOX ERROR.
   RETURN "ADM-ERROR".
END.
/* CONSISTENCIA DE STOCK */

/* RUN vta/stkdispo (s-codcia, s-codalm, PEDI.CodMat:SCREEN-VALUE IN BROWSE {&BROWSE-NAME},
        F-FACTOR * DECIMAL(PEDI.CanPed:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}), 
        OUTPUT S-OK).
IF S-OK = NO
THEN DO:
    MESSAGE "No hay stock disponible " SKIP
            "Stock Actual : " almmmate.StkAct VIEW-AS ALERT-BOX ERROR.
    RETURN "ADM-ERROR".
END.
*/
        
RETURN "OK".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valida-update B-table-Win 
PROCEDURE valida-update :
/*------------------------------------------------------------------------------
  Purpose:     Consistenciar la modificacion de la fila
  Parameters:  Retornar "ADM-ERROR" en caso de bloquear la modificacion
  Notes:       
------------------------------------------------------------------------------*/
IF NOT AVAILABLE PEDI THEN RETURN "ADM-ERROR".
FIND Almmmatg WHERE Almmmatg.CodCia = S-CODCIA AND
     Almmmatg.codmat = PEDI.codmat NO-LOCK NO-ERROR.
S-UNDBAS = Almmmatg.UndBas.
F-FACTOR = PEDI.Factor.
I-NroItm = PEDI.NroItm.
RETURN "OK".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
