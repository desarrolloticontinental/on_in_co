&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          integral         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW


/* Temp-Table and Buffer definitions                                    */
DEFINE SHARED TEMP-TABLE PEDI NO-UNDO LIKE FacDPedi.



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
DEFINE SHARED VARIABLE S-CODCIA  AS INTEGER.
DEFINE SHARED VARIABLE S-USER-ID AS CHAR.
DEFINE SHARED VARIABLE S-CODDOC  AS CHAR.
DEFINE SHARED VARIABLE S-CODDIV  AS CHAR.
DEFINE SHARED VARIABLE S-CODCLI  AS CHAR.
DEFINE SHARED VARIABLE S-CODMON  AS INTEGER.
DEFINE SHARED VARIABLE S-CODALM  AS CHAR.
DEFINE SHARED VARIABLE S-CNDVTA  AS CHAR.
DEFINE SHARED VARIABLE S-TPOCMB  AS DEC.
DEFINE SHARED VARIABLE S-CODIGV  AS INT.
DEFINE SHARED VARIABLE pCodAlm AS CHAR.     /* ALMACEN POR DEFECTO */
DEFINE SHARED VARIABLE S-TPOPED AS CHAR.

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE S-UNDBAS AS CHAR NO-UNDO.
DEFINE VARIABLE S-UNDVTA AS CHAR NO-UNDO.
DEFINE VARIABLE F-FACTOR AS DECI NO-UNDO.
DEFINE VARIABLE F-PREBAS LIKE Almmmatg.PreBas NO-UNDO.
DEFINE VARIABLE F-DSCTOS LIKE Almmmatg.PorMax NO-UNDO.
DEFINE VARIABLE X-CANPED AS DECI NO-UNDO.
DEFINE VARIABLE F-PREVTA AS DECI NO-UNDO.
DEFINE VARIABLE I-NroItm AS INTE NO-UNDO.
DEFINE VARIABLE Y-DSCTOS AS DECI NO-UNDO.
DEFINE VARIABLE Z-DSCTOS AS DECI NO-UNDO.
DEFINE VARIABLE X-TIPDTO AS CHAR NO-UNDO.

DEFINE SHARED TEMP-TABLE T-DPEDI LIKE FacDPedi.
DEFINE BUFFER B-PEDI FOR PEDI.

FIND FacCfgGn WHERE FacCfgGn.CodCia = S-CODCIA NO-LOCK NO-ERROR.

/* PRODUCTOS POR ROTACION */
DEFINE SHARED VAR s-FlgRotacion LIKE gn-divi.flgrotacion.
DEFINE SHARED VAR s-FlgEmpaque  LIKE GN-DIVI.FlgEmpaque.
DEFINE SHARED VAR s-FlgMinVenta LIKE GN-DIVI.FlgMinVenta.
DEFINE SHARED VAR s-VentaMayorista LIKE GN-DIVI.VentaMayorista.

DEFINE VAR s-registro-activo AS LOG INIT NO NO-UNDO.
DEFINE SHARED VARIABLE s-import-ibc AS LOG.

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
&Scoped-define FIELDS-IN-QUERY-br_table PEDI.NroItm PEDI.codmat ~
Almmmatg.DesMat Almmmatg.DesMar PEDI.UndVta PEDI.CanPed PEDI.PreUni ~
PEDI.PorDto PEDI.Por_Dsctos[1] PEDI.Por_Dsctos[2] PEDI.Por_Dsctos[3] ~
PEDI.ImpLin 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table PEDI.codmat PEDI.CanPed 
&Scoped-define ENABLED-TABLES-IN-QUERY-br_table PEDI
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br_table PEDI
&Scoped-define QUERY-STRING-br_table FOR EACH PEDI WHERE ~{&KEY-PHRASE} NO-LOCK, ~
      FIRST Almmmatg OF PEDI NO-LOCK ~
    BY PEDI.NroItm
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH PEDI WHERE ~{&KEY-PHRASE} NO-LOCK, ~
      FIRST Almmmatg OF PEDI NO-LOCK ~
    BY PEDI.NroItm.
&Scoped-define TABLES-IN-QUERY-br_table PEDI Almmmatg
&Scoped-define FIRST-TABLE-IN-QUERY-br_table PEDI
&Scoped-define SECOND-TABLE-IN-QUERY-br_table Almmmatg


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br_table RECT-11 
&Scoped-Define DISPLAYED-OBJECTS F-PorDes F-TotBrt F-ImpExo F-ValVta ~
F-ImpIsc F-ImpIgv F-ImpTot F-ImpDes 

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

DEFINE VARIABLE F-PorDes AS DECIMAL FORMAT "Z,ZZZ,ZZ9.99":U INITIAL 0 
     LABEL "% Dscto" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .69
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE F-TotBrt AS DECIMAL FORMAT "Z,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY .69
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE F-ValVta AS DECIMAL FORMAT "Z,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY .69
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 141 BY 2.35.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      PEDI, 
      Almmmatg SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      PEDI.NroItm COLUMN-LABEL "No" FORMAT ">>>9":U
      PEDI.codmat COLUMN-LABEL "Articulo" FORMAT "X(13)":U
      Almmmatg.DesMat FORMAT "X(45)":U WIDTH 53.14
      Almmmatg.DesMar COLUMN-LABEL "Marca" FORMAT "X(12)":U WIDTH 9.43
      PEDI.UndVta COLUMN-LABEL "Unidad" FORMAT "x(10)":U WIDTH 5.29
      PEDI.CanPed FORMAT ">>>,>>9.99":U
      PEDI.PreUni COLUMN-LABEL "Precio!Unitario" FORMAT ">>,>>9.9999":U
      PEDI.PorDto COLUMN-LABEL "% Dscto.!Incluido" FORMAT "->>9.99":U
      PEDI.Por_Dsctos[1] COLUMN-LABEL "% Dscto.!Manual" FORMAT "->>9.99":U
      PEDI.Por_Dsctos[2] COLUMN-LABEL "% Dscto!Evento" FORMAT "->>9.99":U
      PEDI.Por_Dsctos[3] COLUMN-LABEL "% Dscto!Vol/Prom" FORMAT "->>9.99":U
      PEDI.ImpLin FORMAT ">,>>>,>>9.99":U
  ENABLE
      PEDI.codmat
      PEDI.CanPed
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 141 BY 9.96
         BGCOLOR 15 FONT 4.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br_table AT ROW 1 COL 1
     F-PorDes AT ROW 11.15 COL 17 COLON-ALIGNED
     F-TotBrt AT ROW 12.5 COL 19 NO-LABEL
     F-ImpExo AT ROW 12.5 COL 31 NO-LABEL
     F-ValVta AT ROW 12.5 COL 53.14 COLON-ALIGNED NO-LABEL
     F-ImpIsc AT ROW 12.5 COL 65.86 COLON-ALIGNED NO-LABEL
     F-ImpIgv AT ROW 12.5 COL 78.43 COLON-ALIGNED NO-LABEL
     F-ImpTot AT ROW 12.5 COL 91 COLON-ALIGNED NO-LABEL
     F-ImpDes AT ROW 12.54 COL 41 COLON-ALIGNED NO-LABEL
     "Total Importe" VIEW-AS TEXT
          SIZE 10 BY .5 AT ROW 11.92 COL 93.14
     "Total Bruto" VIEW-AS TEXT
          SIZE 8 BY .5 AT ROW 11.92 COL 20.86
     "I.S.C." VIEW-AS TEXT
          SIZE 4.57 BY .5 AT ROW 11.92 COL 71.57
     "Valor Venta" VIEW-AS TEXT
          SIZE 8 BY .5 AT ROW 11.96 COL 56.86
     "I.G.V." VIEW-AS TEXT
          SIZE 5.14 BY .5 AT ROW 11.92 COL 83.14
     "T.Descuento" VIEW-AS TEXT
          SIZE 9.43 BY .5 AT ROW 11.96 COL 43.57
     "T.Exonerado" VIEW-AS TEXT
          SIZE 9.43 BY .5 AT ROW 11.92 COL 31.43
     RECT-11 AT ROW 10.96 COL 1
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
   Temp-Tables and Buffers:
      TABLE: PEDI T "SHARED" NO-UNDO INTEGRAL FacDPedi
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
         HEIGHT             = 13.81
         WIDTH              = 147.43.
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
/* SETTINGS FOR FILL-IN F-PorDes IN FRAME F-Main
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
     _TblList          = "Temp-Tables.PEDI,INTEGRAL.Almmmatg OF Temp-Tables.PEDI"
     _Options          = "NO-LOCK KEY-PHRASE"
     _TblOptList       = ", FIRST"
     _OrdList          = "Temp-Tables.PEDI.NroItm|yes"
     _FldNameList[1]   > Temp-Tables.PEDI.NroItm
"PEDI.NroItm" "No" ">>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.PEDI.codmat
"PEDI.codmat" "Articulo" "X(13)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > integral.Almmmatg.DesMat
"Almmmatg.DesMat" ? ? "character" ? ? ? ? ? ? no ? no no "53.14" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > integral.Almmmatg.DesMar
"Almmmatg.DesMar" "Marca" "X(12)" "character" ? ? ? ? ? ? no ? no no "9.43" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.PEDI.UndVta
"PEDI.UndVta" "Unidad" "x(10)" "character" ? ? ? ? ? ? no ? no no "5.29" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.PEDI.CanPed
"PEDI.CanPed" ? ">>>,>>9.99" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > Temp-Tables.PEDI.PreUni
"PEDI.PreUni" "Precio!Unitario" ">>,>>9.9999" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > Temp-Tables.PEDI.PorDto
"PEDI.PorDto" "% Dscto.!Incluido" "->>9.99" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > Temp-Tables.PEDI.Por_Dsctos[1]
"PEDI.Por_Dsctos[1]" "% Dscto.!Manual" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > Temp-Tables.PEDI.Por_Dsctos[2]
"PEDI.Por_Dsctos[2]" "% Dscto!Evento" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > Temp-Tables.PEDI.Por_Dsctos[3]
"PEDI.Por_Dsctos[3]" "% Dscto!Vol/Prom" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > Temp-Tables.PEDI.ImpLin
"PEDI.ImpLin" ? ">,>>>,>>9.99" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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


&Scoped-define SELF-NAME PEDI.codmat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL PEDI.codmat br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF PEDI.codmat IN BROWSE br_table /* Articulo */
DO:
    IF SELF:SCREEN-VALUE = "" THEN RETURN.
    IF s-registro-activo = NO THEN RETURN.

    DEF VAR pCodMat AS CHAR NO-UNDO.
    pCodMat = SELF:SCREEN-VALUE.
    RUN vtagn/p-codbrr-01 (INPUT-OUTPUT pCodMat).
    IF pCodMat = '' THEN RETURN NO-APPLY.
    SELF:SCREEN-VALUE = pCodMat.
    FIND Almmmatg WHERE Almmmatg.codcia = s-codcia
        AND Almmmatg.codmat = SELF:SCREEN-VALUE
        NO-LOCK.
    IF Almmmatg.Chr__01 = "" THEN DO:
       MESSAGE "Articulo no tiene unidad de Oficina" VIEW-AS ALERT-BOX ERROR.
       RETURN NO-APPLY.
    END.
    /* Valida Maestro Productos x Almacen */
    ASSIGN 
        S-UNDVTA = Almmmatg.CHR__01
        S-UNDBAS = Almmmatg.UndBas
        F-FACTOR = 1
        X-CANPED = 1.
    CASE s-TpoPed:
        WHEN 'E12' THEN DO:     /* Expo Enero 2012 */
            RUN vtagn/PrecioListaMayorista-2a (
                        "10015",        /*s-CodDiv,*/
                        s-CodCli,
                        s-CodMon,
                        s-TpoCmb,
                        OUTPUT s-UndVta,
                        OUTPUT f-Factor,
                        Almmmatg.CodMat,
                        s-CndVta,
                        x-CanPed,
                        4,
                        OUTPUT f-PreBas,
                        OUTPUT f-PreVta,
                        OUTPUT f-Dsctos,
                        OUTPUT y-Dsctos,
                        OUTPUT z-Dsctos,
                        OUTPUT x-TipDto
                        ).
        END.
        OTHERWISE DO:
            RUN vtagn/PrecioListaMayorista-1 (
                                s-CodDiv,
                                s-CodCli,
                                s-CodMon,
                                s-TpoCmb,
                                OUTPUT s-UndVta,
                                OUTPUT f-Factor,
                                Almmmatg.CodMat,
                                s-CndVta,
                                x-CanPed,
                                4,
                                OUTPUT f-PreBas,
                                OUTPUT f-PreVta,
                                OUTPUT f-Dsctos,
                                OUTPUT y-Dsctos,
                                OUTPUT z-Dsctos,
                                OUTPUT x-TipDto
                                ).
        END.
    END CASE.
    IF RETURN-VALUE = "ADM-ERROR" THEN DO:
        RETURN NO-APPLY.
    END.
    DISPLAY 
        Almmmatg.DesMat @ Almmmatg.DesMat 
        Almmmatg.DesMar @ Almmmatg.DesMar 
        s-UndVta @ PEDI.UndVta 
        F-DSCTOS @ PEDI.PorDto
        F-PREVTA @ PEDI.PreUni 
        z-Dsctos @ PEDI.Por_Dsctos[2]
        y-Dsctos @ PEDI.Por_Dsctos[3]
        WITH BROWSE {&BROWSE-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL PEDI.codmat br_table _BROWSE-COLUMN B-table-Win
ON LEFT-MOUSE-DBLCLICK OF PEDI.codmat IN BROWSE br_table /* Articulo */
OR F8 OF PEDI.codmat
DO:
    IF s-VentaMayorista = 1 THEN DO:    /* LISTA GENERAL */
        ASSIGN
            input-var-1 = pCodAlm
            input-var-2 = ''
            input-var-3 = ''.
        RUN vtagn/c-listpr-01 ('Lista de Precios').
        IF output-var-1 <> ? THEN DO:
            pCodAlm = output-var-3.
            DISPLAY
                output-var-2 @ PEDI.codmat
                WITH BROWSE {&browse-name}.
        END.
    END.
    IF s-VentaMayorista = 2 THEN DO:    /* LISTA DE LA DIVISION */
        ASSIGN
            input-var-1 = pCodAlm
            input-var-2 = ''
            input-var-3 = ''.
        RUN vtagn/c-listpr-02 ('Lista de Precios').
        IF output-var-1 <> ? THEN DO:
            pCodAlm = output-var-3.
            DISPLAY
                output-var-2 @ PEDI.codmat
                WITH BROWSE {&browse-name}.
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME PEDI.CanPed
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL PEDI.CanPed br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF PEDI.CanPed IN BROWSE br_table /* Cantidad */
DO:
    IF s-registro-activo = NO THEN RETURN.
    FIND Almmmatg WHERE Almmmatg.codcia = s-codcia
        AND Almmmatg.codmat = PEDI.codmat:SCREEN-VALUE IN BROWSE {&browse-name}
        NO-LOCK.
    x-CanPed = DEC(PEDI.CanPed:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}).
    CASE s-TpoPed:
        WHEN 'E12' THEN DO:     /* Expo Enero 2012 */
            RUN vtagn/PrecioListaMayorista-2a (
                        "10015",        /*s-CodDiv,*/
                        s-CodCli,
                        s-CodMon,
                        s-TpoCmb,
                        OUTPUT s-UndVta,
                        OUTPUT f-Factor,
                        Almmmatg.CodMat,
                        s-CndVta,
                        x-CanPed,
                        4,
                        OUTPUT f-PreBas,
                        OUTPUT f-PreVta,
                        OUTPUT f-Dsctos,
                        OUTPUT y-Dsctos,
                        OUTPUT z-Dsctos,
                        OUTPUT x-TipDto
                        ).
        END.
        OTHERWISE DO:
            RUN vtagn/PrecioListaMayorista-1 (
                                s-CodDiv,
                                s-CodCli,
                                s-CodMon,
                                s-TpoCmb,
                                OUTPUT s-UndVta,
                                OUTPUT f-Factor,
                                Almmmatg.CodMat,
                                s-CndVta,
                                x-CanPed,
                                4,
                                OUTPUT f-PreBas,
                                OUTPUT f-PreVta,
                                OUTPUT f-Dsctos,
                                OUTPUT y-Dsctos,
                                OUTPUT z-Dsctos,
                                OUTPUT x-TipDto
                                ).
        END.
    END CASE.
    IF RETURN-VALUE = "ADM-ERROR" THEN DO:
        RETURN NO-APPLY.
    END.
    DISPLAY 
        s-UndVta @ PEDI.UndVta 
        F-DSCTOS @ PEDI.PorDto
        F-PREVTA @ PEDI.PreUni 
        y-Dsctos @ PEDI.Por_Dsctos[3]
        z-Dsctos @ PEDI.Por_Dsctos[2]
        WITH BROWSE {&BROWSE-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME PEDI.PreUni
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL PEDI.PreUni br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF PEDI.PreUni IN BROWSE br_table /* Precio!Unitario */
DO:
    IF F-PREVTA > DEC(PEDI.PreUni:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) THEN DO:
       MESSAGE "No Autorizado para modificar Precio " 
       VIEW-AS ALERT-BOX ERROR.
       DISPLAY F-PREVTA @ PEDI.PreUni WITH BROWSE {&BROWSE-NAME}.
       RETURN NO-APPLY.   
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

ON 'RETURN':U OF PEDI.CanPed, PEDI.codmat, PEDI.PreUni 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Calculo-Precios-Supermercados B-table-Win 
PROCEDURE Calculo-Precios-Supermercados :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR f-PorImp AS DEC NO-UNDO.

   F-PorImp = 1.

   /* RHC 12.06.08 al tipo de cambio de la familia */
   IF S-CODMON = 1 THEN DO:
      IF Almmmatg.MonVta = 1 THEN
          ASSIGN F-PREBAS = (Almmmatg.PreOfi * F-PorImp) * F-FACTOR.
      ELSE
          ASSIGN F-PREBAS = (Almmmatg.PreOfi * F-PorImp) * Almmmatg.TpoCmb * F-FACTOR.
   END.
   IF S-CODMON = 2 THEN DO:
      IF Almmmatg.MonVta = 2 THEN
         ASSIGN F-PREBAS = (Almmmatg.PreOfi * F-PorImp) * F-FACTOR.
      ELSE
         ASSIGN F-PREBAS = ((Almmmatg.PreOfi * F-PorImp) / Almmmatg.TpoCmb) * F-FACTOR.
   END.
   f-PreVta = f-PreBas.
   FIND FacTabla WHERE FacTabla.CodCia = s-codcia
       AND FacTabla.Tabla = 'AU' 
       AND FacTabla.Codigo = s-codcli
       NO-LOCK NO-ERROR.
   IF NOT AVAILABLE Factabla THEN RETURN.

   IF Factabla.Valor[1] <> 0 
   THEN f-PreVta = f-PreBas * ( 1 - Factabla.Valor[1] / 100 ).
   ELSE f-PreVta = f-PreBas * ( 1 + Factabla.Valor[2] / 100 ).

    RUN src/BIN/_ROUND1(F-PREVTA, 2, OUTPUT F-PREVTA).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE IBC-Diferencias B-table-Win 
PROCEDURE IBC-Diferencias :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FOR EACH T-DPEDI:
    DELETE T-DPEDI.
END.

FOR EACH PEDI:
    /* Valida Maestro Productos */
    FIND Almmmatg WHERE 
         Almmmatg.CodCia = S-CODCIA AND  
         Almmmatg.codmat = PEDI.codmat
         USE-INDEX matg01
         NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Almmmatg THEN NEXT.
    F-FACTOR = 1.
    RUN Calculo-Precios-Supermercados.
    /* Solo si hay una diferencia mayor al 1% */
    IF ABSOLUTE(PEDI.PreUni - f-PreVta) / f-PreVta * 100 > 0.25 THEN DO:
        CREATE T-DPEDI.
        BUFFER-COPY PEDI TO T-DPEDI
            ASSIGN
                T-DPEDI.Libre_d01 = f-PreVta.
    END.
END.

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

  ASSIGN 
    F-ImpDes = 0
    F-ImpExo = 0
    F-ImpIgv = 0
    F-ImpIsc = 0
    F-ImpTot = 0
    F-TotBrt = 0
    F-ValVta = 0
    F-PorDes = 0.
  FOR EACH B-PEDI:
    F-ImpTot = F-ImpTot + B-PEDI.ImpLin.
    F-Igv = F-Igv + B-PEDI.ImpIgv.
    F-Isc = F-Isc + B-PEDI.ImpIsc.
    /*F-ImpDes = F-ImpDes + B-PEDI.ImpDto.*/
    IF NOT B-PEDI.AftIgv THEN F-ImpExo = F-ImpExo + B-PEDI.ImpLin.
    IF B-PEDI.AftIgv = YES
    THEN f-ImpDes = f-ImpDes + ROUND(B-PEDI.ImpDto / (1 + FacCfgGn.PorIgv / 100), 2).
    ELSE f-ImpDes = f-ImpDes + B-PEDI.ImpDto.
  END.
  F-ImpIgv = ROUND(F-Igv,2).
  F-ImpIsc = ROUND(F-Isc,2).
/*  F-TotBrt = F-ImpTot - F-ImpIgv - F-ImpIsc + F-ImpDes - F-ImpExo.
 *   F-ValVta = F-TotBrt -  F-ImpDes.*/
  F-ValVta = F-ImpTot - F-ImpExo - F-ImpIgv.
  F-TotBrt = F-ValVta + F-ImpIsc + F-ImpDes + F-ImpExo.
  DISPLAY F-ImpDes
        F-ImpExo
        F-ImpIgv
        F-ImpIsc
        F-ImpTot
        F-TotBrt
        F-ValVta WITH FRAME {&FRAME-NAME}.

END PROCEDURE.
/*
  ASSIGN
    FacCPedi.ImpIgv = ROUND(F-IGV,2)
    FacCPedi.ImpIsc = ROUND(F-ISC,2)
    FacCPedi.ImpVta = FacCPedi.ImpTot - FacCPedi.ImpExo - FacCPedi.ImpIgv.
  IF FacCPedi.PorDto > 0 THEN DO:
    ASSIGN
        FacCPedi.ImpDto = FacCPedi.ImpDto + ROUND((FacCPedi.ImpVta + FacCPedi.ImpExo) * FacCPedi.PorDto / 100, 2)
        FacCPedi.ImpTot = ROUND(FacCPedi.ImpTot * (1 - FacCPedi.PorDto / 100),2)
        FacCPedi.ImpVta = ROUND(FacCPedi.ImpVta * (1 - FacCPedi.PorDto / 100),2)
        FacCPedi.ImpExo = ROUND(FacCPedi.ImpExo * (1 - FacCPedi.PorDto / 100),2)
        FacCPedi.ImpIgv = FacCPedi.ImpTot - FacCPedi.ImpExo - FacCPedi.ImpVta.
  END.
  FacCPedi.ImpBrt = FacCPedi.ImpVta + FacCPedi.ImpIsc + FacCPedi.ImpDto + FacCPedi.ImpExo.
*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-add-record B-table-Win 
PROCEDURE local-add-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  IF s-import-ibc = YES THEN DO:
      MESSAGE 'Acceso Denegado' VIEW-AS ALERT-BOX ERROR.
      RETURN 'ADM-ERROR'.
  END.
  I-NroItm = 0.  
  DEFINE VARIABLE N-ITMS AS INTEGER INIT 0 NO-UNDO.
  FOR EACH B-PEDI BY B-PEDI.NroItm:
      N-ITMS = N-ITMS + 1.
      I-NroItm = B-PEDI.NroItm.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  I-NroItm = I-NroItm + 1.
  RUN Procesa-Handle IN lh_handle ('Disable-Head').
  s-registro-activo = YES.
  APPLY 'ENTRY':U TO PEDI.codmat IN BROWSE {&browse-name}.
  
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
  FIND Almmmatg WHERE Almmmatg.CodCia = S-CODCIA 
      AND Almmmatg.codmat = PEDI.codmat:SCREEN-VALUE IN BROWSE {&browse-name}
      NO-LOCK NO-ERROR.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-statement':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN 
    PEDI.CodCia = S-CODCIA
    PEDI.Factor = F-FACTOR
    PEDI.NroItm = I-NroItm.
         
  ASSIGN 
    PEDI.UndVta = PEDI.UndVta:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
    PEDI.PreUni = DEC(PEDI.PreUni:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
    PEDI.PorDto = DEC(PEDI.PorDto:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
    PEDI.Por_Dsctos[1] = DEC(PEDI.Por_Dsctos[1]:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
    PEDI.Por_Dsctos[2] = DEC(PEDI.Por_Dsctos[2]:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
    PEDI.Por_Dsctos[3] = DEC(PEDI.Por_Dsctos[3]:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
    PEDI.PreBas = F-PreBas 
    PEDI.AftIgv = ( IF s-CodIgv = 1 THEN Almmmatg.AftIgv ELSE NO )
    PEDI.AftIsc = Almmmatg.AftIsc.
  ASSIGN
      PEDI.ImpLin = PEDI.CanPed * PEDI.PreUni * 
                    ( 1 - PEDI.Por_Dsctos[1] / 100 ) *
                    ( 1 - PEDI.Por_Dsctos[2] / 100 ) *
                    ( 1 - PEDI.Por_Dsctos[3] / 100 ).
  IF PEDI.Por_Dsctos[1] = 0 AND PEDI.Por_Dsctos[2] = 0 AND PEDI.Por_Dsctos[3] = 0 
      THEN PEDI.ImpDto = 0.
      ELSE PEDI.ImpDto = PEDI.CanPed * PEDI.PreUni - PEDI.ImpLin.
  ASSIGN
      PEDI.ImpLin = ROUND(PEDI.ImpLin, 2)
      PEDI.ImpDto = ROUND(PEDI.ImpDto, 2).
  IF PEDI.AftIsc 
  THEN PEDI.ImpIsc = ROUND(PEDI.PreBas * PEDI.CanPed * (Almmmatg.PorIsc / 100),4).
  IF PEDI.AftIgv 
  THEN PEDI.ImpIgv = PEDI.ImpLin - ROUND( PEDI.ImpLin  / ( 1 + (FacCfgGn.PorIgv / 100) ), 4 ).

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-cancel-record B-table-Win 
PROCEDURE local-cancel-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  s-registro-activo = NO.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN Procesa-Handle IN lh_handle ('Enable-Head').


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
  IF s-import-ibc = YES THEN DO:
      MESSAGE 'Acceso Denegado' VIEW-AS ALERT-BOX ERROR.
      RETURN 'ADM-ERROR'.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
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
  RUN Procesa-Handle IN lh_handle ('Enable-Head').
  s-registro-activo = NO.

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
FOR EACH PEDI, FIRST Almmmatg OF PEDI:
    F-FACTOR = PEDI.Factor.
    x-CanPed = PEDI.CanPed.
    s-UndVta = PEDI.UndVta.
    CASE s-TpoPed:
        WHEN 'E12' THEN DO:
            RUN vtagn/PrecioListaMayorista-2a (
                                "10015",        /*s-CodDiv,*/
                                s-CodCli,
                                s-CodMon,
                                s-TpoCmb,
                                OUTPUT s-UndVta,
                                OUTPUT f-Factor,
                                Almmmatg.CodMat,
                                s-CndVta,
                                x-CanPed,
                                4,
                                OUTPUT f-PreBas,
                                OUTPUT f-PreVta,
                                OUTPUT f-Dsctos,
                                OUTPUT y-Dsctos,
                                OUTPUT z-Dsctos,
                                OUTPUT x-TipDto
                                ).
        END.
        OTHERWISE DO:
            RUN vtagn/PrecioListaMayorista-1 (
                                s-CodDiv,
                                s-CodCli,
                                s-CodMon,
                                s-TpoCmb,
                                OUTPUT s-UndVta,
                                OUTPUT f-Factor,
                                Almmmatg.CodMat,
                                s-CndVta,
                                x-CanPed,
                                4,
                                OUTPUT f-PreBas,
                                OUTPUT f-PreVta,
                                OUTPUT f-Dsctos,
                                OUTPUT y-Dsctos,
                                OUTPUT z-Dsctos,
                                OUTPUT x-TipDto
                                ).
        END.
    END CASE.

    IF RETURN-VALUE = "ADM-ERROR" THEN DO:
        DELETE PEDI.
        NEXT.
    END.
    ASSIGN 
        PEDI.Factor = f-Factor
        PEDI.UndVta = s-UndVta
        PEDI.PreUni = F-PREVTA
        PEDI.PreBas = F-PreBas 
        PEDI.PorDto = F-DSCTOS  /* Ambos descuentos afectan */
        PEDI.PorDto2 = 0        /* el precio unitario */
        PEDI.Por_Dsctos[2] = z-Dsctos
        PEDI.Por_Dsctos[3] = Y-DSCTOS 
        PEDI.AftIgv = ( IF s-CodIgv = 1 THEN Almmmatg.AftIgv ELSE NO )
        PEDI.AftIsc = Almmmatg.AftIsc
        PEDI.ImpIsc = 0
        PEDI.ImpIgv = 0
        PEDI.Libre_c04 = x-TipDto.
    ASSIGN
        PEDI.ImpLin = PEDI.CanPed * PEDI.PreUni * 
                    ( 1 - PEDI.Por_Dsctos[1] / 100 ) *
                    ( 1 - PEDI.Por_Dsctos[2] / 100 ) *
                    ( 1 - PEDI.Por_Dsctos[3] / 100 ).
    IF PEDI.Por_Dsctos[1] = 0 AND PEDI.Por_Dsctos[2] = 0 AND PEDI.Por_Dsctos[3] = 0 
        THEN PEDI.ImpDto = 0.
    ELSE PEDI.ImpDto = PEDI.CanPed * PEDI.PreUni - PEDI.ImpLin.
    ASSIGN
        PEDI.ImpLin = ROUND(PEDI.ImpLin, 2)
        PEDI.ImpDto = ROUND(PEDI.ImpDto, 2).
    IF PEDI.AftIsc THEN 
        PEDI.ImpIsc = ROUND(PEDI.PreBas * PEDI.CanPed * (Almmmatg.PorIsc / 100),4).
    IF PEDI.AftIgv THEN  
        PEDI.ImpIgv = PEDI.ImpLin - ROUND(PEDI.ImpLin  / (1 + (FacCfgGn.PorIgv / 100)),4).
END.
RUN dispatch IN THIS-PROCEDURE ('open-query':U).

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
  
  IF PEDI.CodMat:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = ''
    OR INTEGER(PEDI.CodMat:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) = 0 THEN DO:
       MESSAGE "Codigo de Articulo no puede ser blanco" VIEW-AS ALERT-BOX ERROR.
       APPLY 'ENTRY':U TO PEDI.CodMat.
       RETURN "ADM-ERROR".
  END.
  FIND B-PEDI WHERE B-PEDI.CODCIA = S-CODCIA 
    AND  B-PEDI.CodMat = PEDI.CodMat:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} 
    NO-LOCK NO-ERROR.
  IF AVAILABLE  B-PEDI AND ROWID(B-PEDI) <> ROWID(PEDI) THEN DO:
       MESSAGE "C�digo de Articulo repetido" VIEW-AS ALERT-BOX ERROR.
       APPLY 'ENTRY':U TO PEDI.CodMat.
       RETURN "ADM-ERROR".
  END.
  /* ARTICULO */
  FIND Almmmatg WHERE Almmmatg.CodCia = S-CODCIA 
    AND  Almmmatg.codmat = PEDI.CodMat:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
    NO-LOCK NO-ERROR.
  IF NOT AVAILABLE Almmmatg THEN DO:
        MESSAGE "Codigo de articulo no existe" VIEW-AS ALERT-BOX ERROR.
        RETURN "ADM-ERROR".
  END.
  IF NOT (Almmmatg.TpoArt <= s-FlgRotacion) THEN DO:
      MESSAGE "Articulo no autorizado para venderse" VIEW-AS ALERT-BOX ERROR.
      RETURN "ADM-ERROR".
  END.
  FIND Almtfami OF Almmmatg NO-LOCK NO-ERROR.
  IF AVAILABLE Almtfami AND Almtfami.SwComercial = NO THEN DO:
      MESSAGE 'El producto pertenece a una familia NO autorizada para ventas'
          VIEW-AS ALERT-BOX ERROR.
      RETURN 'ADM-ERROR'.
  END.
  FIND Almsfami OF Almmmatg NO-LOCK NO-ERROR.
  IF AVAILABLE Almsfami AND AlmSFami.SwDigesa = YES 
      AND (Almmmatg.VtoDigesa = ? OR Almmmatg.VtoDigesa < TODAY) THEN DO:
      MESSAGE 'La fecha de DIGESA ya venci� o no se ha registrado su vencimiento'
          VIEW-AS ALERT-BOX ERROR.
      RETURN 'ADM-ERROR'.
  END.
  /* CANTIDAD */
  IF DECIMAL(PEDI.CanPed:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) = 0 THEN DO:
       MESSAGE "Cantidad debe ser mayor a cero" VIEW-AS ALERT-BOX ERROR.
       APPLY 'ENTRY':U TO PEDI.CanPed.
       RETURN "ADM-ERROR".
  END.
  /* EMPAQUE */
  DEF VAR f-Canped AS DEC NO-UNDO.
  IF s-FlgEmpaque = YES THEN DO:
      IF s-VentaMayorista = 2 THEN DO:  /* LISTA POR DIVISION */
          FIND FIRST VtaListaMay OF Almmmatg WHERE Vtalistamay.coddiv = s-coddiv NO-LOCK NO-ERROR.
          IF AVAILABLE VtaListaMay AND Vtalistamay.CanEmp > 0 THEN DO:
              f-CanPed = DECIMAL(PEDI.CanPed:SCREEN-VALUE IN BROWSE {&browse-name}) * f-Factor.
              f-CanPed = (TRUNCATE((f-CanPed / Vtalistamay.CanEmp),0) * Vtalistamay.CanEmp).
              IF f-CanPed <> DECIMAL(PEDI.CanPed:SCREEN-VALUE IN BROWSE {&browse-name}) * f-Factor THEN DO:
                  MESSAGE 'Solo puede vender en empaques de' Vtalistamay.CanEmp Almmmatg.UndBas
                      VIEW-AS ALERT-BOX ERROR.
                  APPLY 'ENTRY':U TO PEDI.CanPed.
                  RETURN "ADM-ERROR".
              END.
          END.
      END.
      ELSE DO:      /* LISTA GENERAL */
          IF Almmmatg.DEC__03 > 0 THEN DO:
              f-CanPed = DECIMAL(PEDI.CanPed:SCREEN-VALUE IN BROWSE {&browse-name}) * f-Factor.
              f-CanPed = (TRUNCATE((f-CanPed / Almmmatg.DEC__03),0) * Almmmatg.DEC__03).
              IF f-CanPed <> DECIMAL(PEDI.CanPed:SCREEN-VALUE IN BROWSE {&browse-name}) * f-Factor THEN DO:
                  MESSAGE 'Solo puede vender en empaques de' Almmmatg.DEC__03 Almmmatg.UndBas
                      VIEW-AS ALERT-BOX ERROR.
                  APPLY 'ENTRY':U TO PEDI.CanPed.
                  RETURN "ADM-ERROR".
              END.
          END.
      END.
  END.
  /* MINIMO DE VENTA */
  IF s-FlgMinVenta = YES /*AND Almmmatg.DEC__03 > 0*/ THEN DO:
      f-CanPed = DECIMAL(PEDI.CanPed:SCREEN-VALUE IN BROWSE {&browse-name}) * f-Factor.
      IF s-VentaMayorista = 2 THEN DO:  /* LISTA POR DIVISION */
          FIND FIRST VtaListaMay OF Almmmatg WHERE Vtalistamay.coddiv = s-coddiv NO-LOCK NO-ERROR.
          IF AVAILABLE VtaListaMay AND Vtalistamay.CanEmp > 0 THEN DO:
              IF f-CanPed < Vtalistamay.CanEmp THEN DO:
                  MESSAGE 'Solo puede vender como m�nimo' Vtalistamay.CanEmp Almmmatg.UndBas
                      VIEW-AS ALERT-BOX ERROR.
                  APPLY 'ENTRY':U TO PEDI.CanPed.
                  RETURN "ADM-ERROR".
              END.
          END.
      END.
      ELSE DO:      /* LISTA GENERAL */
          IF Almmmatg.DEC__03 > 0 THEN DO:
              IF f-CanPed < Almmmatg.DEC__03 THEN DO:
                  MESSAGE 'Solo puede vender como m�nimo' Almmmatg.DEC__03 Almmmatg.UndBas
                      VIEW-AS ALERT-BOX ERROR.
                  APPLY 'ENTRY':U TO PEDI.CanPed.
                  RETURN "ADM-ERROR".
              END.
          END.
      END.
  END.

  /* PRECIO UNITARIO */
  IF DECIMAL(PEDI.PreUni:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) = 0 THEN DO:
       MESSAGE "Precio debe ser mayor a cero" VIEW-AS ALERT-BOX ERROR.
       APPLY 'ENTRY':U TO PEDI.PreUni.
       RETURN "ADM-ERROR".
  END.
  IF PEDI.UndVta:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = "" THEN DO:
       MESSAGE "Codigo de Unidad no puede ser blanco" VIEW-AS ALERT-BOX ERROR.
       RETURN "ADM-ERROR".
  END.

  /*UNIDAD BASE = UNI NO ACEPTE DECIMALES*/
  IF TRIM(PEDI.UndVta) = 'UNI' THEN DO:
      IF (INT(PEDI.CanPed:SCREEN-VALUE) - TRUNCATE(DEC(PEDI.CanPed:SCREEN-VALUE),2)) <> 0 THEN DO:
          MESSAGE "Cantidad de Articulo no permitida"
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          APPLY "ENTRY" TO PEDI.CanPed.
          RETURN "ADM-ERROR".
      END.
  END.

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
  IF s-import-ibc = YES THEN DO:
      MESSAGE 'Acceso Denegado' VIEW-AS ALERT-BOX ERROR.
      RETURN 'ADM-ERROR'.
  END.
IF AVAILABLE PEDI
THEN ASSIGN
        i-nroitm = PEDI.NroItm
        f-Factor = PEDI.Factor
        f-PreBas = PEDI.PreBas
        y-Dsctos = PEDI.Por_Dsctos[3]
        x-TipDto = PEDI.Libre_c04.
  s-registro-activo = NO.
  RUN Procesa-Handle IN lh_handle ('Disable-Head').

RETURN "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
