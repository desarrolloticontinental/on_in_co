&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          integral         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW


/* Temp-Table and Buffer definitions                                    */
DEFINE SHARED TEMP-TABLE ITEM LIKE FacDPedi.



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
DEFINE SHARED VARIABLE pCodDiv  AS CHAR.    /* para la lista de precios a usar */
DEFINE SHARED VARIABLE S-CODCLI  AS CHAR.
DEFINE SHARED VARIABLE S-CODMON  AS INTEGER.
DEFINE SHARED VARIABLE S-CODALM  AS CHAR.
DEFINE SHARED VARIABLE S-CNDVTA  AS CHAR.
DEFINE SHARED VARIABLE S-TPOCMB  AS DEC.
DEFINE SHARED VARIABLE s-PorIgv LIKE Ccbcdocu.PorIgv.
DEFINE SHARED VARIABLE s-TpoPed   AS CHAR.
DEFINE SHARED VARIABLE s-nrodec AS INT.
DEFINE SHARED VARIABLE s-import-ibc AS LOG.
DEFINE SHARED VARIABLE s-import-cissac AS LOG.

DEFINE SHARED TEMP-TABLE T-DPEDI LIKE FacDPedi.

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
DEFINE VARIABLE x-TipDto AS CHAR NO-UNDO.
DEFINE VARIABLE pCodAlm  AS CHAR NO-UNDO.
DEFINE VARIABLE x-DesMat AS CHAR NO-UNDO.
DEFINE VARIABLE x-DesMar AS CHAR NO-UNDO.
DEFINE VARIABLE x-adm-new-record AS CHAR.

DEFINE BUFFER B-ITEM FOR ITEM.

FIND FacCfgGn WHERE FacCfgGn.CodCia = S-CODCIA NO-LOCK NO-ERROR.

/* PRODUCTOS POR ROTACION */
DEFINE SHARED VAR s-FlgRotacion LIKE gn-divi.flgrotacion.
DEFINE SHARED VAR s-FlgEmpaque  LIKE GN-DIVI.FlgEmpaque.
DEFINE SHARED VAR s-FlgMinVenta LIKE GN-DIVI.FlgMinVenta.
DEFINE SHARED VAR s-VentaMayorista LIKE GN-DIVI.VentaMayorista.
DEFINE SHARED VAR s-FlgTipoVenta LIKE GN-DIVI.FlgPreVta.

MESSAGE pcoddiv.

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
&Scoped-define INTERNAL-TABLES ITEM

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table ITEM.NroItm ITEM.codmat ~
fDesMat() @ x-DesMat fDesMar() @ x-DesMar ITEM.UndVta ITEM.CanPed ~
ITEM.PreUni ITEM.Por_Dsctos[1] ITEM.Por_Dsctos[2] ITEM.Por_Dsctos[3] ~
ITEM.ImpLin 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table ITEM.codmat ITEM.CanPed ~
ITEM.PreUni 
&Scoped-define ENABLED-TABLES-IN-QUERY-br_table ITEM
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br_table ITEM
&Scoped-define QUERY-STRING-br_table FOR EACH ITEM WHERE ~{&KEY-PHRASE} NO-LOCK ~
    BY ITEM.NroItm
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH ITEM WHERE ~{&KEY-PHRASE} NO-LOCK ~
    BY ITEM.NroItm.
&Scoped-define TABLES-IN-QUERY-br_table ITEM
&Scoped-define FIRST-TABLE-IN-QUERY-br_table ITEM


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br_table 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-ImpTot 

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fDesMar B-table-Win 
FUNCTION fDesMar RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fDesMat B-table-Win 
FUNCTION fDesMat RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fPrecioUnitario B-table-Win 
FUNCTION fPrecioUnitario RETURNS DECIMAL
  ( INPUT pPreUni AS DEC ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE FILL-IN-ImpTot AS DECIMAL FORMAT "-ZZZ,ZZZ,ZZ9.99":U INITIAL 0 
     LABEL "IMPORTE TOTAL" 
     VIEW-AS FILL-IN 
     SIZE 11.72 BY .81
     BGCOLOR 1 FGCOLOR 15  NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      ITEM SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      ITEM.NroItm COLUMN-LABEL "No" FORMAT ">>>9":U
      ITEM.codmat COLUMN-LABEL "Articulo" FORMAT "X(14)":U
      fDesMat() @ x-DesMat COLUMN-LABEL "Descripci�n" FORMAT "x(60)":U
      fDesMar() @ x-DesMar COLUMN-LABEL "Marca" FORMAT "x(20)":U
      ITEM.UndVta COLUMN-LABEL "Unidad" FORMAT "x(10)":U WIDTH 5.29
      ITEM.CanPed FORMAT ">>>,>>9.99":U
      ITEM.PreUni COLUMN-LABEL "Precio!Unitario" FORMAT ">>,>>9.9999":U
      ITEM.Por_Dsctos[1] COLUMN-LABEL "% Dscto.!Manual" FORMAT "->>9.9999":U
            WIDTH 7.14
      ITEM.Por_Dsctos[2] COLUMN-LABEL "% Dscto!Evento" FORMAT "->>9.9999":U
            WIDTH 7.43
      ITEM.Por_Dsctos[3] COLUMN-LABEL "% Dscto!Vol/Prom" FORMAT "->>9.9999":U
            WIDTH 7.43
      ITEM.ImpLin FORMAT ">,>>>,>>9.99":U WIDTH 8
  ENABLE
      ITEM.codmat
      ITEM.CanPed
      ITEM.PreUni
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SIZE 141 BY 11.04
         BGCOLOR 15 FONT 4 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br_table AT ROW 1 COL 1
     FILL-IN-ImpTot AT ROW 12.04 COL 126 COLON-ALIGNED WIDGET-ID 2
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
      TABLE: ITEM T "SHARED" ? INTEGRAL FacDPedi
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
         HEIGHT             = 12.69
         WIDTH              = 141.72.
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

/* SETTINGS FOR FILL-IN FILL-IN-ImpTot IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _TblList          = "Temp-Tables.ITEM"
     _Options          = "NO-LOCK KEY-PHRASE"
     _TblOptList       = ", FIRST"
     _OrdList          = "Temp-Tables.ITEM.NroItm|yes"
     _FldNameList[1]   > Temp-Tables.ITEM.NroItm
"ITEM.NroItm" "No" ">>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.ITEM.codmat
"ITEM.codmat" "Articulo" "X(14)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > "_<CALC>"
"fDesMat() @ x-DesMat" "Descripci�n" "x(60)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > "_<CALC>"
"fDesMar() @ x-DesMar" "Marca" "x(20)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.ITEM.UndVta
"ITEM.UndVta" "Unidad" "x(10)" "character" ? ? ? ? ? ? no ? no no "5.29" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.ITEM.CanPed
"ITEM.CanPed" ? ">>>,>>9.99" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > Temp-Tables.ITEM.PreUni
"ITEM.PreUni" "Precio!Unitario" ">>,>>9.9999" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > Temp-Tables.ITEM.Por_Dsctos[1]
"ITEM.Por_Dsctos[1]" "% Dscto.!Manual" "->>9.9999" "decimal" ? ? ? ? ? ? no ? no no "7.14" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > Temp-Tables.ITEM.Por_Dsctos[2]
"ITEM.Por_Dsctos[2]" "% Dscto!Evento" "->>9.9999" "decimal" ? ? ? ? ? ? no ? no no "7.43" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > Temp-Tables.ITEM.Por_Dsctos[3]
"ITEM.Por_Dsctos[3]" "% Dscto!Vol/Prom" "->>9.9999" "decimal" ? ? ? ? ? ? no ? no no "7.43" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > Temp-Tables.ITEM.ImpLin
"ITEM.ImpLin" ? ">,>>>,>>9.99" "decimal" ? ? ? ? ? ? no ? no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
  DISPLAY I-NroItm @ ITEM.NroItm WITH BROWSE {&BROWSE-NAME}.
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


&Scoped-define SELF-NAME ITEM.codmat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ITEM.codmat br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF ITEM.codmat IN BROWSE br_table /* Articulo */
DO:
    IF SELF:SCREEN-VALUE = "" THEN RETURN.
    IF x-Adm-New-Record = "NO" AND SELF:SCREEN-VALUE = ITEM.CodMat THEN RETURN.

    DEF VAR pCodMat AS CHAR NO-UNDO.
    pCodMat = SELF:SCREEN-VALUE.
    RUN vta2/p-codigo-producto (INPUT-OUTPUT pCodMat, YES).
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
        F-FACTOR = 1
        X-CANPED = 1.
    RUN vta2/PrecioMayorista-Cred-01 (
        s-TpoPed,
        pCodDiv,
        s-CodCli,
        s-CodMon,
        INPUT-OUTPUT s-UndVta,
        OUTPUT f-Factor,
        ITEM.codmat:SCREEN-VALUE IN BROWSE {&browse-name},
        s-CndVta,
        x-CanPed,
        s-NroDec,
        OUTPUT f-PreBas,
        OUTPUT f-PreVta,
        OUTPUT f-Dsctos,
        OUTPUT y-Dsctos,
        OUTPUT z-Dsctos,
        OUTPUT x-TipDto,
        TRUE
        ).
    IF RETURN-VALUE = 'ADM-ERROR' THEN RETURN NO-APPLY.
    DISPLAY 
        Almmmatg.DesMat @ x-DesMat 
        Almmmatg.DesMar @ x-DesMar 
        s-UndVta @ ITEM.UndVta 
        F-PREVTA @ ITEM.PreUni 
        z-Dsctos @ ITEM.Por_Dsctos[2]
        y-Dsctos @ ITEM.Por_Dsctos[3]
        WITH BROWSE {&BROWSE-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ITEM.codmat br_table _BROWSE-COLUMN B-table-Win
ON LEFT-MOUSE-DBLCLICK OF ITEM.codmat IN BROWSE br_table /* Articulo */
OR F8 OF ITEM.codmat
DO:
    CASE s-TpoPed:
        WHEN "M" THEN DO:
            RUN vta2/c-listaprecios-marco ('Lista de Precios Contrato Marco').
            IF output-var-1 <> ? THEN SELF:SCREEN-VALUE = output-var-2.
        END.
        WHEN "R" THEN DO:
            RUN vta2/c-listaprecios-remate-cred ('Lista de Precios Remate').
            IF output-var-1 <> ? THEN SELF:SCREEN-VALUE = output-var-2.
        END.
        OTHERWISE DO:
            RUN vta2/c-listaprecios-cred ('Lista de Precios').
            IF output-var-1 <> ? THEN SELF:SCREEN-VALUE = output-var-2.
        END.
    END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ITEM.UndVta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ITEM.UndVta br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF ITEM.UndVta IN BROWSE br_table /* Unidad */
DO:
  SELF:SCREEN-VALUE = CAPS(SELF:SCREEN-VALUE).
  IF SELF:SCREEN-VALUE = "" THEN RETURN.
  FIND Almmmatg WHERE Almmmatg.codcia = s-codcia
      AND Almmmatg.codmat = ITEM.codmat:SCREEN-VALUE IN BROWSE {&browse-name}
      NO-LOCK NO-ERROR.
  IF NOT AVAILABLE Almmmatg THEN RETURN.
  ASSIGN 
      F-FACTOR = 1
      X-CANPED = 1
      s-UndVta = SELF:SCREEN-VALUE.
  RUN vta2/PrecioMayorista-Cred-01 (
      s-TpoPed,
      pCodDiv,
      s-CodCli,
      s-CodMon,
      INPUT-OUTPUT s-UndVta,
      OUTPUT f-Factor,
      Almmmatg.CodMat,
      s-CndVta,
      x-CanPed,
      s-NroDec,
      OUTPUT f-PreBas,
      OUTPUT f-PreVta,
      OUTPUT f-Dsctos,
      OUTPUT y-Dsctos,
      OUTPUT z-Dsctos,
      OUTPUT x-TipDto,
      TRUE
      ).
  IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
      s-UndVta = "".
      RUN vta2/PrecioMayorista-Cred-01 (
          s-TpoPed,
          pCodDiv,
          s-CodCli,
          s-CodMon,
          INPUT-OUTPUT s-UndVta,
          OUTPUT f-Factor,
          Almmmatg.CodMat,
          s-CndVta,
          x-CanPed,
          s-NroDec,
          OUTPUT f-PreBas,
          OUTPUT f-PreVta,
          OUTPUT f-Dsctos,
          OUTPUT y-Dsctos,
          OUTPUT z-Dsctos,
          OUTPUT x-TipDto,
          TRUE
          ).
      SELF:SCREEN-VALUE = s-UndVta.
      RETURN NO-APPLY.
  END.
  DISPLAY 
      s-UndVta @ ITEM.UndVta 
      F-PREVTA @ ITEM.PreUni 
      z-Dsctos @ ITEM.Por_Dsctos[2]
      y-Dsctos @ ITEM.Por_Dsctos[3]
      WITH BROWSE {&BROWSE-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ITEM.UndVta br_table _BROWSE-COLUMN B-table-Win
ON LEFT-MOUSE-DBLCLICK OF ITEM.UndVta IN BROWSE br_table /* Unidad */
OR f8 OF ITEM.UndVta
DO:
    IF AVAILABLE Almmmatg THEN DO:
        ASSIGN
            input-var-1 = Almmmatg.UndStk
            input-var-2 = ""
            input-var-3 = "".
        RUN lkup/c-equivl ("Unidades v�lidas").
        IF output-var-1 <> ? THEN DO:
            SELF:SCREEN-VALUE = output-var-2.
            RETURN NO-APPLY.
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ITEM.CanPed
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ITEM.CanPed br_table _BROWSE-COLUMN B-table-Win
ON ENTRY OF ITEM.CanPed IN BROWSE br_table /* Cantidad */
DO:
    IF ITEM.codmat:SCREEN-VALUE IN BROWSE {&browse-name} = '' THEN RETURN.
    RUN vtagn/c-uniofi-01 ("Unidades de Venta",
                        ITEM.codmat:SCREEN-VALUE IN BROWSE {&BROWSE-NAME},
                        s-codalm).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ITEM.CanPed br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF ITEM.CanPed IN BROWSE br_table /* Cantidad */
DO:
    FIND Almmmatg WHERE Almmmatg.codcia = s-codcia
        AND Almmmatg.codmat = ITEM.codmat:SCREEN-VALUE IN BROWSE {&browse-name}
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Almmmatg THEN RETURN.
    x-CanPed = DEC(SELF:SCREEN-VALUE).
    s-UndVta = ITEM.UndVta:SCREEN-VALUE IN BROWSE {&browse-name}.
    IF NOT (s-Import-IBC = YES OR s-Import-Cissac = YES) THEN DO:
        RUN vta2/PrecioMayorista-Cred-01 (
            s-TpoPed,
            pCodDiv,
            s-CodCli,
            s-CodMon,
            INPUT-OUTPUT s-UndVta,
            OUTPUT f-Factor,
            ITEM.codmat:SCREEN-VALUE IN BROWSE {&browse-name},
            s-CndVta,
            x-CanPed,
            s-NroDec,
            OUTPUT f-PreBas,
            OUTPUT f-PreVta,
            OUTPUT f-Dsctos,
            OUTPUT y-Dsctos,
            OUTPUT z-Dsctos,
            OUTPUT x-TipDto,
            TRUE
            ).
        IF RETURN-VALUE = 'ADM-ERROR' THEN RETURN NO-APPLY.
    END.
    DISPLAY 
        F-PREVTA @ ITEM.PreUni 
        z-Dsctos @ ITEM.Por_Dsctos[2]
        y-Dsctos @ ITEM.Por_Dsctos[3]
        WITH BROWSE {&BROWSE-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ITEM.CanPed br_table _BROWSE-COLUMN B-table-Win
ON LEFT-MOUSE-DBLCLICK OF ITEM.CanPed IN BROWSE br_table /* Cantidad */
OR f8 OF ITEM.CanPed
DO:
    RUN vtagn/c-uniofi-01 ("Unidades de Venta",
                        ITEM.codmat:SCREEN-VALUE IN BROWSE {&BROWSE-NAME},
                        s-codalm).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ITEM.PreUni
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ITEM.PreUni br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF ITEM.PreUni IN BROWSE br_table /* Precio!Unitario */
DO:
    /* Determinamos el PRECIO CORRECTO */
    IF ITEM.codmat:SCREEN-VALUE IN BROWSE {&browse-name} = '' THEN RETURN.
    ASSIGN
        x-CanPed = DEC(ITEM.CanPed:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
        s-UndVta = ITEM.UndVta:SCREEN-VALUE IN BROWSE {&browse-name}.
    RUN vta2/PrecioMayorista-Cred-01 (
        s-TpoPed,
        pCodDiv,
        s-CodCli,
        s-CodMon,
        INPUT-OUTPUT s-UndVta,
        OUTPUT f-Factor,
        ITEM.codmat:SCREEN-VALUE IN BROWSE {&browse-name},
        s-CndVta,
        x-CanPed,
        s-NroDec,
        OUTPUT f-PreBas,
        OUTPUT f-PreVta,
        OUTPUT f-Dsctos,
        OUTPUT y-Dsctos,
        OUTPUT z-Dsctos,
        OUTPUT x-TipDto,
        TRUE
        ).
    IF RETURN-VALUE = 'ADM-ERROR' THEN RETURN NO-APPLY.
    
    IF fPrecioUnitario(f-PreVta) > DEC(SELF:SCREEN-VALUE) THEN DO:
        MESSAGE "No autorizado para bajar el precio unitario" 
           VIEW-AS ALERT-BOX ERROR.
       DISPLAY F-PREVTA @ ITEM.PreUni WITH BROWSE {&BROWSE-NAME}.
       RETURN NO-APPLY.   
    END.
/*     IF F-PREVTA > DEC(ITEM.PreUni:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) THEN DO: */
/*         MESSAGE "No autorizado para bajar el precio unitario"                     */
/*            VIEW-AS ALERT-BOX ERROR.                                               */
/*        DISPLAY F-PREVTA @ ITEM.PreUni WITH BROWSE {&BROWSE-NAME}.                 */
/*        RETURN NO-APPLY.                                                           */
/*     END.                                                                          */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

ON 'RETURN':U OF ITEM.CanPed, ITEM.codmat, ITEM.UndVta, ITEM.PreUni 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE desde_cotiz_excel B-table-Win 
PROCEDURE desde_cotiz_excel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imp-Total B-table-Win 
PROCEDURE Imp-Total :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

ASSIGN
    FILL-IN-ImpTot = 0.
FOR EACH B-ITEM:
    FILL-IN-ImpTot = FILL-IN-ImpTot + B-ITEM.ImpLin.
END.
DISPLAY FILL-IN-ImpTot WITH FRAME {&FRAME-NAME}.

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
  IF s-CodCli = '' THEN DO:
      MESSAGE 'Debe ingresar el CLIENTE' VIEW-AS ALERT-BOX ERROR.
      RETURN "ADM-ERROR".
  END.
  IF s-CndVta = '' THEN DO:
      MESSAGE 'Debe ingresar la CONDICION DE VENTA' VIEW-AS ALERT-BOX ERROR.
      RETURN "ADM-ERROR".
  END.
  IF s-import-ibc = YES OR s-Import-Cissac = YES THEN DO:
      MESSAGE 'Acceso Denegado' VIEW-AS ALERT-BOX ERROR.
      RETURN 'ADM-ERROR'.
  END.
  I-NroItm = 0.  
  DEFINE VARIABLE N-ITMS AS INTEGER INIT 0 NO-UNDO.
  FOR EACH B-ITEM BY B-ITEM.NroItm:
      N-ITMS = N-ITMS + 1.
      I-NroItm = B-ITEM.NroItm.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN
      I-NroItm = I-NroItm + 1
      s-UndVta = ""
      x-Adm-New-Record = "YES".
  
  RUN Procesa-Handle IN lh_handle ('Disable-Head').
  APPLY 'ENTRY':U TO ITEM.codmat IN BROWSE {&browse-name}.
  
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
      AND Almmmatg.codmat = ITEM.codmat:SCREEN-VALUE IN BROWSE {&browse-name}
      NO-LOCK NO-ERROR.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-statement':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN 
      ITEM.CodCia = S-CODCIA
      ITEM.Factor = F-FACTOR
      ITEM.NroItm = I-NroItm
      ITEM.PorDto = f-Dsctos
      ITEM.PreBas = F-PreBas 
      ITEM.PreVta[1] = F-PreVta     /* CONTROL DE PRECIO DE LISTA */
      ITEM.AftIgv = Almmmatg.AftIgv
      ITEM.AftIsc = Almmmatg.AftIsc
      ITEM.Libre_c04 = x-TipDto.
  ASSIGN 
      ITEM.UndVta = ITEM.UndVta:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
      ITEM.PreUni = DEC(ITEM.PreUni:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
      ITEM.Por_Dsctos[1] = DEC(ITEM.Por_Dsctos[1]:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
      ITEM.Por_Dsctos[2] = DEC(ITEM.Por_Dsctos[2]:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
      ITEM.Por_Dsctos[3] = DEC(ITEM.Por_Dsctos[3]:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}).
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

  /* REPETIDOS */
  IF CAN-FIND(FIRST B-ITEM WHERE B-ITEM.codmat = ITEM.codmat
              AND ROWID(B-ITEM) <> ROWID(ITEM) NO-LOCK) THEN DO:
      MESSAGE 'Product YA registrado' VIEW-AS ALERT-BOX ERROR.
      APPLY 'ENTRY':U TO ITEM.codmat IN BROWSE {&browse-name}.
      UNDO, RETURN 'ADM-ERROR'.
  END.
  /* ********* */

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

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN Imp-Total.

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
  RUN GET-ATTRIBUTE('ADM-NEW-RECORD').
  IF RETURN-VALUE = "NO" THEN APPLY 'ENTRY':U TO ITEM.CodMat IN BROWSE {&BROWSE-NAME}.
  IF s-Import-IBC = YES OR s-Import-Cissac = YES THEN DO:
      ITEM.codmat:READ-ONLY IN BROWSE {&BROWSE-NAME} = YES.
      ITEM.PreUni:READ-ONLY IN BROWSE {&BROWSE-NAME} = YES.
      APPLY 'ENTRY':U TO ITEM.CanPed IN BROWSE {&BROWSE-NAME}.
  END.
  CASE s-TpoPed:
      WHEN "M" THEN ITEM.PreUni:READ-ONLY IN BROWSE {&BROWSE-NAME} = YES.
  END CASE.

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
  DEF VAR x-adm-new-record AS CHAR.
  RUN valida.
  IF RETURN-VALUE = "ADM-ERROR" THEN RETURN "ADM-ERROR".
  RUN GET-ATTRIBUTE('ADM-NEW-RECORD').
  x-adm-new-record = RETURN-VALUE.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .
  IF RETURN-VALUE = "ADM-ERROR" THEN RETURN "ADM-ERROR".

  /* Code placed here will execute AFTER standard behavior.    */
  RUN dispatch IN THIS-PROCEDURE ('display-fields':U).
  RUN Imp-Total.
  RUN Procesa-Handle IN lh_handle ('Enable-Head').
  IF x-adm-new-record = "YES" THEN RUN Procesa-Handle IN lh_handle ('Add-Record').

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
  Notes:       NO se recalcula si se ha migrado el EDI para el IBC (Supermercados)
------------------------------------------------------------------------------*/

FOR EACH ITEM, FIRST Almmmatg OF ITEM NO-LOCK:
    ASSIGN
        F-FACTOR = ITEM.Factor
        x-CanPed = ITEM.CanPed
        s-UndVta = ITEM.UndVta
        f-PreVta = ITEM.PreUni
        f-PreBas = ITEM.PreBas
        f-Dsctos = ITEM.PorDto
        z-Dsctos = ITEM.Por_Dsctos[2]
        y-Dsctos = ITEM.Por_Dsctos[3].
    IF NOT (s-Import-IBC = YES OR s-Import-Cissac = YES)
        THEN RUN vta2/PrecioMayorista-Cred-01 (
            s-TpoPed,
            pCodDiv,
            s-CodCli,
            s-CodMon,
            INPUT-OUTPUT s-UndVta,
            OUTPUT f-Factor,
            Almmmatg.CodMat,
            s-CndVta,
            x-CanPed,
            s-NroDec,
            OUTPUT f-PreBas,
            OUTPUT f-PreVta,
            OUTPUT f-Dsctos,
            OUTPUT y-Dsctos,
            OUTPUT z-Dsctos,
            OUTPUT x-TipDto,
            TRUE
            ).
    ASSIGN 
        ITEM.Factor = f-Factor
        ITEM.UndVta = s-UndVta
        ITEM.PreUni = F-PREVTA
        ITEM.PreBas = F-PreBas 
        ITEM.PreVta[1] = F-PreVta   /* CONTROL DE PRECIO DE LISTA */
        ITEM.PorDto = F-DSCTOS      /* Ambos descuentos afectan */
        ITEM.PorDto2 = 0            /* el precio unitario */
        ITEM.Por_Dsctos[2] = z-Dsctos
        ITEM.Por_Dsctos[3] = Y-DSCTOS 
        ITEM.AftIgv = Almmmatg.AftIgv
        ITEM.AftIsc = Almmmatg.AftIsc
        ITEM.ImpIsc = 0
        ITEM.ImpIgv = 0.
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
    IF ITEM.AftIsc THEN 
        ITEM.ImpIsc = ROUND(ITEM.PreBas * ITEM.CanPed * (Almmmatg.PorIsc / 100),4).
    ELSE ITEM.ImpIsc = 0.
    IF ITEM.AftIgv THEN  
        ITEM.ImpIgv = ITEM.ImpLin - ROUND(ITEM.ImpLin  / (1 + (s-PorIgv / 100)),4).
    ELSE ITEM.ImpIgv = 0.
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
  {src/adm/template/snd-list.i "ITEM"}

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
  
  IF ITEM.CodMat:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = ''
    OR INTEGER(ITEM.CodMat:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) = 0 THEN DO:
       MESSAGE "Codigo de Producto no puede ser blanco" VIEW-AS ALERT-BOX ERROR.
       APPLY 'ENTRY':U TO ITEM.CodMat.
       RETURN "ADM-ERROR".
  END.
  FIND Almmmatg WHERE Almmmatg.codcia = s-codcia
      AND Almmmatg.codmat = ITEM.CodMat:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
      NO-LOCK NO-ERROR.
  IF NOT AVAILABLE Almmmatg THEN DO:
      MESSAGE 'C�digo de producto NO registrado' VIEW-AS ALERT-BOX ERROR.
      APPLY 'ENTRY':U TO ITEM.CodMat.
      RETURN "ADM-ERROR".
  END.
  IF Almmmatg.TpoArt = "D" THEN DO:
      MESSAGE 'C�digo de producto DESACTIVADO' VIEW-AS ALERT-BOX ERROR.
      APPLY 'ENTRY':U TO ITEM.CodMat.
      RETURN "ADM-ERROR".
  END.
  /* FAMILIA DE VENTAS */
  FIND Almtfami OF Almmmatg NO-LOCK.
  IF Almtfami.SwComercial = NO THEN DO:
      MESSAGE 'L�nea NO autorizada para ventas' VIEW-AS ALERT-BOX ERROR.
      APPLY 'ENTRY':U TO ITEM.CodMat.
      RETURN "ADM-ERROR".
  END.
  FIND Almsfami OF Almmmatg NO-LOCK.
  IF AlmSFami.SwDigesa = YES AND Almmmatg.VtoDigesa <> ? AND Almmmatg.VtoDigesa < TODAY THEN DO:
      MESSAGE 'Producto con autorizaci�n de DIGESA VENCIDA' VIEW-AS ALERT-BOX ERROR.
      APPLY 'ENTRY':U TO ITEM.CodMat.
      RETURN "ADM-ERROR".
  END.
  /* ARTICULO */
  DEF VAR pCodMat AS CHAR.
  pCodMat = ITEM.CodMat:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}.
  RUN vta2/p-codigo-producto (INPUT-OUTPUT pCodMat, YES).
  IF RETURN-VALUE = "ADM-ERROR" THEN DO:
      APPLY 'ENTRY':U TO ITEM.CodMat.
      RETURN "ADM-ERROR".
  END.
  /* RHC 21/08/2012 CONTROL POR TIPO DE PRODUCTO */
  IF Almmmatg.TpoMrg = "1" AND s-FlgTipoVenta = NO THEN DO:
      MESSAGE "No se puede vender este producto al por menor"
          VIEW-AS ALERT-BOX ERROR.
      APPLY 'ENTRY':U TO ITEM.CodMat.
      RETURN 'ADM-ERROR'.
  END.
  IF Almmmatg.TpoMrg = "2" AND s-FlgTipoVenta = YES THEN DO:
      MESSAGE "No se puede vender este producto al por mayor"
          VIEW-AS ALERT-BOX ERROR.
      APPLY 'ENTRY':U TO ITEM.CodMat.
      RETURN 'ADM-ERROR'.
  END.
  /* ********************************************* */
  /* Unidad */
  IF ITEM.UndVta:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = "" THEN DO:
       MESSAGE "NO tiene registrado la unidad de venta" VIEW-AS ALERT-BOX ERROR.
       APPLY 'ENTRY':U TO ITEM.CodMat.
       RETURN "ADM-ERROR".
  END.
  /* CANTIDAD */
  IF DECIMAL(ITEM.CanPed:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) = 0 THEN DO:
       MESSAGE "Cantidad debe ser mayor a cero" VIEW-AS ALERT-BOX ERROR.
       APPLY 'ENTRY':U TO ITEM.CanPed.
       RETURN "ADM-ERROR".
  END.
  IF ITEM.UndVta:SCREEN-VALUE IN BROWSE {&browse-name} = "UNI" 
      AND DECIMAL(ITEM.CanPed:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) -
            TRUNCATE(DECIMAL(ITEM.CanPed:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}), 0) <> 0
      THEN DO:
      MESSAGE "NO se permiten ventas fraccionadas" VIEW-AS ALERT-BOX ERROR.
      APPLY 'ENTRY':U TO ITEM.CanPed.
      RETURN "ADM-ERROR".
  END.
  /* EMPAQUE */
  DEF VAR f-Canped AS DEC NO-UNDO.
  IF s-FlgEmpaque = YES THEN DO:
      f-CanPed = DECIMAL(ITEM.CanPed:SCREEN-VALUE IN BROWSE {&browse-name}) * f-Factor.
      /* RHC solo se va a trabajar con una lista general */
/*       IF s-VentaMayorista = 2 THEN DO:  /* LISTA POR DIVISION */                                            */
/*           FIND FIRST VtaListaMay OF Almmmatg WHERE Vtalistamay.coddiv = s-coddiv NO-LOCK NO-ERROR.          */
/*           IF AVAILABLE VtaListaMay AND Vtalistamay.CanEmp > 0 THEN DO:                                      */
/*               f-CanPed = (TRUNCATE((f-CanPed / Vtalistamay.CanEmp),0) * Vtalistamay.CanEmp).                */
/*               IF f-CanPed <> DECIMAL(ITEM.CanPed:SCREEN-VALUE IN BROWSE {&browse-name}) * f-Factor THEN DO: */
/*                   MESSAGE 'Solo puede vender en empaques de' Vtalistamay.CanEmp Almmmatg.UndBas             */
/*                       VIEW-AS ALERT-BOX ERROR.                                                              */
/*                   APPLY 'ENTRY':U TO ITEM.CanPed.                                                           */
/*                   RETURN "ADM-ERROR".                                                                       */
/*               END.                                                                                          */
/*           END.                                                                                              */
/*       END.                                                                                                  */
/*       ELSE DO:      /* LISTA GENERAL */                                                                     */
/*           IF Almmmatg.DEC__03 > 0 THEN DO:                                                                  */
/*               f-CanPed = (TRUNCATE((f-CanPed / Almmmatg.DEC__03),0) * Almmmatg.DEC__03).                    */
/*               IF f-CanPed <> DECIMAL(ITEM.CanPed:SCREEN-VALUE IN BROWSE {&browse-name}) * f-Factor THEN DO: */
/*                   MESSAGE 'Solo puede vender en empaques de' Almmmatg.DEC__03 Almmmatg.UndBas               */
/*                       VIEW-AS ALERT-BOX ERROR.                                                              */
/*                   APPLY 'ENTRY':U TO ITEM.CanPed.                                                           */
/*                   RETURN "ADM-ERROR".                                                                       */
/*               END.                                                                                          */
/*           END.                                                                                              */
/*       END.                                                                                                  */
      IF Almmmatg.DEC__03 > 0 THEN DO:
          f-CanPed = (TRUNCATE((f-CanPed / Almmmatg.DEC__03),0) * Almmmatg.DEC__03).
          IF f-CanPed <> DECIMAL(ITEM.CanPed:SCREEN-VALUE IN BROWSE {&browse-name}) * f-Factor THEN DO:
              MESSAGE 'Solo puede vender en empaques de' Almmmatg.DEC__03 Almmmatg.UndBas
                  VIEW-AS ALERT-BOX ERROR.
              APPLY 'ENTRY':U TO ITEM.CanPed.
              RETURN "ADM-ERROR".
          END.
      END.
  END.
  /* MINIMO DE VENTA */
  IF s-FlgMinVenta = YES THEN DO:
      f-CanPed = DECIMAL(ITEM.CanPed:SCREEN-VALUE IN BROWSE {&browse-name}) * f-Factor.
      IF s-VentaMayorista = 2 THEN DO:  /* LISTA POR DIVISION */
          FIND FIRST VtaListaMay OF Almmmatg WHERE Vtalistamay.coddiv = pCodDiv NO-LOCK NO-ERROR.
          IF AVAILABLE VtaListaMay AND Vtalistamay.CanEmp > 0 THEN DO:
              IF f-CanPed < Vtalistamay.CanEmp THEN DO:
                  MESSAGE 'Solo puede vender como m�nimo' Vtalistamay.CanEmp Almmmatg.UndBas
                      VIEW-AS ALERT-BOX ERROR.
                  APPLY 'ENTRY':U TO ITEM.CanPed.
                  RETURN "ADM-ERROR".
              END.
          END.
      END.
      ELSE DO:      /* LISTA GENERAL */
          IF Almmmatg.DEC__03 > 0 THEN DO:
              IF f-CanPed < Almmmatg.DEC__03 THEN DO:
                  MESSAGE 'Solo puede vender como m�nimo' Almmmatg.DEC__03 Almmmatg.UndBas
                      VIEW-AS ALERT-BOX ERROR.
                  APPLY 'ENTRY':U TO ITEM.CanPed.
                  RETURN "ADM-ERROR".
              END.
          END.
      END.
  END.

  /* PRECIO UNITARIO */
  IF DECIMAL(ITEM.PreUni:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) <= 0 THEN DO:
       MESSAGE "Precio debe ser mayor a cero" VIEW-AS ALERT-BOX ERROR.
       APPLY 'ENTRY':U TO ITEM.CodMat.
       RETURN "ADM-ERROR".
  END.

  /* CONTRATO MARCO NI REMATES NO TIENE MINIMO NI MARGEN DE UTILIDAD */
  IF LOOKUP(s-TpoPed, "M,R") > 0 THEN RETURN "OK".   

  /* CONSISTENCIA DE STOCK */
  DEFINE VARIABLE s-StkComprometido AS DECIMAL NO-UNDO.

  FIND Almmmate WHERE Almmmate.CodCia = S-CODCIA 
      AND  Almmmate.CodAlm = ENTRY(1, s-CodAlm)
      AND  Almmmate.codmat = ITEM.CodMat:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
      NO-LOCK NO-ERROR.
  IF AVAILABLE Almmmate THEN DO:
      RUN vta2/Stock-Comprometido (ITEM.CodMat:SCREEN-VALUE IN BROWSE {&BROWSE-NAME},
                                   ENTRY(1, s-CodAlm),
                                   OUTPUT s-StkComprometido).
      IF DECIMAL(ITEM.CanPed:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) > (Almmmate.StkAct - s-StkComprometido) THEN DO:
            MESSAGE "No hay STOCK disponible en el almacen" ENTRY(1, s-CodAlm) SKIP(1)
                    "     STOCK ACTUAL : " almmmate.StkAct SKIP
                  "     COMPROMETIDO : " s-StkComprometido  SKIP(1)
                  " Continuamos con la grabacion?"
                  VIEW-AS ALERT-BOX WARNING BUTTONS YES-NO
                  UPDATE rpta AS LOG.
          IF rpta = NO THEN RETURN "ADM-ERROR".
      END.
  END.

  /* RHC 13.12.2010 Margen de Utilidad */
/*   DEF VAR pError AS CHAR NO-UNDO.                                                         */
/*   DEF VAR X-MARGEN AS DEC NO-UNDO.                                                        */
/*   DEF VAR X-LIMITE AS DEC NO-UNDO.                                                        */
/*   DEF VAR x-PreUni AS DEC NO-UNDO.                                                        */
/*                                                                                           */
/*   x-PreUni = DECIMAL ( ITEM.PreUni:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} ) *              */
/*       ( 1 - DECIMAL (ITEM.Por_Dsctos[1]:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} ) / 100 ) * */
/*       ( 1 - DECIMAL (ITEM.Por_Dsctos[2]:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} )/ 100 ) *  */
/*       ( 1 - DECIMAL (ITEM.Por_Dsctos[3]:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} ) / 100 ) . */
/*   RUN vtagn/p-margen-utilidad (                                                           */
/*       ITEM.CodMat:SCREEN-VALUE IN BROWSE {&BROWSE-NAME},      /* Producto */              */
/*       x-PreUni,  /* Precio de venta unitario */                                           */
/*       ITEM.UndVta:SCREEN-VALUE IN BROWSE {&BROWSE-NAME},                                  */
/*       s-CodMon,       /* Moneda de venta */                                               */
/*       s-TpoCmb,       /* Tipo de cambio */                                                */
/*       YES,            /* Muestra el error */                                              */
/*       "",                                                                                 */
/*       OUTPUT x-Margen,        /* Margen de utilidad */                                    */
/*       OUTPUT x-Limite,        /* Margen m�nimo de utilidad */                             */
/*       OUTPUT pError           /* Control de errores: "OK" "ADM-ERROR" */                  */
/*       ).                                                                                  */
/*   IF pError = "ADM-ERROR" THEN DO:                                                        */
/*       APPLY 'ENTRY':U TO ITEM.CodMat.                                                     */
/*       RETURN "ADM-ERROR".                                                                 */
/*   END.                                                                                    */

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
IF AVAILABLE ITEM THEN 
    ASSIGN
    i-nroitm = ITEM.NroItm
    f-Factor = ITEM.Factor
    f-PreBas = ITEM.PreBas
    f-PreVta = ITEM.PreUni
    s-UndVta = ITEM.UndVta
    x-TipDto = ITEM.Libre_c04
    x-Adm-New-Record = "NO".
RUN Procesa-Handle IN lh_handle ('Disable-Head').

RETURN "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fDesMar B-table-Win 
FUNCTION fDesMar RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

IF NOT AVAILABLE ITEM THEN RETURN "".   /* Function return value. */
FIND Almmmatg OF ITEM NO-LOCK NO-ERROR.
IF AVAILABLE Almmmatg THEN RETURN Almmmatg.DesMar.
RETURN "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fDesMat B-table-Win 
FUNCTION fDesMat RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

IF NOT AVAILABLE ITEM THEN RETURN "".   /* Function return value. */
FIND Almmmatg OF ITEM NO-LOCK NO-ERROR.
IF AVAILABLE Almmmatg THEN RETURN Almmmatg.DesMat.
RETURN "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fPrecioUnitario B-table-Win 
FUNCTION fPrecioUnitario RETURNS DECIMAL
  ( INPUT pPreUni AS DEC ):
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

DEF VAR f-PreBas AS DEC NO-UNDO.
DEF VAR s-UndVta AS CHAR NO-UNDO.
DEF VAR f-Factor AS DEC NO-UNDO.
DEF VAR s-TpoCmb AS DEC NO-UNDO.

IF s-CodDiv <> "00018" THEN RETURN pPreUni.

FIND gn-divi WHERE gn-divi.codcia = s-codcia AND gn-divi.coddiv = s-CodDiv NO-LOCK.
IF gn-divi.Libre_L01 = NO THEN RETURN pPreUni.

/* NO debe ser un cliente especial */
FIND VtaTabla WHERE VtaTabla.CodCia = s-codcia
    AND VtaTabla.Tabla = "PL1" 
    AND VtaTabla.Llave_c1 = s-codcli
    NO-LOCK NO-ERROR.
IF AVAILABLE VtaTabla THEN RETURN pPreUni.
/* Debe estar registrado en la lista #1 */
FIND FIRST VtaListaMay OF Almmmatg WHERE VtaListaMay.CodDiv = "LP1" NO-LOCK NO-ERROR.
IF NOT AVAILABLE VtaListaMay THEN RETURN pPreUni.
/* PRECIO BASE Y UNIDAD DE VENTA */
ASSIGN
    F-PreBas = VtaListaMay.PreOfi
    s-UndVta = VtaListaMay.Chr__01.
/* FACTOR DE EQUIVALENCIA */
FIND Almtconv WHERE Almtconv.CodUnid  = Almmmatg.UndStk
    AND Almtconv.Codalter = s-undvta
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE Almtconv THEN RETURN pPreUni.
ASSIGN
    F-FACTOR = Almtconv.Equival / Almmmatg.FacEqu
    s-tpocmb = VtaListaMay.TpoCmb.     /* �? */
/* PRECIO BASE  */
IF S-CODMON = 1 THEN DO:
    IF VtaListaMay.MonVta = 1 THEN ASSIGN F-PREBAS = F-PREBAS * F-FACTOR.
    ELSE ASSIGN F-PREBAS = F-PREBAS * S-TPOCMB * F-FACTOR.
END.
IF S-CODMON = 2 THEN DO:
    IF VtaListaMay.MonVta = 2 THEN ASSIGN F-PREBAS = F-PREBAS * F-FACTOR.
    ELSE ASSIGN F-PREBAS = (F-PREBAS / S-TPOCMB) * F-FACTOR.
END.
RETURN MINIMUM(f-PreBas, pPreUni).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

