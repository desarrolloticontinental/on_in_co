&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          integral         PROGRESS
*/
&Scoped-define WINDOW-NAME W-Win


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE T-MATG NO-UNDO LIKE Almmmatg
       FIELD StkAct AS DEC
       FIELD Barras LIKE Almmmat1.Barras
       FIELD Equival LIKE Almmmat1.Equival
       FIELD CodIntPr1 LIKE Almmmatgext.CodIntPr1.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: 
          
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
DEFINE NEW SHARED VARIABLE lh_Handle  AS HANDLE.

DEFINE        VAR C-OP     AS CHAR.
DEFINE SHARED VAR S-CODCIA AS INTEGER.
DEFINE SHARED VAR PV-CODCIA AS INTEGER.
DEFINE SHARED VAR S-NOMCIA AS CHARACTER.
DEFINE        VAR F-PESALM AS DECIMAL NO-UNDO.
DEFINE SHARED VAR S-CODALM AS CHARACTER.
DEFINE SHARED VAR S-DESALM AS CHARACTER.

DEFINE VAR RUTA AS CHAR NO-UNDO.
GET-KEY-VALUE SECTION "STARTUP" KEY "BASE" VALUE RUTA.

DEFINE IMAGE IMAGE-1 FILENAME "IMG\print" SIZE 5 BY 1.5.
DEF VAR FI-MENSAJE AS CHAR FORMAT "X(40)" .

DEFINE FRAME F-Proceso
     IMAGE-1 AT ROW 1.5 COL 5
     "Espere un momento" VIEW-AS TEXT
          SIZE 18 BY 1 AT ROW 1.5 COL 16 FONT 6
     "por favor ...." VIEW-AS TEXT
          SIZE 10 BY 1 AT ROW 2.5 COL 19 FONT 6
          SKIP
     Fi-Mensaje NO-LABEL FONT 6
     SKIP     
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE 
         BGCOLOR 15 FGCOLOR 0 
         TITLE "Procesando ..." FONT 7.
DEFINE BUFFER T-MATE FOR Almmmate.

/****************/
DEFINE VAR F-STKGEN  AS DECIMAL NO-UNDO.
DEFINE VAR F-PESGEN  AS DECIMAL NO-UNDO.
DEFINE VAR F-VALCTO  AS DECIMAL NO-UNDO.
DEFINE VAR F-PRECTO  AS DECIMAL NO-UNDO.
DEFINE VAR C-MONEDA  AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VAR S-SUBTIT  AS CHARACTER FORMAT "X(40)" NO-UNDO.
DEFINE VAR S-SUBTIT-1 AS CHARACTER FORMAT "X(40)" NO-UNDO.

DEFINE VAR F-STKALM  AS DECIMAL NO-UNDO.

DEFINE BUFFER B-Mate FOR Almmmate.
DEFINE VAR CATEGORIA AS CHAR INIT "MD".
DEFINE VAR x-nompro AS CHAR FORMAT "X(30)".
DEFINE VAR x-codpro AS CHAR FORMAT "X(11)".

/*****************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS x-CodFam x-CodPr1 DesdeC HastaC R-Tipo ~
RADIO-SET-AftIgv Btn_OK-2 Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS x-CodFam x-CodPr1 DesdeC HastaC R-Tipo ~
RADIO-SET-AftIgv txt-mensaje 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel 
     IMAGE-UP FILE "img\b-cancel":U
     LABEL "Cancelar" 
     SIZE 13 BY 1.5
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK 
     IMAGE-UP FILE "img\b-ok":U
     LABEL "Aceptar" 
     SIZE 13 BY 1.5
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK-2 
     IMAGE-UP FILE "img/excel.bmp":U
     LABEL "Aceptar" 
     SIZE 13 BY 1.5
     BGCOLOR 8 .

DEFINE VARIABLE DesdeC AS CHARACTER FORMAT "X(6)":U 
     LABEL "Articulo" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .81 NO-UNDO.

DEFINE VARIABLE HastaC AS CHARACTER FORMAT "X(6)":U 
     LABEL "A" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .81 NO-UNDO.

DEFINE VARIABLE txt-mensaje AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 73.14 BY .81
     FONT 0 NO-UNDO.

DEFINE VARIABLE x-CodFam AS CHARACTER FORMAT "X(3)":U 
     LABEL "Familia" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .81 NO-UNDO.

DEFINE VARIABLE x-CodPr1 AS CHARACTER FORMAT "X(11)":U 
     LABEL "Proveedor" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .81 NO-UNDO.

DEFINE VARIABLE R-Tipo AS CHARACTER INITIAL "A" 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Todos", "",
"Activados", "A",
"Desactivados", "D"
     SIZE 12.86 BY 2.31 NO-UNDO.

DEFINE VARIABLE RADIO-SET-AftIgv AS INTEGER INITIAL 3 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Afectos a IGV", 1,
"NO Afectos a IGV", 2,
"Ambos", 3
     SIZE 17 BY 2.15 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     x-CodFam AT ROW 1.38 COL 24 COLON-ALIGNED
     x-CodPr1 AT ROW 2.35 COL 24 COLON-ALIGNED
     DesdeC AT ROW 3.31 COL 24 COLON-ALIGNED
     HastaC AT ROW 3.31 COL 39 COLON-ALIGNED
     R-Tipo AT ROW 4.27 COL 26 NO-LABEL
     RADIO-SET-AftIgv AT ROW 4.5 COL 52 NO-LABEL WIDGET-ID 6
     txt-mensaje AT ROW 6.65 COL 2.86 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     Btn_OK AT ROW 7.73 COL 7
     Btn_OK-2 AT ROW 7.77 COL 20.57 WIDGET-ID 2
     Btn_Cancel AT ROW 7.77 COL 34.14
     "Estado" VIEW-AS TEXT
          SIZE 8 BY .5 AT ROW 4.27 COL 18
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 8.85
         FONT 4.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: T-MATG T "?" NO-UNDO INTEGRAL Almmmatg
      ADDITIONAL-FIELDS:
          FIELD StkAct AS DEC
          FIELD Barras LIKE Almmmat1.Barras
          FIELD Equival LIKE Almmmat1.Equival
          FIELD CodIntPr1 LIKE Almmmatgext.CodIntPr1
      END-FIELDS.
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Catalogo de Materiales"
         HEIGHT             = 8.85
         WIDTH              = 80
         MAX-HEIGHT         = 8.85
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 8.85
         VIRTUAL-WIDTH      = 80
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT W-Win:LOAD-ICON("img\climnu3":U) THEN
    MESSAGE "Unable to load icon: img\climnu3"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-Win 
/* ************************* Included-Libraries *********************** */

{src/bin/_prns.i}
{src/adm-vm/method/vmviewer.i}
{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   FRAME-NAME L-To-R                                                    */
/* SETTINGS FOR BUTTON Btn_OK IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       Btn_OK:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN txt-mensaje IN FRAME F-Main
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Catalogo de Materiales */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Catalogo de Materiales */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel W-Win
ON CHOOSE OF Btn_Cancel IN FRAME F-Main /* Cancelar */
DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK W-Win
ON CHOOSE OF Btn_OK IN FRAME F-Main /* Aceptar */
DO:
  RUN Asigna-Variables.
  RUN Inhabilita.
  RUN Imprime.
  RUN Habilita.
  RUN Inicializa-Variables.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK-2 W-Win
ON CHOOSE OF Btn_OK-2 IN FRAME F-Main /* Aceptar */
DO:
  RUN Asigna-Variables.
  RUN Inhabilita.
  SESSION:SET-WAIT-STATE('GENERAL').
  RUN Excel.
  SESSION:SET-WAIT-STATE('').
  RUN Habilita.
  RUN Inicializa-Variables.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME DesdeC
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DesdeC W-Win
ON LEAVE OF DesdeC IN FRAME F-Main /* Articulo */
DO:
  IF TRUE <> (SELF:SCREEN-VALUE > "") THEN RETURN.
  ASSIGN 
      SELF:SCREEN-VALUE = STRING(INTEGER(SELF:SCREEN-VALUE),"999999") NO-ERROR.
  FIND Almmmatg WHERE Almmmatg.CodCia = S-CODCIA 
      AND  Almmmatg.CodMat = SELF:SCREEN-VALUE 
      NO-LOCK NO-ERROR.
  IF AVAILABLE Almmmatg THEN SELF:SCREEN-VALUE = Almmmatg.codmat.
/*   IF NOT AVAILABLE Almmmatg THEN DO:                      */
/*       MESSAGE "Codigo no Existe" VIEW-AS ALERT-BOX ERROR. */
/*       RETURN NO-APPLY.                                    */
/*   END.                                                    */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME HastaC
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL HastaC W-Win
ON LEAVE OF HastaC IN FRAME F-Main /* A */
DO:
    IF TRUE <> (SELF:SCREEN-VALUE > "") THEN RETURN.
    ASSIGN 
        SELF:SCREEN-VALUE = STRING(INTEGER(SELF:SCREEN-VALUE),"999999") NO-ERROR.
    FIND Almmmatg WHERE Almmmatg.CodCia = S-CODCIA
        AND  Almmmatg.CodMat = SELF:SCREEN-VALUE
        NO-LOCK NO-ERROR.
    IF AVAILABLE Almmmatg THEN SELF:SCREEN-VALUE = Almmmatg.codmat.
/*     IF NOT AVAILABLE Almmmatg THEN DO:                      */
/*         MESSAGE "Codigo no Existe" VIEW-AS ALERT-BOX ERROR. */
/*         RETURN NO-APPLY.                                    */
/*     END.                                                    */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME x-CodPr1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL x-CodPr1 W-Win
ON LEAVE OF x-CodPr1 IN FRAME F-Main /* Proveedor */
DO:
  IF SELF:SCREEN-VALUE = "" THEN RETURN.
  FIND gn-prov WHERE gn-prov.CodCia = S-CODCIA 
                AND  gn-prov.CodPro = SELF:SCREEN-VALUE 
               NO-LOCK NO-ERROR.
  IF AVAILABLE gn-prov THEN do:
/*     DISPLAY gn-prov.NomPro @ FILL-IN-NomPro1 WITH FRAME {&FRAME-NAME}.*/
  END.
  ELSE DO:
      FIND gn-prov WHERE gn-prov.CodCia = 0 
                    AND  gn-prov.CodPro = SELF:SCREEN-VALUE 
                   NO-LOCK NO-ERROR.
      IF NOT AVAILABLE gn-prov THEN DO:
           MESSAGE "Codigo de Proveedor no Existe" VIEW-AS ALERT-BOX ERROR.
           RETURN NO-APPLY.
      END.
  END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */
/* VM - INCLUDE PARA LA CREACION DEL MENU BAR */
{src/adm/template/cntnrwin.i}

/* Include custom  Main Block code for SmartWindows. */

{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects W-Win  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available W-Win  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Asigna-Variables W-Win 
PROCEDURE Asigna-Variables :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  ASSIGN DesdeC HastaC R-Tipo x-CodFam x-CodPr1.
  ASSIGN RADIO-SET-AftIgv.
  
  IF HastaC <> "" THEN S-SUBTIT = "Materiales del " + DesdeC + " al " + HastaC .
  ELSE S-SUBTIT = "".

  /*IF HastaC = "" THEN HastaC = "999999999".*/
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Carga-Temporal W-Win 
PROCEDURE Carga-Temporal :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  EMPTY TEMP-TABLE T-MATG.
  
  FOR EACH Almmmatg NO-LOCK WHERE Almmmatg.codcia = s-codcia
      AND (TRUE <> (x-CodPr1 > '') OR Almmmatg.CodPr1 = x-CodPr1)
      AND (TRUE <> (x-CodFam > '') OR almmmatg.codfam = x-CodFam)
      AND (TRUE <> (DesdeC > '') OR Almmmatg.codmat >= DesdeC) 
      AND (TRUE <> (HastaC > '') OR Almmmatg.codmat <= HastaC),
      FIRST Almtfami OF Almmmatg NO-LOCK,
      FIRST Almsfami OF Almmmatg NO-LOCK:
      IF r-Tipo = 'A' AND Almmmatg.TpoArt = 'D' THEN NEXT.
      IF r-Tipo = 'D' AND Almmmatg.TpoArt <> 'D' THEN NEXT.
      IF RADIO-SET-AftIgv = 1 AND Almmmatg.AftIgv <> YES THEN NEXT.
      IF RADIO-SET-AftIgv = 2 AND Almmmatg.AftIgv <> NO THEN NEXT.

      DISPLAY "Cargando: " + Almmmatg.CodMat @ txt-Mensaje WITH FRAME {&FRAME-NAME}.
      FIND FIRST gn-prov WHERE gn-prov.codcia = pv-codcia AND gn-prov.codpro = almmmatg.codpr1 
          NO-LOCK NO-ERROR.
      CREATE T-MATG.
      BUFFER-COPY ALMMMATG 
          TO T-MATG
          ASSIGN
          T-MATG.CodFam = Almmmatg.codfam + ' ' + Almtfami.desfam
          T-MATG.SubFam = Almmmatg.subfam + ' ' + AlmSFami.dessub.
      ASSIGN 
            t-matg.codpr1 = t-matg.codpr1 + " " + IF (AVAILABLE gn-prov) THEN gn-prov.nompro ELSE "".
      /* Stock actual de la compa�ia */
      T-MATG.StkAct = 0.
      FOR EACH Almmmate OF Almmmatg NO-LOCK:
          T-MATG.StkAct = T-MATG.StkAct + Almmmate.StkAct.
      END.
      /* Ultima venta */
      T-MATG.FchUSal = ?.
      FIND LAST Almdmov USE-INDEX Almd02 WHERE Almdmov.codcia = Almmmatg.codcia
          AND Almdmov.codmat = Almmmatg.codmat
          AND Almdmov.fchdoc <= TODAY
          AND Almdmov.tipmov = 'S'
          AND Almdmov.codmov = 02
          NO-LOCK NO-ERROR.
      IF AVAILABLE Almdmov THEN T-MATG.FchUSal = Almdmov.fchdoc.
      /* Solo para almac�n */
      CASE Almmmatg.TpoMrg:
          WHEN "1" THEN T-MATG.TpoMrg = "Mayorista".
          WHEN "2" THEN T-MATG.TpoMrg = "Minorista".
          OTHERWISE T-MATG.TpoMrg = "Ambos".
      END CASE.
      FIND Almmmatgext OF ALmmmatg NO-LOCK NO-ERROR.
      IF AVAILABLE Almmmatgext THEN T-MATG.CodIntPr1 = Almmmatgext.CodIntPr1.
  END.
  /* Cargamos ENA13 y ENA 14 */
  FOR EACH T-MATG EXCLUSIVE-LOCK, FIRST Almmmat1 OF T-MATG NO-LOCK:
      DISPLAY "EAN14: " + T-MATG.CodMat @ txt-Mensaje WITH FRAME {&FRAME-NAME}.
      BUFFER-COPY Almmmat1 USING Almmmat1.Barras Almmmat1.Equival TO T-MATG.
  END.

  DISPLAY "" @ txt-Mensaje WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
  THEN DELETE WIDGET W-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-Win  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY x-CodFam x-CodPr1 DesdeC HastaC R-Tipo RADIO-SET-AftIgv txt-mensaje 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE x-CodFam x-CodPr1 DesdeC HastaC R-Tipo RADIO-SET-AftIgv Btn_OK-2 
         Btn_Cancel 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Excel W-Win 
PROCEDURE Excel :
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
DEFINE VARIABLE cValue                  AS CHARACTER.
DEFINE VARIABLE t-Column                AS INTEGER INIT 1.
DEFINE VARIABLE cFila1 AS CHAR FORMAT 'X' INIT ''.
DEFINE VARIABLE cFila2 AS CHAR FORMAT 'X' INIT ''.

RUN Carga-Temporal.

/* create a new Excel Application object */
CREATE "Excel.Application" chExcelApplication.

/* create a new Workbook */
chWorkbook = chExcelApplication:Workbooks:Add().

/* get the active Worksheet */
chWorkSheet = chExcelApplication:Sheets:Item(1).

chWorkSheet:Range("B2"):Value = "CATALOGO DE MATERIALES".
chWorkSheet:Range("A3"):Value = "CODIGO".
chWorkSheet:Range("B3"):Value = "DESCRIPCION".
chWorkSheet:Range("C3"):Value = "MARCA".
chWorkSheet:Range("D3"):Value = "DESCRIPCION".
chWorkSheet:Range("E3"):Value = "FAMILIA".
chWorkSheet:Range("F3"):Value = "SUB FAMILIA".
chWorkSheet:Range("G3"):Value = "UNID BASE".
chWorkSheet:Range("H3"):Value = "COMPRA".
chWorkSheet:Range("I3"):Value = "UNID STOCK".
chWorkSheet:Range("J3"):Value = "UNIDAD COMPRA".
chWorkSheet:Range("K3"):Value = "MINIMO DE VENTA MAYORISTA".
chWorkSheet:Range("L3"):Value = "VOLUMEN (M3)".
chWorkSheet:Range("M3"):Value = "PESO (KG)".
chWorkSheet:Range("N3"):Value = "ACTIVADO/DESACTIVADO/BLOQUEADO".
chWorkSheet:Range("O3"):Value = "FECHA CREACION".
chWorkSheet:Range("P3"):Value = "USUARIO CREACION".
chWorkSheet:Range("Q3"):Value = "EMPAQUE MASTER".
chWorkSheet:Range("R3"):Value = "EMPAQUE INNER".
chWorkSheet:Range("S3"):Value = "ULT. MODIF.".
chWorkSheet:Range("T3"):Value = "USUARIO".
chWorkSheet:Range("U3"):Value = "CTO. REP. S/. S/IGV".
chWorkSheet:Range("V3"):Value = "CTO. PROM. S/. S/IGV".
chWorkSheet:Range("W3"):Value = "MIN.VTA EXPO".
chWorkSheet:Range("X3"):Value = "EMPAQUE EXPO".
chWorkSheet:Range("Y3"):Value = "UND.VTA OFICINA".
chWorkSheet:Range("Z3"):Value = "UND.VTA UTILEX".
chWorkSheet:Range("AA3"):Value = "UND.VTA MOSTRADOR A".
chWorkSheet:Range("AB3"):Value = "UND.VTA MOSTRADOR B".
chWorkSheet:Range("AC3"):Value = "UND.VTA MOSTRADOR C".
chWorkSheet:Range("AD3"):Value = "STOCK TOTAL".
chWorkSheet:Range("AE3"):Value = "ULTIMA VENTA".
chWorkSheet:Range("AF3"):Value = "CAT. CONTABLE".
chWorkSheet:Range("AG3"):Value = "INDICADOR COMERCIAL".
chWorkSheet:Range("AH3"):Value = "EAN13".
chWorkSheet:Range("AI3"):Value = "ENA14".
chWorkSheet:Range("AJ3"):Value = "FACTOR".
chWorkSheet:Range("AK3"):Value = "ENA14".
chWorkSheet:Range("AL3"):Value = "FACTOR".
chWorkSheet:Range("AM3"):Value = "ENA14".
chWorkSheet:Range("AN3"):Value = "FACTOR".
chWorkSheet:Range("AO3"):Value = "ENA14".
chWorkSheet:Range("AP3"):Value = "FACTOR".
chWorkSheet:Range("AQ3"):Value = "ENA14".
chWorkSheet:Range("AR3"):Value = "FACTOR".
chWorkSheet:Range("AS3"):Value = "SOLO PARA ALMACENES".
chWorkSheet:Range("AT3"):Value = "CAMPA�A - CLASIFIC.GENERAL".
chWorkSheet:Range("AU3"):Value = "CAMPA�A - CLASIFIC.UTILEX-INSTITUCIONALES".
chWorkSheet:Range("AV3"):Value = "CAMPA�A - CLASIFIC.MAYORISTA".
chWorkSheet:Range("AW3"):Value = "NO CAMPA�A - CLASIFIC.GENERAL".
chWorkSheet:Range("AX3"):Value = "NO CAMPA�A - CLASIFIC.UTILEX-INSTITUCIONALES".
chWorkSheet:Range("AY3"):Value = "NO CAMPA�A - CLASIFIC.MAYORISTA".
chWorkSheet:Range("AZ3"):Value = "PROPIOS/TERCEROS/NINGUNO".
chWorkSheet:Range("BA3"):Value = "TIENE MOVIMIENTOS?".
/* Datos nuevos */
chWorkSheet:Range("BB3"):Value = "Requiere # Serie".
chWorkSheet:Range("BC3"):Value = "Requiere Fecha de Vcto.".
chWorkSheet:Range("BD3"):Value = "Cod. Equiv. Proveedor".
chWorkSheet:Range("BE3"):Value = "AFECTO A IGV?".

chWorkSheet:Range("BF3"):Value = "LICENCIA".
chWorkSheet:Range("BG3"):Value = "DESCRIPCION".


/* Formatos */
chWorkSheet:COLUMNS("A"):NumberFormat = "@".
chWorkSheet:COLUMNS("C"):NumberFormat = "@".
chWorkSheet:COLUMNS("E"):NumberFormat = "@".
chWorkSheet:COLUMNS("F"):NumberFormat = "@".
chWorkSheet:COLUMNS("O"):NumberFormat = "dd/mm/yyyy".
chWorkSheet:COLUMNS("S"):NumberFormat = "dd/mm/yyyy".
chWorkSheet:COLUMNS("AH"):NumberFormat = "@".
chWorkSheet:COLUMNS("AI"):NumberFormat = "@".
chWorkSheet:COLUMNS("AK"):NumberFormat = "@".
chWorkSheet:COLUMNS("AM"):NumberFormat = "@".
chWorkSheet:COLUMNS("AO"):NumberFormat = "@".
chWorkSheet:COLUMNS("AQ"):NumberFormat = "@".
chWorkSheet:COLUMNS("BC"):NumberFormat = "@".
chWorkSheet:COLUMNS("BF"):NumberFormat = "@".

DEF VAR x-CtoLis AS DEC NO-UNDO.
DEF VAR x-CtoPro AS DEC NO-UNDO.

DISPLAY "Generando EXCEL" @ txt-Mensaje WITH FRAME {&FRAME-NAME}.
PAUSE 0.

t-column = 3.
FOR EACH T-MATG NO-LOCK:
    t-column = t-column + 1. 
    cColumn = STRING(t-Column).                                                                                        
    cRange = "A" + cColumn.                                                                                                                                
    chWorkSheet:Range(cRange):Value = T-MATG.codmat.
    cRange = "B" + cColumn.                                                                                                                                
    chWorkSheet:Range(cRange):Value = T-MATG.DesMat.
    cRange = "C" + cColumn.                                                                                                                                
    chWorkSheet:Range(cRange):Value = T-MATG.CodMar.
    cRange = "D" + cColumn.                                                                                                                                
    chWorkSheet:Range(cRange):Value = T-MATG.DesMar.

    cRange = "E" + cColumn.                                                                                                                                
    chWorkSheet:Range(cRange):Value = T-MATG.CodFam.
    cRange = "F" + cColumn.                                                                                                                          
    chWorkSheet:Range(cRange):Value = T-MATG.SubFam .
    cRange = "G" + cColumn.                                                                                                                                
    chWorkSheet:Range(cRange):Value = T-MATG.UndBas.
    cRange = "H" + cColumn.                                                                                                                                
    chWorkSheet:Range(cRange):Value = T-MATG.CodPr1.
    cRange = "I" + cColumn.                                                                                                                                
    chWorkSheet:Range(cRange):Value = T-MATG.UndStk.
    cRange = "J" + cColumn.                                                                                                                                
    chWorkSheet:Range(cRange):Value = T-MATG.UndCmp.
    cRange = "K" + cColumn.                                                                                                                                
    chWorkSheet:Range(cRange):Value = T-MATG.Dec__03.
    cRange = "L" + cColumn.
    chWorkSheet:Range(cRange):Value = T-MATG.libre_d02 / 1000000.
    cRange = "M" + cColumn.                                                                                                                                
    chWorkSheet:Range(cRange):Value = T-MATG.pesmat.
    cRange = "N" + cColumn.
    chWorkSheet:Range(cRange):Value = T-MATG.tpoart.
    cRange = "O" + cColumn.
    chWorkSheet:Range(cRange):Value = T-MATG.fching.
    cRange = "P" + cColumn.
    chWorkSheet:Range(cRange):Value = ENTRY(1, T-MATG.Libre_c05, '|').
    cRange = "Q" + cColumn.
    chWorkSheet:Range(cRange):Value = T-MATG.canemp.
    cRange = "R" + cColumn.
    chWorkSheet:Range(cRange):Value = T-MATG.stkrep.
    cRange = "S" + cColumn.
    chWorkSheet:Range(cRange):Value = T-MATG.fchact.
    cRange = "T" + cColumn.
    chWorkSheet:Range(cRange):Value = T-MATG.usuario.
    ASSIGN
        x-CtoLis = T-MATG.CtoLis
        x-CtoPro = 0.
    IF T-MATG.MonVta = 2 THEN x-CtoLis = x-CtoLis * T-MATG.TpoCmb.
    FIND LAST AlmStkge  WHERE AlmStkge.CodCia = s-codcia
        AND AlmStkge.codmat = T-MATG.codmat
        AND AlmStkge.Fecha <= TODAY 
        NO-LOCK NO-ERROR.
    IF AVAILABLE AlmStkge THEN x-CtoPro = AlmStkge.CtoUni.
    cRange = "U" + cColumn.
    chWorkSheet:Range(cRange):Value = x-CtoLis.
    cRange = "V" + cColumn.
    chWorkSheet:Range(cRange):Value = x-CtoPro.
    cRange = "W" + cColumn.
    chWorkSheet:Range(cRange):Value = T-MATG.StkMax.
    
    cRange = "X" + cColumn.
    chWorkSheet:Range(cRange):Value = T-MATG.libre_d03.    
    cRange = "Y" + cColumn.
    chWorkSheet:Range(cRange):Value = T-MATG.CHR__01.
    cRange = "Z" + cColumn.
    chWorkSheet:Range(cRange):Value = T-MATG.UndBas.  
    cRange = "AA" + cColumn.
    chWorkSheet:Range(cRange):Value = T-MATG.UndA.
    cRange = "AB" + cColumn.
    chWorkSheet:Range(cRange):Value = T-MATG.UndB.
    cRange = "AC" + cColumn.
    chWorkSheet:Range(cRange):Value = T-MATG.UndC.
    cRange = "AD" + cColumn.
    chWorkSheet:Range(cRange):Value = T-MATG.StkAct.
    cRange = "AE" + cColumn.
    chWorkSheet:Range(cRange):Value = T-MATG.FchUSal.
    cRange = "AF" + cColumn.
    chWorkSheet:Range(cRange):Value = T-MATG.catconta[1].
    cRange = "AG" + cColumn.
    chWorkSheet:Range(cRange):Value = T-MATG.flgcomercial.
    /* EAN's */
    cRange = "AH" + cColumn.
    chWorkSheet:Range(cRange):Value = T-MATG.CodBrr.
    cRange = "AI" + cColumn.
    chWorkSheet:Range(cRange):Value = T-MATG.Barras[1].
    cRange = "AJ" + cColumn.
    chWorkSheet:Range(cRange):Value = T-MATG.Equival[1].
    cRange = "AK" + cColumn.
    chWorkSheet:Range(cRange):Value = T-MATG.Barras[2].
    cRange = "AL" + cColumn.
    chWorkSheet:Range(cRange):Value = T-MATG.Equival[2].
    cRange = "AM" + cColumn.
    chWorkSheet:Range(cRange):Value = T-MATG.Barras[3].
    cRange = "AN" + cColumn.
    chWorkSheet:Range(cRange):Value = T-MATG.Equival[3].
    cRange = "AO" + cColumn.
    chWorkSheet:Range(cRange):Value = T-MATG.Barras[4].
    cRange = "AP" + cColumn.
    chWorkSheet:Range(cRange):Value = T-MATG.Equival[4].
    cRange = "AQ" + cColumn.
    chWorkSheet:Range(cRange):Value = T-MATG.Barras[5].
    cRange = "AR" + cColumn.
    chWorkSheet:Range(cRange):Value = T-MATG.Equival[5].
    cRange = "AS" + cColumn.
    chWorkSheet:Range(cRange):Value = T-MATG.TpoMrg.

    /* Ranking */
    FIND FIRST factabla WHERE factabla.codcia = s-codcia AND
                                factabla.tabla = 'RANKVTA' AND
                                factabla.codigo = T-MATG.codmat NO-LOCK NO-ERROR.
    IF AVAILABLE factabla THEN DO:
        cRange = "AT" + cColumn.
        chWorkSheet:Range(cRange):Value = factabla.campo-c[1].
        cRange = "AU" + cColumn.
        chWorkSheet:Range(cRange):Value = factabla.campo-c[2].
        cRange = "AV" + cColumn.
        chWorkSheet:Range(cRange):Value = factabla.campo-c[3].
        cRange = "AW" + cColumn.
        chWorkSheet:Range(cRange):Value = factabla.campo-c[4].
        cRange = "AX" + cColumn.
        chWorkSheet:Range(cRange):Value = factabla.campo-c[5].
        cRange = "AY" + cColumn.
        chWorkSheet:Range(cRange):Value = factabla.campo-c[6].        
    END.
    cRange = "AZ" + cColumn.
    IF T-MATG.CHR__02 = 'T' THEN chWorkSheet:Range(cRange):Value = "TERCEROS".
    IF T-MATG.CHR__02 = 'P' THEN chWorkSheet:Range(cRange):Value = "PROPIOS".
    IF (T-MATG.CHR__02 <> 'T' AND T-MATG.CHR__02 <> 'P') THEN chWorkSheet:Range(cRange):Value = "NINGUNO".

    /* RHC 17/10/2019 */
    cRange = "BA" + cColumn.
    cValue = "NO".
    FIND FIRST Almdmov WHERE Almdmov.codcia = s-codcia AND Almdmov.codmat = T-MATG.codmat NO-LOCK NO-ERROR.
    IF AVAILABLE Almdmov THEN cValue = "SI".
    chWorkSheet:Range(cRange):Value = cValue.

    /* Datos adicionales */
    cRange = "BB" + cColumn.
    chWorkSheet:Range(cRange):Value = T-MATG.RequiereSerialNr .
    cRange = "BC" + cColumn.
    chWorkSheet:Range(cRange):Value = T-MATG.RequiereDueDate.
    cRange = "BD" + cColumn.
    chWorkSheet:Range(cRange):Value = T-MATG.CodIntPr1.
    cRange = "BE" + cColumn.
    chWorkSheet:Range(cRange):Value = (IF T-MATG.AftIgv = YES THEN "SI" ELSE "NO").

    cRange = "BF" + cColumn.
    chWorkSheet:Range(cRange):Value = T-MATG.Licencia[1].

    FIND Almtabla WHERE Almtabla.tabla = 'LC' AND Almtabla.Codigo = T-MATG.Licencia[1]
        NO-LOCK NO-ERROR.
    IF AVAILABLE ALmtabla THEN DO:
        cRange = "BG" + cColumn.
        chWorkSheet:Range(cRange):Value = Almtabla.Nombre.
    END.

END.

/* launch Excel so it is visible to the user */
chExcelApplication:VISIBLE = TRUE.

/* release com-handles */
RELEASE OBJECT chExcelApplication.      
RELEASE OBJECT chWorkbook.
RELEASE OBJECT chWorksheet.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Formato2 W-Win 
PROCEDURE Formato2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       Sin el almacen 83 ni 83b y costo unitario
------------------------------------------------------------------------------*/
  DEFINE FRAME F-REPORTE
    T-MATG.codmat       FORMAT 'x(6)'   COLUMN-LABEL "Codigo"
    T-MATG.DesMat       FORMAT 'x(50)'  COLUMN-LABEL "Descripcion" 
    T-MATG.DesMar       FORMAT "X(15)"  COLUMN-LABEL 'Marca'
    T-MATG.CodFam                       COLUMN-LABEL 'Familia'
    T-MATG.SubFam                       COLUMN-LABEL 'Sub-familia'
    T-MATG.UndBas                       COLUMN-LABEL "Unid"
    T-MATG.CodPr1       FORMAT 'x(60)'  COLUMN-LABEL 'Proveedor'
    T-MATG.CanEmp                       COLUMN-LABEL 'Empaque'
    T-MATG.PesMat                       COLUMN-LABEL 'Peso'
    T-MATG.UndBas                       COLUMN-LABEL 'Unidad!Base'
    T-MATG.Dec__03                      COLUMN-LABEL 'M�nimo!de Venta'
    T-MATG.Licencia[1]  FORMAT 'x(6)'   COLUMN-LABEL 'Licencia'
    almtabla.Nombre     FORMAT 'x(30)'  COLUMN-LABEL 'Descripci�n'
    T-MATG.libre_d02                    COLUMN-LABEL "Volumen"
    T-MATG.pesmat                       COLUMN-LABEL "Peso (KG)"
    T-MATG.tpoart                       COLUMN-LABEL "Activo-Desact-Bloqueado"
    T-MATG.fching                       COLUMN-LABEL "Fecha!Creacion"
    T-MATG.usuario                      COLUMN-LABEL "Usuario!Creacion"

    HEADER
    S-NOMCIA FORMAT "X(50)" SKIP
    "CATALOGO DE MATERIALES" AT 20  
    "Pagina :" TO 80 PAGE-NUMBER(REPORT) FORMAT "ZZZZZ9" SKIP
    "Fecha :"  TO 80 TODAY FORMAT "99/99/9999" SKIP
    "Hora :"   TO 80 STRING(TIME,"HH:MM:SS") SKIP(1)
  WITH WIDTH 200 NO-BOX STREAM-IO DOWN.
  
  FOR EACH T-MATG:
      FIND almtabla WHERE almtabla.Tabla = "LC" AND
          almtabla.Codigo = T-MATG.Licencia[1]
          NO-LOCK NO-ERROR.
    DISPLAY STREAM REPORT 
        T-MATG.codmat       
        T-MATG.DesMat       
        T-MATG.DesMar       
        T-MATG.CodFam       
        T-MATG.SubFam       
        T-MATG.UndBas       
        T-MATG.CodPr1     
        T-MATG.CanEmp     
        T-MATG.PesMat     
        T-MATG.UndBas     
        T-MATG.Dec__03    
        T-MATG.Licencia[1]
        almtabla.Nombre WHEN AVAILABLE almtabla
        T-MATG.libre_d02
        T-MATG.pesmat
        T-MATG.tpoart
        T-MATG.fching
        T-MATG.usuario

        WITH FRAME F-REPORTE.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Habilita W-Win 
PROCEDURE Habilita :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
    ENABLE ALL EXCEPT txt-mensaje Btn_Ok.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imprime W-Win 
PROCEDURE Imprime :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE VARIABLE c-Copias AS INTEGER NO-UNDO.

    RUN Carga-Temporal.   
    RUN lib/Imprimir2.
    IF s-salida-impresion = 0 THEN RETURN.

    IF s-salida-impresion = 1 THEN 
        s-print-file = SESSION:TEMP-DIRECTORY +
        STRING(NEXT-VALUE(sec-arc,integral)) + ".prn".

    DO c-Copias = 1 TO s-nro-copias ON ERROR UNDO, LEAVE ON STOP UNDO, LEAVE:
        CASE s-salida-impresion:
            WHEN 1 OR WHEN 3 THEN
                OUTPUT STREAM REPORT TO VALUE(s-print-file) PAGED PAGE-SIZE 62.
            WHEN 2 THEN
                OUTPUT STREAM REPORT TO PRINTER PAGED PAGE-SIZE 62. /* Impresora */
        END CASE.
        PUT STREAM REPORT CONTROL {&Prn0} + {&Prn5A} + CHR(66) + {&Prn3}.
        RUN Formato2.
        PAGE STREAM REPORT.
        OUTPUT STREAM report CLOSE.
    END.
    OUTPUT STREAM report CLOSE.

    CASE s-salida-impresion:
        WHEN 1 OR WHEN 3 THEN DO:
            FRAME {&FRAME-NAME}:SENSITIVE = FALSE.
            RUN LIB/W-README.R(s-print-file).
            FRAME {&FRAME-NAME}:SENSITIVE = TRUE.
            IF s-salida-impresion = 1 THEN
                OS-DELETE VALUE(s-print-file).
        END.
    END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inhabilita W-Win 
PROCEDURE Inhabilita :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
    DISABLE ALL.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inicializa-Variables W-Win 
PROCEDURE Inicializa-Variables :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  ASSIGN DesdeC 
         HastaC R-Tipo.
  
  IF HastaC <> "" THEN HastaC = "".
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit W-Win 
PROCEDURE local-exit :
/* -----------------------------------------------------------
  Purpose:  Starts an "exit" by APPLYing CLOSE event, which starts "destroy".
  Parameters:  <none>
  Notes:    If activated, should APPLY CLOSE, *not* dispatch adm-exit.   
-------------------------------------------------------------*/
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   
   RETURN.
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize W-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
     DISPLAY R-Tipo.
  END.
  
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Procesa-Parametros W-Win 
PROCEDURE Procesa-Parametros :
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
        WHEN "" THEN .
    END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE recoge-parametros W-Win 
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
        WHEN "" THEN ASSIGN input-var-1 = "".
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records W-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartWindow, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed W-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _Header W-Win 
PROCEDURE _Header :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*
titulo = "REPORTE DE MOVIMIENTOS POR DIA".

mens1 = "TIPO Y CODIGO DE MOVIMIENTO : " + C-TipMov + "-" + STRING(I-CodMov, "99") + " " + D-Movi:SCREEN-VALUE IN FRAME {&FRAME-NAME}.
mens2 = "MATERIAL : " + DesdeC + " A: " + HastaC .
mens3 = "FECHA : " + STRING(F-FchDes, "99/99/9999") + " A: " + STRING(F-FchHas, "99/99/9999").

titulo = S-NomCia + fill(" ", (INT((90 - length(titulo)) / 2)) - length(S-NomCia)) + titulo.
mens1 = fill(" ", INT((90 - length(mens1)) / 2)) + mens1.
mens2 = fill(" ", INT((90 - length(mens2)) / 2)) + mens2.
mens3 = C-condicion:SCREEN-VALUE + fill(" ", INT((90 - length(mens3)) / 2) - LENGTH(C-condicion:SCREEN-VALUE)) + mens3.
*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
