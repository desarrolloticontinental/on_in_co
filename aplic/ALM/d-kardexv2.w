&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME W-Win
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
DEFINE SHARED VAR S-NOMCIA AS CHARACTER.
DEFINE        VAR F-PESALM AS DECIMAL NO-UNDO.

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

/*******/
DEFINE SHARED VAR S-DESALM AS CHARACTER.
DEFINE SHARED VAR S-CODALM AS CHARACTER.

/* Local Variable Definitions ---                                       */
/*
DEFINE VAR F-Ingreso AS DECIMAL FORMAT "(>>,>>>,>>9.99)" NO-UNDO.
DEFINE VAR F-PreIng  AS DECIMAL FORMAT "(>>,>>>,>>9.9999)" NO-UNDO.
DEFINE VAR F-TotIng  AS DECIMAL FORMAT "(>>,>>>,>>9.99)" NO-UNDO.
DEFINE VAR F-Salida  AS DECIMAL FORMAT "(>>,>>>,>>9.99)" NO-UNDO.
DEFINE VAR F-PreSal  AS DECIMAL FORMAT "(>>,>>>,>>9.9999)" NO-UNDO.
DEFINE VAR F-TotSal  AS DECIMAL FORMAT "(>>,>>>,>>9.99)" NO-UNDO.
DEFINE VAR F-Saldo   AS DECIMAL FORMAT "(>>,>>>,>>9.99)" NO-UNDO.
DEFINE VAR F-STKGEN  AS DECIMAL FORMAT "(>>,>>>,>>9.99)" NO-UNDO.
DEFINE VAR F-PRECIO  AS DECIMAL FORMAT "(>,>>>,>>9.9999)" NO-UNDO.
*/
DEFINE VAR F-Ingreso AS DECIMAL FORMAT "(>>>,>>9.99)" NO-UNDO.
DEFINE VAR F-PreIng  AS DECIMAL FORMAT "(>>>,>>9.9999)" NO-UNDO.
DEFINE VAR F-TotIng  AS DECIMAL FORMAT "(>>>,>>9.99)" NO-UNDO.
DEFINE VAR F-Salida  AS DECIMAL FORMAT "(>>>,>>9.99)" NO-UNDO.
DEFINE VAR F-PreSal  AS DECIMAL FORMAT "(>>>,>>9.9999)" NO-UNDO.
DEFINE VAR F-TotSal  AS DECIMAL FORMAT "(>>>,>>9.99)" NO-UNDO.
DEFINE VAR F-Saldo   AS DECIMAL FORMAT "(>>,>>>,>>9.99)" NO-UNDO.
DEFINE VAR F-STKGEN  AS DECIMAL FORMAT "(>>>,>>9.99)" NO-UNDO.
DEFINE VAR F-PRECIO  AS DECIMAL FORMAT "(>>>,>>9.9999)" NO-UNDO.

DEFINE VAR F-VALCTO  AS DECIMAL FORMAT "(>>,>>>,>>9.99)" NO-UNDO.
DEFINE VAR S-SUBTIT  AS CHARACTER FORMAT "X(40)" NO-UNDO.
DEFINE VAR F-SPeso   AS DECIMAL FORMAT "(>,>>>,>>9.99)" NO-UNDO.

DEFINE BUFFER DMOV FOR Almdmov. 

DEFINE VAR I-NROITM  AS INTEGER.

DEFINE TEMP-TABLE  tmp-report LIKE w-report.

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
&Scoped-Define ENABLED-OBJECTS RECT-57 F-CodFam F-SubFam x-Licencia DesdeC ~
HastaC DesdeF HastaF nCodMon R-Tipo T-Resumen TOGGLE-Transferencias Btn_OK ~
Btn_Cancel BUTTON-1 BUTTON-2 
&Scoped-Define DISPLAYED-OBJECTS F-CodFam F-DesFam F-SubFam F-DesSub ~
x-Licencia x-NomLic DesdeC HastaC DesdeF HastaF nCodMon R-Tipo ~
TOGGLE-Transferencias x-mensaje 

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
     SIZE 11 BY 1.5
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK 
     IMAGE-UP FILE "img\b-ok":U
     LABEL "Aceptar" 
     SIZE 11 BY 1.5
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-1 
     IMAGE-UP FILE "img\tbldat":U
     LABEL "Button 1" 
     SIZE 12 BY 1.5.

DEFINE BUTTON BUTTON-2 
     IMAGE-UP FILE "img/excel.bmp":U
     LABEL "Button 2" 
     SIZE 12 BY 1.5.

DEFINE VARIABLE DesdeC AS CHARACTER FORMAT "X(9)":U 
     LABEL "Articulo" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .69 NO-UNDO.

DEFINE VARIABLE DesdeF AS DATE FORMAT "99/99/9999":U 
     LABEL "Desde" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .69 NO-UNDO.

DEFINE VARIABLE F-CodFam AS CHARACTER FORMAT "X(3)":U 
     LABEL "Linea" 
     VIEW-AS FILL-IN 
     SIZE 5.43 BY .69 NO-UNDO.

DEFINE VARIABLE F-DesFam AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 32 BY .69 NO-UNDO.

DEFINE VARIABLE F-DesSub AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 32 BY .69 NO-UNDO.

DEFINE VARIABLE F-SubFam AS CHARACTER FORMAT "X(256)":U 
     LABEL "Sub-linea" 
     VIEW-AS FILL-IN 
     SIZE 5.43 BY .69 NO-UNDO.

DEFINE VARIABLE HastaC AS CHARACTER FORMAT "X(9)":U 
     LABEL "A" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .69 NO-UNDO.

DEFINE VARIABLE HastaF AS DATE FORMAT "99/99/9999":U 
     LABEL "A" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .69 NO-UNDO.

DEFINE VARIABLE x-Licencia AS CHARACTER FORMAT "X(3)":U 
     LABEL "Licenciatario" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .69 NO-UNDO.

DEFINE VARIABLE x-mensaje AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 69 BY .81 NO-UNDO.

DEFINE VARIABLE x-NomLic AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 31 BY .69 NO-UNDO.

DEFINE VARIABLE nCodMon AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Soles  ", 1,
"Dolares", 2
     SIZE 19.43 BY .46 NO-UNDO.

DEFINE VARIABLE R-Tipo AS CHARACTER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Activados", "A",
"Desactivados", "D",
"Ambos", ""
     SIZE 12.72 BY 1.73 NO-UNDO.

DEFINE RECTANGLE RECT-46
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 80 BY 1.88
     BGCOLOR 7 FGCOLOR 0 .

DEFINE RECTANGLE RECT-57
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 78.86 BY 10.31.

DEFINE VARIABLE T-Resumen AS LOGICAL INITIAL no 
     LABEL "Total General Resumido (Solo Excel)" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY .69 NO-UNDO.

DEFINE VARIABLE TOGGLE-Transferencias AS LOGICAL INITIAL no 
     LABEL "Incluir Transferencias" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY .77 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     F-CodFam AT ROW 1.85 COL 18 COLON-ALIGNED
     F-DesFam AT ROW 1.85 COL 24.29 COLON-ALIGNED NO-LABEL
     F-SubFam AT ROW 2.62 COL 18 COLON-ALIGNED
     F-DesSub AT ROW 2.62 COL 24.29 COLON-ALIGNED NO-LABEL
     x-Licencia AT ROW 3.42 COL 18 COLON-ALIGNED WIDGET-ID 2
     x-NomLic AT ROW 3.42 COL 25 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     DesdeC AT ROW 4.12 COL 18 COLON-ALIGNED
     HastaC AT ROW 4.12 COL 47 COLON-ALIGNED
     DesdeF AT ROW 4.81 COL 18 COLON-ALIGNED
     HastaF AT ROW 4.81 COL 47 COLON-ALIGNED
     nCodMon AT ROW 6.58 COL 20 NO-LABEL
     R-Tipo AT ROW 7.38 COL 20 NO-LABEL
     T-Resumen AT ROW 8.12 COL 48
     TOGGLE-Transferencias AT ROW 8.88 COL 48 WIDGET-ID 10
     x-mensaje AT ROW 10.15 COL 3 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     Btn_OK AT ROW 11.69 COL 2.14
     Btn_Cancel AT ROW 11.69 COL 13.72
     BUTTON-1 AT ROW 11.69 COL 25.29
     BUTTON-2 AT ROW 11.69 COL 37.86 WIDGET-ID 6
     "Moneda" VIEW-AS TEXT
          SIZE 6.86 BY .58 AT ROW 6.54 COL 12.14
          FONT 6
     "Criterio de Seleccion" VIEW-AS TEXT
          SIZE 17.72 BY .5 AT ROW 1 COL 5.43
          FONT 6
     "Estado" VIEW-AS TEXT
          SIZE 6.86 BY .69 AT ROW 7.46 COL 12.14
          FONT 1
     RECT-46 AT ROW 11.5 COL 1
     RECT-57 AT ROW 1.19 COL 1.72
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 12.5
         FONT 4.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Kardex General Contable"
         HEIGHT             = 12.5
         WIDTH              = 80
         MAX-HEIGHT         = 12.5
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 12.5
         VIRTUAL-WIDTH      = 80
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
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
/* SETTINGS FOR FILL-IN F-DesFam IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F-DesSub IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-46 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX T-Resumen IN FRAME F-Main
   NO-DISPLAY                                                           */
/* SETTINGS FOR FILL-IN x-mensaje IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN x-NomLic IN FRAME F-Main
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Kardex General Contable */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Kardex General Contable */
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


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 W-Win
ON CHOOSE OF BUTTON-1 IN FRAME F-Main /* Button 1 */
DO:
  RUN Asigna-Variables.
  RUN Inhabilita.
  RUN Texto.
  RUN Habilita.
  RUN Inicializa-Variables.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 W-Win
ON CHOOSE OF BUTTON-2 IN FRAME F-Main /* Button 2 */
DO:
    RUN Asigna-Variables.
    RUN Inhabilita.
    IF T-Resumen THEN RUN Excel3.
    ELSE RUN Excel.
    RUN Habilita.
    RUN Inicializa-Variables.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME DesdeC
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DesdeC W-Win
ON LEAVE OF DesdeC IN FRAME F-Main /* Articulo */
DO:
/*   IF SELF:SCREEN-VALUE = "" THEN RETURN.                           */
/*   SELF:SCREEN-VALUE = STRING(INTEGER(SELF:SCREEN-VALUE),"999999"). */
/*   FIND Almmmatg WHERE Almmmatg.CodCia = S-CODCIA                   */
/*                  AND  Almmmatg.CodMat = SELF:SCREEN-VALUE          */
/*                 NO-LOCK NO-ERROR.                                  */
/*   IF NOT AVAILABLE Almmmatg THEN DO:                               */
/*      MESSAGE "Codigo no Existe" VIEW-AS ALERT-BOX ERROR.           */
/*      RETURN NO-APPLY.                                              */
/*   END.                                                             */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME F-CodFam
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-CodFam W-Win
ON LEAVE OF F-CodFam IN FRAME F-Main /* Linea */
DO:
   ASSIGN F-CodFam.
   FIND Almtfami WHERE Almtfami.CodCia = S-CODCIA 
                  AND  Almtfami.codfam = F-CodFam 
                 NO-LOCK NO-ERROR.
   IF AVAILABLE Almtfami THEN 
      DISPLAY Almtfami.desfam @ F-DesFam WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME F-SubFam
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-SubFam W-Win
ON LEAVE OF F-SubFam IN FRAME F-Main /* Sub-linea */
DO:
   ASSIGN F-CodFam.
   IF SELF:SCREEN-VALUE = "" THEN RETURN.
   IF F-CodFam = "" THEN DO:
      SELF:SCREEN-VALUE = "".
      RETURN.
   END.
   FIND AlmSFami WHERE AlmSFami.CodCia = S-CODCIA 
                  AND  AlmSFami.codfam = F-CodFam 
                  AND  AlmSFami.subfam = SELF:SCREEN-VALUE 
                 NO-LOCK NO-ERROR.
   IF AVAILABLE AlmSFami THEN 
      DISPLAY AlmSFami.dessub @ F-DesSub WITH FRAME {&FRAME-NAME}.
   ELSE DO:
      MESSAGE "Codigo de Sub-Familia no Existe" VIEW-AS ALERT-BOX ERROR.
      RETURN NO-APPLY.
   END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME HastaC
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL HastaC W-Win
ON LEAVE OF HastaC IN FRAME F-Main /* A */
DO:
/*    IF SELF:SCREEN-VALUE = "" THEN RETURN.                          */
/*   SELF:SCREEN-VALUE = STRING(INTEGER(SELF:SCREEN-VALUE),"999999"). */
/*   FIND Almmmatg WHERE Almmmatg.CodCia = S-CODCIA                   */
/*                  AND  Almmmatg.CodMat = SELF:SCREEN-VALUE          */
/*                 NO-LOCK NO-ERROR.                                  */
/*   IF NOT AVAILABLE Almmmatg THEN DO:                               */
/*      MESSAGE "Codigo no Existe" VIEW-AS ALERT-BOX ERROR.           */
/*      RETURN NO-APPLY.                                              */
/*   END.                                                             */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME x-Licencia
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL x-Licencia W-Win
ON LEAVE OF x-Licencia IN FRAME F-Main /* Licenciatario */
DO:
    IF SELF:SCREEN-VALUE = "" THEN RETURN.
    FIND almtabla WHERE almtabla.Tabla = "LC" AND
         almtabla.Codigo = SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
    IF NOT AVAILABLE almtabla THEN DO:
     MESSAGE "Codigo de Licencia no Existe" VIEW-AS ALERT-BOX ERROR.
     RETURN NO-APPLY.
    END.
    x-NomLic:SCREEN-VALUE = almtabla.Nombre.
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
  ASSIGN DesdeC DesdeF F-CodFam F-DesFam HastaC HastaF nCodMon T-Resumen R-Tipo x-Licencia.
  ASSIGN TOGGLE-Transferencias.

  IF HastaC <> "" THEN 
    S-SUBTIT = "Materiales del " + DesdeC + " al " + HastaC .
  ELSE 
    S-SUBTIT = "".

  IF HastaC = "" THEN HastaC = "999999999".
    
  S-SUBTIT = "Periodo del " + STRING(DesdeF) + " al " + STRING(HastaF).


  IF HastaC = "" THEN HastaC = "999999".
  IF DesdeF = ?  THEN DesdeF = 01/01/1900.
  IF HastaF = ?  THEN HastaF = 01/01/3000.

END.

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
  DISPLAY F-CodFam F-DesFam F-SubFam F-DesSub x-Licencia x-NomLic DesdeC HastaC 
          DesdeF HastaF nCodMon R-Tipo TOGGLE-Transferencias x-mensaje 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE RECT-57 F-CodFam F-SubFam x-Licencia DesdeC HastaC DesdeF HastaF 
         nCodMon R-Tipo T-Resumen TOGGLE-Transferencias Btn_OK Btn_Cancel 
         BUTTON-1 BUTTON-2 
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
    DEFINE VARIABLE chExcelApplication AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE chWorkbook         AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE chWorksheet        AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE iCount             AS INTEGER INITIAL 1 NO-UNDO.
    DEFINE VARIABLE cColumn            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cRange             AS CHARACTER NO-UNDO.

    /* create a new Excel Application object */
    CREATE "Excel.Application" chExcelApplication.

    /* create a new Workbook */
    chWorkbook = chExcelApplication:Workbooks:Add().

    /* get the active Worksheet */
    chWorkSheet = chExcelApplication:Sheets:Item(1).

    /* Encabezados */
    DEFINE VARIABLE S-CODMOV AS CHAR NO-UNDO. 
    DEFINE VARIABLE x-codpro LIKE Almcmov.codpro NO-UNDO.
    DEFINE VARIABLE x-codcli LIKE Almcmov.codcli NO-UNDO.
    DEFINE VARIABLE x-nrorf1 LIKE Almcmov.nrorf1 NO-UNDO.
    DEFINE VARIABLE x-nrorf2 LIKE Almcmov.nrorf2 NO-UNDO.
    DEFINE VARIABLE x-codmov LIKE Almcmov.codmov NO-UNDO.
    DEFINE VARIABLE x-inggen LIKE Almdmov.candes NO-UNDO.
    DEFINE VARIABLE x-salgen LIKE Almdmov.candes NO-UNDO.
    DEFINE VARIABLE x-totgen LIKE Almdmov.candes NO-UNDO.
    DEFINE VARIABLE x-codmon LIKE Almdmov.candes NO-UNDO.
    DEFINE VARIABLE x-tpocmb LIKE Almdmov.candes NO-UNDO.
    DEFINE VARIABLE x-total AS DECIMAL.

    iCount = iCount + 1.
    cColumn = STRING(iCount).
    cRange = "D" + cColumn.
    chWorkSheet:Range(cRange):Value = "KARDEX GENERAL".
    iCount = iCount + 1.
    cColumn = STRING(iCount).
    cRange = "D" + cColumn.
    chWorkSheet:Range(cRange):Value = s-subtit.
    iCount = iCount + 1.
    cColumn = STRING(iCount).
    cRange = "D" + cColumn.
    chWorkSheet:Range(cRange):Value = IF nCodmon = 1 THEN "Expresado en Nuevos Soles " ELSE "Expresado en Dolares Americanos".
    iCount = iCount + 1.
    cColumn = STRING(iCount).
    cRange = "A" + cColumn.
    chWorkSheet:Range(cRange):Value = "Cod. Alm.".
    cRange = "B" + cColumn.
    chWorkSheet:Range(cRange):Value = "Cod. Mov.".
    cRange = "C" + cColumn.
    chWorkSheet:Range(cRange):Value = "N�mero Doc.".
    cRange = "D" + cColumn.
    chWorkSheet:Range(cRange):Value = "Alm. Origen".
    cRange = "E" + cColumn.
    chWorkSheet:Range(cRange):Value = "C�digo Proveedor".
    cRange = "F" + cColumn.
    chWorkSheet:Range(cRange):Value = "C�digo Cliente".
    cRange = "G" + cColumn.
    chWorkSheet:Range(cRange):Value = "Referencia".
    cRange = "H" + cColumn.
    chWorkSheet:Range(cRange):Value = "Referencia".
    cRange = "I" + cColumn.
    chWorkSheet:Range(cRange):Value = "Fecha Documento".
    cRange = "J" + cColumn.
    chWorkSheet:Range(cRange):Value = "Ingresos".
    cRange = "K" + cColumn.
    chWorkSheet:Range(cRange):Value = "Salidas".
    cRange = "L" + cColumn.
    chWorkSheet:Range(cRange):Value = "Costo Ingresos".
    cRange = "M" + cColumn.
    chWorkSheet:Range(cRange):Value = "Costo Promedio".
    cRange = "N" + cColumn.
    chWorkSheet:Range(cRange):Value = "Saldos".
    cRange = "O" + cColumn.
    chWorkSheet:Range(cRange):Value = "Costo Total".

    ASSIGN
       x-inggen = 0
       x-salgen = 0
       x-totgen = 0  
       x-total  = 0.
    FIND LAST gn-tcmb WHERE gn-tcmb.fecha <= DesdeF NO-LOCK NO-ERROR.
    KARDEX:
    FOR EACH Almmmatg NO-LOCK WHERE Almmmatg.CodCia = S-CODCIA 
        AND  Almmmatg.CodMat >= DesdeC  
        AND  Almmmatg.CodMat <= HastaC  
        AND  Almmmatg.codfam BEGINS F-CodFam 
        AND  Almmmatg.TpoArt BEGINS R-Tipo 
        AND LOOKUP(TRIM(Almmmatg.CatConta[1]), 'AF,SV,XX') = 0
        AND  Almmmatg.Licencia[1] BEGINS x-Licencia,
        EACH Almdmov NO-LOCK USE-INDEX ALMD02 WHERE Almdmov.CodCia = Almmmatg.CodCia 
        AND  Almdmov.codmat = Almmmatg.CodMat 
        AND  Almdmov.FchDoc >= DesdeF 
        AND  Almdmov.FchDoc <= HastaF,
        FIRST Almtmov NO-LOCK WHERE Almtmovm.Codcia = Almdmov.Codcia 
        AND Almtmovm.TipMov = Almdmov.TipMov 
        AND Almtmovm.Codmov = Almdmov.Codmov
        AND (TOGGLE-Transferencias = YES OR Almtmovm.Movtrf = NO),
        FIRST Almacen OF Almdmov NO-LOCK WHERE Almacen.FlgRep = Yes
        AND Almacen.AlmCsg = No
        BREAK BY Almmmatg.CodCia BY Almmmatg.CodMat BY Almdmov.FchDoc:
        /* control de filas en el Excel */
        IF iCount > 65500 THEN DO:
            MESSAGE 'Este reporte ha superado la capacidad de la hoja de c�lculo'
                VIEW-AS ALERT-BOX WARNING.
            LEAVE KARDEX.
        END.
        /* **************************** */
        /*
        DISPLAY Almmmatg.CodMat @ Fi-Mensaje LABEL "Codigo de Articulo "
                FORMAT "X(11)" 
                WITH FRAME F-Proceso.
        */
        DISPLAY "Codigo de Articulo: " + Almmmatg.CodMat @ x-mensaje
            WITH FRAME {&FRAME-NAME}.

        IF FIRST-OF(Almmmatg.CodMat) THEN DO:
           /* BUSCAMOS SI TIENE MOVIMEINTOS ANTERIORES A DesdeF */
           FIND LAST AlmStkGe WHERE AlmstkGe.Codcia = Almmmatg.Codcia AND
                                  AlmstkGe.CodMat = Almmmatg.CodMat AND
                                  AlmstkGe.Fecha < DesdeF
                                  NO-LOCK NO-ERROR.
           F-STKGEN = 0.
           F-SALDO  = 0.
           F-PRECIO = 0.
           F-VALCTO = 0.
           IF AVAILABLE AlmStkGe THEN DO:
              F-STKGEN = AlmStkGe.StkAct.
              F-SALDO  = AlmStkGe.StkAct.
              F-PRECIO = AlmStkGe.CtoUni.
              F-VALCTO = F-STKGEN * F-PRECIO.
           END.
           iCount = iCount + 1.
           cColumn = STRING(iCount).
           cRange = "A" + cColumn.
           chWorkSheet:Range(cRange):Value = Almmmatg.codmat.
           cRange = "B" + cColumn.
           chWorkSheet:Range(cRange):Value = Almmmatg.desmat.
           cRange = "F" + cColumn.
           chWorkSheet:Range(cRange):Value = Almmmatg.desmar.
           cRange = "H" + cColumn.
           chWorkSheet:Range(cRange):Value = Almmmatg.undstk.
           cRange = "M" + cColumn.
           chWorkSheet:Range(cRange):Value = f-precio.
           cRange = "N" + cColumn.
           chWorkSheet:Range(cRange):Value = f-saldo.
           cRange = "O" + cColumn.
           chWorkSheet:Range(cRange):Value = f-valcto.
        END.
        x-codpro = "".
        x-codcli = "".
        x-nrorf1 = "".
        x-nrorf2 = "".
        FIND Almcmov WHERE Almcmov.CodCia = Almdmov.codcia 
            AND  Almcmov.CodAlm = Almdmov.codalm 
            AND  Almcmov.TipMov = Almdmov.tipmov 
            AND  Almcmov.CodMov = Almdmov.codmov 
            AND  Almcmov.NroDoc = Almdmov.nrodoc 
            NO-LOCK NO-ERROR.
        IF AVAILABLE Almcmov THEN DO:
           ASSIGN
              x-codpro = Almcmov.codpro
              x-codcli = Almcmov.codcli
              x-nrorf1 = Almcmov.nrorf1
              x-nrorf2 = Almcmov.nrorf2
              x-codmon = Almcmov.codmon
              x-tpocmb = Almcmov.tpocmb.
        END.
        S-CODMOV  = Almdmov.TipMov + STRING(Almdmov.CodMov,"99"). 
        F-Ingreso = IF LOOKUP(Almdmov.TipMov,"I,U,R") > 0 THEN (Almdmov.CanDes * Almdmov.Factor) ELSE 0.
        F-PreIng  = 0.
        F-TotIng  = 0.
        IF nCodmon = x-Codmon THEN DO:
           IF Almdmov.Tipmov = 'I' THEN DO:
              F-PreIng  = Almdmov.PreUni / Almdmov.Factor.
              F-TotIng  = Almdmov.ImpCto.
           END.
           ELSE DO:
               IF Almtmovm.TipMov = "S" AND Almtmov.MovCmp = YES THEN DO:
                   F-PreIng  = Almdmov.PreUni / Almdmov.Factor.
                   F-TotIng  = Almdmov.ImpCto.
               END.
               ELSE DO:
                   F-PreIng  = 0.
                   F-TotIng  = F-PreIng * F-Ingreso.
               END.
           END.
        END.
        ELSE DO:
           IF nCodmon = 1 THEN DO:
              IF Almdmov.Tipmov = 'I' THEN DO:
                 F-PreIng  = ROUND(Almdmov.PreUni * Almdmov.Tpocmb / Almdmov.Factor, 4).
                 F-TotIng  = (Almdmov.ImpCto * Almdmov.TpoCmb ).
              END.
              IF LOOKUP(Almdmov.Tipmov, 'U,R') > 0 THEN DO:
                 F-PreIng  = 0.
                 F-TotIng  = F-PreIng * F-Ingreso.
              END.
              IF Almtmovm.TipMov = "S" AND Almtmov.MovCmp = YES THEN DO:
                  F-PreIng  = ROUND(Almdmov.PreUni * Almdmov.Tpocmb / Almdmov.Factor, 4).
                  F-TotIng  = (Almdmov.ImpCto * Almdmov.TpoCmb ).
              END.
           END.
           ELSE DO:
              IF Almdmov.Tipmov = 'I' THEN DO:
                 F-PreIng  = ROUND(Almdmov.PreUni / Almdmov.TpoCmb / Almdmov.Factor, 4).
                 F-TotIng  = ROUND(Almdmov.ImpCto / Almdmov.TpoCmb, 2).
              END.
              IF LOOKUP(Almdmov.Tipmov, 'U,R') > 0 THEN DO:
                 F-PreIng  = 0.
                 F-TotIng  = F-PreIng * F-Ingreso.
              END.
              IF Almtmovm.TipMov = "S" AND Almtmov.MovCmp = YES THEN DO:
                  F-PreIng  = ROUND(Almdmov.PreUni / Almdmov.TpoCmb / Almdmov.Factor, 4).
                  F-TotIng  = ROUND(Almdmov.ImpCto / Almdmov.TpoCmb, 2).
              END.
           END.
        END.
        F-Salida  = IF LOOKUP(Almdmov.TipMov,"S,T") > 0 THEN (Almdmov.CanDes * Almdmov.Factor) ELSE 0.
        F-Saldo   = F-Saldo + F-Ingreso - F-Salida.
        /*F-Saldo   = Almdmov.StkAct.*/
        F-VALCTO = F-Saldo * Almdmov.VCtoMn1.
        /*F-VALCTO = Almdmov.StkAct * Almdmov.VctoMn1.*/
        F-PRECIO = Almdmov.VctoMn1.
        ACCUMULATE F-Ingreso (TOTAL BY Almmmatg.CodMat).
        ACCUMULATE F-Salida  (TOTAL BY Almmmatg.CodMat).
        iCount = iCount + 1.
        cColumn = STRING(iCount).
        cRange = "A" + cColumn.
        chWorkSheet:Range(cRange):Value = almdmov.codalm.
        cRange = "B" + cColumn.
        chWorkSheet:Range(cRange):Value = s-codmov.
        cRange = "C" + cColumn.
        chWorkSheet:Range(cRange):Value = almdmov.nrodoc.
        IF almdmov.codmov = 03 THEN DO:
            cRange = "D" + cColumn.
            chWorkSheet:Range(cRange):Value = almdmov.almori.
        END.
        cRange = "E" + cColumn.
        chWorkSheet:Range(cRange):Value = x-codpro.
        cRange = "F" + cColumn.
        chWorkSheet:Range(cRange):Value = x-codcli.
        cRange = "G" + cColumn.
        chWorkSheet:Range(cRange):Value = x-nrorf1.
        cRange = "H" + cColumn.
        chWorkSheet:Range(cRange):Value = x-nrorf2.
        cRange = "I" + cColumn.
        chWorkSheet:Range(cRange):Value = almdmov.fchdoc.
        cRange = "J" + cColumn.
        chWorkSheet:Range(cRange):Value = f-ingreso.
        cRange = "K" + cColumn.
        chWorkSheet:Range(cRange):Value = f-salida.
        IF  Almdmov.TipMov = "I" AND (ALmtmovm.TpoCto = 0 OR ALmtmovm.TpoCto = 1) THEN DO:
            cRange = "L" + cColumn.
            chWorkSheet:Range(cRange):Value = f-preing.
        END.
        cRange = "M" + cColumn.
        chWorkSheet:Range(cRange):Value = f-precio.
        cRange = "N" + cColumn.
        chWorkSheet:Range(cRange):Value = f-saldo.
        cRange = "O" + cColumn.
        chWorkSheet:Range(cRange):Value = f-valcto.
        IF LAST-OF(Almmmatg.CodMat) THEN DO:
            iCount = iCount + 1.
            cColumn = STRING(iCount).
            cRange = "J" + cColumn.
            chWorkSheet:Range(cRange):Value = (ACCUM TOTAL BY Almmmatg.CodMat F-Ingreso).
            cRange = "K" + cColumn.
            chWorkSheet:Range(cRange):Value = (ACCUM TOTAL BY Almmmatg.CodMat F-Salida).
            x-total = x-total + F-VALCTO.
        END.
        IF LAST-OF(Almmmatg.CodCia) THEN DO:
           iCount = iCount + 1.
           cColumn = STRING(iCount).
           cRange = "O" + cColumn.
           chWorkSheet:Range(cRange):Value = x-total.
           iCount = iCount + 1.
        END.
    END.    
    /*
    HIDE FRAME F-PROCESO.
    */
    DISPLAY "" @ x-mensaje WITH FRAME {&FRAME-NAME}.
    /* launch Excel so it is visible to the user */
    chExcelApplication:Visible = TRUE.

    /* release com-handles */
    RELEASE OBJECT chExcelApplication.      
    RELEASE OBJECT chWorkbook.
    RELEASE OBJECT chWorksheet.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Excel2 W-Win 
PROCEDURE Excel2 :
/*------------------------------------------------------------------------------
  Purpose:     Kardex resumido EN SOLES
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE VARIABLE chExcelApplication AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE chWorkbook         AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE chWorksheet        AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE iCount             AS INTEGER INITIAL 1 NO-UNDO.
    DEFINE VARIABLE cColumn            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cRange             AS CHARACTER NO-UNDO.

    /* create a new Excel Application object */
    CREATE "Excel.Application" chExcelApplication.

    /* create a new Workbook */
    chWorkbook = chExcelApplication:Workbooks:Add().

    /* get the active Worksheet */
    chWorkSheet = chExcelApplication:Sheets:Item(1).

    /* Encabezados */
    DEFINE VARIABLE S-CODMOV AS CHAR NO-UNDO. 
    DEFINE VARIABLE x-codpro LIKE Almcmov.codpro NO-UNDO.
    DEFINE VARIABLE x-codcli LIKE Almcmov.codcli NO-UNDO.
    DEFINE VARIABLE x-nrorf1 LIKE Almcmov.nrorf1 NO-UNDO.
    DEFINE VARIABLE x-nrorf2 LIKE Almcmov.nrorf2 NO-UNDO.
    DEFINE VARIABLE x-codmov LIKE Almcmov.codmov NO-UNDO.
    DEFINE VARIABLE x-inggen LIKE Almdmov.candes NO-UNDO.
    DEFINE VARIABLE x-salgen LIKE Almdmov.candes NO-UNDO.
    DEFINE VARIABLE x-totgen LIKE Almdmov.candes NO-UNDO.
    DEFINE VARIABLE x-codmon LIKE Almdmov.candes NO-UNDO.
    DEFINE VARIABLE x-tpocmb LIKE Almdmov.candes NO-UNDO.
    DEFINE VARIABLE x-total AS DECIMAL.

    /* PARCHE: TODO EXPRESADO EN SOLES */
    DEF VAR nCodMon AS INT NO-UNDO.
    nCodMon = 1.
    /* ******************************* */

    iCount = iCount + 1.
    cColumn = STRING(iCount).
    cRange = "D" + cColumn.
    chWorkSheet:Range(cRange):Value = "KARDEX GENERAL".
    iCount = iCount + 1.
    cColumn = STRING(iCount).
    cRange = "D" + cColumn.
    chWorkSheet:Range(cRange):Value = s-subtit.
    iCount = iCount + 1.
    cColumn = STRING(iCount).
    cRange = "D" + cColumn.
    chWorkSheet:Range(cRange):Value = IF nCodmon = 1 THEN "Expresado en Nuevos Soles " ELSE "Expresado en Dolares Americanos".
    iCount = iCount + 1.
    cColumn = STRING(iCount).
    cRange = "A" + cColumn.
    chWorkSheet:Range(cRange):Value = "Codigo".
    cRange = "B" + cColumn.
    chWorkSheet:Range(cRange):Value = "Descripcion".
    cRange = "C" + cColumn.
    chWorkSheet:Range(cRange):Value = "Cat Contable".
    cRange = "D" + cColumn.
    chWorkSheet:Range(cRange):Value = "Marca".
    cRange = "E" + cColumn.
    chWorkSheet:Range(cRange):Value = "Unidad".
    cRange = "F" + cColumn.
    chWorkSheet:Range(cRange):Value = "Saldo Inicial".
    cRange = "G" + cColumn.
    chWorkSheet:Range(cRange):Value = "Promedio Inicial".
    cRange = "H" + cColumn.
    chWorkSheet:Range(cRange):Value = "Valor Inicial".
    cRange = "I" + cColumn.
    chWorkSheet:Range(cRange):Value = "Ingresos".
    cRange = "J" + cColumn.
    chWorkSheet:Range(cRange):Value = "Total Ingresos".
    cRange = "K" + cColumn.
    chWorkSheet:Range(cRange):Value = "Salidas".
    cRange = "L" + cColumn.
    chWorkSheet:Range(cRange):Value = "Total Salidas".
    cRange = "M" + cColumn.
    chWorkSheet:Range(cRange):Value = "Costo Promedio".
    cRange = "N" + cColumn.
    chWorkSheet:Range(cRange):Value = "Saldos".
    cRange = "O" + cColumn.
    chWorkSheet:Range(cRange):Value = "Costo Total".

    ASSIGN
       x-inggen = 0
       x-salgen = 0
       x-totgen = 0  
       x-total  = 0.
    FIND LAST gn-tcmb WHERE gn-tcmb.fecha <= DesdeF NO-LOCK NO-ERROR.
    KARDEX:
    FOR EACH Almmmatg NO-LOCK WHERE Almmmatg.CodCia = S-CODCIA 
        AND  Almmmatg.CodMat >= DesdeC  
        AND  Almmmatg.CodMat <= HastaC  
        AND  Almmmatg.codfam BEGINS F-CodFam 
        AND  Almmmatg.TpoArt BEGINS R-Tipo 
        /*AND LOOKUP(TRIM(Almmmatg.CatConta[1]), 'AF,SV,XX') = 0*/
        AND  Almmmatg.Licencia[1] BEGINS x-Licencia
        BREAK BY Almmmatg.CodCia BY Almmmatg.CodMat:

        FIND LAST AlmStkGe WHERE almstkge.codcia = s-codcia
            AND almstkge.codmat = almmmatg.codmat
            AND almstkge.fecha <= HastaF
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE AlmStkGe OR Almstkge.stkact = 0 THEN NEXT.

        DISPLAY "Codigo de Articulo: " + Almmmatg.CodMat @ x-mensaje
            WITH FRAME {&FRAME-NAME}.

        IF FIRST-OF(Almmmatg.CodMat) THEN DO:
           /* BUSCAMOS SI TIENE MOVIMIENTOS ANTERIORES A DesdeF */
           FIND LAST AlmStkGe WHERE AlmstkGe.Codcia = Almmmatg.Codcia 
               AND AlmstkGe.CodMat = Almmmatg.CodMat 
               AND AlmstkGe.Fecha < DesdeF
               NO-LOCK NO-ERROR.
           ACCUMULATE F-Ingreso (TOTAL BY Almmmatg.CodMat).
           ACCUMULATE F-TotIng  (TOTAL BY Almmmatg.CodMat).
           ACCUMULATE F-Salida  (TOTAL BY Almmmatg.CodMat).
           ACCUMULATE F-TotSal  (TOTAL BY Almmmatg.CodMat).
           ASSIGN
               F-STKGEN = 0
               F-SALDO  = 0
               F-PRECIO = 0
               F-VALCTO = 0.
           IF AVAILABLE AlmStkGe THEN DO:
              F-STKGEN = AlmStkGe.StkAct.
              F-SALDO  = AlmStkGe.StkAct.
              F-PRECIO = AlmStkGe.CtoUni.
              F-VALCTO = F-STKGEN * F-PRECIO.
           END.
           iCount = iCount + 1.
           cColumn = STRING(iCount).
           cRange = "A" + cColumn.
           chWorkSheet:Range(cRange):Value = "'" + Almmmatg.codmat.
           cRange = "B" + cColumn.
           chWorkSheet:Range(cRange):Value = Almmmatg.desmat.
           cRange = "C" + cColumn.
           chWorkSheet:Range(cRange):Value = Almmmatg.catconta[1].
           cRange = "D" + cColumn.
           chWorkSheet:Range(cRange):Value = Almmmatg.desmar.
           cRange = "E" + cColumn.
           chWorkSheet:Range(cRange):Value = Almmmatg.undstk.
           cRange = "F" + cColumn.
           chWorkSheet:Range(cRange):Value = f-saldo.
           cRange = "G" + cColumn.
           chWorkSheet:Range(cRange):Value = f-precio.
           cRange = "H" + cColumn.
           chWorkSheet:Range(cRange):Value = f-valcto.
        END.

        ASSIGN
            F-Ingreso = 0
            F-TotIng = 0
            F-Salida = 0
            F-TotSal = 0.
        FOR EACH Almdmov NO-LOCK USE-INDEX ALMD02 WHERE Almdmov.CodCia = Almmmatg.CodCia 
                AND  Almdmov.codmat = Almmmatg.CodMat 
                AND  Almdmov.FchDoc >= DesdeF 
                AND  Almdmov.FchDoc <= HastaF,
                FIRST Almtmov NO-LOCK WHERE Almtmovm.Codcia = Almdmov.Codcia 
                    AND Almtmovm.TipMov = Almdmov.TipMov 
                    AND Almtmovm.Codmov = Almdmov.Codmov
                    AND Almtmovm.Movtrf = No,
                FIRST Almacen OF Almdmov NO-LOCK WHERE Almacen.FlgRep = YES AND Almacen.AlmCsg = No:
            FIND Almcmov WHERE Almcmov.CodCia = Almdmov.codcia 
                          AND  Almcmov.CodAlm = Almdmov.codalm 
                          AND  Almcmov.TipMov = Almdmov.tipmov 
                          AND  Almcmov.CodMov = Almdmov.codmov 
                          AND  Almcmov.NroDoc = Almdmov.nrodoc 
                         NO-LOCK NO-ERROR.

            IF AVAILABLE Almcmov THEN DO:
               ASSIGN
                  x-codpro = Almcmov.codpro
                  x-codcli = Almcmov.codcli
                  x-nrorf1 = Almcmov.nrorf1
                  x-nrorf2 = Almcmov.nrorf2
                  x-codmon = Almcmov.codmon
                  x-tpocmb = Almcmov.tpocmb.
            END.


            S-CODMOV  = Almdmov.TipMov + STRING(Almdmov.CodMov,"99"). 
            /* INGRESOS */
            F-Ingreso = IF LOOKUP(Almdmov.TipMov,"I,U,R") > 0 THEN (Almdmov.CanDes * Almdmov.Factor) ELSE 0.
            F-PreIng  = 0.
            F-TotIng  = 0.
            IF nCodmon = x-Codmon THEN DO:
               IF Almdmov.Tipmov = 'I' THEN DO:
                  F-PreIng  = Almdmov.PreUni / Almdmov.Factor.
                  F-TotIng  = Almdmov.ImpCto.
               END.
               ELSE DO:
                  F-PreIng  = 0.
                  F-TotIng  = F-PreIng * F-Ingreso.
               END.
               END.
            ELSE DO:
               IF nCodmon = 1 THEN DO:
                  IF Almdmov.Tipmov = 'I' THEN DO:
                     F-PreIng  = ROUND(Almdmov.PreUni * Almdmov.Tpocmb / Almdmov.Factor, 4).
                     F-TotIng  = (Almdmov.ImpCto * Almdmov.TpoCmb ).
                  END.
                  IF LOOKUP(Almdmov.Tipmov, 'U,R') > 0 THEN DO:
                     F-PreIng  = 0.
                     F-TotIng  = F-PreIng * F-Ingreso.
                  END.
                  END.
               ELSE DO:
                  IF Almdmov.Tipmov = 'I' THEN DO:
                     F-PreIng  = ROUND(Almdmov.PreUni / Almdmov.TpoCmb / Almdmov.Factor, 4).
                     F-TotIng  = ROUND(Almdmov.ImpCto / Almdmov.TpoCmb, 2).
                  END.
                  IF LOOKUP(Almdmov.Tipmov, 'U,R') > 0 THEN DO:
                     F-PreIng  = 0.
                     F-TotIng  = F-PreIng * F-Ingreso.
                  END.
               END.
            END.
            /* SALIDAS */
/*             F-Salida = IF LOOKUP(Almdmov.TipMov,"S,T") > 0 THEN (Almdmov.CanDes * Almdmov.Factor) ELSE 0.                                        */
/*             IF nCodMon = 1 THEN DO:                                                                                                              */
/*                 F-TotSal = IF LOOKUP(Almdmov.TipMov,"S,T") > 0 THEN (Almdmov.CanDes * Almdmov.Factor * Almdmov.VctoMn1) ELSE 0.                  */
/*             END.                                                                                                                                 */
/*             ELSE DO:                                                                                                                             */
/*                 F-TotSal = IF LOOKUP(Almdmov.TipMov,"S,T") > 0 THEN (Almdmov.CanDes * Almdmov.Factor * Almdmov.VctoMn1 / Almdmov.TpoCmb) ELSE 0. */
/*             END.                                                                                                                                 */
            f-Salida = 0.
            IF LOOKUP (Almdmov.TipMov, 'S,T') > 0 THEN DO:
                f-Salida = Almdmov.CanDes * Almdmov.Factor.
                /* Costo Unitario */
                FIND LAST AlmStkGe WHERE almstkge.codcia = s-codcia
                    AND almstkge.codmat = almmmatg.codmat
                    AND almstkge.fecha <= Almdmov.fchdoc
                    NO-LOCK NO-ERROR.
                IF AVAILABLE AlmStkGe THEN f-TotSal = Almdmov.CanDes * Almdmov.Factor * AlmStkge.CtoUni.
            END.
            
            /* ACUMULADOS */
            ASSIGN
                F-Saldo   = Almdmov.StkAct
                F-VALCTO = Almdmov.StkAct * Almdmov.VctoMn1
                F-PRECIO = Almdmov.VctoMn1.
            ACCUMULATE F-Ingreso (TOTAL BY Almmmatg.CodMat).
            ACCUMULATE F-TotIng  (TOTAL BY Almmmatg.CodMat).
            ACCUMULATE F-Salida  (TOTAL BY Almmmatg.CodMat).
            ACCUMULATE F-TotSal  (TOTAL BY Almmmatg.CodMat).
        END.

        IF LAST-OF(Almmmatg.CodMat) THEN DO:
            /* Costo Unitario */
            FIND LAST AlmStkGe WHERE almstkge.codcia = s-codcia
                AND almstkge.codmat = almmmatg.codmat
                AND almstkge.fecha <= HastaF
                NO-LOCK NO-ERROR.
            IF AVAILABLE AlmStkGe THEN DO:
                ASSIGN
                    f-Saldo = AlmStkge.StkAct
                    f-Precio = AlmStkge.CtoUni
                    f-ValCto = f-Saldo * f-Precio.
            END.
            cColumn = STRING(iCount).
            cRange = "I" + cColumn.
            chWorkSheet:Range(cRange):Value = (ACCUM TOTAL BY Almmmatg.CodMat F-Ingreso).
            cRange = "J" + cColumn.
            chWorkSheet:Range(cRange):Value = (ACCUM TOTAL BY Almmmatg.CodMat F-TotIng).
            cRange = "K" + cColumn.
            chWorkSheet:Range(cRange):Value = (ACCUM TOTAL BY Almmmatg.CodMat F-Salida).
            cRange = "L" + cColumn.
            chWorkSheet:Range(cRange):Value = (ACCUM TOTAL BY Almmmatg.CodMat F-TotSal).
            cRange = "M" + cColumn.
            chWorkSheet:Range(cRange):Value = F-Precio.
            cRange = "N" + cColumn.
            chWorkSheet:Range(cRange):Value = F-Saldo.
            cRange = "O" + cColumn.
            chWorkSheet:Range(cRange):Value = F-ValCto.
            x-total = x-total + F-VALCTO.
        END.
    END.    
    /*
    HIDE FRAME F-PROCESO.
    */
    DISPLAY "" @ x-mensaje WITH FRAME {&FRAME-NAME}.

    /* launch Excel so it is visible to the user */
    chExcelApplication:Visible = TRUE.

    /* release com-handles */
    RELEASE OBJECT chExcelApplication.      
    RELEASE OBJECT chWorkbook.
    RELEASE OBJECT chWorksheet.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Excel3 W-Win 
PROCEDURE Excel3 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE VARIABLE chExcelApplication AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE chWorkbook         AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE chWorksheet        AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE iCount             AS INTEGER INITIAL 1 NO-UNDO.
    DEFINE VARIABLE cColumn            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cRange             AS CHARACTER NO-UNDO.

    /* create a new Excel Application object */
    CREATE "Excel.Application" chExcelApplication.

    /* create a new Workbook */
    chWorkbook = chExcelApplication:Workbooks:Add().

    /* get the active Worksheet */
    chWorkSheet = chExcelApplication:Sheets:Item(1).

    /* Encabezados */
    DEFINE VARIABLE S-CODMOV AS CHAR NO-UNDO. 
    DEFINE VARIABLE x-codpro LIKE Almcmov.codpro NO-UNDO.
    DEFINE VARIABLE x-codcli LIKE Almcmov.codcli NO-UNDO.
    DEFINE VARIABLE x-nrorf1 LIKE Almcmov.nrorf1 NO-UNDO.
    DEFINE VARIABLE x-nrorf2 LIKE Almcmov.nrorf2 NO-UNDO.
    DEFINE VARIABLE x-codmov LIKE Almcmov.codmov NO-UNDO.
    DEFINE VARIABLE x-inggen LIKE Almdmov.candes NO-UNDO.
    DEFINE VARIABLE x-salgen LIKE Almdmov.candes NO-UNDO.
    DEFINE VARIABLE x-totgen LIKE Almdmov.candes NO-UNDO.
    DEFINE VARIABLE x-codmon LIKE Almdmov.candes NO-UNDO.
    DEFINE VARIABLE x-tpocmb LIKE Almdmov.candes NO-UNDO.
    DEFINE VARIABLE x-total AS DECIMAL.

    /* PARCHE: TODO EXPRESADO EN SOLES */
    DEF VAR nCodMon AS INT NO-UNDO.
    nCodMon = 1.
    /* ******************************* */

    iCount = iCount + 1.
    cColumn = STRING(iCount).
    cRange = "D" + cColumn.
    chWorkSheet:Range(cRange):Value = "KARDEX GENERAL".
    iCount = iCount + 1.
    cColumn = STRING(iCount).
    cRange = "D" + cColumn.
    chWorkSheet:Range(cRange):Value = s-subtit.
    iCount = iCount + 1.
    cColumn = STRING(iCount).
    cRange = "D" + cColumn.
    chWorkSheet:Range(cRange):Value = IF nCodmon = 1 THEN "Expresado en Nuevos Soles " ELSE "Expresado en Dolares Americanos".
    iCount = iCount + 1.
    cColumn = STRING(iCount).
    cRange = "A" + cColumn.
    chWorkSheet:Range(cRange):Value = "Codigo".
    cRange = "B" + cColumn.
    chWorkSheet:Range(cRange):Value = "Descripcion".
    cRange = "C" + cColumn.
    chWorkSheet:Range(cRange):Value = "Cat Contable".
    cRange = "D" + cColumn.
    chWorkSheet:Range(cRange):Value = "Marca".
    cRange = "E" + cColumn.
    chWorkSheet:Range(cRange):Value = "Unidad".
    cRange = "F" + cColumn.
    chWorkSheet:Range(cRange):Value = "Saldo Inicial".
    cRange = "G" + cColumn.
    chWorkSheet:Range(cRange):Value = "Promedio Inicial".
    cRange = "H" + cColumn.
    chWorkSheet:Range(cRange):Value = "Valor Inicial".
    cRange = "I" + cColumn.
    chWorkSheet:Range(cRange):Value = "Ingresos".
    cRange = "J" + cColumn.
    chWorkSheet:Range(cRange):Value = "Total Ingresos".
    cRange = "K" + cColumn.
    chWorkSheet:Range(cRange):Value = "Salidas".
    cRange = "L" + cColumn.
    chWorkSheet:Range(cRange):Value = "Total Salidas".
    cRange = "M" + cColumn.
    chWorkSheet:Range(cRange):Value = "Costo Promedio".
    cRange = "N" + cColumn.
    chWorkSheet:Range(cRange):Value = "Saldos".
    cRange = "O" + cColumn.
    chWorkSheet:Range(cRange):Value = "Costo Total".

    ASSIGN
       x-inggen = 0
       x-salgen = 0
       x-totgen = 0  
       x-total  = 0.
    FIND LAST gn-tcmb WHERE gn-tcmb.fecha <= DesdeF NO-LOCK NO-ERROR.
    KARDEX:
    FOR EACH Almmmatg NO-LOCK WHERE Almmmatg.CodCia = S-CODCIA 
        AND  Almmmatg.CodMat >= DesdeC  
        AND  Almmmatg.CodMat <= HastaC  
        AND  Almmmatg.codfam BEGINS F-CodFam 
        AND  Almmmatg.TpoArt BEGINS R-Tipo 
        AND LOOKUP(TRIM(Almmmatg.CatConta[1]), 'AF,SV,XX') = 0
        AND  Almmmatg.Licencia[1] BEGINS x-Licencia,
        EACH Almdmov NO-LOCK USE-INDEX ALMD02 WHERE Almdmov.CodCia = Almmmatg.CodCia 
            AND  Almdmov.codmat = Almmmatg.CodMat 
            AND  Almdmov.FchDoc >= DesdeF 
            AND  Almdmov.FchDoc <= HastaF,
            FIRST Almtmov NO-LOCK WHERE Almtmovm.Codcia = Almdmov.Codcia 
                AND Almtmovm.TipMov = Almdmov.TipMov 
                AND Almtmovm.Codmov = Almdmov.Codmov
                AND Almtmovm.Movtrf = No,
                FIRST Almacen OF Almdmov NO-LOCK 
                    WHERE Almacen.FlgRep = YES AND Almacen.AlmCsg = No
                    BREAK BY Almmmatg.CodCia BY Almmmatg.CodMat:

        FIND LAST AlmStkGe WHERE almstkge.codcia = s-codcia
            AND almstkge.codmat = almmmatg.codmat
            AND almstkge.fecha <= HastaF
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE AlmStkGe OR Almstkge.stkact = 0 THEN NEXT.

        DISPLAY "Codigo de Articulo: " + Almmmatg.CodMat @ x-mensaje
            WITH FRAME {&FRAME-NAME}.

        IF FIRST-OF(Almmmatg.CodMat) THEN DO:
           /* BUSCAMOS SI TIENE MOVIMIENTOS ANTERIORES A DesdeF */
           FIND LAST AlmStkGe WHERE AlmstkGe.Codcia = Almmmatg.Codcia 
               AND AlmstkGe.CodMat = Almmmatg.CodMat 
               AND AlmstkGe.Fecha < DesdeF
               NO-LOCK NO-ERROR.
           ACCUMULATE F-Ingreso (TOTAL BY Almmmatg.CodMat).
           ACCUMULATE F-TotIng  (TOTAL BY Almmmatg.CodMat).
           ACCUMULATE F-Salida  (TOTAL BY Almmmatg.CodMat).
           ACCUMULATE F-TotSal  (TOTAL BY Almmmatg.CodMat).
           ASSIGN
               F-STKGEN = 0
               F-SALDO  = 0
               F-PRECIO = 0
               F-VALCTO = 0.
           IF AVAILABLE AlmStkGe THEN DO:
              F-STKGEN = AlmStkGe.StkAct.
              F-SALDO  = AlmStkGe.StkAct.
              F-PRECIO = AlmStkGe.CtoUni.
              F-VALCTO = F-STKGEN * F-PRECIO.
           END.
           iCount = iCount + 1.
           cColumn = STRING(iCount).
           cRange = "A" + cColumn.
           chWorkSheet:Range(cRange):Value = "'" + Almmmatg.codmat.
           cRange = "B" + cColumn.
           chWorkSheet:Range(cRange):Value = Almmmatg.desmat.
           cRange = "C" + cColumn.
           chWorkSheet:Range(cRange):Value = Almmmatg.catconta[1].
           cRange = "D" + cColumn.
           chWorkSheet:Range(cRange):Value = Almmmatg.desmar.
           cRange = "E" + cColumn.
           chWorkSheet:Range(cRange):Value = Almmmatg.undstk.
           cRange = "F" + cColumn.
           chWorkSheet:Range(cRange):Value = f-saldo.
           cRange = "G" + cColumn.
           chWorkSheet:Range(cRange):Value = f-precio.
           cRange = "H" + cColumn.
           chWorkSheet:Range(cRange):Value = f-valcto.
        END.

        ASSIGN
            F-Ingreso = 0
            F-TotIng = 0
            F-Salida = 0
            F-TotSal = 0.

        FIND Almcmov WHERE Almcmov.CodCia = Almdmov.codcia 
            AND  Almcmov.CodAlm = Almdmov.codalm 
            AND  Almcmov.TipMov = Almdmov.tipmov 
            AND  Almcmov.CodMov = Almdmov.codmov 
            AND  Almcmov.NroDoc = Almdmov.nrodoc NO-LOCK NO-ERROR.

        IF AVAILABLE Almcmov THEN DO:
            ASSIGN
                x-codpro = Almcmov.codpro
                x-codcli = Almcmov.codcli
                x-nrorf1 = Almcmov.nrorf1
                x-nrorf2 = Almcmov.nrorf2
                x-codmon = Almcmov.codmon
                x-tpocmb = Almcmov.tpocmb.
        END.

        S-CODMOV  = Almdmov.TipMov + STRING(Almdmov.CodMov,"99"). 
        
        /* INGRESOS */
        F-Ingreso = IF LOOKUP(Almdmov.TipMov,"I,U,R") > 0 THEN (Almdmov.CanDes * Almdmov.Factor) ELSE 0.
        F-PreIng  = 0.
        F-TotIng  = 0.
        IF nCodmon = x-Codmon THEN DO:
            IF Almdmov.Tipmov = 'I' THEN DO:
                F-PreIng  = Almdmov.PreUni / Almdmov.Factor.
                F-TotIng  = Almdmov.ImpCto.
            END.
            ELSE DO:
                F-PreIng  = 0.
                F-TotIng  = F-PreIng * F-Ingreso.
            END.
        END.
        ELSE DO:
            IF nCodmon = 1 THEN DO:
                IF Almdmov.Tipmov = 'I' THEN DO:
                    F-PreIng  = ROUND(Almdmov.PreUni * Almdmov.Tpocmb / Almdmov.Factor, 4).
                    F-TotIng  = (Almdmov.ImpCto * Almdmov.TpoCmb ).
                END.
                IF LOOKUP(Almdmov.Tipmov, 'U,R') > 0 THEN DO:
                    F-PreIng  = 0.
                    F-TotIng  = F-PreIng * F-Ingreso.
                END.
            END.
            ELSE DO:
                IF Almdmov.Tipmov = 'I' THEN DO:
                    F-PreIng  = ROUND(Almdmov.PreUni / Almdmov.TpoCmb / Almdmov.Factor, 4).
                    F-TotIng  = ROUND(Almdmov.ImpCto / Almdmov.TpoCmb, 2).
                END.
                IF LOOKUP(Almdmov.Tipmov, 'U,R') > 0 THEN DO:
                    F-PreIng  = 0.
                    F-TotIng  = F-PreIng * F-Ingreso.
                END.
            END.
        END.
        f-Salida = 0.
        IF LOOKUP (Almdmov.TipMov, 'S,T') > 0 THEN DO:
            f-Salida = Almdmov.CanDes * Almdmov.Factor.
            /* Costo Unitario */
            FIND LAST AlmStkGe WHERE almstkge.codcia = s-codcia
                AND almstkge.codmat = almmmatg.codmat
                AND almstkge.fecha <= Almdmov.fchdoc NO-LOCK NO-ERROR.
            IF AVAILABLE AlmStkGe THEN f-TotSal = Almdmov.CanDes * Almdmov.Factor * AlmStkge.CtoUni.
        END.
            
        /* ACUMULADOS */
        ASSIGN
            F-Saldo   = Almdmov.StkAct
            F-VALCTO = Almdmov.StkAct * Almdmov.VctoMn1
            F-PRECIO = Almdmov.VctoMn1.
        ACCUMULATE F-Ingreso (TOTAL BY Almmmatg.CodMat).
        ACCUMULATE F-TotIng  (TOTAL BY Almmmatg.CodMat).
        ACCUMULATE F-Salida  (TOTAL BY Almmmatg.CodMat).
        ACCUMULATE F-TotSal  (TOTAL BY Almmmatg.CodMat).
        
        IF LAST-OF(Almmmatg.CodMat) THEN DO:
            /* Costo Unitario */
            FIND LAST AlmStkGe WHERE almstkge.codcia = s-codcia
                AND almstkge.codmat = almmmatg.codmat
                AND almstkge.fecha <= HastaF
                NO-LOCK NO-ERROR.
            IF AVAILABLE AlmStkGe THEN DO:
                ASSIGN
                    f-Saldo = AlmStkge.StkAct
                    f-Precio = AlmStkge.CtoUni
                    f-ValCto = f-Saldo * f-Precio.
            END.
            cColumn = STRING(iCount).
            cRange = "I" + cColumn.
            chWorkSheet:Range(cRange):Value = (ACCUM TOTAL BY Almmmatg.CodMat F-Ingreso).
            cRange = "J" + cColumn.
            chWorkSheet:Range(cRange):Value = (ACCUM TOTAL BY Almmmatg.CodMat F-TotIng).
            cRange = "K" + cColumn.
            chWorkSheet:Range(cRange):Value = (ACCUM TOTAL BY Almmmatg.CodMat F-Salida).
            cRange = "L" + cColumn.
            chWorkSheet:Range(cRange):Value = (ACCUM TOTAL BY Almmmatg.CodMat F-TotSal).
            cRange = "M" + cColumn.
            chWorkSheet:Range(cRange):Value = F-Precio.
            cRange = "N" + cColumn.
            chWorkSheet:Range(cRange):Value = F-Saldo.
            cRange = "O" + cColumn.
            chWorkSheet:Range(cRange):Value = F-ValCto.
            x-total = x-total + F-VALCTO.
        END.
    END.    
    /*
    HIDE FRAME F-PROCESO.
    */
    DISPLAY "" @ x-mensaje WITH FRAME {&FRAME-NAME}.

    /* launch Excel so it is visible to the user */
    chExcelApplication:Visible = TRUE.

    /* release com-handles */
    RELEASE OBJECT chExcelApplication.      
    RELEASE OBJECT chWorkbook.
    RELEASE OBJECT chWorksheet.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Formato W-Win 
PROCEDURE Formato :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE S-CODMOV AS CHAR NO-UNDO. 
  DEFINE VARIABLE x-codpro LIKE Almcmov.codpro NO-UNDO.
  DEFINE VARIABLE x-codcli LIKE Almcmov.codcli NO-UNDO.
  DEFINE VARIABLE x-nrorf1 LIKE Almcmov.nrorf1 NO-UNDO.
  DEFINE VARIABLE x-nrorf2 LIKE Almcmov.nrorf2 NO-UNDO.
  DEFINE VARIABLE x-nrorf3 LIKE Almcmov.nrorf3 NO-UNDO.
  DEFINE VARIABLE x-codmov LIKE Almcmov.codmov NO-UNDO.

  DEFINE VARIABLE x-inggen LIKE Almdmov.candes NO-UNDO.
  DEFINE VARIABLE x-salgen LIKE Almdmov.candes NO-UNDO.
  DEFINE VARIABLE x-totgen LIKE Almdmov.candes NO-UNDO.
  DEFINE VARIABLE x-codmon LIKE Almdmov.candes NO-UNDO.
  DEFINE VARIABLE x-tpocmb LIKE Almdmov.candes NO-UNDO.

  DEFINE VARIABLE x-total AS DECIMAL.
  /* Tipo de movimiento de la salida por G/R */
  
  DEFINE FRAME F-REPORTE
      Almdmov.CodAlm COLUMN-LABEL "Cod!Alm"          FORMAT "X(3)"
      S-CODMOV       COLUMN-LABEL "Cod!Mov"          FORMAT "X(3)"
      Almdmov.NroDoc COLUMN-LABEL "Numero!Interno"   FORMAT ">>>>>>9"
      x-NroRf1       COLUMN-LABEL "Numero!Documento" FORMAT 'x(12)'
      x-NroRf2       COLUMN-LABEL "G/R"              FORMAT 'x(12)'
      x-NroRf3       COLUMN-LABEL "FAC"              FORMAT 'x(12)'
      Almdmov.Almori COLUMN-LABEL "Alm!Ori/Des"      FORMAT "X(7)" 
      x-CodPro       COLUMN-LABEL "Cliente o!Proveedor" FORMAT 'x(11)'
      /*x-CodPro       COLUMN-LABEL "Codigo!Proveedor" FORMAT 'x(11)'*/
      /*x-CodCli       COLUMN-LABEL "Codigo!Cliente"   FORMAT 'x(11)'*/
      Almdmov.FchDoc COLUMN-LABEL "Fecha de!Documento"   FORMAT '99/99/9999'
      F-Ingreso      COLUMN-LABEL "INGRESOS"         FORMAT '>>>>>>9.99'
      F-Salida       COLUMN-LABEL "SALIDAS"          FORMAT '>>>>>>9.99'
      F-PreIng       COLUMN-LABEL "CTO.INGRESO"      FORMAT '>>>>>>9.9999'
      F-PRECIO       COLUMN-LABEL "CTO.PROMEDIO"     FORMAT '(>>>>9.9999)'
      F-SALDO        COLUMN-LABEL "SALDO"            FORMAT '(>>>>>>9.99)'
      F-VALCTO       COLUMN-LABEL "COSTO!TOTAL"      FORMAT '(>>>>>>>9.99)'
      WITH WIDTH 250 NO-BOX NO-LABELS NO-UNDERLINE STREAM-IO DOWN. 

  DEFINE FRAME F-HEADER
      HEADER
      S-NOMCIA FORMAT "X(50)" AT 1 SKIP
      "KARDEX GENERAL" AT 80
      "Pagina :" TO 150 PAGE-NUMBER(REPORT) FORMAT "ZZZZZ9" SKIP
      S-SUBTIT AT 1       SKIP
      IF nCodmon = 1 THEN "Expresado en Nuevos Soles " ELSE "Expresado en Dolares Americanos" FORMAT 'X(40)' SKIP
      "----------------------------------------------------------------------------------------------------------------------------------------------------------------" SKIP
      "Cod Cod Numero  Numero                                 Alm     Cliente o   Fecha de                                                                       COSTO " SKIP
      "Alm Mov Interno Documento    G/R          FAC          Ori/Des Proveedor   Documento    INGRESOS    SALIDAS   CTO.INGRE. CTO.PROMEDIO       SALDO         TOTAL " SKIP
      "----------------------------------------------------------------------------------------------------------------------------------------------------------------" SKIP
/*     123 123 >>>>>>9 123456789012 123456789012 123456789012 1234567 12345678901 99/99/9999 >>>>>>9.99 >>>>>>9.99 >>>>>>9.9999 (>>>>9.9999) (>>>>>>9.99) (>>>>>>>9.99)
*/
  WITH PAGE-TOP WIDTH 250 NO-BOX NO-LABELS NO-UNDERLINE STREAM-IO DOWN. 

  ASSIGN
     x-inggen = 0
     x-salgen = 0
     x-totgen = 0  
     x-total  = 0.
  FIND LAST gn-tcmb WHERE gn-tcmb.fecha <= DesdeF NO-LOCK NO-ERROR.
  FOR EACH Almmmatg NO-LOCK WHERE Almmmatg.CodCia = S-CODCIA 
                             AND  Almmmatg.CodMat >= DesdeC  
                             AND  Almmmatg.CodMat <= HastaC  
                             AND  Almmmatg.codfam BEGINS F-CodFam 
                             AND  Almmmatg.TpoArt BEGINS R-Tipo 
                             AND LOOKUP(TRIM(Almmmatg.CatConta[1]), 'AF,SV,XX') = 0
                             AND  Almmmatg.Licencia[1] BEGINS x-Licencia,
      EACH Almdmov NO-LOCK USE-INDEX ALMD02 WHERE Almdmov.CodCia = Almmmatg.CodCia 
                            AND  Almdmov.codmat = Almmmatg.CodMat 
                            AND  Almdmov.FchDoc >= DesdeF 
                            AND  Almdmov.FchDoc <= HastaF,
      FIRST Almtmov NO-LOCK WHERE Almtmovm.Codcia = Almdmov.Codcia 
                            AND Almtmovm.TipMov = Almdmov.TipMov 
                            AND Almtmovm.Codmov = Almdmov.Codmov
                            AND Almtmovm.Movtrf = No,
      FIRST Almacen OF Almdmov NO-LOCK WHERE Almacen.FlgRep = Yes
                            AND Almacen.AlmCsg = No
                           BREAK BY Almmmatg.CodCia 
                                 BY Almmmatg.CodMat
                                 BY Almdmov.FchDoc:
      /*
      DISPLAY Almmmatg.CodMat @ Fi-Mensaje LABEL "Codigo de Articulo "
              FORMAT "X(11)" 
              WITH FRAME F-Proceso.
      */
      DISPLAY "Codigo de Articulo: " + Almmmatg.CodMat @ x-mensaje
          WITH FRAME {&FRAME-NAME}.
      VIEW STREAM REPORT FRAME F-HEADER.
            
      IF FIRST-OF(Almmmatg.CodMat) THEN DO:
         /* BUSCAMOS SI TIENE MOVIMEINTOS ANTERIORES A DesdeF */
         FIND LAST AlmStkGe WHERE AlmstkGe.Codcia = Almmmatg.Codcia AND
                                AlmstkGe.CodMat = Almmmatg.CodMat AND
                                AlmstkGe.Fecha < DesdeF
                                NO-LOCK NO-ERROR.
         F-STKGEN = 0.
         F-SALDO  = 0.
         F-PRECIO = 0.
         F-VALCTO = 0.
     
         IF AVAILABLE AlmStkGe THEN DO:
            F-STKGEN = AlmStkGe.StkAct.
            F-SALDO  = AlmStkGe.StkAct.
            F-PRECIO = AlmStkGe.CtoUni.
            IF F-PRECIO = ? THEN F-PRECIO = 0.
            F-VALCTO = F-STKGEN * F-PRECIO.
         END.
         PUT STREAM REPORT Almmmatg.CodMat AT 002 FORMAT "X(9)"
                           Almmmatg.DesMat AT 014 FORMAT "X(50)"
                           Almmmatg.DesMar        FORMAT "x(30)"
                           Almmmatg.UndStk        FORMAT "X(4)"
                           F-PRECIO        AT 113 FORMAT "->,>>>,>>9.9999"
                           F-Saldo         AT 129 FORMAT "->>>>>,>>9.99"
                           F-VALCTO        AT 144 FORMAT "->>>>>,>>9.99".
         DOWN STREAM REPORT WITH FRAME F-REPORTE.
      END.
      x-codpro = "".
      x-codcli = "".
      x-nrorf1 = "".
      x-nrorf2 = "".
      x-nrorf3 = "".

      FIND Almcmov WHERE Almcmov.CodCia = Almdmov.codcia 
                    AND  Almcmov.CodAlm = Almdmov.codalm 
                    AND  Almcmov.TipMov = Almdmov.tipmov 
                    AND  Almcmov.CodMov = Almdmov.codmov 
                    AND  Almcmov.NroDoc = Almdmov.nrodoc 
                   NO-LOCK NO-ERROR.

      IF AVAILABLE Almcmov THEN DO:
         ASSIGN
            x-codpro = Almcmov.codpro
            x-codcli = Almcmov.codcli
            x-nrorf1 = Almcmov.nrorf1
            x-nrorf2 = Almcmov.nrorf2
            x-nrorf3 = Almcmov.nrorf3
            x-codmon = Almcmov.codmon
            x-tpocmb = Almcmov.tpocmb.
      END.
                  
      ASSIGN
          S-CODMOV  = Almdmov.TipMov + STRING(Almdmov.CodMov,"99")
          F-Ingreso = ( IF LOOKUP(Almdmov.TipMov,"I,U,R") > 0 THEN (Almdmov.CanDes * Almdmov.Factor) ELSE 0 )
          F-PreIng  = 0
          F-TotIng  = 0.
      IF nCodmon = x-Codmon THEN DO:
         IF Almdmov.Tipmov = 'I' THEN DO:
            F-PreIng  = Almdmov.PreUni / Almdmov.Factor.
            F-TotIng  = Almdmov.ImpCto.
         END.
         ELSE DO:
             IF Almtmovm.TipMov = "S" AND Almtmov.MovCmp = YES THEN DO:
                 F-PreIng  = Almdmov.PreUni / Almdmov.Factor.
                 F-TotIng  = Almdmov.ImpCto.
             END.
             ELSE DO:
                 F-PreIng  = 0.
                 F-TotIng  = F-PreIng * F-Ingreso.
             END.
         END.
      END.
      ELSE DO:
         IF nCodmon = 1 THEN DO:
            IF Almdmov.Tipmov = 'I' THEN DO:
               F-PreIng  = ROUND(Almdmov.PreUni * Almdmov.Tpocmb / Almdmov.Factor, 4).
               F-TotIng  = (Almdmov.ImpCto * Almdmov.TpoCmb ).
            END.
            IF LOOKUP(Almdmov.Tipmov, 'U,R') > 0 THEN DO:
               F-PreIng  = 0.
               F-TotIng  = F-PreIng * F-Ingreso.
            END.
            IF Almtmovm.TipMov = "S" AND Almtmov.MovCmp = YES THEN DO:
                F-PreIng  = ROUND(Almdmov.PreUni * Almdmov.Tpocmb / Almdmov.Factor, 4).
                F-TotIng  = (Almdmov.ImpCto * Almdmov.TpoCmb ).
            END.
         END.
         ELSE DO:
            IF Almdmov.Tipmov = 'I' THEN DO:
               F-PreIng  = ROUND(Almdmov.PreUni / Almdmov.TpoCmb / Almdmov.Factor, 4).
               F-TotIng  = ROUND(Almdmov.ImpCto / Almdmov.TpoCmb, 2).
            END.
            IF LOOKUP(Almdmov.Tipmov, 'U,R') > 0 THEN DO:
               F-PreIng  = 0.
               F-TotIng  = F-PreIng * F-Ingreso.
            END.
            IF Almtmovm.TipMov = "S" AND Almtmov.MovCmp = YES THEN DO:
                F-PreIng  = ROUND(Almdmov.PreUni / Almdmov.TpoCmb / Almdmov.Factor, 4).
                F-TotIng  = ROUND(Almdmov.ImpCto / Almdmov.TpoCmb, 2).
            END.
         END.
      END.

      F-Salida  = IF LOOKUP(Almdmov.TipMov,"S,T") > 0 THEN (Almdmov.CanDes * Almdmov.Factor) ELSE 0.

      F-Saldo   = Almdmov.StkAct.

      IF Almdmov.VctoMn1 = ?  THEN DO:
        F-VALCTO = 0.
        F-PRECIO = 0.
      END.
      ELSE DO: 
          F-VALCTO = Almdmov.StkAct * Almdmov.VctoMn1.
          F-PRECIO = Almdmov.VctoMn1.
      END.               
 
      ACCUMULATE F-Ingreso (TOTAL BY Almmmatg.CodMat).
      ACCUMULATE F-Salida  (TOTAL BY Almmmatg.CodMat).
      
      DISPLAY STREAM REPORT 
               Almdmov.CodAlm 
               S-CODMOV  
               Almdmov.NroDoc 
               Almdmov.Almori WHEN Almdmov.Codmov = 03
               x-CodPro + x-CodCli @ x-CodPro
               /*x-CodPro
               x-CodCli */
               x-NroRf1 
               x-NroRf2 
               x-NroRf3
               Almdmov.FchDoc 
               F-Ingreso   
               F-Salida    
               /*F-PreIng    WHEN Almdmov.TipMov = "I" AND (ALmtmovm.TpoCto = 0 OR ALmtmovm.TpoCto = 1) */
               F-PreIng WHEN F-PreIng <> 0
               F-PRECIO    
               F-SALDO     
               F-VALCTO    
               WITH FRAME F-REPORTE.
      IF LAST-OF(Almmmatg.CodMat) THEN DO:
      
         UNDERLINE STREAM REPORT 
               F-Ingreso   
               F-Salida    
               F-PreIng    
               F-SALDO     
               F-VALCTO    
               WITH FRAME F-REPORTE.
      
         DISPLAY STREAM  REPORT 
                 ACCUM TOTAL BY Almmmatg.CodMat F-Ingreso WHEN (ACCUM TOTAL BY Almmmatg.CodMat F-Ingreso) > 0 @ F-Ingreso 
                 ACCUM TOTAL BY Almmmatg.CodMat F-Salida  @ F-Salida  
                 WITH FRAME F-REPORTE.

         x-total = x-total + F-VALCTO.
         
      END.
      IF LAST-OF(Almmmatg.CodCia) THEN DO:
         UNDERLINE STREAM REPORT 
                F-VALCTO 
                WITH FRAME F-REPORTE.
      
         DISPLAY STREAM  REPORT 
                 x-total @ F-ValCto
                 WITH FRAME F-REPORTE.
         UNDERLINE STREAM REPORT 
                F-VALCTO
                WITH FRAME F-REPORTE.
         DOWN STREAM REPORT WITH FRAME F-REPORTE.
      END.
  END.    
  /*
  HIDE FRAME F-PROCESO.
  */
  DISPLAY "" @ x-mensaje WITH FRAME {&FRAME-NAME}.
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
    ENABLE ALL EXCEPT F-DesFam F-DesSub x-NomLic x-mensaje.
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
        PUT STREAM REPORT CONTROL {&Prn0} + {&Prn5A} + CHR(66) + {&Prn4}.
        RUN Formato.
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
  ASSIGN DesdeC DesdeF F-CodFam F-DesFam HastaC HastaF nCodMon T-Resumen R-Tipo.
  
  IF HastaC <> "" THEN HastaC = "".
  IF DesdeF <> ?  THEN DesdeF = ?.
  IF HastaF <> ?  THEN HastaF = ?.

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
     ASSIGN DesdeF = TODAY  + 1 - DAY(TODAY).
            HastaF = TODAY.
            R-Tipo = ''.
     DISPLAY DesdeF HastaF R-Tipo.
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
        WHEN "F-Subfam" THEN ASSIGN input-var-1 = F-CodFam:SCREEN-VALUE IN FRAME {&FRAME-NAME}.
        WHEN "F-marca1" OR WHEN "F-marca2" THEN ASSIGN input-var-1 = "MK".
        WHEN "x-Licencia" THEN ASSIGN input-var-1 = "LC".
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Texto W-Win 
PROCEDURE Texto :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE VARIABLE S-CODMOV AS CHAR NO-UNDO. 
  DEFINE VARIABLE x-codpro LIKE Almcmov.codpro NO-UNDO.
  DEFINE VARIABLE x-codcli LIKE Almcmov.codcli NO-UNDO.
  DEFINE VARIABLE x-nrorf1 LIKE Almcmov.nrorf1 NO-UNDO.
  DEFINE VARIABLE x-nrorf2 LIKE Almcmov.nrorf2 NO-UNDO.
  DEFINE VARIABLE x-nrorf3 LIKE Almcmov.nrorf3 NO-UNDO.
  DEFINE VARIABLE x-codmov LIKE Almcmov.codmov NO-UNDO.

  DEFINE VARIABLE x-inggen LIKE Almdmov.candes NO-UNDO.
  DEFINE VARIABLE x-salgen LIKE Almdmov.candes NO-UNDO.
  DEFINE VARIABLE x-totgen LIKE Almdmov.candes NO-UNDO.
  DEFINE VARIABLE x-codmon LIKE Almdmov.candes NO-UNDO.
  DEFINE VARIABLE x-tpocmb LIKE Almdmov.candes NO-UNDO.

  DEFINE VARIABLE x-total AS DECIMAL.

  DEF VAR x-Archivo AS CHAR NO-UNDO.
  DEF VAR x-Rpta    AS LOG  NO-UNDO.

  x-Archivo = 'KardexContable.txt'.
  SYSTEM-DIALOG GET-FILE x-Archivo
    FILTERS 'Texto' '*.txt'
    ASK-OVERWRITE
    CREATE-TEST-FILE
    DEFAULT-EXTENSION '.txt'
    INITIAL-DIR 'c:\tmp'
    RETURN-TO-START-DIR 
    USE-FILENAME
    SAVE-AS
    UPDATE x-rpta.
  IF x-rpta = NO THEN RETURN.


  FIND LAST gn-tcmb WHERE gn-tcmb.fecha <= DesdeF NO-LOCK NO-ERROR.

  OUTPUT STREAM REPORT TO VALUE(x-Archivo).

    PUT STREAM REPORT UNFORMATTED
        "CODIGO|"
        "DESCRIPCION|"
        "CAT CONTABLE|"
        "MARCA|"
        "UM|"
        "ALMACEN|"
        "CODMOV|"
        "NUMERO|"
        "ALM ORIGEN|"
        "PROVEEDOR|"
        "CLIENTE|"
        "NRO DOCUMENTO|"
        "REFERENCIA|"
        "FAC|"
        "FECHA|"
        "INGRESO|"
        "SALIDA|"
        "CTO INGRESO|"
        "CTO PROMEDIO|"
        "SALDO|"
        "CTO TOTAL|"
        SKIP.
       
  ASSIGN
     x-inggen = 0
     x-salgen = 0
     x-totgen = 0  
     x-total  = 0.
  FIND LAST gn-tcmb WHERE gn-tcmb.fecha <= DesdeF NO-LOCK NO-ERROR.
  FOR EACH Almmmatg NO-LOCK WHERE Almmmatg.CodCia = S-CODCIA 
                             AND  Almmmatg.CodMat >= DesdeC  
                             AND  Almmmatg.CodMat <= HastaC  
                             AND  Almmmatg.codfam BEGINS F-CodFam 
                             AND  Almmmatg.TpoArt BEGINS R-Tipo 
                             AND LOOKUP(TRIM(Almmmatg.CatConta[1]), 'AF,SV,XX') = 0,
      EACH Almdmov NO-LOCK USE-INDEX ALMD02 WHERE Almdmov.CodCia = Almmmatg.CodCia 
                            AND  Almdmov.codmat = Almmmatg.CodMat 
                            AND  Almdmov.FchDoc >= DesdeF 
                            AND  Almdmov.FchDoc <= HastaF,
      FIRST Almtmov NO-LOCK WHERE Almtmovm.Codcia = Almdmov.Codcia 
                            AND Almtmovm.TipMov = Almdmov.TipMov 
                            AND Almtmovm.Codmov = Almdmov.Codmov
                            AND (TOGGLE-Transferencias = YES OR Almtmovm.Movtrf = NO),
      FIRST Almacen OF Almdmov NO-LOCK WHERE Almacen.FlgRep = Yes
                            AND Almacen.AlmCsg = No
                           BREAK BY Almmmatg.CodCia 
                                 BY Almmmatg.CodMat
                                 BY Almdmov.FchDoc:

      /*
      DISPLAY Almmmatg.CodMat @ Fi-Mensaje LABEL "Codigo de Articulo "
              FORMAT "X(11)" 
              WITH FRAME F-Proceso.
      */
      DISPLAY "Codigo de Articulo: " + Almmmatg.CodMat @ x-mensaje
          WITH FRAME {&FRAME-NAME}.

      IF FIRST-OF(Almmmatg.CodMat) THEN DO:

         /* BUSCAMOS SI TIENE MOVIMEINTOS ANTERIORES A DesdeF */
         FIND LAST AlmStkGe WHERE AlmstkGe.Codcia = Almmmatg.Codcia AND
                                AlmstkGe.CodMat = Almmmatg.CodMat AND
                                AlmstkGe.Fecha < DesdeF
                                NO-LOCK NO-ERROR.
         F-STKGEN = 0.
         F-SALDO  = 0.
         F-PRECIO = 0.
         F-VALCTO = 0.
     
         IF AVAILABLE AlmStkGe THEN DO:
            F-STKGEN = AlmStkGe.StkAct.
            F-SALDO  = AlmStkGe.StkAct.
            F-PRECIO = AlmStkGe.CtoUni.
            F-VALCTO = F-STKGEN * F-PRECIO.
         END.

      END.

      x-codpro = "".
      x-codcli = "".
      x-nrorf1 = "".
      x-nrorf2 = "".
      x-nrorf3 = "".

      FIND Almcmov WHERE Almcmov.CodCia = Almdmov.codcia 
                    AND  Almcmov.CodAlm = Almdmov.codalm 
                    AND  Almcmov.TipMov = Almdmov.tipmov 
                    AND  Almcmov.CodMov = Almdmov.codmov 
                    AND  Almcmov.NroDoc = Almdmov.nrodoc 
                   NO-LOCK NO-ERROR.

      IF AVAILABLE Almcmov THEN DO:
         ASSIGN
            x-codpro = Almcmov.codpro
            x-codcli = Almcmov.codcli
            x-nrorf1 = Almcmov.nrorf1
            x-nrorf2 = Almcmov.nrorf2
            X-nrorf3 = Almcmov.nrorf3
            x-codmon = Almcmov.codmon
            x-tpocmb = Almcmov.tpocmb.
      END.
                  
      S-CODMOV  = Almdmov.TipMov + STRING(Almdmov.CodMov,"99"). 

      F-Ingreso = IF LOOKUP(Almdmov.TipMov,"I,U,R") > 0 THEN (Almdmov.CanDes * Almdmov.Factor) ELSE 0.
      F-PreIng  = 0.
      F-TotIng  = 0.

      IF nCodmon = x-Codmon THEN DO:
         IF Almdmov.Tipmov = 'I' THEN DO:
            F-PreIng  = Almdmov.PreUni / Almdmov.Factor.
            F-TotIng  = Almdmov.ImpCto.
         END.
         ELSE DO:
            F-PreIng  = 0.
            F-TotIng  = F-PreIng * F-Ingreso.
         END.
         END.
      ELSE DO:
         IF nCodmon = 1 THEN DO:
            IF Almdmov.Tipmov = 'I' THEN DO:
               F-PreIng  = ROUND(Almdmov.PreUni * Almdmov.Tpocmb / Almdmov.Factor, 4).
               F-TotIng  = (Almdmov.ImpCto * Almdmov.TpoCmb ).
            END.
            IF LOOKUP(Almdmov.Tipmov, 'U,R') > 0 THEN DO:
               F-PreIng  = 0.
               F-TotIng  = F-PreIng * F-Ingreso.
            END.
            END.
         ELSE DO:
            IF Almdmov.Tipmov = 'I' THEN DO:
               F-PreIng  = ROUND(Almdmov.PreUni / Almdmov.TpoCmb / Almdmov.Factor, 4).
               F-TotIng  = ROUND(Almdmov.ImpCto / Almdmov.TpoCmb, 2).
            END.
            IF LOOKUP(Almdmov.Tipmov, 'U,R') > 0 THEN DO:
               F-PreIng  = 0.
               F-TotIng  = F-PreIng * F-Ingreso.
            END.
         END.
      END.

      F-Salida  = IF LOOKUP(Almdmov.TipMov,"S,T") > 0 THEN (Almdmov.CanDes * Almdmov.Factor) ELSE 0.
      F-Saldo   = F-Saldo + F-Ingreso - F-Salida.                                                                                                  
      /*F-Saldo   = Almdmov.StkAct.*/
      F-VALCTO = F-saldo * Almdmov.VCtoMn1.
      /*F-VALCTO = Almdmov.StkAct * Almdmov.VctoMn1.*/
      F-PRECIO = Almdmov.VctoMn1.
      ACCUMULATE F-Ingreso (TOTAL BY Almmmatg.CodMat).
      ACCUMULATE F-Salida  (TOTAL BY Almmmatg.CodMat).

        PUT STREAM REPORT UNFORMATTED
            Almmmatg.CodMat "|"
            Almmmatg.DesMat "|"
            Almmmatg.catconta[1] "|"
            Almmmatg.DesMar "|"
            Almmmatg.UndStk "|"
            Almdmov.CodAlm  "|"
            S-CODMOV "|"
            STRING(Almdmov.NroDoc,">999999") "|".
        IF Almdmov.Codmov = 03 THEN PUT STREAM REPORT UNFORMATTED Almdmov.Almori "|".
        ELSE PUT STREAM REPORT UNFORMATTED "|".
        PUT STREAM REPORT UNFORMATTED
            x-CodPro "|"
            x-CodCli "|"
            x-NroRf1 "|"
            x-NroRf2 "|"
            x-NroRf3 "|"
            Almdmov.FchDoc "|"
            F-Ingreso "|"
            F-Salida "|".
        IF Almdmov.TipMov = "I" AND (ALmtmovm.TpoCto = 0 OR ALmtmovm.TpoCto = 1) THEN
            PUT STREAM REPORT UNFORMATTED F-PreIng "|".
        ELSE PUT STREAM REPORT UNFORMATTED "|".
        PUT STREAM REPORT UNFORMATTED
            F-PRECIO "|"
            F-SALDO "|"
            F-VALCTO "|"
            SKIP.
      IF LAST-OF(Almmmatg.CodMat) THEN DO:
        x-total = x-total + F-VALCTO.
      END.
  END.    
  /*
  HIDE FRAME F-PROCESO.
  */
  DISPLAY "" @ x-mensaje WITH FRAME {&FRAME-NAME}.
  OUTPUT STREAM REPORT CLOSE.
  MESSAGE 'Proceso Terminado' VIEW-AS ALERT-BOX.

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

