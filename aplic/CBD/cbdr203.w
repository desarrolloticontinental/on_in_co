&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME DIALOG-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS DIALOG-1 
/*
    @PRINTER2.W    VERSION 1.0

    Modific�    Fecha       Objetivo
    --------    ----------- --------------------------------------------
    MLR-1       24/Set/2008 Modificaci�n de formato para campos num�ricos.
*/    

DEFINE STREAM report.
DEFINE BUFFER B-Cuentas FOR cb-ctas.

/* VARIABLES GENERALES :  IMPRESION,SISTEMA,MODULO,USUARIO */
DEF NEW SHARED VAR input-var-1 AS CHAR.
DEF NEW SHARED VAR input-var-2 AS CHAR.
DEF NEW SHARED VAR input-var-3 AS CHAR.
DEF NEW SHARED VAR output-var-1 AS ROWID.
DEF NEW SHARED VAR output-var-2 AS CHAR.
DEF NEW SHARED VAR output-var-3 AS CHAR.

DEFINE VARIABLE P-Largo    AS INTEGER NO-UNDO.
DEFINE VARIABLE P-Ancho    AS INTEGER NO-UNDO.
DEFINE VARIABLE P-pagini   AS INTEGER FORMAT ">>>9" NO-UNDO.
DEFINE VARIABLE P-pagfin   AS INTEGER FORMAT ">>>9" NO-UNDO.
DEFINE VARIABLE P-copias   AS INTEGER FORMAT ">9" NO-UNDO.
DEFINE VARIABLE P-select   AS INTEGER FORMAT "9" NO-UNDO.
DEFINE VARIABLE P-archivo  AS CHARACTER FORMAT "x(30)" NO-UNDO.

DEFINE VARIABLE P-detalle  LIKE TermImp.Detalle NO-UNDO.
DEFINE VARIABLE P-comando  LIKE TermImp.Comando NO-UNDO.
DEFINE VARIABLE P-device   LIKE TermImp.Device NO-UNDO.
DEFINE VARIABLE P-name     LIKE TermImp.p-name NO-UNDO.

DEFINE VARIABLE P-Reset    AS CHARACTER NO-UNDO.
DEFINE VARIABLE P-Flen     AS CHARACTER NO-UNDO.
DEFINE VARIABLE P-6lpi     AS CHARACTER NO-UNDO.
DEFINE VARIABLE P-8lpi     AS CHARACTER NO-UNDO.
DEFINE VARIABLE P-10cpi    AS CHARACTER NO-UNDO.
DEFINE VARIABLE P-12cpi    AS CHARACTER NO-UNDO.
DEFINE VARIABLE P-15cpi    AS CHARACTER NO-UNDO.
DEFINE VARIABLE P-20cpi    AS CHARACTER NO-UNDO.
DEFINE VARIABLE P-Landscap AS CHARACTER NO-UNDO.
DEFINE VARIABLE P-Portrait AS CHARACTER NO-UNDO.
DEFINE VARIABLE P-DobleOn  AS CHARACTER NO-UNDO.
DEFINE VARIABLE P-DobleOff AS CHARACTER NO-UNDO.
DEFINE VARIABLE P-BoldOn   AS CHARACTER NO-UNDO.
DEFINE VARIABLE P-BoldOff  AS CHARACTER NO-UNDO.
DEFINE VARIABLE P-UlineOn  AS CHARACTER NO-UNDO.
DEFINE VARIABLE P-UlineOff AS CHARACTER NO-UNDO.
DEFINE VARIABLE P-ItalOn   AS CHARACTER NO-UNDO.
DEFINE VARIABLE P-ItalOff  AS CHARACTER NO-UNDO.
DEFINE VARIABLE P-SuperOn  AS CHARACTER NO-UNDO.
DEFINE VARIABLE P-SuperOff AS CHARACTER NO-UNDO.
DEFINE VARIABLE P-SubOn    AS CHARACTER NO-UNDO.
DEFINE VARIABLE P-SubOff   AS CHARACTER NO-UNDO.
DEFINE VARIABLE P-Proptnal AS CHARACTER NO-UNDO.
DEFINE VARIABLE P-Lpi      AS CHARACTER NO-UNDO.
DEFINE VARIABLE l-immediate-display AS LOGICAL NO-UNDO.
DEFINE VARIABLE x-Raya     AS CHARACTER FORMAT "X(150)" NO-UNDO.
DEFINE {&NEW} SHARED VARIABLE xTerm     AS CHARACTER INITIAL "".
DEFINE {&NEW} SHARED VARIABLE s-aplic-id  LIKE Modulos.Modulo.
DEFINE {&NEW} SHARED VARIABLE s-user-id  LIKE _user._userid.
DEFINE {&NEW} SHARED VARIABLE cb-niveles  AS CHARACTER INITIAL "2,3,4,5".
DEFINE        VARIABLE P-config  AS CHARACTER NO-UNDO.
DEFINE        VARIABLE c-Pagina  AS INTEGER LABEL "                 Imprimiendo Pagina " NO-UNDO.
DEFINE        VARIABLE c-Copias  AS INTEGER NO-UNDO.
DEFINE        VARIABLE x-Detalle LIKE Modulos.Detalle NO-UNDO.
DEFINE        VARIABLE i           AS INTEGER NO-UNDO.
DEFINE        VARIABLE OKpressed   AS LOGICAL NO-UNDO.
DEFINE        VARIABLE Ult-Nivel   AS INTEGER NO-UNDO.
DEFINE        VARIABLE Max-Digitos AS INTEGER NO-UNDO.

DEFINE        VARIABLE cb-codcia AS INTEGER NO-UNDO.
DEFINE        VARIABLE pv-codcia AS INTEGER NO-UNDO.
DEFINE        VARIABLE cl-codcia AS INTEGER NO-UNDO.
DEFINE        VARIABLE PTO        AS LOGICAL NO-UNDO.

/* MACRO NUEVA-PAGINA */
&GLOBAL-DEFINE NEW-PAGE READKEY PAUSE 0. ~
IF LASTKEY = KEYCODE("F10") THEN RETURN ERROR. ~
IF LINE-COUNTER( report ) > (P-Largo - 8 ) OR c-Pagina = 0 ~
THEN DO: ~
    RUN NEW-PAGE NO-ERROR. ~
    IF ERROR-STATUS:ERROR THEN RETURN ERROR. ~
END


/*VARIABLES PARTICULARES DE LA RUTINA */
DEFINE VARIABLE pinta-mes  AS CHARACTER FORMAT "X(40)" NO-UNDO.
DEFINE VARIABLE x-moneda   AS CHARACTER FORMAT "X(40)" NO-UNDO.
DEFINE VARIABLE x-expres   AS CHARACTER FORMAT "X(40)" NO-UNDO.
DEFINE VARIABLE x-fchast   AS CHARACTER FORMAT "X(5)" NO-UNDO.
DEFINE VARIABLE x-fchdoc   AS CHARACTER FORMAT "X(5)" NO-UNDO.
DEFINE VARIABLE x-fchvto   AS CHARACTER FORMAT "X(5)" NO-UNDO.
DEFINE VARIABLE x-clfaux   LIKE cb-dmov.clfaux NO-UNDO.
DEFINE VARIABLE x-codaux   LIKE cb-dmov.codaux NO-UNDO.
DEFINE VARIABLE x-nroast   LIKE cb-dmov.nroast NO-UNDO.
DEFINE VARIABLE x-codope   LIKE cb-dmov.codope NO-UNDO.
DEFINE VARIABLE x-coddoc   LIKE cb-dmov.coddoc NO-UNDO.
DEFINE VARIABLE x-nrodoc   LIKE cb-dmov.nrodoc NO-UNDO.
DEFINE VARIABLE x-nroref   LIKE cb-dmov.nroref NO-UNDO.
DEFINE VARIABLE x-glodoc   LIKE cb-dmov.glodoc NO-UNDO.
DEFINE VARIABLE x-CodCta   LIKE cb-dmov.CodCta NO-UNDO.

DEFINE VARIABLE x-Cco      LIKE cb-dmov.Cco    NO-UNDO.

/*MLR-1* Inicio de bloque comentado ***
DEFINE VARIABLE x-debe     AS DECIMAL FORMAT "ZZZ,ZZZ,ZZ9.99-" 
                           COLUMN-LABEL "M o v i m i!Cargos     " NO-UNDO. 
DEFINE VARIABLE x-haber    AS DECIMAL FORMAT "ZZZ,ZZZ,ZZ9.99-" 
                           COLUMN-LABEL "e n t o s      !Abonos     " NO-UNDO.
DEFINE VARIABLE x-saldoi   AS DECIMAL FORMAT "ZZZ,ZZZ,ZZ9.99-"
                           COLUMN-LABEL "Saldo     !Inicial    " NO-UNDO.
DEFINE VARIABLE x-saldof   AS DECIMAL FORMAT "ZZZ,ZZZ,ZZ9.99-"
                           COLUMN-LABEL "S a l d o!Deudor    " NO-UNDO.
DEFINE VARIABLE x-deudor   AS DECIMAL FORMAT "ZZZ,ZZZ,ZZ9.99-"
                           COLUMN-LABEL "S a l d o!Deudor    " NO-UNDO.
DEFINE VARIABLE x-acreedor AS DECIMAL FORMAT "ZZZ,ZZZ,ZZ9.99-" 
                           COLUMN-LABEL "A c t u a l    !Acreedor   " NO-UNDO.
DEFINE VARIABLE x-importe AS DECIMAL FORMAT "->>>>>>,>>9.99" 
                           COLUMN-LABEL "Importe S/." NO-UNDO. 
*MLR-1* Fin de bloque comentado ***/
/*MLR-1* Inicio de Bloque */
DEFINE VARIABLE x-debe     AS DECIMAL FORMAT "ZZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99-" 
                           COLUMN-LABEL "M o v i m i!Cargos     " NO-UNDO. 
DEFINE VARIABLE x-haber    AS DECIMAL FORMAT "ZZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99-" 
                           COLUMN-LABEL "e n t o s      !Abonos     " NO-UNDO.
DEFINE VARIABLE x-saldoi   AS DECIMAL FORMAT "ZZZZ,ZZZ,ZZ9.99-"
                           COLUMN-LABEL "Saldo     !Inicial    " NO-UNDO.
DEFINE VARIABLE x-saldof   AS DECIMAL FORMAT "ZZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99-"
                           COLUMN-LABEL "S a l d o!Deudor    " NO-UNDO.
DEFINE VARIABLE x-deudor   AS DECIMAL FORMAT "ZZZZ,ZZZ,ZZ9.99-"
                           COLUMN-LABEL "S a l d o!Deudor    " NO-UNDO.
DEFINE VARIABLE x-acreedor AS DECIMAL FORMAT "ZZZZ,ZZZ,ZZ9.99-" 
                           COLUMN-LABEL "A c t u a l    !Acreedor   " NO-UNDO.
DEFINE VARIABLE x-importe AS DECIMAL FORMAT "->>>>>>>,>>9.99" 
                           COLUMN-LABEL "Importe S/." NO-UNDO. 
/*MLR-1* Fin de Bloque */

DEFINE VARIABLE c-debe     AS DECIMAL NO-UNDO.
DEFINE VARIABLE c-haber    AS DECIMAL NO-UNDO.
DEFINE VARIABLE t-debe     AS DECIMAL NO-UNDO.
DEFINE VARIABLE t-haber    AS DECIMAL NO-UNDO.
DEFINE VARIABLE t-saldoi   AS DECIMAL NO-UNDO.
DEFINE VARIABLE t-saldof   AS DECIMAL NO-UNDO.

DEFINE VARIABLE x-conreg   AS INTEGER NO-UNDO.

DEFINE VARIABLE v-Saldoi   AS DECIMAL   EXTENT 10 NO-UNDO.
DEFINE VARIABLE v-Debe     AS DECIMAL   EXTENT 10 NO-UNDO.
DEFINE VARIABLE v-Haber    AS DECIMAL   EXTENT 10 NO-UNDO.
DEFINE VARIABLE v-CodCta   AS CHARACTER EXTENT 10 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE s-NroMes    AS INTEGER INITIAL 3.
DEFINE {&NEW} SHARED VARIABLE s-periodo    AS INTEGER INITIAL 1996.
DEFINE {&NEW} SHARED VARIABLE s-codcia AS INTEGER INITIAL 1.
DEFINE {&NEW} SHARED VARIABLE s-nomcia AS CHARACTER FORMAT "X(40)".
DEFINE {&NEW} SHARED VARIABLE x-DIRCIA AS CHARACTER FORMAT "X(40)".

FIND Empresas WHERE Empresas.CodCia = s-codcia NO-LOCK.
IF NOT Empresas.Campo-CodCbd THEN cb-codcia = s-codcia.
IF NOT Empresas.Campo-CodCli THEN cl-codcia = s-codcia.
IF NOT Empresas.Campo-CodPro THEN pv-codcia = s-codcia.

DEFINE IMAGE IMAGE-1
     FILENAME "IMG/print"
     SIZE 5 BY 1.5.

DEFINE FRAME F-Mensaje
     IMAGE-1 AT ROW 1.5 COL 5
     "Espere un momento" VIEW-AS TEXT
          SIZE 18 BY 1 AT ROW 1.5 COL 16
          font 4
     "por favor ...." VIEW-AS TEXT
          SIZE 10 BY 1 AT ROW 2.5 COL 19
          font 4
     "F10 = Cancela Reporte" VIEW-AS TEXT
          SIZE 21 BY 1 AT ROW 3.5 COL 12
          font 4          
     SPACE(10.28) SKIP(0.14)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE 
         BGCOLOR 15 FGCOLOR 0 
         TITLE "Imprimiendo ...".

def var x-coddiv like cb-dmov.coddiv.

DEFINE FRAME f-cab
        x-coddiv       COLUMN-LABEL "Cod.!Div."
        x-cco          FORMAT "XX" COLUMN-LABEL "CC"
        x-fchast       COLUMN-LABEL "Fecha! Ast."
        x-nroast       COLUMN-LABEL "Compro!bante"  
        x-codope       COLUMN-LABEL "Li-!bro"
        x-ClfAux       COLUMN-LABEL "Clf!Aux"
        x-codaux       COLUMN-LABEL "Cod.!Auxiliar"
        x-fchdoc       COLUMN-LABEL "Fecha! Doc."
        x-CodDoc       FORMAT "XX" COLUMN-LABEL "Co!Do"
        x-nrodoc   
        x-nroref   
        x-glodoc       FORMAT "X(24)"     
        x-debe
        x-haber
        x-importe  
        x-saldof       COLUMN-LABEL "Saldo     !Final     "
        HEADER
        S-NOMCIA FORMAT "X(50)"
        "L I B R O   M A Y O R   G E N E R A L" TO 100
        SKIP
        pinta-mes AT 62
        "PAGINA :" TO 150 c-pagina FORMAT "ZZZZ9" TO 160 
        x-expres  AT 62
        "HORA   :" TO 150 STRING(TIME,"HH:MM AM") TO 160
        SKIP(2)
        WITH WIDTH 230 NO-BOX DOWN STREAM-IO.

DEFINE FRAME f-cab-t
        x-coddiv       COLUMN-LABEL "Cod.!Div."
        x-cco          COLUMN-LABEL "C.Cos"
        x-fchast       COLUMN-LABEL "Fecha! Ast."
        x-nroast       COLUMN-LABEL "Compro!bante"  
        x-codope       COLUMN-LABEL "Li-!bro"
        x-ClfAux       COLUMN-LABEL "Clf!Aux"
        x-codaux       COLUMN-LABEL "Cod.!Auxiliar"
        x-fchdoc       COLUMN-LABEL "Fecha! Doc."
        x-CodDoc       COLUMN-LABEL "Cod!Doc" FORMAT "xx"
        x-nrodoc
/*MLR* 07/Mart/2008 ***
        x-fchvto       COLUMN-LABEL "Fecha! Vto."
* ***/
        x-nroref   
        x-glodoc       FORMAT "X(24)"     
/*MLR* 07/Mart/2008 ***
        x-saldoi  
* ***/
        x-debe
        x-haber
        x-importe
        x-saldof       COLUMN-LABEL "Saldo     !Final     "
        WITH WIDTH 230 NO-BOX DOWN STREAM-IO.

def var x-codmon as integer init 1.
def var x-clfaux-1 as char.
def var x-clfaux-2 as char.

DEF VAR G-NOMCTA AS CHAR FORMAT "X(60)".
DEFINE VAR X-MENSAJE AS CHAR FORMAT "X(40)".
DEFINE FRAME F-AUXILIAR
     X-MENSAJE
WITH TITLE "Espere un momento por favor" VIEW-AS DIALOG-BOX CENTERED
          NO-LABELS.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DIALOG-1

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-4 RECT-5 C-Moneda y-CodDiv C-Mes ~
y-codope x-cta-ini x-cta-fin x-Clasificacion x-auxiliar FILL-IN-Cco ~
TOGGLE-Sustento RADIO-SET-1 RB-NUMBER-COPIES B-impresoras RB-BEGIN-PAGE ~
RB-END-PAGE B-imprime B-cancela BUTTON-2 BUTTON-3 
&Scoped-Define DISPLAYED-OBJECTS C-Moneda y-CodDiv C-Mes y-codope x-cta-ini ~
x-cta-fin x-Clasificacion x-auxiliar FILL-IN-Cco TOGGLE-Sustento ~
RADIO-SET-1 RB-NUMBER-COPIES RB-BEGIN-PAGE RB-END-PAGE 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON b-archivo 
     IMAGE-UP FILE "IMG/pvstop":U
     LABEL "&Archivos.." 
     SIZE 5 BY 1.

DEFINE BUTTON B-cancela AUTO-END-KEY 
     IMAGE-UP FILE "img/b-cancel.bmp":U
     LABEL "&Cancelar" 
     SIZE 11 BY 1.5.

DEFINE BUTTON B-impresoras 
     IMAGE-UP FILE "IMG/pvprint":U
     IMAGE-DOWN FILE "IMG/pvprintd":U
     LABEL "" 
     SIZE 5 BY 1.

DEFINE BUTTON B-imprime AUTO-GO 
     IMAGE-UP FILE "img/b-print.bmp":U
     LABEL "&Imprimir" 
     SIZE 11 BY 1.54.

DEFINE BUTTON BUTTON-1 
     IMAGE-UP FILE "img\tbldat":U
     LABEL "Button 1" 
     SIZE 12 BY 1.5.

DEFINE BUTTON BUTTON-2 
     IMAGE-UP FILE "img/excel.bmp":U
     LABEL "Button 2" 
     SIZE 11 BY 1.5.

DEFINE BUTTON BUTTON-3 
     LABEL "EXCEL PLANO" 
     SIZE 13 BY 1.5.

DEFINE VARIABLE C-Mes AS INTEGER FORMAT "99":U INITIAL 0 
     LABEL "Mes" 
     VIEW-AS COMBO-BOX INNER-LINES 14
     LIST-ITEMS "0","1","2","3","4","5","6","7","8","9","10","11","12","13" 
     DROP-DOWN-LIST
     SIZE 8 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE C-Moneda AS CHARACTER FORMAT "X(256)":U INITIAL "Soles" 
     LABEL "Moneda" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Soles","D�lares" 
     DROP-DOWN-LIST
     SIZE 16 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FILL-IN-Cco AS CHARACTER FORMAT "X(256)":U 
     LABEL "Centro de Costo" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE RB-BEGIN-PAGE AS INTEGER FORMAT "ZZZ9":U INITIAL 1 
     LABEL "P�gina Desde" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .69
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE RB-END-PAGE AS INTEGER FORMAT "ZZZ9":U INITIAL 9999 
     LABEL "P�gina Hasta" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .69
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE RB-NUMBER-COPIES AS INTEGER FORMAT "ZZZ9":U INITIAL 1 
     LABEL "No. Copias" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .69
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE RB-OUTPUT-FILE AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 29 BY .69
     BGCOLOR 15 FGCOLOR 0 FONT 4 NO-UNDO.

DEFINE VARIABLE x-auxiliar AS CHARACTER FORMAT "X(11)":U 
     LABEL "Auxiliar" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE x-Clasificacion AS CHARACTER FORMAT "X(256)":U 
     LABEL "Clasificaci�n" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE x-cta-fin AS CHARACTER FORMAT "X(10)":U 
     LABEL "Hasta la Cuenta" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE x-cta-ini AS CHARACTER FORMAT "X(10)":U 
     LABEL "Desde la Cuenta" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE y-CodDiv AS CHARACTER FORMAT "X(5)":U 
     LABEL "Divisi�n" 
     VIEW-AS FILL-IN 
     SIZE 8.14 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE y-codope AS CHARACTER FORMAT "X(3)":U 
     LABEL "Operaci�n" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .81
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE RADIO-SET-1 AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Pantalla", 2,
"Impresora", 1,
"Archivo", 3
     SIZE 10.72 BY 3 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 63.72 BY 6.19.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 63.72 BY 4.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 63.72 BY 2.

DEFINE VARIABLE TOGGLE-Sustento AS LOGICAL INITIAL no 
     LABEL "Filtrar por Gastos NO Sustentados" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY .77 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     C-Moneda AT ROW 1.19 COL 9 COLON-ALIGNED
     y-CodDiv AT ROW 1.19 COL 41 COLON-ALIGNED
     C-Mes AT ROW 1.96 COL 9 COLON-ALIGNED
     y-codope AT ROW 1.96 COL 41 COLON-ALIGNED
     x-cta-ini AT ROW 2.73 COL 41 COLON-ALIGNED
     x-cta-fin AT ROW 3.5 COL 41 COLON-ALIGNED
     x-Clasificacion AT ROW 4.27 COL 19 COLON-ALIGNED
     x-auxiliar AT ROW 4.27 COL 41 COLON-ALIGNED
     FILL-IN-Cco AT ROW 5.04 COL 41 COLON-ALIGNED
     TOGGLE-Sustento AT ROW 5.85 COL 12 WIDGET-ID 4
     RADIO-SET-1 AT ROW 8.27 COL 2 NO-LABEL
     RB-NUMBER-COPIES AT ROW 8.31 COL 56.43 COLON-ALIGNED
     B-impresoras AT ROW 9.23 COL 12.43
     RB-BEGIN-PAGE AT ROW 9.31 COL 56.43 COLON-ALIGNED
     b-archivo AT ROW 10.23 COL 12.43
     RB-END-PAGE AT ROW 10.31 COL 56.43 COLON-ALIGNED
     RB-OUTPUT-FILE AT ROW 10.5 COL 16.43 COLON-ALIGNED NO-LABEL
     B-imprime AT ROW 11.96 COL 3
     B-cancela AT ROW 11.96 COL 14
     BUTTON-2 AT ROW 11.96 COL 25 WIDGET-ID 2
     BUTTON-3 AT ROW 11.96 COL 36 WIDGET-ID 6
     BUTTON-1 AT ROW 11.96 COL 49
     " Configuraci�n de Impresi�n" VIEW-AS TEXT
          SIZE 63.72 BY .62 AT ROW 7.19 COL 1
          BGCOLOR 1 FGCOLOR 15 
     RECT-6 AT ROW 11.81 COL 1
     RECT-4 AT ROW 1 COL 1
     RECT-5 AT ROW 7.77 COL 1
     SPACE(0.13) SKIP(2.10)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 FONT 4
         TITLE "Libro Mayor General".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
   FRAME-NAME                                                           */
ASSIGN 
       FRAME DIALOG-1:SCROLLABLE       = FALSE.

/* SETTINGS FOR BUTTON b-archivo IN FRAME DIALOG-1
   NO-ENABLE                                                            */
ASSIGN 
       b-archivo:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR BUTTON BUTTON-1 IN FRAME DIALOG-1
   NO-ENABLE                                                            */
ASSIGN 
       BUTTON-1:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN RB-OUTPUT-FILE IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       RB-OUTPUT-FILE:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON GO OF FRAME DIALOG-1 /* Libro Mayor General */
DO:
    APPLY "CHOOSE" TO B-imprime.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME b-archivo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b-archivo DIALOG-1
ON CHOOSE OF b-archivo IN FRAME DIALOG-1 /* Archivos.. */
DO:
     SYSTEM-DIALOG GET-FILE RB-OUTPUT-FILE
        TITLE      "Archivo de Impresi�n ..."
        FILTERS    "Archivos Impresi�n (*.txt)"   "*.txt",
                   "Todos (*.*)"   "*.*"
        INITIAL-DIR "./txt"
        MUST-EXIST
        USE-FILENAME
        UPDATE OKpressed.
      
    IF OKpressed = TRUE THEN
        RB-OUTPUT-FILE:SCREEN-VALUE = RB-OUTPUT-FILE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-impresoras
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-impresoras DIALOG-1
ON CHOOSE OF B-impresoras IN FRAME DIALOG-1
DO:
    SYSTEM-DIALOG PRINTER-SETUP.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-imprime
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-imprime DIALOG-1
ON CHOOSE OF B-imprime IN FRAME DIALOG-1 /* Imprimir */
DO:
        ASSIGN 
            x-cta-ini 
            x-cta-fin
            x-auxiliar    
            c-mes
            y-CodOpe
            y-coddiv
            x-Clasificacion
            FILL-IN-Cco
            TOGGLE-Sustento.
               
        IF x-cta-fin < x-cta-ini 
        THEN DO:
            BELL.
            MESSAGE "La cuenta fin es menor" SKIP
                    "que la cuenta inicio" VIEW-AS ALERT-BOX ERROR.
            APPLY "ENTRY" TO x-cta-ini.
            RETURN NO-APPLY.
        END.

    ASSIGN x-Cta-Fin = SUBSTR( x-Cta-Fin + "9999999999", 1, 10).

    RUN bin/_mes.p ( INPUT c-Mes , 1,  OUTPUT pinta-mes ).
    pinta-mes = "MES DE " + pinta-mes + " DE " + STRING( s-periodo , "9999" ).

    IF x-codmon = 1 THEN DO:
        x-moneda = "NUEVOS SOLES".
        x-expres = "(EXPRESADO EN NUEVOS SOLES)".
    END.
    ELSE DO:
        x-moneda = "DOLARES".
        x-expres = "(EXPRESADO EN DOLARES)".
    END.

    RUN bin/_centrar.p ( INPUT pinta-mes, 40 , OUTPUT pinta-mes).
    RUN bin/_centrar.p ( INPUT x-moneda,  40 , OUTPUT x-moneda ).
    RUN bin/_centrar.p ( INPUT x-expres,  40 , OUTPUT x-expres ).
    Ult-Nivel   = NUM-ENTRIES(cb-niveles).
    Max-Digitos = INTEGER( ENTRY( Ult-Nivel, cb-niveles) ).

    P-largo  = 66.
    P-Ancho  = FRAME f-cab:WIDTH.
    P-Copias = INPUT FRAME DIALOG-1 RB-NUMBER-COPIES.
    P-pagIni = INPUT FRAME DIALOG-1 RB-BEGIN-PAGE.
    P-pagfin = INPUT FRAME DIALOG-1 RB-END-PAGE.
    P-select = INPUT FRAME DIALOG-1 RADIO-SET-1.
    P-archivo= INPUT FRAME DIALOG-1 RB-OUTPUT-FILE.
    P-detalle = "Impresora Local (EPSON)".
    P-name  = "Epson E/F/J/RX/LQ".
    P-device  = "PRN".


    IF P-select = 2 
    THEN P-archivo = SESSION:TEMP-DIRECTORY + 
          STRING(NEXT-VALUE(sec-arc,integral)) + ".scr".
    ELSE RUN setup-print.      
    IF P-select <> 1 
    THEN P-copias = 1.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 DIALOG-1
ON CHOOSE OF BUTTON-1 IN FRAME DIALOG-1 /* Button 1 */
DO:
        ASSIGN 
            x-cta-ini 
            x-cta-fin
            x-auxiliar    
            c-mes
            y-CodOpe
            y-coddiv
            x-Clasificacion
            FILL-IN-Cco
            TOGGLE-Sustento.
               
        IF x-cta-fin < x-cta-ini 
        THEN DO:
            BELL.
            MESSAGE "La cuenta fin es menor" SKIP
                    "que la cuenta inicio" VIEW-AS ALERT-BOX ERROR.
            APPLY "ENTRY" TO x-cta-ini.
            RETURN NO-APPLY.
        END.

    ASSIGN x-Cta-Fin = SUBSTR( x-Cta-Fin + "9999999999", 1, 10).

    RUN bin/_mes.p ( INPUT c-Mes , 1,  OUTPUT pinta-mes ).
    pinta-mes = "MES DE " + pinta-mes + " DE " + STRING( s-periodo , "9999" ).

    IF x-codmon = 1 THEN DO:
        x-moneda = "NUEVOS SOLES".
        x-expres = "(EXPRESADO EN NUEVOS SOLES)".
    END.
    ELSE DO:
        x-moneda = "DOLARES".
        x-expres = "(EXPRESADO EN DOLARES)".
    END.

    RUN bin/_centrar.p ( INPUT pinta-mes, 40 , OUTPUT pinta-mes).
    RUN bin/_centrar.p ( INPUT x-moneda,  40 , OUTPUT x-moneda ).
    RUN bin/_centrar.p ( INPUT x-expres,  40 , OUTPUT x-expres ).
    Ult-Nivel   = NUM-ENTRIES(cb-niveles).
    Max-Digitos = INTEGER( ENTRY( Ult-Nivel, cb-niveles) ).

  RUN Texto.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 DIALOG-1
ON CHOOSE OF BUTTON-2 IN FRAME DIALOG-1 /* Button 2 */
DO:
    ASSIGN 
        x-cta-ini 
        x-cta-fin
        x-auxiliar    
        c-mes
        y-CodOpe
        y-coddiv
        x-Clasificacion
        FILL-IN-Cco
        TOGGLE-Sustento.

    IF x-cta-fin < x-cta-ini 
    THEN DO:
        BELL.
        MESSAGE "La cuenta fin es menor" SKIP
                "que la cuenta inicio" VIEW-AS ALERT-BOX ERROR.
        APPLY "ENTRY" TO x-cta-ini.
        RETURN NO-APPLY.
    END.

    ASSIGN x-Cta-Fin = SUBSTR( x-Cta-Fin + "9999999999", 1, 10).

    RUN bin/_mes.p ( INPUT c-Mes , 1,  OUTPUT pinta-mes ).
    pinta-mes = "MES DE " + pinta-mes + " DE " + STRING( s-periodo , "9999" ).

    IF x-codmon = 1 THEN DO:
        x-moneda = "NUEVOS SOLES".
        x-expres = "(EXPRESADO EN NUEVOS SOLES)".
    END.
    ELSE DO:
        x-moneda = "DOLARES".
        x-expres = "(EXPRESADO EN DOLARES)".
    END.

    RUN bin/_centrar.p ( INPUT pinta-mes, 40 , OUTPUT pinta-mes).
    RUN bin/_centrar.p ( INPUT x-moneda,  40 , OUTPUT x-moneda ).
    RUN bin/_centrar.p ( INPUT x-expres,  40 , OUTPUT x-expres ).
    Ult-Nivel   = NUM-ENTRIES(cb-niveles).
    Max-Digitos = INTEGER( ENTRY( Ult-Nivel, cb-niveles) ).

    RUN Excel.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-3 DIALOG-1
ON CHOOSE OF BUTTON-3 IN FRAME DIALOG-1 /* EXCEL PLANO */
DO:
    ASSIGN 
        x-cta-ini 
        x-cta-fin
        x-auxiliar    
        c-mes
        y-CodOpe
        y-coddiv
        x-Clasificacion
        FILL-IN-Cco
        TOGGLE-Sustento.

    IF x-cta-fin < x-cta-ini 
    THEN DO:
        BELL.
        MESSAGE "La cuenta fin es menor" SKIP
                "que la cuenta inicio" VIEW-AS ALERT-BOX ERROR.
        APPLY "ENTRY" TO x-cta-ini.
        RETURN NO-APPLY.
    END.

    ASSIGN x-Cta-Fin = SUBSTR( x-Cta-Fin + "9999999999", 1, 10).

    RUN bin/_mes.p ( INPUT c-Mes , 1,  OUTPUT pinta-mes ).
    pinta-mes = "MES DE " + pinta-mes + " DE " + STRING( s-periodo , "9999" ).

    IF x-codmon = 1 THEN DO:
        x-moneda = "NUEVOS SOLES".
        x-expres = "(EXPRESADO EN NUEVOS SOLES)".
    END.
    ELSE DO:
        x-moneda = "DOLARES".
        x-expres = "(EXPRESADO EN DOLARES)".
    END.

    RUN bin/_centrar.p ( INPUT pinta-mes, 40 , OUTPUT pinta-mes).
    RUN bin/_centrar.p ( INPUT x-moneda,  40 , OUTPUT x-moneda ).
    RUN bin/_centrar.p ( INPUT x-expres,  40 , OUTPUT x-expres ).
    Ult-Nivel   = NUM-ENTRIES(cb-niveles).
    Max-Digitos = INTEGER( ENTRY( Ult-Nivel, cb-niveles) ).

    RUN Excel-Plano.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME C-Moneda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Moneda DIALOG-1
ON VALUE-CHANGED OF C-Moneda IN FRAME DIALOG-1 /* Moneda */
DO:
  
  x-codmon = lookup (self:screen-value,c-moneda:list-items).
  
                                   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-Cco
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-Cco DIALOG-1
ON LEFT-MOUSE-DBLCLICK OF FILL-IN-Cco IN FRAME DIALOG-1 /* Centro de Costo */
OR F8 OF FILL-IN-Cco
DO:
  ASSIGN
    input-var-1 = 'CCO'
    input-var-2 = ''
    input-var-3 = ''
    output-var-1 = ?
    output-var-2 = ''
    output-var-3 = ''.
  RUN lkup/c-auxil ('Centro de Costos').
  IF output-var-1 <> ?
  THEN DO:
    FIND cb-auxi WHERE ROWID(cb-auxi) = output-var-1 NO-LOCK NO-ERROR.
    IF AVAILABLE cb-auxi
    THEN DO:
        SELF:SCREEN-VALUE = cb-auxi.codaux.
        RETURN NO-APPLY.
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RADIO-SET-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RADIO-SET-1 DIALOG-1
ON VALUE-CHANGED OF RADIO-SET-1 IN FRAME DIALOG-1
DO:
    IF SELF:SCREEN-VALUE = "3"
    THEN ASSIGN b-archivo:VISIBLE = YES
                RB-OUTPUT-FILE:VISIBLE = YES
                b-archivo:SENSITIVE = YES
                RB-OUTPUT-FILE:SENSITIVE = YES.
    ELSE ASSIGN b-archivo:VISIBLE = NO
                RB-OUTPUT-FILE:VISIBLE = NO
                b-archivo:SENSITIVE = NO
                RB-OUTPUT-FILE:SENSITIVE = NO.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME x-auxiliar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL x-auxiliar DIALOG-1
ON LEFT-MOUSE-DBLCLICK OF x-auxiliar IN FRAME DIALOG-1 /* Auxiliar */
OR F8 OF x-auxiliar DO:

x-clfaux = "".
assign
  x-clfaux-1 = ""
  x-clfaux-2 = "".
  
find cb-ctas where cb-ctas.codcia = cb-codcia and
                   cb-ctas.codcta = x-cta-ini:screen-value
                   no-lock no-error.
                   
if avail cb-ctas then  x-clfaux-1 = cb-ctas.clfaux.
                      
find cb-ctas where cb-ctas.codcia = cb-codcia and
                   cb-ctas.codcta = x-cta-fin:screen-value
                   no-lock no-error.
if avail cb-ctas then  x-clfaux-2 = cb-ctas.clfaux.

if x-clfaux-1 = x-clfaux-2 then x-clfaux = x-clfaux-1.

if x-clfaux-1 <> "" and  x-clfaux-2  = "" then return no-apply.
if x-clfaux-1 =  "" and  x-clfaux-2 <> "" then return no-apply.


if x-clfaux = "" then return.
  

  
    DEF VAR T-ROWID AS ROWID.
    DEF VAR T-RECID AS RECID.  

    
    CASE X-CLFAUX :
    WHEN "@PV"  THEN DO:
        RUN ADM/H-PROV01.W(s-codcia , OUTPUT T-ROWID).
        IF T-ROWID <> ?
        THEN DO:
            FIND gn-prov WHERE ROWID(gn-prov) = T-ROWID NO-LOCK NO-ERROR.
            IF AVAILABLE gn-prov
            THEN  SELF:SCREEN-VALUE  = gn-prov.CodPro.
        END.
    END.
    WHEN "@CL" THEN DO:
        RUN ADM/H-CLIE01.W(s-codcia, OUTPUT T-ROWID).    
        IF T-ROWID <> ?
        THEN DO:
            FIND gn-clie WHERE ROWID(gn-clie) = T-ROWID NO-LOCK NO-ERROR.
            IF AVAIL gn-clie
            THEN SELF:SCREEN-VALUE  = gn-clie.codcli.
        END.
    END.
    WHEN "@CT" THEN DO:
            RUN cbd/q-ctas2.w(cb-codcia, "9", OUTPUT T-RECID).
            IF T-RECID <> ?
            THEN DO:
                find cb-ctas WHERE RECID(cb-ctas) = T-RECID NO-LOCK  NO-ERROR.
                IF avail cb-ctas
                THEN  self:screen-value = cb-ctas.CodCta.
            END.
  
    END.
    OTHERWISE DO:
        RUN CBD/H-AUXI01(s-codcia, X-ClfAux , OUTPUT T-ROWID ).     
        IF T-ROWID <> ?
        THEN DO:
            FIND cb-auxi WHERE ROWID(cb-auxi) = T-ROWID NO-LOCK  NO-ERROR.
            IF AVAIL cb-auxi
            THEN self:screen-value = cb-auxi.CodAux.
        END.
    END.
    END CASE.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME x-Clasificacion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL x-Clasificacion DIALOG-1
ON MOUSE-SELECT-DBLCLICK OF x-Clasificacion IN FRAME DIALOG-1 /* Clasificaci�n */
OR "F8" OF x-Clasificacion
DO:
   DEFINE VAR RECID-STACK AS RECID NO-UNDO.
   RUN cbd/q-clfaux.w("01", OUTPUT RECID-stack).
   IF RECID-stack <> 0 THEN DO:
      FIND cb-tabl WHERE RECID( cb-tabl ) = RECID-stack NO-LOCK  NO-ERROR.
      IF AVAIL cb-tabl THEN x-Clasificacion:SCREEN-VALUE = cb-tabl.codigo.
      ELSE MESSAGE "No existen Registros." VIEW-AS ALERT-BOX ERROR BUTTONS OK.
   END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME x-cta-fin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL x-cta-fin DIALOG-1
ON F8 OF x-cta-fin IN FRAME DIALOG-1 /* Hasta la Cuenta */
OR "MOUSE-SELECT-DBLCLICK":U OF X-CTA-FIN DO:
   {ADM/H-CTAS01.I NO SELF}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME x-cta-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL x-cta-ini DIALOG-1
ON F8 OF x-cta-ini IN FRAME DIALOG-1 /* Desde la Cuenta */
OR "MOUSE-SELECT-DBLCLICK":U OF X-CTA-INI DO: 
   {ADM/H-CTAS01.I NO SELF}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME y-CodDiv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL y-CodDiv DIALOG-1
ON F8 OF y-CodDiv IN FRAME DIALOG-1 /* Divisi�n */
DO: 
   {CBD/H-DIVI01.I NO SELF}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL y-CodDiv DIALOG-1
ON LEFT-MOUSE-DBLCLICK OF y-CodDiv IN FRAME DIALOG-1 /* Divisi�n */
DO:
   {CBD/H-DIVI01.I NO SELF}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME y-codope
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL y-codope DIALOG-1
ON F8 OF y-codope IN FRAME DIALOG-1 /* Operaci�n */
DO:
   DEF VAR X AS RECID.
   RUN cbd/q-oper(cb-codcia,output X).
   FIND cb-oper WHERE RECID(cb-oper) = X
        NO-LOCK NO-ERROR.
   IF AVAILABLE cb-oper THEN DO:
        ASSIGN {&SELF-NAME} = cb-oper.CodOpe.
        DISPLAY {&SELF-NAME} WITH FRAME {&FRAME-NAME}.
   END.     

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL y-codope DIALOG-1
ON LEFT-MOUSE-DBLCLICK OF y-codope IN FRAME DIALOG-1 /* Operaci�n */
DO:
   DEF VAR X AS RECID.
   RUN cbd/q-oper(cb-codcia,output X).
   FIND cb-oper WHERE RECID(cb-oper) = X
        NO-LOCK NO-ERROR.
   IF AVAILABLE cb-oper THEN DO:
        ASSIGN {&SELF-NAME} = cb-oper.CodOpe.
        DISPLAY {&SELF-NAME} WITH FRAME {&FRAME-NAME}.
   END.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK DIALOG-1 


/* ***************************  Main Block  *************************** */
/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

/* Add Trigger to equate WINDOW-CLOSE to END-ERROR                      */
ON WINDOW-CLOSE OF FRAME {&FRAME-NAME} APPLY "END-ERROR":U TO SELF.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */

ASSIGN c-mes = s-nromes.

MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  PTO                  = SESSION:SET-WAIT-STATE("").    
  l-immediate-display  = SESSION:IMMEDIATE-DISPLAY.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
  RUN disable_UI.

  FRAME F-Mensaje:TITLE =  FRAME DIALOG-1:TITLE.
  SESSION:IMMEDIATE-DISPLAY = YES.
  STATUS INPUT OFF.  
  DO c-Copias = 1 to P-copias ON ERROR UNDO, LEAVE ON STOP UNDO, LEAVE:
      OUTPUT STREAM report TO NUL PAGED PAGE-SIZE 1000.
      c-Pagina = 0.
      RUN IMPRIMIR NO-ERROR.
      OUTPUT STREAM report CLOSE.        
  END.
  OUTPUT STREAM report CLOSE.        
  SESSION:IMMEDIATE-DISPLAY = l-immediate-display.
  IF NOT LASTKEY = KEYCODE("ESC") AND P-select = 2 THEN DO: 
        RUN bin/_vcat.p ( P-archivo ). 
  END.    
  HIDE FRAME F-Mensaje.  
  RETURN.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI DIALOG-1  _DEFAULT-DISABLE
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
  HIDE FRAME DIALOG-1.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI DIALOG-1  _DEFAULT-ENABLE
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
  DISPLAY C-Moneda y-CodDiv C-Mes y-codope x-cta-ini x-cta-fin x-Clasificacion 
          x-auxiliar FILL-IN-Cco TOGGLE-Sustento RADIO-SET-1 RB-NUMBER-COPIES 
          RB-BEGIN-PAGE RB-END-PAGE 
      WITH FRAME DIALOG-1.
  ENABLE RECT-6 RECT-4 RECT-5 C-Moneda y-CodDiv C-Mes y-codope x-cta-ini 
         x-cta-fin x-Clasificacion x-auxiliar FILL-IN-Cco TOGGLE-Sustento 
         RADIO-SET-1 RB-NUMBER-COPIES B-impresoras RB-BEGIN-PAGE RB-END-PAGE 
         B-imprime B-cancela BUTTON-2 BUTTON-3 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Excel DIALOG-1 
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

    /* Encabezado */
    iCount = iCount + 1.
    cColumn = STRING(iCount).
    cRange = "D" + cColumn.
    chWorkSheet:Range(cRange):Value = "L I B R O   M A Y O R    G E N E R A L".
    iCount = iCount + 1.
    cColumn = STRING(iCount).
    cRange = "D" + cColumn.
    chWorkSheet:Range(cRange):Value = pinta-mes.
    iCount = iCount + 1.
    cColumn = STRING(iCount).
    cRange = "D" + cColumn.
    chWorkSheet:Range(cRange):Value = x-expres.
    iCount = iCount + 2.
    /* set the column names for the Worksheet */
    iCount = iCount + 1.
    cColumn = STRING(iCount).
    cRange = "A" + cColumn.
    chWorkSheet:Range(cRange):Value = "Division".
    cRange = "B" + cColumn.
    chWorkSheet:Range(cRange):Value = "C. Costo".
    cRange = "C" + cColumn.
    chWorkSheet:Range(cRange):Value = "Fecha Asiento".
    cRange = "D" + cColumn.
    chWorkSheet:Range(cRange):Value = "Comprobante".
    cRange = "E" + cColumn.
    chWorkSheet:Range(cRange):Value = "Libro".
    cRange = "F" + cColumn.
    chWorkSheet:Range(cRange):Value = "Clf. Aux.".
    cRange = "G" + cColumn.
    chWorkSheet:Range(cRange):Value = "Cod. Aux.".
    cRange = "H" + cColumn.
    chWorkSheet:Range(cRange):Value = "Fecha Doc.".
    cRange = "I" + cColumn.
    chWorkSheet:Range(cRange):Value = "Cod. Doc.".
    cRange = "J" + cColumn.
    chWorkSheet:Range(cRange):Value = "Nro. Doc.".
    cRange = "K" + cColumn.
    chWorkSheet:Range(cRange):Value = "Nro. Refer.".
    cRange = "L" + cColumn.
    chWorkSheet:Range(cRange):Value = "Detalle".
    cRange = "M" + cColumn.
    chWorkSheet:Range(cRange):Value = "Cargos".
    cRange = "N" + cColumn.
    chWorkSheet:Range(cRange):Value = "Abonos".
    cRange = "O" + cColumn.
    chWorkSheet:Range(cRange):Value = "Importe S/.".
    cRange = "P" + cColumn.
    chWorkSheet:Range(cRange):Value = "Saldo Final".
    iCount = iCount + 1.

    DEF VAR con-ctas AS INTEGER.
    DEF VAR xi AS INTEGER INIT 0.
    DEF VAR No-tiene-mov AS LOGICAL.

    ASSIGN t-debe   = 0
           t-haber  = 0
           t-saldoi = 0
           t-saldof = 0
           con-ctas = 0 .

    DISPLAY "Seleccionando informaci�n solicitada" @ x-mensaje with frame f-auxiliar.
    PAUSE 0.

    FOR EACH cb-ctas NO-LOCK WHERE cb-ctas.codcia = cb-codcia
        AND cb-ctas.codcta >= x-cta-ini
        AND cb-ctas.codcta <= x-cta-fin
        AND cb-ctas.CodCta <> ""
        AND cb-ctas.ClfAux BEGINS x-Clasificacion
        AND LENGTH( cb-ctas.CodCta ) = Max-Digitos
        AND (TOGGLE-Sustento = NO OR Cb-Ctas.Sustento = YES)
        BY Cb-ctas.codcta:
/*         BREAK BY (cb-ctas.Codcta) */
/*         ON ERROR UNDO, LEAVE:     */

        x-CodCta = cb-ctas.CodCta.
        ASSIGN c-debe   = 0
               c-haber  = 0
               x-saldoi = 0 
               xi       = 0 
               No-tiene-mov = yes.

        IF y-codope   = "" AND y-coddiv   = "" AND x-auxiliar = "" 
        THEN /* SALDO INICIAL */
            FOR EACH cb-dmov NO-LOCK WHERE cb-dmov.codcia  = s-codcia
                                       AND cb-dmov.periodo = s-periodo 
                                       AND cb-dmov.nromes  < c-mes
                                       AND cb-dmov.codcta  = x-CodCta :
                 CASE x-codmon :
                      when 1 then if (not cb-dmov.tpomov) 
                                  then x-saldoi = x-saldoi + cb-dmov.impmn1.
                                  else x-saldoi = x-saldoi - cb-dmov.impmn1.     
                      when 2 then if (not cb-dmov.tpomov) 
                                  then x-saldoi = x-saldoi + cb-dmov.impmn2.
                                  else x-saldoi = x-saldoi - cb-dmov.impmn2.     
                 END case.

            END.
        t-saldoi = t-saldoi + x-saldoi.
        ASSIGN x-nroast = x-codcta
               x-GloDoc = ""
               x-fchast = ""
               x-codope = ""
               x-fchDoc = ""
               x-nroDoc = ""
               x-fchVto = ""
               x-nroref = "".
        x-GloDoc = "S a l d o  I n i c i a l " .
        IF x-saldoi <> 0 THEN DO:
            xi = xi + 1.
            G-NOMCTA = cb-ctas.codcta + " " + cb-ctas.nomcta.
            cColumn = STRING(iCount).
            cRange = "A" + cColumn.
            chWorkSheet:Range(cRange):Value = G-NOMCTA.
            iCount = iCount + 1.
            cColumn = STRING(iCount).
            cRange = "C" + cColumn.
            chWorkSheet:Range(cRange):Value = x-fchast.
            cRange = "D" + cColumn.
            chWorkSheet:Range(cRange):Value = x-nroast.
            cRange = "E" + cColumn.
            chWorkSheet:Range(cRange):Value = x-codope.
            cRange = "H" + cColumn.
            chWorkSheet:Range(cRange):Value = x-fchdoc.
            cRange = "J" + cColumn.
            chWorkSheet:Range(cRange):Value = x-nrodoc.
            cRange = "K" + cColumn.
            chWorkSheet:Range(cRange):Value = x-nroref.
            cRange = "L" + cColumn.
            chWorkSheet:Range(cRange):Value = x-glodoc.
            cRange = "M" + cColumn.
            chWorkSheet:Range(cRange):Value = x-saldoi.
            iCount = iCount + 1.
        END.
        FOR EACH cb-dmov NO-LOCK WHERE cb-dmov.codcia  = s-codcia
            AND cb-dmov.periodo = s-periodo 
            AND cb-dmov.nromes  = c-mes
            AND cb-dmov.codope BEGINS (y-codope)
            AND cb-dmov.codcta  = x-CodCta
            AND cb-dmov.clfaux  begins x-Clasificacion
            AND cb-dmov.codaux  begins x-auxiliar
            AND cb-dmov.coddiv  begins y-coddiv
            AND cb-dmov.cco     BEGINS FILL-IN-Cco
            AND (TOGGLE-Sustento = NO OR cb-dmov.LOG_01 = NO)
            BREAK BY cb-dmov.CodOpe BY cb-dmov.NroAst BY cb-dmov.NroItm:

            No-tiene-mov = no.

            IF xi = 0 THEN DO:
               xi = xi + 1.
               G-NOMCTA = cb-ctas.codcta + " " + cb-ctas.nomcta.
               cColumn = STRING(iCount).
               cRange = "A" + cColumn.
               chWorkSheet:Range(cRange):Value = G-NOMCTA.
               iCount = iCount + 1.
            END.    

            /* Buscando la cabecera correspondiente */
            x-fchast = ?.
            FIND cb-cmov WHERE cb-cmov.codcia  = cb-dmov.codcia
                           AND cb-cmov.periodo = cb-dmov.periodo 
                           AND cb-cmov.nromes  = cb-dmov.nromes
                           AND cb-cmov.codope  = cb-dmov.codope
                           AND cb-cmov.nroast  = cb-dmov.nroast
                           NO-LOCK NO-ERROR.
             IF AVAILABLE cb-cmov THEN x-fchast = STRING(cb-cmov.fchast).
             ASSIGN x-glodoc = cb-dmov.glodoc
                    x-NroAst = cb-dmov.NroAst
                    x-CodOpe = cb-dmov.CodOpe
                    x-codaux = cb-dmov.codaux
                    x-NroDoc = cb-dmov.NroDoc
                    x-NroRef = cb-dmov.NroRef
                    x-coddiv = cb-dmov.CodDiv
                    x-clfaux = cb-dmov.clfaux
                    x-coddoc = cb-dmov.coddoc
                    x-cco    = cb-dmov.cco.

             IF x-glodoc = "" THEN IF AVAILABLE cb-cmov THEN x-glodoc = cb-cmov.notast.
             IF  x-glodoc = "" THEN DO:
                CASE cb-dmov.clfaux:
                    WHEN "@CL" THEN DO:
                        FIND gn-clie WHERE gn-clie.codcli = cb-dmov.codaux
                                        AND gn-clie.CodCia = cl-codcia
                                    NO-LOCK NO-ERROR. 
                        IF AVAILABLE gn-clie THEN
                            x-glodoc = gn-clie.nomcli.
                    END.
                    WHEN "@PV" THEN DO:
                        FIND gn-prov WHERE gn-prov.codpro = cb-dmov.codaux
                                        AND gn-prov.CodCia = pv-codcia
                                    NO-LOCK NO-ERROR.                      
                        IF AVAILABLE gn-prov THEN 
                            x-glodoc = gn-prov.nompro.
                    END.
                    WHEN "@CT" THEN DO:
                        find b-cuentas WHERE b-cuentas.codcta = cb-dmov.codaux
                                        AND b-cuentas.CodCia = cb-codcia
                                    NO-LOCK NO-ERROR.                      
                        IF AVAILABLE b-cuentas THEN x-glodoc = b-cuentas.nomcta.
                    END.
                    OTHERWISE DO:
                        FIND cb-auxi WHERE cb-auxi.clfaux = cb-dmov.clfaux
                                        AND cb-auxi.codaux = cb-dmov.codaux
                                        AND cb-auxi.CodCia = cb-codcia
                                    NO-LOCK NO-ERROR.                      
                        IF AVAILABLE cb-auxi THEN 
                            x-glodoc = cb-auxi.nomaux.
                    END.
                END CASE.
            END.
            IF NOT tpomov THEN DO:
                x-importe = ImpMn1.
                CASE x-codmon:
                    WHEN 1 THEN DO:
                        x-debe  = ImpMn1.
                        x-haber = 0.
                    END.
                    WHEN 2 THEN DO:
                        x-debe  = ImpMn2.
                        x-haber = 0.
                    END.
                END CASE.
            END.
            ELSE DO:
                x-importe = ImpMn1 * -1. 
                CASE x-codmon:
                    WHEN 1 THEN DO:
                        x-debe  = 0.
                        x-haber = ImpMn1.
                    END.
                    WHEN 2 THEN DO:
                        x-debe  = 0.
                        x-haber = ImpMn2.
                    END.
                END CASE.            
            END.
            IF NOT (x-haber = 0 AND x-debe = 0) AND x-debe <> ? AND x-haber <> ? THEN DO:
                x-fchdoc = STRING(cb-dmov.fchdoc).  
                x-fchvto = STRING(cb-dmov.fchvto).
                t-Debe  = t-Debe  + x-Debe.
                t-Haber = t-Haber + x-Haber.
                c-Debe  = c-Debe  + x-Debe.
                c-Haber = c-Haber + x-Haber.
                cColumn = STRING(iCount).
                cRange = "A" + cColumn.
                chWorkSheet:Range(cRange):Value = x-coddiv.
                cRange = "B" + cColumn.
                chWorkSheet:Range(cRange):Value = x-cco.
                cRange = "C" + cColumn.
                chWorkSheet:Range(cRange):Value = (IF x-fchast <> ? THEN x-fchast ELSE "").
                cRange = "D" + cColumn.
                chWorkSheet:Range(cRange):Value = x-nroast.
                cRange = "E" + cColumn.
                chWorkSheet:Range(cRange):Value = x-codope.
                cRange = "H" + cColumn.
                chWorkSheet:Range(cRange):Value = (IF x-fchdoc <> ? THEN x-fchdoc ELSE "").
                cRange = "I" + cColumn.
                chWorkSheet:Range(cRange):Value = x-coddoc.
                cRange = "J" + cColumn.
                chWorkSheet:Range(cRange):Value = x-nrodoc.
                cRange = "K" + cColumn.
                chWorkSheet:Range(cRange):Value = x-nroref.
                cRange = "L" + cColumn.
                chWorkSheet:Range(cRange):Value = x-glodoc.
                IF x-debe <> 0 THEN DO:
                    cRange = "M" + cColumn.
                    chWorkSheet:Range(cRange):Value = x-debe.
                END.
                IF x-haber <> 0 THEN DO:
                    cRange = "N" + cColumn.
                    chWorkSheet:Range(cRange):Value = x-haber.
                END.
                IF x-importe <> 0 AND x-codmon = 2 THEN DO:
                    cRange = "O" + cColumn.
                    chWorkSheet:Range(cRange):Value = x-importe.
                END.
                iCount = iCount + 1.                                                                                                         
                x-conreg = x-conreg + 1.
            END.            
        END.

        IF no-tiene-mov THEN NEXT.

        ASSIGN x-Debe   = c-Debe
               x-Haber  = c-Haber
               x-SaldoF = x-SaldoI + x-Debe - x-Haber
               x-nroast = x-codcta
               x-fchast = ""
               x-codope = ""
               x-fchDoc = ""
               x-nroDoc = "TOTAL"
               x-nroref = x-CodCta
               x-GloDoc = cb-ctas.NomCta.
        iCount = iCount + 1.
        cColumn = STRING(iCount).
        cRange = "K" + cColumn.
        chWorkSheet:Range(cRange):Value = x-nroref.
        cRange = "L" + cColumn.
        chWorkSheet:Range(cRange):Value = x-glodoc.
        cRange = "M" + cColumn.
        chWorkSheet:Range(cRange):Value = x-debe.
        cRange = "N" + cColumn.
        chWorkSheet:Range(cRange):Value = x-haber.
        cRange = "P" + cColumn.
        chWorkSheet:Range(cRange):Value = x-saldof.
        iCount = iCount + 1.
        con-ctas = con-ctas + 1 .
    END.
    HIDE FRAME f-auxiliar.
    ASSIGN x-saldoi = t-saldoi
           x-Debe   = t-Debe
           x-Haber  = t-Haber
           x-SaldoF = t-SaldoI + x-Debe - x-Haber
           x-nroast = x-codcta
           x-fchast = ""
           x-codope = ""
           x-fchDoc = ""
           x-nroDoc = ""
           x-fchVto = ""
           x-nroref = ""
           x-GloDoc = "T O T A L   G E N E R A L ".

    iCount = iCount + 1.
    cColumn = STRING(iCount).
    cRange = "K" + cColumn.
    chWorkSheet:Range(cRange):Value = x-nroref.
    cRange = "L" + cColumn.
    chWorkSheet:Range(cRange):Value = x-glodoc.
    cRange = "M" + cColumn.
    chWorkSheet:Range(cRange):Value = x-debe.
    cRange = "N" + cColumn.
    chWorkSheet:Range(cRange):Value = x-haber.
    cRange = "P" + cColumn.
    chWorkSheet:Range(cRange):Value = x-saldof.
    iCount = iCount + 1.

    /* launch Excel so it is visible to the user */
    chExcelApplication:Visible = TRUE.

    /* release com-handles */
    RELEASE OBJECT chExcelApplication.      
    RELEASE OBJECT chWorkbook.
    RELEASE OBJECT chWorksheet.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Excel-Plano DIALOG-1 
PROCEDURE Excel-Plano :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR x-FchAst AS DATE NO-UNDO.
DEF VAR x-FchDoc AS DATE NO-UNDO.

    DEFINE VARIABLE chExcelApplication AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE chWorkbook         AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE chWorksheet        AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE iCount             AS INTEGER INITIAL 0 NO-UNDO.
    DEFINE VARIABLE cColumn            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cRange             AS CHARACTER NO-UNDO.

    /* create a new Excel Application object */
    CREATE "Excel.Application" chExcelApplication.

    /* create a new Workbook */
    chWorkbook = chExcelApplication:Workbooks:Add().

    /* get the active Worksheet */
    chWorkSheet = chExcelApplication:Sheets:Item(1).

    /* Encabezado */
    iCount = iCount + 1.
    cColumn = STRING(iCount).
    cRange = "D" + cColumn.
    chWorkSheet:Range(cRange):Value = "L I B R O   M A Y O R    G E N E R A L".
    iCount = iCount + 1.
    cColumn = STRING(iCount).
    cRange = "D" + cColumn.
    chWorkSheet:Range(cRange):Value = pinta-mes.
    iCount = iCount + 1.
    cColumn = STRING(iCount).
    cRange = "D" + cColumn.
    chWorkSheet:Range(cRange):Value = x-expres.
    /* set the column names for the Worksheet */
    iCount = iCount + 1.
    cColumn = STRING(iCount).
    cRange = "A" + cColumn.
    chWorkSheet:Range(cRange):Value = "Cuenta".
    chWorkSheet:COLUMNS("A"):NumberFormat = "@".
    cRange = "B" + cColumn.
    chWorkSheet:Range(cRange):Value = "Division".
    chWorkSheet:COLUMNS("B"):NumberFormat = "@".
    cRange = "C" + cColumn.
    chWorkSheet:Range(cRange):Value = "C. Costo".
    chWorkSheet:COLUMNS("C"):NumberFormat = "@".
    cRange = "D" + cColumn.
    chWorkSheet:Range(cRange):Value = "Fecha Asiento".
    chWorkSheet:COLUMNS("D"):NumberFormat = "dd/mm/yyyy".
    cRange = "E" + cColumn.
    chWorkSheet:Range(cRange):Value = "Comprobante".
    chWorkSheet:COLUMNS("E"):NumberFormat = "@".
    cRange = "F" + cColumn.
    chWorkSheet:Range(cRange):Value = "Libro".
    chWorkSheet:COLUMNS("F"):NumberFormat = "@".
    cRange = "G" + cColumn.
    chWorkSheet:Range(cRange):Value = "Clf. Aux.".
    cRange = "H" + cColumn.
    chWorkSheet:Range(cRange):Value = "Cod. Aux.".
    chWorkSheet:COLUMNS("H"):NumberFormat = "@".
    cRange = "I" + cColumn.
    chWorkSheet:Range(cRange):Value = "Fecha Doc.".
    chWorkSheet:COLUMNS("I"):NumberFormat = "dd/mm/yyyy".
    cRange = "J" + cColumn.
    chWorkSheet:Range(cRange):Value = "Cod. Doc.".
    chWorkSheet:COLUMNS("J"):NumberFormat = "@".
    cRange = "K" + cColumn.
    chWorkSheet:Range(cRange):Value = "Nro. Doc.".
    chWorkSheet:COLUMNS("K"):NumberFormat = "@".
    cRange = "L" + cColumn.
    chWorkSheet:Range(cRange):Value = "Nro. Refer.".
    chWorkSheet:COLUMNS("L"):NumberFormat = "@".
    cRange = "M" + cColumn.
    chWorkSheet:Range(cRange):Value = "Detalle".
    cRange = "N" + cColumn.
    chWorkSheet:Range(cRange):Value = "Cargos".
    cRange = "O" + cColumn.
    chWorkSheet:Range(cRange):Value = "Abonos".
    cRange = "P" + cColumn.
    chWorkSheet:Range(cRange):Value = "Importe S/.".
    cRange = "Q" + cColumn.
    chWorkSheet:Range(cRange):Value = "Saldo Final".
    iCount = iCount + 1.

    DEF VAR con-ctas AS INTEGER.
    DEF VAR xi AS INTEGER INIT 0.
    DEF VAR No-tiene-mov AS LOGICAL.

    ASSIGN t-debe   = 0
           t-haber  = 0
           t-saldoi = 0
           t-saldof = 0
           con-ctas = 0 .

    DISPLAY "Seleccionando informaci�n solicitada" @ x-mensaje with frame f-auxiliar.
    PAUSE 0.

    FOR EACH cb-ctas NO-LOCK WHERE cb-ctas.codcia = cb-codcia
        AND cb-ctas.codcta >= x-cta-ini
        AND cb-ctas.codcta <= x-cta-fin
        AND cb-ctas.CodCta <> ""
        AND (x-Clasificacion = '' OR cb-ctas.ClfAux BEGINS x-Clasificacion)
        AND LENGTH( cb-ctas.CodCta ) = Max-Digitos
        AND (TOGGLE-Sustento = NO OR Cb-Ctas.Sustento = YES)
        BY Cb-ctas.codcta:
        x-CodCta = cb-ctas.CodCta.
        G-NOMCTA = cb-ctas.codcta + " " + cb-ctas.nomcta.
        ASSIGN 
            c-debe   = 0
            c-haber  = 0
            x-saldoi = 0 
            xi       = 0 
            No-tiene-mov = yes.
        /* SALDO INICIAL */
        IF y-codope   = "" AND y-coddiv   = "" AND x-auxiliar = "" THEN
            FOR EACH cb-dmov NO-LOCK WHERE cb-dmov.codcia  = s-codcia
                                       AND cb-dmov.periodo = s-periodo 
                                       AND cb-dmov.nromes  < c-mes
                                       AND cb-dmov.codcta  = x-CodCta :
                 CASE x-codmon :
                      when 1 then if (not cb-dmov.tpomov) 
                                  then x-saldoi = x-saldoi + cb-dmov.impmn1.
                                  else x-saldoi = x-saldoi - cb-dmov.impmn1.     
                      when 2 then if (not cb-dmov.tpomov) 
                                  then x-saldoi = x-saldoi + cb-dmov.impmn2.
                                  else x-saldoi = x-saldoi - cb-dmov.impmn2.     
                 END case.

            END.
        t-saldoi = t-saldoi + x-saldoi.
        ASSIGN x-nroast = ""
               x-GloDoc = ""
               x-fchast = ?
               x-codope = ""
               x-fchDoc = ?
               x-nroDoc = ""
               x-fchVto = ""
               x-nroref = "".
        x-GloDoc = "S a l d o  I n i c i a l " .
        IF x-saldoi <> 0 THEN DO:
            xi = xi + 1.
            cColumn = STRING(iCount).
            cRange = "A" + cColumn.
            chWorkSheet:Range(cRange):Value = G-NOMCTA.
            cColumn = STRING(iCount).
            cRange = "D" + cColumn.
            chWorkSheet:Range(cRange):Value = x-fchast.
            cRange = "E" + cColumn.
            chWorkSheet:Range(cRange):Value = x-nroast.
            cRange = "F" + cColumn.
            chWorkSheet:Range(cRange):Value = x-codope.
            cRange = "I" + cColumn.
            chWorkSheet:Range(cRange):Value = x-fchdoc.
            cRange = "K" + cColumn.
            chWorkSheet:Range(cRange):Value = x-nrodoc.
            cRange = "L" + cColumn.
            chWorkSheet:Range(cRange):Value = x-nroref.
            cRange = "M" + cColumn.
            chWorkSheet:Range(cRange):Value = x-glodoc.
            cRange = "N" + cColumn.
            chWorkSheet:Range(cRange):Value = x-saldoi.
            iCount = iCount + 1.
        END.
        FOR EACH cb-dmov NO-LOCK WHERE cb-dmov.codcia  = s-codcia
            AND cb-dmov.periodo = s-periodo 
            AND cb-dmov.nromes  = c-mes
            AND (y-codope = '' OR cb-dmov.codope = y-codope)
            AND cb-dmov.codcta  = x-CodCta
            AND (x-Clasificacion = '' OR cb-dmov.clfaux BEGINS x-Clasificacion)
            AND (x-auxiliar = '' OR cb-dmov.codaux = x-auxiliar)
            AND (y-coddiv = '' OR cb-dmov.coddiv = y-coddiv)
            AND (FILL-IN-Cco = '' OR cb-dmov.cco = FILL-IN-Cco)
            AND (TOGGLE-Sustento = NO OR cb-dmov.LOG_01 = NO)
            BREAK BY cb-dmov.CodOpe BY cb-dmov.NroAst BY cb-dmov.NroItm:

            No-tiene-mov = no.

            /* Buscando la cabecera correspondiente */
            x-fchast = ?.
            FIND cb-cmov WHERE cb-cmov.codcia  = cb-dmov.codcia
                           AND cb-cmov.periodo = cb-dmov.periodo 
                           AND cb-cmov.nromes  = cb-dmov.nromes
                           AND cb-cmov.codope  = cb-dmov.codope
                           AND cb-cmov.nroast  = cb-dmov.nroast
                           NO-LOCK NO-ERROR.
             IF AVAILABLE cb-cmov THEN x-fchast = cb-cmov.fchast.
             ASSIGN x-glodoc = cb-dmov.glodoc
                    x-NroAst = cb-dmov.NroAst
                    x-CodOpe = cb-dmov.CodOpe
                    x-codaux = cb-dmov.codaux
                    x-NroDoc = cb-dmov.NroDoc
                    x-NroRef = cb-dmov.NroRef
                    x-coddiv = cb-dmov.CodDiv
                    x-clfaux = cb-dmov.clfaux
                    x-coddoc = cb-dmov.coddoc
                    x-cco    = cb-dmov.cco.

             IF x-glodoc = "" THEN IF AVAILABLE cb-cmov THEN x-glodoc = cb-cmov.notast.
             IF  x-glodoc = "" THEN DO:
                CASE cb-dmov.clfaux:
                    WHEN "@CL" THEN DO:
                        FIND gn-clie WHERE gn-clie.codcli = cb-dmov.codaux
                                        AND gn-clie.CodCia = cl-codcia
                                    NO-LOCK NO-ERROR. 
                        IF AVAILABLE gn-clie THEN
                            x-glodoc = gn-clie.nomcli.
                    END.
                    WHEN "@PV" THEN DO:
                        FIND gn-prov WHERE gn-prov.codpro = cb-dmov.codaux
                                        AND gn-prov.CodCia = pv-codcia
                                    NO-LOCK NO-ERROR.                      
                        IF AVAILABLE gn-prov THEN 
                            x-glodoc = gn-prov.nompro.
                    END.
                    WHEN "@CT" THEN DO:
                        find b-cuentas WHERE b-cuentas.codcta = cb-dmov.codaux
                                        AND b-cuentas.CodCia = cb-codcia
                                    NO-LOCK NO-ERROR.                      
                        IF AVAILABLE b-cuentas THEN x-glodoc = b-cuentas.nomcta.
                    END.
                    OTHERWISE DO:
                        FIND cb-auxi WHERE cb-auxi.clfaux = cb-dmov.clfaux
                                        AND cb-auxi.codaux = cb-dmov.codaux
                                        AND cb-auxi.CodCia = cb-codcia
                                    NO-LOCK NO-ERROR.                      
                        IF AVAILABLE cb-auxi THEN 
                            x-glodoc = cb-auxi.nomaux.
                    END.
                END CASE.
            END.
            IF NOT tpomov THEN DO:
                x-importe = ImpMn1.
                CASE x-codmon:
                    WHEN 1 THEN DO:
                        x-debe  = ImpMn1.
                        x-haber = 0.
                    END.
                    WHEN 2 THEN DO:
                        x-debe  = ImpMn2.
                        x-haber = 0.
                    END.
                END CASE.
            END.
            ELSE DO:
                x-importe = ImpMn1 * -1. 
                CASE x-codmon:
                    WHEN 1 THEN DO:
                        x-debe  = 0.
                        x-haber = ImpMn1.
                    END.
                    WHEN 2 THEN DO:
                        x-debe  = 0.
                        x-haber = ImpMn2.
                    END.
                END CASE.            
            END.
            IF NOT (x-haber = 0 AND x-debe = 0) AND x-debe <> ? AND x-haber <> ? THEN DO:
                x-fchdoc = cb-dmov.fchdoc.
                x-fchvto = STRING(cb-dmov.fchvto).
                t-Debe  = t-Debe  + x-Debe.
                t-Haber = t-Haber + x-Haber.
                c-Debe  = c-Debe  + x-Debe.
                c-Haber = c-Haber + x-Haber.
                cColumn = STRING(iCount).
                cRange = "A" + cColumn.
                chWorkSheet:Range(cRange):Value = G-NOMCTA.
                cRange = "B" + cColumn.
                chWorkSheet:Range(cRange):Value = x-coddiv.
                cRange = "C" + cColumn.
                chWorkSheet:Range(cRange):Value = x-cco.
                cRange = "D" + cColumn.
                chWorkSheet:Range(cRange):Value = (IF x-fchast <> ? THEN x-fchast ELSE ?).
                cRange = "E" + cColumn.
                chWorkSheet:Range(cRange):Value = x-nroast.
                cRange = "F" + cColumn.
                chWorkSheet:Range(cRange):Value = x-codope.
                cRange = "I" + cColumn.
                chWorkSheet:Range(cRange):Value = (IF x-fchdoc <> ? THEN x-fchdoc ELSE ?).
                cRange = "J" + cColumn.
                chWorkSheet:Range(cRange):Value = x-coddoc.
                cRange = "K" + cColumn.
                chWorkSheet:Range(cRange):Value = x-nrodoc.
                cRange = "L" + cColumn.
                chWorkSheet:Range(cRange):Value = x-nroref.
                cRange = "M" + cColumn.
                chWorkSheet:Range(cRange):Value = x-glodoc.
                IF x-debe <> 0 THEN DO:
                    cRange = "N" + cColumn.
                    chWorkSheet:Range(cRange):Value = x-debe.
                END.
                IF x-haber <> 0 THEN DO:
                    cRange = "O" + cColumn.
                    chWorkSheet:Range(cRange):Value = x-haber.
                END.
                IF x-importe <> 0 AND x-codmon = 2 THEN DO:
                    cRange = "P" + cColumn.
                    chWorkSheet:Range(cRange):Value = x-importe.
                END.
                iCount = iCount + 1.                                                                                                         
                x-conreg = x-conreg + 1.
            END.            
        END.
    END.
    HIDE FRAME f-auxiliar.

    /* launch Excel so it is visible to the user */
    chExcelApplication:Visible = TRUE.

    /* release com-handles */
    RELEASE OBJECT chExcelApplication.      
    RELEASE OBJECT chWorkbook.
    RELEASE OBJECT chWorksheet.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imp-Detalle DIALOG-1 
PROCEDURE Imp-Detalle :
def var xi as integer init 0.
def var No-tiene-mov as logical.

ASSIGN c-debe   = 0
       c-haber  = 0
       x-saldoi = 0 
       xi       = 0 
       No-tiene-mov = yes.

if y-codope   = "" and
   y-coddiv   = "" and
   x-auxiliar = "" then
FOR EACH cb-dmov NO-LOCK WHERE cb-dmov.codcia  = s-codcia
                           AND cb-dmov.periodo = s-periodo 
                           AND cb-dmov.nromes  < c-mes
                           AND cb-dmov.codcta  = x-CodCta :
     case x-codmon :
          when 1 then if (not cb-dmov.tpomov) 
                      then x-saldoi = x-saldoi + cb-dmov.impmn1.
                      else x-saldoi = x-saldoi - cb-dmov.impmn1.     
          when 2 then if (not cb-dmov.tpomov) 
                      then x-saldoi = x-saldoi + cb-dmov.impmn2.
                      else x-saldoi = x-saldoi - cb-dmov.impmn2.     
     end case.

END.
t-saldoi = t-saldoi + x-saldoi.

ASSIGN x-nroast = x-codcta
       x-GloDoc = ""
       x-fchast = ""
       x-codope = ""
       x-fchDoc = ""
       x-nroDoc = ""
       x-fchVto = ""
       x-nroref = "".

x-GloDoc = "S a l d o  I n i c i a l " .
IF x-saldoi <> 0 then do:
   xi = xi + 1.
   hide frame f-auxiliar.
   view frame f-mensaje.
   {&NEW-PAGE}.
   DISPLAY STREAM report WITH FRAME f-cab.
   PUT STREAM report CONTROL P-dobleon.
   G-NOMCTA = cb-ctas.codcta + " " + cb-ctas.nomcta.
   PUT STREAM report G-NOMCTA.
   PUT STREAM report CONTROL P-dobleoff.  
   DOWN STREAM report WITH FRAME f-cab.
   DISPLAY STREAM report x-fchast
                        x-nroast
                        x-codope              
                        x-fchdoc
                        x-nrodoc
                        x-nroref
                        x-glodoc 
                        x-saldoi @ x-debe
                WITH FRAME f-cab.
   {&NEW-PAGE}.
   DOWN STREAM report WITH FRAME F-cab.
END.


FOR EACH cb-dmov NO-LOCK WHERE cb-dmov.codcia  = s-codcia
    AND cb-dmov.periodo = s-periodo 
    AND cb-dmov.nromes  = c-mes
    AND (y-codope = '' OR cb-dmov.codope = y-codope)
    AND cb-dmov.codcta  = x-CodCta
    AND (x-Clasificacion = '' OR cb-dmov.clfaux = x-Clasificacion)
    AND (x-auxiliar = '' OR cb-dmov.codaux = x-auxiliar)
    and (y-coddiv = '' OR cb-dmov.coddiv = y-coddiv)
    AND (FILL-IN-Cco = '' OR cb-dmov.cco = FILL-IN-Cco)
    AND (TOGGLE-Sustento = NO OR cb-dmov.LOG_01 = NO)
    BREAK BY cb-dmov.CodOpe BY cb-dmov.NroAst BY cb-dmov.NroItm:
    No-tiene-mov = no.
    if xi = 0 then do:
       xi = xi + 1.
       hide frame f-auxiliar.
       view frame f-mensaje.
       {&NEW-PAGE}.
       DISPLAY STREAM report WITH FRAME f-cab.
       PUT STREAM report CONTROL P-dobleon.
       G-NOMCTA = cb-ctas.codcta + " " + cb-ctas.nomcta.
       PUT STREAM report G-NOMCTA.
       PUT STREAM report CONTROL P-dobleoff.  
       DOWN STREAM report WITH FRAME f-cab.
    end.    
    
    /* Buscando la cabecera correspondiente */
    x-fchast = ?.
    FIND cb-cmov WHERE cb-cmov.codcia  = cb-dmov.codcia
                   AND cb-cmov.periodo = cb-dmov.periodo 
                   AND cb-cmov.nromes  = cb-dmov.nromes
                   AND cb-cmov.codope  = cb-dmov.codope
                   AND cb-cmov.nroast  = cb-dmov.nroast
                   NO-LOCK NO-ERROR.
     IF AVAILABLE cb-cmov 
     THEN x-fchast = STRING(cb-cmov.fchast).
     ASSIGN x-glodoc = cb-dmov.glodoc
            x-NroAst = cb-dmov.NroAst
            x-CodOpe = cb-dmov.CodOpe
            x-codaux = cb-dmov.codaux
            x-NroDoc = cb-dmov.NroDoc
            x-NroRef = cb-dmov.NroRef
            x-coddiv = cb-dmov.CodDiv
            x-clfaux = cb-dmov.clfaux
            x-coddoc = cb-dmov.coddoc
            x-cco    = cb-dmov.cco.

     IF x-glodoc = "" 
     THEN IF AVAILABLE cb-cmov THEN x-glodoc = cb-cmov.notast.
     IF  x-glodoc = ""
     THEN DO:
        CASE cb-dmov.clfaux:
            WHEN "@CL" THEN DO:
                FIND gn-clie WHERE gn-clie.codcli = cb-dmov.codaux
                                AND gn-clie.CodCia = cl-codcia
                            NO-LOCK NO-ERROR. 
                IF AVAILABLE gn-clie THEN x-glodoc = gn-clie.nomcli.
            END.
            WHEN "@PV" THEN DO:
                FIND gn-prov WHERE gn-prov.codpro = cb-dmov.codaux
                                AND gn-prov.CodCia = pv-codcia
                            NO-LOCK NO-ERROR.                      
                IF AVAILABLE gn-prov THEN x-glodoc = gn-prov.nompro.
            END.
            WHEN "@CT" THEN DO:
                find cb-ctas WHERE cb-ctas.codcta = cb-dmov.codaux
                                AND cb-ctas.CodCia = cb-codcia
                            NO-LOCK NO-ERROR.                      
                IF AVAILABLE cb-ctas THEN 
                    x-glodoc = cb-ctas.nomcta.
            END.
            OTHERWISE DO:
                FIND cb-auxi WHERE cb-auxi.clfaux = cb-dmov.clfaux
                                AND cb-auxi.codaux = cb-dmov.codaux
                                AND cb-auxi.CodCia = cb-codcia
                            NO-LOCK NO-ERROR.                      
                IF AVAILABLE cb-auxi THEN 
                    x-glodoc = cb-auxi.nomaux.
            END.
        END CASE.
    END.
    /* Limpiamos Glosa */
    RUN lib/limpiar-texto (x-glodoc, '', OUTPUT x-glodoc).
 
    IF NOT tpomov 
    THEN DO:
        x-importe = ImpMn1.
        CASE x-codmon:
            WHEN 1 THEN DO:
                x-debe  = ImpMn1.
                x-haber = 0.
            END.
            WHEN 2 THEN DO:
                x-debe  = ImpMn2.
                x-haber = 0.
            END.
        END CASE.
    END.
    ELSE DO:
        x-importe = ImpMn1 * -1. 
        CASE x-codmon:
            WHEN 1 THEN DO:
                x-debe  = 0.
                x-haber = ImpMn1.
            END.
            WHEN 2 THEN DO:
                x-debe  = 0.
                x-haber = ImpMn2.
            END.
        END CASE.            
    END.
    {&NEW-PAGE}.
    IF x-Debe = ? THEN x-Debe = 0.
    IF x-Haber = ? THEN x-Haber = 0.
    IF x-Debe <> 0 OR x-Haber <> 0 THEN DO:
        x-fchdoc = STRING(cb-dmov.fchdoc).  
        x-fchvto = STRING(cb-dmov.fchvto).
        t-Debe  = t-Debe  + x-Debe.
        t-Haber = t-Haber + x-Haber.
        c-Debe  = c-Debe  + x-Debe.
        c-Haber = c-Haber + x-Haber.
        {&NEW-PAGE}. 
        /*RDP*/ x-glodoc = REPLACE(X-glodoc,CHR(10)," ").
        DISPLAY STREAM report x-coddiv
                              x-cco 
                              x-fchast WHEN (x-fchast <> ?)
                              x-nroast
                              x-codope
                              x-clfaux
                              x-codaux              
                              x-fchdoc WHEN (x-fchdoc <> ?)
                              x-coddoc
                              x-nrodoc
                              x-nroref
                              x-glodoc 
                              x-debe   WHEN (x-debe  <> 0)
                              x-haber  WHEN (x-haber <> 0)
                              x-importe WHEN (x-importe <> 0 AND x-codmon = 2)
              WITH FRAME f-cab.
        DOWN STREAM report  WITH FRAME F-cab.
        x-conreg = x-conreg + 1.
    END.            
END.

If no-tiene-mov then return.

ASSIGN x-Debe   = c-Debe
       x-Haber  = c-Haber
       x-SaldoF = x-SaldoI + x-Debe - x-Haber
       x-nroast = x-codcta
       x-fchast = ""
       x-codope = ""
       x-fchDoc = ""
       x-nroDoc = "TOTAL"
       x-nroref = x-CodCta
       x-GloDoc = cb-ctas.NomCta.
/*RDP*/ x-glodoc = REPLACE(X-glodoc,CHR(10)," ").
{&NEW-PAGE}.
UNDERLINE STREAM report 
                        x-debe
                        x-haber
                        x-Saldof
              WITH FRAME f-cab.

DOWN STREAM report WITH FRAME f-cab.
{&NEW-PAGE}.
DISPLAY STREAM report  
                        x-nroRef
                        x-GloDoc
                        x-debe
                        x-haber
                        x-Saldof
              WITH FRAME f-cab.

DOWN STREAM report WITH FRAME f-cab.
{&NEW-PAGE}.
UNDERLINE STREAM report x-fchast
                        x-nroast
                        x-codope              
                        x-fchdoc
                        x-nrodoc
                        x-nroref
                        x-glodoc
                        x-debe   
                        x-haber 
                        x-Saldof 
              WITH FRAME f-cab.
DOWN STREAM report WITH FRAME F-cab.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imp-Detalle-T DIALOG-1 
PROCEDURE Imp-Detalle-T :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def var xi as integer init 0.
def var No-tiene-mov as logical.

ASSIGN c-debe   = 0
       c-haber  = 0
       x-saldoi = 0 
       xi       = 0 
       No-tiene-mov = yes.

if y-codope   = "" and
   y-coddiv   = "" and
   x-auxiliar = "" then
FOR EACH cb-dmov NO-LOCK WHERE cb-dmov.codcia  = s-codcia
                           AND cb-dmov.periodo = s-periodo 
                           AND cb-dmov.nromes  < c-mes
                           AND cb-dmov.codcta  = x-CodCta :
                           
     case x-codmon :
          when 1 then if (not cb-dmov.tpomov) 
                      then x-saldoi = x-saldoi + cb-dmov.impmn1.
                      else x-saldoi = x-saldoi - cb-dmov.impmn1.     
          when 2 then if (not cb-dmov.tpomov) 
                      then x-saldoi = x-saldoi + cb-dmov.impmn2.
                      else x-saldoi = x-saldoi - cb-dmov.impmn2.     
     end case.

end.
t-saldoi = t-saldoi + x-saldoi.

ASSIGN x-nroast = x-codcta
       x-GloDoc = ""
       x-fchast = ""
       x-codope = ""
       x-fchDoc = ""
       x-nroDoc = ""
       x-fchVto = ""
       x-nroref = "".

x-GloDoc = "S a l d o  I n i c i a l " .
if x-saldoi <> 0 then do:
   xi = xi + 1.

   DISPLAY STREAM report WITH FRAME f-cab-t.
   G-NOMCTA = cb-ctas.codcta + " " + cb-ctas.nomcta.
   PUT STREAM report G-NOMCTA.
  DOWN STREAM report WITH FRAME f-cab-t.
    DISPLAY STREAM report x-fchast
                      x-nroast
                      x-codope              
                      x-fchdoc
                      x-nrodoc
/*MLR* 07/Mar/2008 ***
                      x-fchvto
* ***/
                      x-nroref
                      x-glodoc 
                      x-saldoi @ x-debe
              WITH FRAME f-cab-t.
    DOWN STREAM report WITH FRAME f-cab-t.
end.


FOR EACH cb-dmov NO-LOCK WHERE cb-dmov.codcia  = s-codcia
    AND cb-dmov.periodo = s-periodo 
    AND cb-dmov.nromes  = c-mes
    AND cb-dmov.codope BEGINS (y-codope)
    AND cb-dmov.codcta  = x-CodCta
    AND cb-dmov.clfaux  begins x-Clasificacion
    AND cb-dmov.codaux  begins x-auxiliar
    and cb-dmov.coddiv  begins y-coddiv
    AND cb-dmov.cco     BEGINS FILL-IN-Cco
    AND (TOGGLE-Sustento = NO OR cb-dmov.LOG_01 = NO)
    BREAK BY cb-dmov.CodOpe BY cb-dmov.NroAst BY cb-dmov.NroItm:

    No-tiene-mov = no.
    
    if xi = 0 then do:
       xi = xi + 1.
       DISPLAY STREAM report WITH FRAME f-cab-t.
       G-NOMCTA = cb-ctas.codcta + " " + cb-ctas.nomcta.
       PUT STREAM report G-NOMCTA.
       DOWN STREAM report WITH FRAME f-cab-t.
    end.    
    
    /* Buscando la cabecera correspondiente */
    x-fchast = ?.
    FIND cb-cmov WHERE cb-cmov.codcia  = cb-dmov.codcia
                   AND cb-cmov.periodo = cb-dmov.periodo 
                   AND cb-cmov.nromes  = cb-dmov.nromes
                   AND cb-cmov.codope  = cb-dmov.codope
                   AND cb-cmov.nroast  = cb-dmov.nroast
                   NO-LOCK NO-ERROR.
     IF AVAILABLE cb-cmov 
     THEN x-fchast = STRING(cb-cmov.fchast).
     ASSIGN x-glodoc = cb-dmov.glodoc
            x-NroAst = cb-dmov.NroAst
            x-CodOpe = cb-dmov.CodOpe
            x-codaux = cb-dmov.codaux
            x-NroDoc = cb-dmov.NroDoc
            x-NroRef = cb-dmov.NroRef
            x-coddiv = cb-dmov.CodDiv
            x-clfaux = cb-dmov.clfaux
            x-coddoc = cb-dmov.coddoc
            x-cco    = cb-dmov.cco.

     IF x-glodoc = "" 
     THEN IF AVAILABLE cb-cmov 
          THEN x-glodoc = cb-cmov.notast.
     IF  x-glodoc = ""
     THEN DO:
        CASE cb-dmov.clfaux:
            WHEN "@CL" THEN DO:
                FIND gn-clie WHERE gn-clie.codcli = cb-dmov.codaux
                                AND gn-clie.CodCia = cl-codcia
                            NO-LOCK NO-ERROR. 
                IF AVAILABLE gn-clie THEN
                    x-glodoc = gn-clie.nomcli.
            END.
            WHEN "@PV" THEN DO:
                FIND gn-prov WHERE gn-prov.codpro = cb-dmov.codaux
                                AND gn-prov.CodCia = pv-codcia
                            NO-LOCK NO-ERROR.                      
                IF AVAILABLE gn-prov THEN 
                    x-glodoc = gn-prov.nompro.
            END.
            WHEN "@CT" THEN DO:
                find cb-ctas WHERE cb-ctas.codcta = cb-dmov.codaux
                                AND cb-ctas.CodCia = cb-codcia
                            NO-LOCK NO-ERROR.                      
                IF AVAILABLE cb-ctas THEN 
                    x-glodoc = cb-ctas.nomcta.
            END.
            OTHERWISE DO:
                FIND cb-auxi WHERE cb-auxi.clfaux = cb-dmov.clfaux
                                AND cb-auxi.codaux = cb-dmov.codaux
                                AND cb-auxi.CodCia = cb-codcia
                            NO-LOCK NO-ERROR.                      
                IF AVAILABLE cb-auxi THEN 
                    x-glodoc = cb-auxi.nomaux.
            END.
        END CASE.
    END.
 
    IF NOT tpomov 
    THEN DO:
        x-importe = ImpMn1.
        CASE x-codmon:
            WHEN 1 THEN DO:
                x-debe  = ImpMn1.
                x-haber = 0.
            END.
            WHEN 2 THEN DO:
                x-debe  = ImpMn2.
                x-haber = 0.
            END.
        END CASE.
    END.
    ELSE DO:
        x-importe = ImpMn1 * -1.
        CASE x-codmon:
            WHEN 1 THEN DO:
                x-debe  = 0.
                x-haber = ImpMn1.
            END.
            WHEN 2 THEN DO:
                x-debe  = 0.
                x-haber = ImpMn2.
            END.
        END CASE.            
    END.
    IF NOT (x-haber = 0 AND x-debe = 0) AND x-debe <> ? AND x-haber <> ?
    THEN DO:
        x-fchdoc = STRING(cb-dmov.fchdoc).  
        x-fchvto = STRING(cb-dmov.fchvto).
        t-Debe  = t-Debe  + x-Debe.
        t-Haber = t-Haber + x-Haber.
        c-Debe  = c-Debe  + x-Debe.
        c-Haber = c-Haber + x-Haber.

        /*RDP*/ x-glodoc = REPLACE(X-glodoc,CHR(10)," ").
        DISPLAY STREAM report x-coddiv
                              x-cco 
                              x-fchast WHEN (x-fchast <> ?)
                              x-nroast
                              x-codope
                              x-clfaux
                              x-codaux              
                              x-fchdoc WHEN (x-fchdoc <> ?)
                              x-coddoc
                              x-nrodoc
/*MLR* 07/Mar/2008 ***
                              x-fchvto WHEN (x-fchvto <> ?)
* ***/
                              x-nroref
                              x-glodoc 
                              x-debe   WHEN (x-debe  <> 0)
                              x-haber  WHEN (x-haber <> 0)
                              x-importe WHEN (x-importe <> 0 AND x-codmon = 2)
              WITH FRAME f-cab-t.
        DOWN STREAM report  WITH FRAME f-cab-t.
        x-conreg = x-conreg + 1.
    END.            
END.

If no-tiene-mov then return.

ASSIGN x-Debe   = c-Debe
       x-Haber  = c-Haber
       x-SaldoF = x-SaldoI + x-Debe - x-Haber
       x-nroast = x-codcta
       x-fchast = ""
       x-codope = ""
       x-fchDoc = ""
/*MLR* 07/Mar/2008 ***
       x-nroDoc = ""
       x-fchVto = "TOTAL"
* ***/
       x-nrodoc = "TOTAL"
       x-nroref = x-CodCta
       x-GloDoc = cb-ctas.NomCta.
/*RDP*/ x-glodoc = REPLACE(X-glodoc,CHR(10)," ").
UNDERLINE STREAM report /* x-Saldoi *MLR* 07/Mar/2008 */
                        x-debe
                        x-haber
                        x-Saldof
              WITH FRAME f-cab-t.

DOWN STREAM report WITH FRAME f-cab-t.
DISPLAY STREAM report   /* x-fchvto *MLR* 07/Mar/2007 */
                        x-nroRef
                        x-GloDoc
/*MLR* 07/Mar/2008 ***
                        x-Saldoi
* ***/
                        x-debe
                        x-haber
                        x-Saldof
              WITH FRAME f-cab-t.

DOWN STREAM report WITH FRAME f-cab-t.
UNDERLINE STREAM report x-fchast
                        x-nroast
                        x-codope              
                        x-fchdoc
                        x-nrodoc
/*MLR* 07/Mar/2008 ***
                        x-fchvto
* ***/
                        x-nroref
                        x-glodoc
/*MLR* 07/Mar/2008 ***
                        x-Saldoi
* ***/
                        x-debe   
                        x-haber 
                        x-Saldof 
              WITH FRAME f-cab-t.
DOWN STREAM report WITH FRAME f-cab-t.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE IMPRIMIR DIALOG-1 
PROCEDURE IMPRIMIR :
P-largo  = 88. /* En el caso de 88 lineas por p�gina usando 8 lpi ( P-8lpi )
                      hay que definir en el triger "CHOOSE" de b-imprime en 66 
                      L�neas por p�gina y aqui en 88.
                      Si se usa 6 lpi ( Default ) solo basta definir en
                      b-imprime.
                      ** OJO ** en b-imprime el P-largo configura a la impresora
                      a saltar en ese largo por p�gina
                      */
                       
    P-Config = P-20cpi + P-8lpi.
    x-Raya   = FILL("-", P-Ancho).

    DEF VAR con-ctas AS INTEGER.
    ASSIGN t-debe   = 0
           t-haber  = 0
           t-saldoi = 0
           t-saldof = 0
           con-ctas = 0 .

    DISPLAY "Seleccionando informaci�n solicitada" @ x-mensaje with frame f-auxiliar.
    PAUSE 0.

    FOR EACH cb-ctas NO-LOCK WHERE cb-ctas.codcia = cb-codcia
        AND cb-ctas.codcta >= x-cta-ini
        AND cb-ctas.codcta <= x-cta-fin
        AND cb-ctas.CodCta <> ""
        AND cb-ctas.ClfAux BEGINS x-Clasificacion
        AND LENGTH( cb-ctas.CodCta ) = Max-Digitos
        AND (TOGGLE-Sustento = NO OR Cb-Ctas.Sustento = YES)
        BY Cb-ctas.codcta:
        x-CodCta = cb-ctas.CodCta.
        RUN Imp-Detalle NO-ERROR.
        IF ERROR-STATUS:ERROR THEN LEAVE.
        con-ctas = con-ctas + 1 .
    END.
    HIDE frame f-auxiliar.
    IF con-ctas <= 1 then return.
    ASSIGN x-saldoi = t-saldoi
       x-Debe   = t-Debe
       x-Haber  = t-Haber
       x-SaldoF = t-SaldoI + x-Debe - x-Haber
       x-nroast = x-codcta
       x-fchast = ""
       x-codope = ""
       x-fchDoc = ""
       x-nroDoc = ""
       x-fchVto = ""
       x-nroref = ""
       x-GloDoc = "T O T A L   G E N E R A L ".

    {&NEW-PAGE}.
    UNDERLINE STREAM report 
        x-debe
        x-haber
        x-Saldof
        WITH FRAME f-cab.
    DOWN STREAM report WITH FRAME f-cab.
    {&NEW-PAGE}.
    DISPLAY STREAM report 
        x-nroRef
        x-GloDoc
        x-debe
        x-haber
        x-Saldof
        WITH FRAME f-cab.
    DOWN STREAM report WITH FRAME f-cab.
    {&NEW-PAGE}.
    UNDERLINE STREAM report 
        x-fchast
        x-nroast
        x-codope              
        x-fchdoc
        x-nrodoc
        x-nroref
        x-glodoc
        x-debe   
        x-haber 
        x-Saldof 
        WITH FRAME f-cab.
    DOWN STREAM report WITH FRAME F-cab.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NEW-PAGE DIALOG-1 
PROCEDURE NEW-PAGE :
/* -----------------------------------------------------------
  Purpose: Imprimir las cabezeras de los reportes     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/

    c-Pagina = c-Pagina + 1.

    IF c-Pagina > P-pagfin THEN RETURN ERROR.

    DISPLAY c-Pagina WITH FRAME f-mensaje.
    IF c-Pagina > 1 THEN PAGE STREAM report.
    IF P-pagini = c-Pagina 
    THEN DO:
        OUTPUT STREAM report CLOSE.
        IF P-select = 1 
        THEN DO:
               OUTPUT STREAM report TO PRINTER NO-MAP NO-CONVERT UNBUFFERED
                    PAGED PAGE-SIZE 1000.
               PUT STREAM report CONTROL P-reset NULL P-flen NULL P-config NULL.
        END.
        ELSE DO:
            OUTPUT STREAM report TO VALUE ( P-archivo ) NO-MAP NO-CONVERT UNBUFFERED
                 PAGED PAGE-SIZE 1000.
            IF P-select = 3 THEN
                PUT STREAM report CONTROL P-reset P-flen P-config.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Quiebre DIALOG-1 
PROCEDURE Quiebre :
/* Solo puede haber quiebres cuando son todas las operaciones 
     IF NOT CodOpe = "" THEN RETURN. */
    
     DO i = (Ult-Nivel - 1) TO 1 BY -1 :
     
        IF NOT v-CodCta[ i ] = "" 
           AND 
           ( NOT x-CodCta BEGINS v-CodCta[ i ] OR x-CodCta = "" )
        THEN DO:
            ASSIGN x-Saldoi = v-Saldoi[ i ]
                   x-Debe   = v-Debe[ i ]
                   x-Haber  = v-Haber[ i ]
                   x-SaldoF = x-SaldoI + x-Debe - x-Haber
                   x-nroast = ""
                   x-fchast = ""
                   x-codope = ""
                   x-fchDoc = FILL("*", i)
                   x-nroDoc = "TOTAL"
                   x-fchVto = ""
                   x-nroref = v-CodCta[ i ]
                   x-GloDoc = ""
                   v-Saldoi[ i ] = 0
                   v-Debe[ i ]   = 0
                   v-Haber[ i ]  = 0.
            FIND B-Cuentas WHERE b-cuentas.CodCia = cb-codcia
                             AND b-cuentas.CodCta = v-CodCta[ i ]
                             NO-LOCK NO-ERROR.
            IF AVAILABLE B-Cuentas THEN x-GloDoc = b-cuentas.NomCta.
            v-CodCta[ i ] = "".
            {&NEW-PAGE}.
            DISPLAY STREAM report
                                    x-nrodoc
/*MLR* 07/Mar/2008 ***
                                    x-fchvto
* ***/
                                    x-nroRef
                                    x-GloDoc
/*MLR* 07/Mar/2008 ***
                                    x-Saldoi 
* ***/
                                    x-debe
                                    x-haber
                                    x-Saldof
                          WITH FRAME f-cab.
            DOWN STREAM report WITH FRAME f-cab.

            UNDERLINE STREAM report 
                        x-fchast
                        x-nroast
                        x-codope              
                        x-fchdoc
                        x-nrodoc
/*MLR* 07/Mar/2008 ***
                        x-fchvto
* ***/
                        x-nroref
                        x-glodoc
/*MLR* 07/Mar/2008 ***
                        x-Saldoi 
* ***/
                        x-debe   
                        x-haber 
                        x-Saldof 
                      WITH FRAME f-cab.
            DOWN STREAM report WITH FRAME F-cab.
            IF i = 1 THEN RUN NEW-PAGE.
        END.
     END.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE remvar DIALOG-1 
PROCEDURE remvar :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER IN-VAR AS CHARACTER.
    DEFINE OUTPUT PARAMETER OU-VAR AS CHARACTER.
    DEFINE VARIABLE P-pos AS INTEGER.
    OU-VAR = IN-VAR.
    IF P-select = 2 THEN DO:
        OU-VAR = "".
        RETURN.
    END.
    P-pos =  INDEX(OU-VAR, "[NULL]" ).
    IF P-pos <> 0
    THEN OU-VAR = SUBSTR(OU-VAR, 1, P-pos - 1) +
                     CHR(0) + SUBSTR(OU-VAR, P-pos + 6).
    P-pos =  INDEX(OU-VAR, "[#B]" ).
    IF P-pos <> 0
    THEN OU-VAR = SUBSTR(OU-VAR, 1, P-pos - 1) +
                     CHR(P-Largo) + SUBSTR(OU-VAR, P-pos + 4).
    P-pos =  INDEX(OU-VAR, "[#]" ).
    IF P-pos <> 0
    THEN OU-VAR = SUBSTR(OU-VAR, 1, P-pos - 1) +
                     STRING(P-Largo, ">>9" ) + SUBSTR(OU-VAR, P-pos + 3).
    RETURN.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setup-print DIALOG-1 
PROCEDURE setup-print :
/* -----------------------------------------------------------
  Purpose: Configura los codigos de escape de impresi�n
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
    FIND integral.P-Codes WHERE integral.P-Codes.Name = P-name NO-LOCK NO-ERROR.
    IF NOT AVAILABLE integral.P-Codes
    THEN DO:
        MESSAGE "Invalido Tabla de Impresora" SKIP
                "configurado al Terminal" XTerm
                VIEW-AS ALERT-BOX ERROR.
        RETURN ERROR.
    END.
    /* Configurando Variables de Impresion */
    RUN RemVar (INPUT integral.P-Codes.Reset,    OUTPUT P-Reset).
    RUN RemVar (INPUT integral.P-Codes.Flen,     OUTPUT P-Flen).
    RUN RemVar (INPUT integral.P-Codes.C6lpi,    OUTPUT P-6lpi).
    RUN RemVar (INPUT integral.P-Codes.C8lpi,    OUTPUT P-8lpi).
    RUN RemVar (INPUT integral.P-Codes.C10cpi,   OUTPUT P-10cpi).
    RUN RemVar (INPUT integral.P-Codes.C12cpi,   OUTPUT P-12cpi).
    RUN RemVar (INPUT integral.P-Codes.C15cpi,   OUTPUT P-15cpi).
    RUN RemVar (INPUT integral.P-Codes.C20cpi,   OUTPUT P-20cpi).
    RUN RemVar (INPUT integral.P-Codes.Landscap, OUTPUT P-Landscap).
    RUN RemVar (INPUT integral.P-Codes.Portrait, OUTPUT P-Portrait).
    RUN RemVar (INPUT integral.P-Codes.DobleOn,  OUTPUT P-DobleOn).
    RUN RemVar (INPUT integral.P-Codes.DobleOff, OUTPUT P-DobleOff).
    RUN RemVar (INPUT integral.P-Codes.BoldOn,   OUTPUT P-BoldOn).
    RUN RemVar (INPUT integral.P-Codes.BoldOff,  OUTPUT P-BoldOff).
    RUN RemVar (INPUT integral.P-Codes.UlineOn,  OUTPUT P-UlineOn).
    RUN RemVar (INPUT integral.P-Codes.UlineOff, OUTPUT P-UlineOff).
    RUN RemVar (INPUT integral.P-Codes.ItalOn,   OUTPUT P-ItalOn).
    RUN RemVar (INPUT integral.P-Codes.ItalOff,  OUTPUT P-ItalOff).
    RUN RemVar (INPUT integral.P-Codes.SuperOn,  OUTPUT P-SuperOn).
    RUN RemVar (INPUT integral.P-Codes.SuperOff, OUTPUT P-SuperOff).
    RUN RemVar (INPUT integral.P-Codes.SubOn,    OUTPUT P-SubOn).
    RUN RemVar (INPUT integral.P-Codes.SubOff,   OUTPUT P-SubOff).
    RUN RemVar (INPUT integral.P-Codes.Proptnal, OUTPUT P-Proptnal).
    RUN RemVar (INPUT integral.P-Codes.Lpi,      OUTPUT P-Lpi).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Texto DIALOG-1 
PROCEDURE Texto :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF VAR x-Archivo AS CHAR NO-UNDO.
  DEF VAR x-Rpta    AS LOG  NO-UNDO.

  x-Archivo = 'Mayor' + STRING(s-Periodo, '9999') + STRING(s-NroMes, '99') + '.txt'.
  SYSTEM-DIALOG GET-FILE x-Archivo
    FILTERS 'Texto' '*.txt'
    ASK-OVERWRITE
    CREATE-TEST-FILE
    DEFAULT-EXTENSION '.txt'
    INITIAL-DIR 'M:\'
    RETURN-TO-START-DIR 
    USE-FILENAME
    SAVE-AS
    UPDATE x-rpta.
  IF x-rpta = NO THEN RETURN.


  OUTPUT STREAM REPORT TO VALUE(x-Archivo).


    x-Raya   = FILL("-", P-Ancho).
    def var con-ctas as integer.
    ASSIGN t-debe   = 0
           t-haber  = 0
           t-saldoi = 0
           t-saldof = 0
           con-ctas = 0 .

    display "Seleccionando informaci�n solicitada" @ x-mensaje with frame f-auxiliar.
    pause 0.

    FOR EACH cb-ctas NO-LOCK WHERE cb-ctas.codcia = cb-codcia
        AND cb-ctas.codcta >= x-cta-ini
        AND cb-ctas.codcta <= x-cta-fin
        AND cb-ctas.CodCta <> ""
        AND cb-ctas.ClfAux BEGINS x-Clasificacion
        AND LENGTH( cb-ctas.CodCta ) = Max-Digitos
        AND (TOGGLE-Sustento = NO OR Cb-Ctas.Sustento = YES)
        BY Cb-ctas.codcta:
/*         BREAK BY (cb-ctas.Codcta) */
/*         ON ERROR UNDO, LEAVE:     */
        
        x-CodCta = cb-ctas.CodCta.
        RUN Imp-Detalle-T.
        con-ctas = con-ctas + 1 .
    END.
   
    hide frame f-auxiliar.
    
    if con-ctas <= 1 then return.
       
    ASSIGN x-saldoi = t-saldoi
           x-Debe   = t-Debe
           x-Haber  = t-Haber
           x-SaldoF = t-SaldoI + x-Debe - x-Haber
           x-nroast = x-codcta
           x-fchast = ""
           x-codope = ""
           x-fchDoc = ""
           x-nroDoc = ""
           x-fchVto = ""
           x-nroref = ""
           x-GloDoc = "T O T A L   G E N E R A L ".
    
    UNDERLINE STREAM report x-Saldoi 
                            x-debe
                            x-haber
                            x-Saldof
                  WITH FRAME f-cab-t.
    
    DOWN STREAM report WITH FRAME f-cab-t.
    
    DISPLAY STREAM report   /* x-fchvto *MLR* 07/Mar/2007 */
                            x-nroRef
                            x-GloDoc
/*MLR* 07/Mar/2008 ***
                            x-Saldoi
* ***/
                            x-debe
                            x-haber
                            x-Saldof
                  WITH FRAME f-cab-t.
    
    DOWN STREAM report WITH FRAME f-cab-t.
    
    UNDERLINE STREAM report x-fchast
                            x-nroast
                            x-codope              
                            x-fchdoc
                            x-nrodoc
/*MLR* 07/Mar/2008 ***
                            x-fchvto
* ***/
                            x-nroref
                            x-glodoc
/*MLR* 07/Mar/2008 ***
                            x-Saldoi 
* ***/
                            x-debe   
                            x-haber 
                            x-Saldof 
                  WITH FRAME f-cab-t.
    DOWN STREAM report WITH FRAME F-cab-t.

  OUTPUT STREAM REPORT CLOSE.
  MESSAGE 'Proceso Terminado' VIEW-AS ALERT-BOX.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
