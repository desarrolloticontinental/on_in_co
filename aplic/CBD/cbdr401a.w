&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME DIALOG-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS DIALOG-1 
/*
    @PRINTER2.W    VERSION 1.0
*/    
DEFINE STREAM report.

/* VARIABLES GENERALES :  IMPRESION,SISTEMA,MODULO,USUARIO */
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



DEFINE {&NEW} SHARED VARIABLE cb-niveles  AS CHARACTER INITIAL "2,3,4,5".

DEFINE        VARIABLE P-config  AS CHARACTER NO-UNDO.
DEFINE        VARIABLE c-Pagina  AS INTEGER LABEL "                 Imprimiendo Pagina " NO-UNDO.
DEFINE        VARIABLE c-Copias  AS INTEGER NO-UNDO.
DEFINE        VARIABLE x-Detalle LIKE Modulos.Detalle NO-UNDO.
DEFINE        VARIABLE i           AS INTEGER NO-UNDO.
DEFINE        VARIABLE OKpressed   AS LOGICAL NO-UNDO.

    

DEFINE        VARIABLE cb-codcia AS INTEGER NO-UNDO INITIAL 0.
DEFINE        VARIABLE pv-codcia AS INTEGER NO-UNDO INITIAL 0.
DEFINE        VARIABLE cl-codcia AS INTEGER NO-UNDO INITIAL 0.
DEFINE        VARIABLE PTO        AS LOGICAL NO-UNDO .


/* MACRO NUEVA-PAGINA */
&GLOBAL-DEFINE NEW-PAGE READKEY PAUSE 0. ~
IF LASTKEY = KEYCODE("F10") THEN RETURN ERROR. ~
IF LINE-COUNTER( report ) > (P-Largo - 8 ) OR c-Pagina = 0 ~
THEN RUN NEW-PAGE

/*VARIABLES PARTICULARES DE LA RUTINA */
DEFINE BUFFER detalle FOR cb-dmov.
DEFINE VARIABLE x-nomcta   AS CHARACTER FORMAT "X(50)".
DEFINE VARIABLE x-nombala  AS CHARACTER FORMAT "X(50)".
DEFINE VARIABLE pinta-mes  AS CHARACTER FORMAT "X(40)".
DEFINE VARIABLE x-expres   AS CHARACTER FORMAT "X(40)".
DEFINE VARIABLE x-nomaux   AS CHARACTER FORMAT "X(50)".
DEFINE VARIABLE imp-aux    AS LOGICAL.
DEFINE VARIABLE imprime    AS LOGICAL INITIAL YES.
DEFINE VARIABLE imprimemes AS LOGICAL INITIAL NO.
DEFINE VARIABLE x-conreg   AS INTEGER.
DEFINE VARIABLE x-condoc   AS INTEGER.
DEFINE VARIABLE x-glodoc   LIKE cb-dmov.glodoc.
DEFINE VARIABLE x-debe     AS DECIMAL FORMAT "ZZZ,ZZZ,ZZ9.99-" 
                           LABEL "Cargos     ". 
DEFINE VARIABLE x-haber    AS DECIMAL FORMAT "ZZZ,ZZZ,ZZ9.99-" 
                           LABEL "Abonos     ".
DEFINE VARIABLE x-importe  AS DECIMAL FORMAT "ZZZ,ZZZ,ZZ9.99-"
                           LABEL "Importe    ".
DEFINE VARIABLE x-saldo    AS DECIMAL FORMAT "ZZZ,ZZZ,ZZ9.99-"
                           COLUMN-LABEL "Saldo     !Actual    ".

DEF VAR X-CLFAUX AS CHAR INIT "".



/*VARIABLES GLOBALES */


DEFINE {&NEW} SHARED VARIABLE s-NroMes    AS INTEGER INITIAL 12.
DEFINE {&NEW} SHARED VARIABLE s-periodo    AS INTEGER INITIAL 1996.

DEFINE {&NEW} SHARED VARIABLE s-codcia AS INTEGER INITIAL 1.
DEFINE {&NEW} SHARED VARIABLE s-nomcia AS CHARACTER FORMAT "X(50)".
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

    DEFINE FRAME f-cab
        cb-dmov.fchdoc COLUMN-LABEL " Fecha!  Doc."
        cb-dmov.coddiv COLUMN-LABEL "Div"
        cb-dmov.cco    COLUMN-LABEL "C.Cos"
        cb-dmov.nroast COLUMN-LABEL "Compro!bante"
        cb-dmov.codope COLUMN-LABEL "Li-!bro"
        cb-dmov.nromes LABEL        "Mes"
        cb-dmov.coddoc COLUMN-LABEL "Cod.!Doc."  FORMAT "X(4)"
        cb-dmov.nrodoc COLUMN-LABEL "  Nro.!Documento"
        cb-dmov.nroref
        cb-dmov.fchvto COLUMN-LABEL " Fecha!  Vto."
        cb-dmov.glodoc FORMAT "X(50)"          
        x-importe  
        x-debe
        x-haber  
        x-saldo
        HEADER
        S-Nomcia
        "B A L A N C E   D E   C U E N T A S" AT 71
        /*"FECHA : " TO 179 TODAY TO 190*/
        SKIP
      /*  
        empresas.direccion
      */  
        pinta-mes AT 68
        "PAGINA :" TO 179 c-pagina FORMAT "ZZZ9" TO 190
        x-nombala AT 63
        x-expres  AT 68

        SKIP(3)
        WITH WIDTH 255 NO-BOX DOWN STREAM-IO.


DEFINE TEMP-TABLE T-REPORT
FIELD codcia AS INT
FIELD fchdoc AS DATE    COLUMN-LABEL " Fecha!  Doc."
FIELD coddiv AS CHAR    FORMAT "X(5)"  COLUMN-LABEL "Div"
FIELD cco    AS CHAR    FORMAT "X(5)"  COLUMN-LABEL "C.Cos"
FIELD nroast AS CHAR    FORMAT "X(6)"  COLUMN-LABEL "Compro!bante"
FIELD codope AS CHAR    FORMAT "X(3)"  COLUMN-LABEL "Li-!bro"
FIELD nromes AS INTEGER FORMAT "99"    COLUMN-LABEL "Mes"
FIELD coddoc AS CHAR    FORMAT "X(3)"  COLUMN-LABEL "Cod!Doc"  
FIELD nrodoc AS CHAR    FORMAT "X(14)" COLUMN-LABEL "  Nro.!Documento"
FIELD nroref AS CHAR    FORMAT "X(10)" COLUMN-LABEL "  Nro.!Referencia"
FIELD fchvto AS DATE    COLUMN-LABEL " Fecha!  Vto."
FIELD glodoc AS CHAR    FORMAT "X(50)" COLUMN-LABEL "D e t a l l e"           
FIELD t-importe AS DECIMAL    FORMAT "ZZ,ZZZ,ZZ9.99-" COLUMN-LABEL "Importe" 
FIELD t-debe    AS DECIMAL    FORMAT "ZZ,ZZZ,ZZ9.99-" COLUMN-LABEL "Cargos"
FIELD t-haber   AS DECIMAL    FORMAT "ZZ,ZZZ,ZZ9.99-" COLUMN-LABEL "Abonos"
FIELD t-saldo   AS DECIMAL    FORMAT "ZZ,ZZZ,ZZ9.99-" COLUMN-LABEL "Saldo     !Actual    "
FIELD clfaux    AS CHAR
FIELD codaux    AS CHAR       FORMAT "X(8)"  COLUMN-LABEL "C�digo !Auxiliar"
FIELD codcta    AS CHAR       FORMAT "X(8)"  COLUMN-LABEL "C�digo!Cuenta"
FIELD FECHA-ID  AS DATE  
FIELD Voucher   AS CHAR FORMAT "X(9)" 
INDEX IDX01 codcta codaux Voucher fecha-id coddoc nrodoc codope nroast fchdoc
INDEX IDX02 codcta codaux coddoc nrodoc
INDEX IDX03 codaux codcta Voucher fecha-id coddoc nrodoc codope nroast fchdoc.
/* INDEX IDX01 codcta codaux Voucher fecha-id coddoc nrodoc fchdoc */
/* INDEX IDX03 codaux codcta Voucher fecha-id coddoc nrodoc fchdoc. */


DEFINE FRAME T-cab
       T-REPORT.fchdoc COLUMN-LABEL " Fecha!  Doc."
       T-REPORT.coddiv COLUMN-LABEL "Div"
       T-REPORT.cco    COLUMN-LABEL "C.Cos"
       T-REPORT.nroast COLUMN-LABEL "Compro!bante"
       T-REPORT.codope COLUMN-LABEL "Ope!rac"
       T-REPORT.nromes LABEL        "Mes"
       T-REPORT.coddoc COLUMN-LABEL "Cod!Doc" FORMAT "X(3)"          
       T-REPORT.nrodoc COLUMN-LABEL "  Nro.!Documento" 
       T-REPORT.fchvto COLUMN-LABEL " Fecha!  Vto."
       T-REPORT.NroRef COLUMN-LABEL "Referencia" FORMAT "X(10)" 
       T-REPORT.glodoc COLUMN-LABEL "D e t a l l e" FORMAT "X(25)"          
       T-REPORT.T-importe  
       T-REPORT.T-debe
       T-REPORT.T-haber  
       T-REPORT.T-saldo
       HEADER
       S-Nomcia
       "A N A L I S I S  D E   C U E N T A S" AT 70
       /*"FECHA : " TO 149 TODAY TO 160*/
       SKIP
       pinta-mes AT 68
       "PAGINA :" TO 149 c-pagina FORMAT "ZZZ9" TO 160
       x-nombala AT 63
       x-expres  AT 68
       SKIP(3)
       WITH WIDTH 255 NO-BOX DOWN STREAM-IO.

DEFINE FRAME T-cab-4
       T-REPORT.fchdoc COLUMN-LABEL " Fecha!  Doc."
       T-REPORT.codcta COLUMN-LABEL "Cuenta"
       T-REPORT.coddiv COLUMN-LABEL "Div"
       T-REPORT.cco    COLUMN-LABEL "C.Cos"
       T-REPORT.nroast COLUMN-LABEL "Compro!bante"
       T-REPORT.codope COLUMN-LABEL "Ope!rac"
       T-REPORT.nromes LABEL        "Mes"
       T-REPORT.coddoc COLUMN-LABEL "Cod!Doc" FORMAT "X(3)"          
       T-REPORT.nrodoc COLUMN-LABEL "  Nro.!Documento" 
       T-REPORT.fchvto COLUMN-LABEL " Fecha!  Vto."
       /*T-REPORT.NroRef COLUMN-LABEL "Referencia" FORMAT "X(10)" */
       T-REPORT.CodAux COLUMN-LABEL "Auxiliar"   FORMAT "x(11)"
       T-REPORT.glodoc COLUMN-LABEL "D e t a l l e" FORMAT "X(25)"          
       T-REPORT.T-importe  
       T-REPORT.T-debe
       T-REPORT.T-haber  
       HEADER
       S-Nomcia
       "A N A L I S I S  D E   C U E N T A S" AT 70
       /*"FECHA : " TO 149 TODAY TO 160*/
       SKIP
       pinta-mes AT 68
       "PAGINA :" TO 149 c-pagina FORMAT "ZZZ9" TO 160
       x-nombala AT 63
       x-expres  AT 68
       SKIP(3)
       WITH WIDTH 255 NO-BOX DOWN STREAM-IO.

  DEFINE VAR X-MENSAJE AS CHAR FORMAT "X(40)".
  DEFINE FRAME F-AUXILIAR X-MENSAJE
  WITH TITLE "Espere un momento por favor" VIEW-AS DIALOG-BOX CENTERED NO-LABELS.

  DEFINE VAR x-formato AS INTEGER init 1.
  DEFINE VAR q-saldo   AS LOGICAL init no.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DIALOG-1

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-4 RECT-25 RECT-6 RECT-5 C-Mes R-TipRep ~
x-codmon x-CodOpe X-DIV x-cuenta x-Clasificacion x-auxiliar x-cuenta-2 ~
T-Fechas RADIO-SET-1 RB-NUMBER-COPIES B-impresoras RB-BEGIN-PAGE ~
RB-END-PAGE B-imprime B-cancela BUTTON-EXCELL 
&Scoped-Define DISPLAYED-OBJECTS C-Mes R-TipRep x-codmon x-CodOpe X-DIV ~
x-cuenta x-Clasificacion x-auxiliar x-cuenta-2 T-Fechas F-Fecha1 F-Fecha2 ~
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
     LABEL "&Cancelar" 
     SIZE 11 BY 1.08.

DEFINE BUTTON B-impresoras 
     IMAGE-UP FILE "IMG/pvprint":U
     IMAGE-DOWN FILE "IMG/pvprintd":U
     LABEL "" 
     SIZE 5 BY 1.

DEFINE BUTTON B-imprime AUTO-GO 
     LABEL "&Imprimir" 
     SIZE 11 BY 1.08.

DEFINE BUTTON BUTTON-EXCELL 
     LABEL "EXCEL" 
     SIZE 11 BY 1.08.

DEFINE VARIABLE C-Mes AS INTEGER FORMAT "99":U INITIAL 0 
     LABEL "Al Mes" 
     VIEW-AS COMBO-BOX INNER-LINES 13
     LIST-ITEMS "0","1","2","3","4","5","6","7","8","9","10","11","12" 
     DROP-DOWN-LIST
     SIZE 8 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F-Fecha1 AS DATE FORMAT "99/99/9999":U 
     LABEL "Desde" 
     VIEW-AS FILL-IN 
     SIZE 9.86 BY .69
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F-Fecha2 AS DATE FORMAT "99/99/9999":U 
     LABEL "Hasta" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .69
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE RB-BEGIN-PAGE AS INTEGER FORMAT "ZZZ9":U INITIAL 1 
     LABEL "P�gina Desde" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .69
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE RB-END-PAGE AS INTEGER FORMAT "ZZZ9":U INITIAL 9999 
     LABEL "P�gina Hasta" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .69
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE RB-NUMBER-COPIES AS INTEGER FORMAT "ZZZ9":U INITIAL 1 
     LABEL "No. Copias" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .69
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE RB-OUTPUT-FILE AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 33.86 BY .69
     BGCOLOR 15 FGCOLOR 0 FONT 4 NO-UNDO.

DEFINE VARIABLE x-auxiliar AS CHARACTER FORMAT "X(11)":U 
     LABEL "Auxiliar" 
     VIEW-AS FILL-IN 
     SIZE 13.86 BY .69
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE x-Clasificacion AS CHARACTER FORMAT "X(256)":U 
     LABEL "Clasificacion" 
     VIEW-AS FILL-IN 
     SIZE 5.43 BY .69
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE x-CodOpe AS CHARACTER FORMAT "X(3)":U 
     LABEL "Operacion" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .69
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE x-cuenta AS CHARACTER FORMAT "X(10)":U 
     LABEL "Desde la Cuenta" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .69
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE x-cuenta-2 AS CHARACTER FORMAT "X(10)":U 
     LABEL "Hasta la Cuenta" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .69
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE X-DIV AS CHARACTER FORMAT "X(256)":U 
     LABEL "Divisi�n" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .69
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE R-TipRep AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Movimiento Total  : Cuenta - Auxiliar", 1,
"Cuentas Pendientes: Cuenta - Auxiliar", 2,
"Cuentas Pendientes: Auxiliar - Cuenta", 3,
"Cuentas Pendientes: Vencimiento", 4
     SIZE 28 BY 3.35 NO-UNDO.

DEFINE VARIABLE RADIO-SET-1 AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Pantalla", 2,
"Impresora", 1,
"Archivo", 3
     SIZE 12 BY 3 NO-UNDO.

DEFINE VARIABLE x-codmon AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Soles", 1,
"D�lares", 2
     SIZE 9 BY 1.23 NO-UNDO.

DEFINE RECTANGLE RECT-25
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 17.57 BY 2.19.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 78.29 BY 7.5.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 78.29 BY 4.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 78.29 BY 2.

DEFINE VARIABLE T-Fechas AS LOGICAL INITIAL no 
     LABEL "Rango de Fechas de Movimiento" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY .69 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     C-Mes AT ROW 1.19 COL 41 COLON-ALIGNED
     R-TipRep AT ROW 1.31 COL 1.72 NO-LABEL
     x-codmon AT ROW 1.46 COL 65.14 NO-LABEL
     x-CodOpe AT ROW 2.15 COL 41 COLON-ALIGNED
     X-DIV AT ROW 2.92 COL 41.14 COLON-ALIGNED
     x-cuenta AT ROW 3.77 COL 41.14 COLON-ALIGNED
     x-Clasificacion AT ROW 4.65 COL 29 COLON-ALIGNED
     x-auxiliar AT ROW 4.69 COL 41.14 COLON-ALIGNED
     x-cuenta-2 AT ROW 5.54 COL 41.14 COLON-ALIGNED
     T-Fechas AT ROW 6.58 COL 12
     F-Fecha1 AT ROW 6.58 COL 41 COLON-ALIGNED
     F-Fecha2 AT ROW 7.42 COL 41 COLON-ALIGNED
     RADIO-SET-1 AT ROW 9.77 COL 2 NO-LABEL
     RB-NUMBER-COPIES AT ROW 9.77 COL 64 COLON-ALIGNED
     B-impresoras AT ROW 10.77 COL 15
     RB-BEGIN-PAGE AT ROW 10.77 COL 64 COLON-ALIGNED
     b-archivo AT ROW 11.77 COL 15
     RB-END-PAGE AT ROW 11.77 COL 64 COLON-ALIGNED
     RB-OUTPUT-FILE AT ROW 12.04 COL 19 COLON-ALIGNED NO-LABEL
     B-imprime AT ROW 13.92 COL 12.43
     B-cancela AT ROW 13.92 COL 24.86
     BUTTON-EXCELL AT ROW 13.92 COL 37.14 WIDGET-ID 4
     " Configuraci�n de Impresi�n" VIEW-AS TEXT
          SIZE 78.29 BY .62 AT ROW 8.5 COL 1
          BGCOLOR 1 FGCOLOR 15 
     RECT-4 AT ROW 1 COL 1
     RECT-25 AT ROW 4.31 COL 59.57
     RECT-6 AT ROW 13.31 COL 1
     RECT-5 AT ROW 9.27 COL 1
     SPACE(0.13) SKIP(2.10)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 FONT 4
         TITLE "An�lisis de Cuentas".


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

/* SETTINGS FOR FILL-IN F-Fecha1 IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F-Fecha2 IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RB-OUTPUT-FILE IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       RB-OUTPUT-FILE:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON GO OF FRAME DIALOG-1 /* An�lisis de Cuentas */
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
   ASSIGN  x-cuenta
           x-cuenta-2
           x-codmon
           x-auxiliar
           X-div
           c-mes
           R-TipRep
           x-Clasificacion
           F-Fecha1 F-Fecha2 T-Fechas
           x-CodOpe.

   IF T-Fechas THEN DO:
      IF F-Fecha1 = ? THEN  APPLY 'ENTRY' TO F-Fecha1.
      IF F-Fecha2 = ? THEN  APPLY 'ENTRY' TO F-Fecha2.
   END.   
   IF NOT CAN-FIND(integral.cb-ctas WHERE 
                     cb-ctas.codcia = cb-codcia AND
                     cb-ctas.codcta = x-cuenta)
        THEN DO: 
              BELL.
              MESSAGE "Cuenta Inicial no existe " SKIP
                    "Intente otra vez" VIEW-AS ALERT-BOX ERROR.
              APPLY "ENTRY" TO x-cuenta.
              RETURN NO-APPLY.
            END.
    IF NOT CAN-FIND(integral.cb-ctas WHERE 
                     cb-ctas.codcia = cb-codcia AND
                     cb-ctas.codcta = x-cuenta-2)
        THEN DO: 
              BELL.
              MESSAGE "Cuenta Final no existe " SKIP
                    "Intente otra vez" VIEW-AS ALERT-BOX ERROR.
              APPLY "ENTRY" TO x-cuenta-2.
              RETURN NO-APPLY.
            END.        
    x-cuenta-2 = x-cuenta-2 + "ZZZZZZZZZZZZZZZZZZ".
    
    IF X-CUENTA > X-CUENTA-2 THEN DO:
       MESSAGE "Rango de Cuentas no Valido"
       VIEW-AS ALERT-BOX.
    APPLY "ENTRY" TO x-cuenta.
    RETURN NO-APPLY.
    END.
    
    RUN bin/_mes.p ( INPUT c-mes , 1,  OUTPUT pinta-mes ).
    pinta-mes = "MES DE " + pinta-mes + " DE " + STRING( s-periodo , "9999" ).
    IF x-codmon = 1 THEN x-expres = "(EXPRESADO EN NUEVOS SOLES)".
    ELSE x-expres = "(EXPRESADO EN DOLARES)".
    CASE r-tiprep :
    WHEN 1 THEN x-nombala = "MOVIMIENTO TOTAL CUENTA - AUXILIAR".
    WHEN 2 THEN x-nombala = "CUENTAS PENDIENTES CUENTA - AUXILIAR".
    WHEN 3 THEN x-nombala = "CUENTAS PENDIENTES AUXILIAR - CUENTA".
    END CASE.
    
    RUN bin/_centrar.p ( INPUT pinta-mes, 40 , OUTPUT pinta-mes).
    RUN bin/_centrar.p ( INPUT x-expres,  40 , OUTPUT x-expres ).
    RUN bin/_centrar.p ( INPUT x-nombala, 50 , OUTPUT x-nombala).
           
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


&Scoped-define SELF-NAME BUTTON-EXCELL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-EXCELL DIALOG-1
ON CHOOSE OF BUTTON-EXCELL IN FRAME DIALOG-1 /* EXCEL */
DO:
    ASSIGN  x-cuenta
            x-cuenta-2
            x-codmon
            x-auxiliar
            X-div
            c-mes
            R-TipRep
            x-Clasificacion
            F-Fecha1 F-Fecha2 T-Fechas
            x-CodOpe.

    IF T-Fechas THEN DO:
       IF F-Fecha1 = ? THEN  APPLY 'ENTRY' TO F-Fecha1.
       IF F-Fecha2 = ? THEN  APPLY 'ENTRY' TO F-Fecha2.
    END.   
    IF NOT CAN-FIND(integral.cb-ctas WHERE 
                      cb-ctas.codcia = cb-codcia AND
                      cb-ctas.codcta = x-cuenta)
         THEN DO: 
               BELL.
               MESSAGE "Cuenta Inicial no existe " SKIP
                     "Intente otra vez" VIEW-AS ALERT-BOX ERROR.
               APPLY "ENTRY" TO x-cuenta.
               RETURN NO-APPLY.
             END.
     IF NOT CAN-FIND(integral.cb-ctas WHERE 
                      cb-ctas.codcia = cb-codcia AND
                      cb-ctas.codcta = x-cuenta-2)
         THEN DO: 
               BELL.
               MESSAGE "Cuenta Final no existe " SKIP
                     "Intente otra vez" VIEW-AS ALERT-BOX ERROR.
               APPLY "ENTRY" TO x-cuenta-2.
               RETURN NO-APPLY.
             END.        
     x-cuenta-2 = x-cuenta-2 + "ZZZZZZZZZZZZZZZZZZ".

     IF X-CUENTA > X-CUENTA-2 THEN DO:
        MESSAGE "Rango de Cuentas no Valido"
        VIEW-AS ALERT-BOX.
     APPLY "ENTRY" TO x-cuenta.
     RETURN NO-APPLY.
     END.

     RUN bin/_mes.p ( INPUT c-mes , 1,  OUTPUT pinta-mes ).
     pinta-mes = "MES DE " + pinta-mes + " DE " + STRING( s-periodo , "9999" ).
     IF x-codmon = 1 THEN x-expres = "(EXPRESADO EN NUEVOS SOLES)".
     ELSE x-expres = "(EXPRESADO EN DOLARES)".
     CASE r-tiprep :
     WHEN 1 THEN x-nombala = "MOVIMIENTO TOTAL CUENTA - AUXILIAR".
     WHEN 2 THEN x-nombala = "CUENTAS PENDIENTES CUENTA - AUXILIAR".
     WHEN 3 THEN x-nombala = "CUENTAS PENDIENTES AUXILIAR - CUENTA".
     END CASE.

     RUN bin/_centrar.p ( INPUT pinta-mes, 40 , OUTPUT pinta-mes).
     RUN bin/_centrar.p ( INPUT x-expres,  40 , OUTPUT x-expres ).
     RUN bin/_centrar.p ( INPUT x-nombala, 50 , OUTPUT x-nombala).

     c-Copias = 1.
     IF T-Fechas THEN RUN Carga-Temporal1.
     ELSE RUN carga-temporal.
     CASE R-TipRep:
         WHEN 1 OR WHEN 2 THEN RUN Excel-1.
         WHEN 3 THEN RUN Excel-3.
         WHEN 4 THEN RUN Excel-4.
     END CASE.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME F-Fecha1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-Fecha1 DIALOG-1
ON LEAVE OF F-Fecha1 IN FRAME DIALOG-1 /* Desde */
DO:
  IF INPUT F-Fecha1 = ? THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME F-Fecha2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-Fecha2 DIALOG-1
ON LEAVE OF F-Fecha2 IN FRAME DIALOG-1 /* Hasta */
DO:
  IF INPUT F-Fecha2 = ? THEN RETURN NO-APPLY.
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


&Scoped-define SELF-NAME T-Fechas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-Fechas DIALOG-1
ON VALUE-CHANGED OF T-Fechas IN FRAME DIALOG-1 /* Rango de Fechas de Movimiento */
DO:
  ASSIGN T-Fechas.
  DO WITH FRAME {&FRAME-NAME}:
     F-Fecha1:SENSITIVE = T-Fechas.
     F-Fecha2:SENSITIVE = T-Fechas.
     C-mes:SENSITIVE = NOT T-Fechas.
  END.
     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME x-auxiliar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL x-auxiliar DIALOG-1
ON ENTRY OF x-auxiliar IN FRAME DIALOG-1 /* Auxiliar */
DO:
  
   X-CLFAUX = "".
   FIND CB-CTAS WHERE CB-CTAS.CODCIA = CB-CODCIA AND
                      CB-CTAS.CODCTA = X-CUENTA:SCREEN-VALUE
                      NO-LOCK NO-ERROR.
   IF AVAIL CB-CTAS THEN X-CLFAUX = CB-CTAS.CLFAUX.                   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL x-auxiliar DIALOG-1
ON MOUSE-SELECT-DBLCLICK OF x-auxiliar IN FRAME DIALOG-1 /* Auxiliar */
OR "F8" OF X-AUXILIAR DO:
    DEF VAR T-ROWID AS ROWID.
    DEF VAR T-RECID AS RECID.  
    
    IF x-Clasificacion <> "" THEN X-CLFAUX = x-Clasificacion.
    IF X-CLFAUX = "" THEN RETURN.
     
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
ON LEAVE OF x-Clasificacion IN FRAME DIALOG-1 /* Clasificacion */
DO:
  ASSIGN x-Clasificacion.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL x-Clasificacion DIALOG-1
ON MOUSE-SELECT-DBLCLICK OF x-Clasificacion IN FRAME DIALOG-1 /* Clasificacion */
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


&Scoped-define SELF-NAME x-CodOpe
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL x-CodOpe DIALOG-1
ON LEFT-MOUSE-DBLCLICK OF x-CodOpe IN FRAME DIALOG-1 /* Operacion */
OR F8 OF x-CodOpe
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


&Scoped-define SELF-NAME x-cuenta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL x-cuenta DIALOG-1
ON F8 OF x-cuenta IN FRAME DIALOG-1 /* Desde la Cuenta */
OR "MOUSE-SELECT-DBLCLICK":U OF X-CUENTA DO:
  
  {ADM/H-CTAS02.I NO SELF}
   
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL x-cuenta DIALOG-1
ON LEAVE OF x-cuenta IN FRAME DIALOG-1 /* Desde la Cuenta */
DO:  
   X-CLFAUX = "".
   FIND CB-CTAS WHERE CB-CTAS.CODCIA = CB-CODCIA AND
                      CB-CTAS.CODCTA = X-CUENTA:SCREEN-VALUE
                      NO-LOCK NO-ERROR.
   IF AVAIL CB-CTAS THEN X-CLFAUX = CB-CTAS.CLFAUX.                   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME x-cuenta-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL x-cuenta-2 DIALOG-1
ON F8 OF x-cuenta-2 IN FRAME DIALOG-1 /* Hasta la Cuenta */
OR "MOUSE-SELECT-DBLCLICK":U OF X-CUENTA-2 DO:
  
  {ADM/H-CTAS02.I NO SELF}
   
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL x-cuenta-2 DIALOG-1
ON LEAVE OF x-cuenta-2 IN FRAME DIALOG-1 /* Hasta la Cuenta */
DO:
  
   X-CLFAUX = "".
   FIND CB-CTAS WHERE CB-CTAS.CODCIA = CB-CODCIA AND
                      CB-CTAS.CODCTA = X-CUENTA:SCREEN-VALUE
                      NO-LOCK NO-ERROR.
   IF AVAIL CB-CTAS THEN X-CLFAUX = CB-CTAS.CLFAUX.                   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME X-DIV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL X-DIV DIALOG-1
ON MOUSE-SELECT-DBLCLICK OF X-DIV IN FRAME DIALOG-1 /* Divisi�n */
OR "F8" OF X-DIV DO:
  {CBD/H-DIVI01.I NO SELF}
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
c-mes = s-nromes.
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  PTO                  = SESSION:SET-WAIT-STATE("").    
  l-immediate-display  = SESSION:IMMEDIATE-DISPLAY.

  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
  RUN disable_UI.

  FRAME F-Mensaje:TITLE =  FRAME DIALOG-1:TITLE.

  SESSION:IMMEDIATE-DISPLAY = YES.
  STATUS INPUT OFF.  

  DO WITH FRAME {&FRAME-NAME}:
     F-Fecha1:SENSITIVE = FALSE.
     F-Fecha2:SENSITIVE = FALSE.
  END.
  
  DO c-Copias = 1 to P-copias ON ERROR UNDO, LEAVE
                                ON STOP UNDO, LEAVE:
        OUTPUT STREAM report TO NUL PAGED PAGE-SIZE 1000.
        c-Pagina = 0.
        IF T-Fechas THEN RUN IMP-FECHAS.
        ELSE RUN IMPRIMIR.
        OUTPUT STREAM report CLOSE.        
  END.
  OUTPUT STREAM report CLOSE.        
  SESSION:IMMEDIATE-DISPLAY = l-immediate-display.
  HIDE FRAME F-Mensaje.  
  IF NOT LASTKEY = KEYCODE("ESC") AND P-select = 2 THEN DO: 
     RUN bin/_vcat.p ( P-archivo ). 
  END.      
  RETURN.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE borra DIALOG-1 
PROCEDURE borra :
for each t-report where
         t-report.codcta = cb-dmov.codcta and
         t-report.codaux = cb-dmov.codaux and
         t-report.coddoc = cb-dmov.coddoc and
         t-report.nrodoc = cb-dmov.nrodoc :
    delete t-report.                   
                                  
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Carga-Temporal DIALOG-1 
PROCEDURE Carga-Temporal :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR X-MONCTA AS INTEGER.
DEF VAR CON-CTA  AS INTEGER.
DEF VAR Y-SALDO  AS DECIMAL.
DEF VAR K AS INTEGER.
DEF VAR T-FECHA AS DATE.
DEF VAR X-VOUCHER AS CHAR.   
DEF VAR X-S1 AS DECIMAL.
DEF VAR X-S2 AS DECIMAL.

/*ML01*/ DEFINE VARIABLE dFechAst LIKE cb-cmov.fchast NO-UNDO.

FOR EACH t-report:
    DELETE t-report.
END.

    K = 0.
    Y-SALDO = 0.
    DISPLAY "Ordenando Informaci�n Requerida" @ x-mensaje with frame f-auxiliar.
    pause 0.
    FOR EACH cb-dmov NO-LOCK WHERE
             cb-dmov.codcia    =      s-codcia           AND
             cb-dmov.periodo   =      s-periodo          AND
             cb-dmov.codcta   >=      (x-cuenta)         AND
             cb-dmov.codcta   <=      (x-cuenta-2)       AND
             cb-dmov.codaux   BEGINS  (x-auxiliar)       AND
             cb-dmov.nromes   <=      c-mes              AND
             cb-dmov.coddiv   BEGINS  (x-div)            AND
             cb-dmov.clfaux   BEGINS  (x-Clasificacion)  AND
             (x-CodOpe = '' OR cb-dmov.codope = x-codope)
            BREAK BY cb-dmov.codcia
                  BY cb-dmov.periodo 
                  BY cb-dmov.Codcta
                  BY cb-dmov.codaux
                  BY cb-dmov.coddoc
                  BY cb-dmov.nrodoc 
                  BY cb-dmov.nromes 
                  BY cb-dmov.fchdoc :
                  
        IF R-TipRep >= 2 THEN DO:     
           IF FIRST-OF(cb-dmov.codcta) then do:
              FIND cb-ctas WHERE cb-ctas.codcia = cb-codcia
                             AND cb-ctas.codcta = cb-dmov.codcta
                             NO-LOCK NO-ERROR.
              IF AVAILABLE cb-ctas THEN x-MoNcta = cb-ctas.codmon.
           END.
           if first-of(cb-dmov.nrodoc) then assign x-s1 = 0
                                                   x-s2 = 0.
           if cb-dmov.tpomov then assign x-s1 = x-s1 - cb-dmov.impmn1
                                         x-s2 = x-s2 - cb-dmov.impmn2.
                             else assign x-s1 = x-s1 + cb-dmov.impmn1
                                         x-s2 = x-s2 + cb-dmov.impmn2.
        END.         
        IF FIRST-OF(CB-DMOV.nrodoc) and FIRST-OF(CB-DMOV.FCHDOC) THEN 
           ASSIGN T-FECHA = CB-DMOV.FCHDOC
                  X-VOUCHER = cb-dmov.codope + cb-dmov.nroast.
        x-glodoc = cb-dmov.glodoc.
        IF x-glodoc = "" THEN DO:
           FIND cb-cmov WHERE cb-cmov.codcia   = cb-dmov.codcia
                            AND cb-cmov.periodo = cb-dmov.periodo 
                            AND cb-cmov.nromes  = cb-dmov.nromes
                            AND cb-cmov.codope  = cb-dmov.codope
                            AND cb-cmov.nroast  = cb-dmov.nroast
                        NO-LOCK NO-ERROR.
           IF AVAILABLE cb-cmov THEN
               x-glodoc = cb-cmov.notast.
        END.
        IF x-glodoc = "" THEN DO:
            find cb-ctas WHERE cb-ctas.codcta = cb-dmov.codcta
                                AND cb-ctas.CodCia = cb-codcia
                            NO-LOCK NO-ERROR.                      
                IF AVAILABLE cb-ctas THEN 
                    x-glodoc = cb-ctas.nomcta.
        END.
        
        IF NOT tpomov THEN 
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
       
        ELSE 
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
       
        IF cb-dmov.codmon = x-codmon THEN 
            x-importe = 0.
        ELSE
            CASE cb-dmov.codmon:
            WHEN 1 THEN 
                x-importe = cb-dmov.ImpMn1.
            WHEN 2 THEN 
                x-importe = cb-dmov.ImpMn2.
            END CASE.
       
        CREATE T-REPORT.
        ASSIGN T-REPORT.CODCTA    = CB-DMOV.CODCTA
               T-REPORT.CLFAUX    = CB-DMOV.CLFAUX
               T-REPORT.CODAUX    = CB-DMOV.CODAUX
               T-REPORT.FCHDOC    = CB-DMOV.FCHDOC
               T-REPORT.CODDIV    = cb-dmov.coddiv
               T-REPORT.CCO       = cb-dmov.cco
               T-REPORT.NROAST    = cb-dmov.nroast
               T-REPORT.CODOPE    = cb-dmov.codope
               T-REPORT.NROMES    = cb-dmov.nromes
               T-REPORT.CODDOC    = cb-dmov.coddoc
               T-REPORT.NRODOC    = cb-dmov.nrodoc
               T-REPORT.NROREF    = cb-dmov.nroref
               T-REPORT.FCHVTO    = cb-dmov.fchvto
               T-REPORT.GLODOC    = x-glodoc
               T-REPORT.T-IMPORTE = x-importe
               T-REPORT.T-DEBE    = x-debe
               T-REPORT.T-HABER   = x-haber
               T-REPORT.FECHA-ID  = T-FECHA
               T-REPORT.VOUCHER   = X-VOUCHER.

/*ML01* Inicio de bloque */
        IF cb-dmov.codope = "002" THEN DO:
            FIND cb-cmov
                WHERE cb-cmov.codcia   = cb-dmov.codcia
                AND cb-cmov.periodo = cb-dmov.periodo 
                AND cb-cmov.nromes  = cb-dmov.nromes
                AND cb-cmov.codope  = cb-dmov.codope
                AND cb-cmov.nroast  = cb-dmov.nroast
                NO-LOCK NO-ERROR.
            IF AVAILABLE cb-cmov THEN T-REPORT.FCHDOC = cb-cmov.fchast.
        END.
/*ML01* Fin de bloque */

        IF R-TipRep >= 2 AND 
           LAST-OF(CB-DMOV.NRODOC) THEN DO:
           case x-moncta :
                when 1 then if  x-s1 = 0 then run borra.
                when 2 then if  x-s2 = 0 then run borra.
                otherwise do:
                   case x-codmon :
                        when 1 then if  x-s1 = 0 then run borra.
                        when 2 then if  x-s2 = 0 then run borra.
                   end case .
                end.
           end case.
        END.   
    END. /* FIN DEL FOR EACH */    
    HIDE FRAME f-Auxiliar.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Carga-Temporal1 DIALOG-1 
PROCEDURE Carga-Temporal1 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR X-MONCTA  AS INTEGER NO-UNDO.
DEF VAR CON-CTA   AS INTEGER NO-UNDO.
DEF VAR Y-SALDO   AS DECIMAL NO-UNDO.
DEF VAR K         AS INTEGER NO-UNDO.
DEF VAR T-FECHA   AS DATE    NO-UNDO.
DEF VAR X-VOUCHER AS CHAR    NO-UNDO.   
DEF VAR X-S1      AS DECIMAL NO-UNDO.
DEF VAR X-S2      AS DECIMAL NO-UNDO.
DEF VAR X-MES1    AS INTEGER NO-UNDO.
DEF VAR X-MES2    AS INTEGER NO-UNDO.
X-MES1 = MONTH(F-FECHA1).
X-MES2 = MONTH(F-FECHA2).
IF C-COPIAS = 1 THEN  DO:
    K = 0.
    Y-SALDO = 0.
    DISPLAY "Ordenando Informaci�n Requerida" @ x-mensaje with frame f-auxiliar.
    pause 0.
    FOR EACH cb-dmov NO-LOCK WHERE
             cb-dmov.codcia    =      s-codcia           AND
             cb-dmov.periodo   =      s-periodo          AND
             cb-dmov.coddiv   BEGINS  (x-div)            AND
             cb-dmov.codcta   >=      (x-cuenta)         AND
             cb-dmov.codcta   <=      (x-cuenta-2)       AND
             cb-dmov.codaux   BEGINS  (x-auxiliar)       AND
             cb-dmov.nromes   >=      x-mes1             AND
             cb-dmov.nromes   <=      x-mes2             AND
             (x-CodOpe = '' OR cb-dmov.codope = x-codope) AND
             cb-dmov.clfaux   BEGINS  (x-Clasificacion)  
            BREAK BY cb-dmov.codcia
                  BY cb-dmov.periodo 
                  BY cb-dmov.Codcta
                  BY cb-dmov.codaux
                  BY cb-dmov.coddoc
                  BY cb-dmov.nrodoc 
                  BY cb-dmov.nromes 
                  BY cb-dmov.fchdoc :
     /*IF cb-dmov.fchdoc >= F-Fecha1 AND cb-dmov.fchdoc <= F-Fecha2 THEN DO:*/
        IF R-TipRep >= 2 THEN DO:     
           IF FIRST-OF(cb-dmov.codcta) then do:
              FIND cb-ctas WHERE cb-ctas.codcia = cb-codcia
                             AND cb-ctas.codcta = cb-dmov.codcta
                             NO-LOCK NO-ERROR.
              IF AVAILABLE cb-ctas THEN x-MoNcta = cb-ctas.codmon.
           END.
           if first-of(cb-dmov.nrodoc) then assign x-s1 = 0
                                                   x-s2 = 0.
           if cb-dmov.tpomov then assign x-s1 = x-s1 - cb-dmov.impmn1
                                         x-s2 = x-s2 - cb-dmov.impmn2.
                             else assign x-s1 = x-s1 + cb-dmov.impmn1
                                         x-s2 = x-s2 + cb-dmov.impmn2.
        END.         
        IF FIRST-OF(CB-DMOV.nrodoc) and FIRST-OF(CB-DMOV.FCHDOC) THEN 
           ASSIGN T-FECHA = CB-DMOV.FCHDOC
                  X-VOUCHER = cb-dmov.codope + cb-dmov.nroast.
        x-glodoc = cb-dmov.glodoc.
        IF x-glodoc = "" THEN DO:
           FIND cb-cmov WHERE cb-cmov.codcia   = cb-dmov.codcia
                            AND cb-cmov.periodo = cb-dmov.periodo 
                            AND cb-cmov.nromes  = cb-dmov.nromes
                            AND cb-cmov.codope  = cb-dmov.codope
                            AND cb-cmov.nroast  = cb-dmov.nroast
                        NO-LOCK NO-ERROR.
           IF AVAILABLE cb-cmov THEN
               x-glodoc = cb-cmov.notast.
        END.
        IF x-glodoc = "" THEN DO:
            find cb-ctas WHERE cb-ctas.codcta = cb-dmov.codcta
                                AND cb-ctas.CodCia = cb-codcia
                            NO-LOCK NO-ERROR.                      
                IF AVAILABLE cb-ctas THEN 
                    x-glodoc = cb-ctas.nomcta.
        END.
        IF NOT tpomov THEN 
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
       
        ELSE 
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
       
        IF cb-dmov.codmon = x-codmon THEN 
            x-importe = 0.
        ELSE
            CASE cb-dmov.codmon:
            WHEN 1 THEN 
                x-importe = cb-dmov.ImpMn1.
            WHEN 2 THEN 
                x-importe = cb-dmov.ImpMn2.
            END CASE.
       
        CREATE T-REPORT.
        ASSIGN T-REPORT.CODCTA    = CB-DMOV.CODCTA
               T-REPORT.CLFAUX    = CB-DMOV.CLFAUX
               T-REPORT.CODAUX    = CB-DMOV.CODAUX
               T-REPORT.FCHDOC    = CB-DMOV.FCHDOC
               T-REPORT.CODDIV    = cb-dmov.coddiv
               T-REPORT.CCO       = cb-dmov.cco
               T-REPORT.NROAST    = cb-dmov.nroast
               T-REPORT.CODOPE    = cb-dmov.codope
               T-REPORT.NROMES    = cb-dmov.nromes
               T-REPORT.CODDOC    = cb-dmov.coddoc
               T-REPORT.NRODOC    = cb-dmov.nrodoc
               T-REPORT.NROREF    = cb-dmov.nroref
               T-REPORT.FCHVTO    = cb-dmov.fchvto
               T-REPORT.GLODOC    = x-glodoc
               T-REPORT.T-IMPORTE = x-importe
               T-REPORT.T-DEBE    = x-debe
               T-REPORT.T-HABER   = x-haber
               T-REPORT.FECHA-ID  = T-FECHA
               T-REPORT.VOUCHER   = X-VOUCHER.
        IF R-TipRep >= 2 AND 
           LAST-OF(CB-DMOV.NRODOC) THEN DO:
           case x-moncta :
                when 1 then if  x-s1 = 0 then run borra.
                when 2 then if  x-s2 = 0 then run borra.
                otherwise do:
                   case x-codmon :
                        when 1 then if  x-s1 = 0 then run borra.
                        when 2 then if  x-s2 = 0 then run borra.
                   end case .
                end.
           end case.
        END.   
     /*END.   */
    END. /* FIN DEL FOR EACH */    
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  DISPLAY C-Mes R-TipRep x-codmon x-CodOpe X-DIV x-cuenta x-Clasificacion 
          x-auxiliar x-cuenta-2 T-Fechas F-Fecha1 F-Fecha2 RADIO-SET-1 
          RB-NUMBER-COPIES RB-BEGIN-PAGE RB-END-PAGE 
      WITH FRAME DIALOG-1.
  ENABLE RECT-4 RECT-25 RECT-6 RECT-5 C-Mes R-TipRep x-codmon x-CodOpe X-DIV 
         x-cuenta x-Clasificacion x-auxiliar x-cuenta-2 T-Fechas RADIO-SET-1 
         RB-NUMBER-COPIES B-impresoras RB-BEGIN-PAGE RB-END-PAGE B-imprime 
         B-cancela BUTTON-EXCELL 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Excel-1 DIALOG-1 
PROCEDURE Excel-1 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR X-MONCTA AS INTEGER.
    DEF VAR K AS INTEGER.
    DEF VAR CON-CTA  AS INTEGER.
    DEF VAR Y-SALDO  AS DECIMAL.

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

    /* titulos */
    iCount = iCount + 1.
    cColumn = STRING(iCount).
    cRange = "J" + cColumn.
    chWorkSheet:Range(cRange):Value = "ANALISIS DE CUENTAS".
    iCount = iCount + 1.
    cColumn = STRING(iCount).
    cRange = "J" + cColumn.
    chWorkSheet:Range(cRange):Value = pinta-mes.
    iCount = iCount + 1.
    cColumn = STRING(iCount).
    cRange = "J" + cColumn.
    chWorkSheet:Range(cRange):Value = x-nombala.
    iCount = iCount + 1.
    cColumn = STRING(iCount).
    cRange = "J" + cColumn.
    chWorkSheet:Range(cRange):Value = x-expres.
    iCount = iCount + 2.
    /* set the column names for the Worksheet */
    iCount = iCount + 1.
    cColumn = STRING(iCount).
    cRange = "A" + cColumn.
    chWorkSheet:Range(cRange):Value = "Fecha Doc.".
    cRange = "B" + cColumn.
    chWorkSheet:Range(cRange):Value = "Division".
    cRange = "C" + cColumn.
    chWorkSheet:Range(cRange):Value = "Centro Costo".
    cRange = "D" + cColumn.
    chWorkSheet:Range(cRange):Value = "Comprobante".
    cRange = "E" + cColumn.
    chWorkSheet:Range(cRange):Value = "Operacion".
    cRange = "F" + cColumn.
    chWorkSheet:Range(cRange):Value = "Nro. Mes".
    cRange = "G" + cColumn.
    chWorkSheet:Range(cRange):Value = "Cod Doc".
    cRange = "H" + cColumn.
    chWorkSheet:Range(cRange):Value = "Nro Doc".
    cRange = "I" + cColumn.
    chWorkSheet:Range(cRange):Value = "Fecha Vto.".
    cRange = "J" + cColumn.
    chWorkSheet:Range(cRange):Value = "Ref".
    cRange = "K" + cColumn.
    chWorkSheet:Range(cRange):Value = "Detalle".
    cRange = "L" + cColumn.
    chWorkSheet:Range(cRange):Value = "Importe".
    cRange = "M" + cColumn.
    chWorkSheet:Range(cRange):Value = "Cargos".
    cRange = "N" + cColumn.
    chWorkSheet:Range(cRange):Value = "Abonos".
    cRange = "O" + cColumn.
    chWorkSheet:Range(cRange):Value = "Saldo".

    FOR EACH T-REPORT NO-LOCK USE-INDEX IDX01
          BREAK  BY T-REPORT.CODCTA
                 BY T-REPORT.CODAUX
                 BY T-REPORT.VOUCHER
                 BY T-REPORT.FECHA-ID
                 BY T-REPORT.CODDOC
                 BY T-REPORT.NRODOC
                 BY T-REPORT.CODOPE
                 BY T-REPORT.NROAST
                 BY T-REPORT.FCHDOC:
        K = K + 1.
        IF K = 1 THEN DO:
           VIEW FRAME F-Mensaje.   
           PAUSE 0.           
        END.          
        IF FIRST-OF (T-REPORT.nrodoc) THEN x-conreg = 0.
        IF FIRST-OF (T-REPORT.codcta) THEN DO:
           i = 0.
           CON-CTA = CON-CTA + 1.
           find cb-ctas WHERE 
                cb-ctas.codcia = cb-codcia AND
                cb-ctas.codcta = T-REPORT.codcta
                 NO-LOCK NO-ERROR.
           IF AVAILABLE cb-ctas THEN DO:
              x-nomcta = T-REPORT.codcta + " " + cb-ctas.nomcta.
              iCount = iCount + 1.
              cColumn = STRING(iCount).
              cRange = "A" + cColumn.
              chWorkSheet:Range(cRange):Value = x-nomcta.
              iCount = iCount + 1.
           END.
        END.
        IF FIRST-OF (T-REPORT.codaux) THEN DO:
           RUN T-nom-aux.
           x-nomaux = T-REPORT.codaux + " " + x-nomaux.
           iCount = iCount + 1.
           cColumn = STRING(iCount).
           cRange = "A" + cColumn.
           chWorkSheet:Range(cRange):Value = x-nomaux.
           iCount = iCount + 1.
           x-condoc = 0.
        END.
        ACCUMULATE T-debe  (SUB-TOTAL BY T-REPORT.nrodoc).
        ACCUMULATE T-haber (SUB-TOTAL BY T-REPORT.nrodoc).
        ACCUMULATE T-debe  (SUB-TOTAL BY T-REPORT.codcta).
        ACCUMULATE T-haber (SUB-TOTAL BY T-REPORT.codcta).
        ACCUMULATE T-debe  (SUB-TOTAL BY T-REPORT.codaux).
        ACCUMULATE T-haber (SUB-TOTAL BY T-REPORT.codaux).
        ACCUMULATE T-debe  (TOTAL) .
        ACCUMULATE T-haber (TOTAL) .
        X-CONREG = X-CONREG + 1. 
        IF LAST-OF(T-REPORT.NRODOC) AND X-CONREG > 0 THEN DO:
           Y-SALDO =   (ACCUMULATE    SUB-TOTAL BY T-REPORT.NroDoc T-debe ) -
                       (ACCUMULATE    SUB-TOTAL BY T-REPORT.NroDoc T-haber) .
           iCount = iCount + 1.
           cColumn = STRING(iCount).
           cRange = "A" + cColumn.
           chWorkSheet:Range(cRange):Value = t-report.fchdoc.
           cRange = "B" + cColumn.
           chWorkSheet:Range(cRange):Value = "'" + t-report.coddiv.
           cRange = "C" + cColumn.
           chWorkSheet:Range(cRange):Value = "'" + t-report.cco.
           cRange = "D" + cColumn.
           chWorkSheet:Range(cRange):Value = "'" + t-report.nroast.
           cRange = "E" + cColumn.
           chWorkSheet:Range(cRange):Value = "'" + t-report.codope.
           cRange = "F" + cColumn.
           chWorkSheet:Range(cRange):Value = t-report.nromes.
           cRange = "G" + cColumn.
           chWorkSheet:Range(cRange):Value = "'" + t-report.coddoc.
           cRange = "H" + cColumn.
           chWorkSheet:Range(cRange):Value = t-report.nrodoc.
           cRange = "I" + cColumn.
           chWorkSheet:Range(cRange):Value = t-report.fchvto.
           cRange = "J" + cColumn.
           chWorkSheet:Range(cRange):Value = t-report.nroref.
           cRange = "K" + cColumn.
           chWorkSheet:Range(cRange):Value = t-report.glodoc.
           cRange = "L" + cColumn.
           chWorkSheet:Range(cRange):Value = (IF t-importe <> 0 THEN t-report.t-importe ELSE 0).
           cRange = "M" + cColumn.
           chWorkSheet:Range(cRange):Value = (IF t-debe <> 0 THEN t-report.t-debe ELSE 0).
           cRange = "N" + cColumn.
           chWorkSheet:Range(cRange):Value = (IF t-haber <> 0 THEN t-report.t-haber ELSE 0).
           cRange = "O" + cColumn.
           chWorkSheet:Range(cRange):Value = y-saldo.
           iCount = iCount + 1.
           X-CONDOC = X-CONDOC + 1.
        END.
        ELSE DO:
            iCount = iCount + 1.
            cColumn = STRING(iCount).
            cRange = "A" + cColumn.
            chWorkSheet:Range(cRange):Value = t-report.fchdoc.
            cRange = "B" + cColumn.
            chWorkSheet:Range(cRange):Value = "'" + t-report.coddiv.
            cRange = "C" + cColumn.                
            chWorkSheet:Range(cRange):Value = "'" + t-report.cco.
            cRange = "D" + cColumn.                
            chWorkSheet:Range(cRange):Value = "'" + t-report.nroast.
            cRange = "E" + cColumn.                
            chWorkSheet:Range(cRange):Value = "'" + t-report.codope.
            cRange = "F" + cColumn.                
            chWorkSheet:Range(cRange):Value = t-report.nromes.
            cRange = "G" + cColumn.                
            chWorkSheet:Range(cRange):Value = "'" + t-report.coddoc.
            cRange = "H" + cColumn.
            chWorkSheet:Range(cRange):Value = t-report.nrodoc.
            cRange = "I" + cColumn.
            chWorkSheet:Range(cRange):Value = t-report.fchvto.
            cRange = "J" + cColumn.
            chWorkSheet:Range(cRange):Value = t-report.nroref.
            cRange = "K" + cColumn.
            chWorkSheet:Range(cRange):Value = t-report.glodoc.
            cRange = "L" + cColumn.
            chWorkSheet:Range(cRange):Value = (IF t-importe <> 0 THEN t-report.t-importe ELSE 0).
            cRange = "M" + cColumn.
            chWorkSheet:Range(cRange):Value = (IF t-debe <> 0 THEN t-report.t-debe ELSE 0).
            cRange = "N" + cColumn.
            chWorkSheet:Range(cRange):Value = (IF t-haber <> 0 THEN t-report.t-haber ELSE 0).
        END.
        IF LAST-OF(T-REPORT.CODAUX) AND X-CONDOC > 0 THEN DO:
           Y-SALDO = (ACCUMULATE    SUB-TOTAL BY T-REPORT.CODAUX T-debe ) -
                     (ACCUMULATE    SUB-TOTAL BY T-REPORT.CODAUX T-haber) .
           iCount = iCount + 1.
           cColumn = STRING(iCount).
           cRange = "K" + cColumn.
           chWorkSheet:Range(cRange):Value = "Total Auxiliar".
           cRange = "M" + cColumn.
           chWorkSheet:Range(cRange):Value = (ACCUMULATE SUB-TOTAL BY T-REPORT.CODAUX T-debe).
           cRange = "N" + cColumn.
           chWorkSheet:Range(cRange):Value = (ACCUMULATE SUB-TOTAL BY T-REPORT.CODAUX T-haber).
           cRange = "O" + cColumn.
           chWorkSheet:Range(cRange):Value = y-saldo.
           iCount = iCount + 1.
        END.
        IF LAST-OF(T-REPORT.CODCTA)  THEN DO:
           Y-SALDO = (ACCUMULATE    SUB-TOTAL BY T-REPORT.CODCTA T-debe ) -
                     (ACCUMULATE    SUB-TOTAL BY T-REPORT.CODCTA T-haber) .
           iCount = iCount + 1.
           cColumn = STRING(iCount).
           cRange = "K" + cColumn.
           chWorkSheet:Range(cRange):Value = "Total Cuenta".
           cRange = "M" + cColumn.
           chWorkSheet:Range(cRange):Value = (ACCUMULATE SUB-TOTAL BY T-REPORT.CODCTA T-debe).
           cRange = "N" + cColumn.
           chWorkSheet:Range(cRange):Value = (ACCUMULATE SUB-TOTAL BY T-REPORT.CODCTA T-haber).
           cRange = "O" + cColumn.
           chWorkSheet:Range(cRange):Value = y-saldo.
           iCount = iCount + 1.
        END.
    END.
    IF CON-CTA > 1 THEN DO:
       Y-SALDO = (ACCUMULATE    TOTAL T-debe ) -
                 (ACCUMULATE    TOTAL T-haber) .
       iCount = iCount + 1.
       cColumn = STRING(iCount).
       cRange = "K" + cColumn.
       chWorkSheet:Range(cRange):Value = "Total General".
       cRange = "M" + cColumn.
       chWorkSheet:Range(cRange):Value = (ACCUMULATE TOTAL T-debe).
       cRange = "N" + cColumn.
       chWorkSheet:Range(cRange):Value = (ACCUMULATE TOTAL T-haber).
       cRange = "O" + cColumn.
       chWorkSheet:Range(cRange):Value = y-saldo.
       iCount = iCount + 1.
    END. /* FIN DEL CON-CTA */

    chExcelApplication:Visible = TRUE.

    /* release com-handles */
    RELEASE OBJECT chExcelApplication.      
    RELEASE OBJECT chWorkbook.
    RELEASE OBJECT chWorksheet.

    HIDE FRAME F-Mensaje.   
    
    MESSAGE
        "Proceso Terminado con suceso"
        VIEW-AS ALERT-BOX INFORMA.
    HIDE FRAME F-Auxiliar.  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Excel-3 DIALOG-1 
PROCEDURE Excel-3 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR X-MONCTA AS INTEGER.
    DEF VAR K AS INTEGER.
    DEF VAR CON-CTA  AS INTEGER.
    DEF VAR Y-SALDO  AS DECIMAL.

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

    /* titulos */
    iCount = iCount + 1.
    cColumn = STRING(iCount).
    cRange = "J" + cColumn.
    chWorkSheet:Range(cRange):Value = "ANALISIS DE CUENTAS".
    iCount = iCount + 1.
    cColumn = STRING(iCount).
    cRange = "J" + cColumn.
    chWorkSheet:Range(cRange):Value = pinta-mes.
    iCount = iCount + 1.
    cColumn = STRING(iCount).
    cRange = "J" + cColumn.
    chWorkSheet:Range(cRange):Value = x-nombala.
    iCount = iCount + 1.
    cColumn = STRING(iCount).
    cRange = "J" + cColumn.
    chWorkSheet:Range(cRange):Value = x-expres.
    iCount = iCount + 2.
    /* set the column names for the Worksheet */
    iCount = iCount + 1.
    cColumn = STRING(iCount).
    cRange = "A" + cColumn.
    chWorkSheet:Range(cRange):Value = "Fecha Doc.".
    cRange = "B" + cColumn.
    chWorkSheet:Range(cRange):Value = "Division".
    cRange = "C" + cColumn.
    chWorkSheet:Range(cRange):Value = "Centro Costo".
    cRange = "D" + cColumn.
    chWorkSheet:Range(cRange):Value = "Comprobante".
    cRange = "E" + cColumn.
    chWorkSheet:Range(cRange):Value = "Operacion".
    cRange = "F" + cColumn.
    chWorkSheet:Range(cRange):Value = "Nro. Mes".
    cRange = "G" + cColumn.
    chWorkSheet:Range(cRange):Value = "Cod Doc".
    cRange = "H" + cColumn.
    chWorkSheet:Range(cRange):Value = "Nro Doc".
    cRange = "I" + cColumn.
    chWorkSheet:Range(cRange):Value = "Fecha Vto.".
    cRange = "J" + cColumn.
    chWorkSheet:Range(cRange):Value = "Ref".
    cRange = "K" + cColumn.
    chWorkSheet:Range(cRange):Value = "Detalle".
    cRange = "L" + cColumn.
    chWorkSheet:Range(cRange):Value = "Importe".
    cRange = "M" + cColumn.
    chWorkSheet:Range(cRange):Value = "Cargos".
    cRange = "N" + cColumn.
    chWorkSheet:Range(cRange):Value = "Abonos".
    cRange = "O" + cColumn.
    chWorkSheet:Range(cRange):Value = "Saldo".

    FOR EACH T-REPORT NO-LOCK USE-INDEX IDX03
          BREAK  BY T-REPORT.CODAUX
                 BY T-REPORT.CODCTA
                 BY T-REPORT.VOUCHER
                 BY T-REPORT.FECHA-ID
                 BY T-REPORT.CODDOC
                 BY T-REPORT.NRODOC
                 BY T-REPORT.CODOPE
                 BY T-REPORT.NROAST
                 BY T-REPORT.FCHDOC:
        K = K + 1.
        IF K = 1 THEN DO:
           VIEW FRAME F-Mensaje.   
           PAUSE 0.           
        END.          
        IF FIRST-OF (T-REPORT.nrodoc) THEN x-conreg = 0.
        IF FIRST-OF (T-REPORT.codaux) THEN DO:
           RUN T-nom-aux.
           x-nomaux = T-REPORT.codaux + " " + x-nomaux.
           iCount = iCount + 1.
           cColumn = STRING(iCount).
           cRange = "A" + cColumn.
           chWorkSheet:Range(cRange):Value = x-nomaux.
           iCount = iCount + 1.
           CON-CTA = CON-CTA + 1.           
        END.
        IF FIRST-OF(T-REPORT.codcta) THEN DO:
            i = 0.
            x-condoc = 0.
            find cb-ctas WHERE cb-ctas.codcia = cb-codcia 
                AND cb-ctas.codcta = T-REPORT.codcta NO-LOCK NO-ERROR.

           IF AVAILABLE cb-ctas THEN DO:
              x-nomcta = T-REPORT.codcta + " " + cb-ctas.nomcta.
              iCount = iCount + 1.
              cColumn = STRING(iCount).
              cRange = "A" + cColumn.
              chWorkSheet:Range(cRange):Value = x-nomcta.
              iCount = iCount + 1.
           END.
        END.
        
        ACCUMULATE T-debe  (SUB-TOTAL BY T-REPORT.nrodoc).
        ACCUMULATE T-haber (SUB-TOTAL BY T-REPORT.nrodoc).
        ACCUMULATE T-debe  (SUB-TOTAL BY T-REPORT.codcta).
        ACCUMULATE T-haber (SUB-TOTAL BY T-REPORT.codcta).
        ACCUMULATE T-debe  (SUB-TOTAL BY T-REPORT.codaux).
        ACCUMULATE T-haber (SUB-TOTAL BY T-REPORT.codaux).
        ACCUMULATE T-debe  (TOTAL) .
        ACCUMULATE T-haber (TOTAL) .
        X-CONREG = X-CONREG + 1. 
        IF LAST-OF(T-REPORT.NRODOC) AND X-CONREG > 0 THEN DO:
           Y-SALDO =   (ACCUMULATE    SUB-TOTAL BY T-REPORT.NroDoc T-debe ) -
                       (ACCUMULATE    SUB-TOTAL BY T-REPORT.NroDoc T-haber) .
           iCount = iCount + 1.
           cColumn = STRING(iCount).
           cRange = "A" + cColumn.
           chWorkSheet:Range(cRange):Value = t-report.fchdoc.
           cRange = "B" + cColumn.
           chWorkSheet:Range(cRange):Value = "'" + t-report.coddiv.
           cRange = "C" + cColumn.                
           chWorkSheet:Range(cRange):Value = "'" + t-report.cco.
           cRange = "D" + cColumn.                
           chWorkSheet:Range(cRange):Value = "'" + t-report.nroast.
           cRange = "E" + cColumn.                
           chWorkSheet:Range(cRange):Value = "'" + t-report.codope.
           cRange = "F" + cColumn.                
           chWorkSheet:Range(cRange):Value = t-report.nromes.
           cRange = "G" + cColumn.                
           chWorkSheet:Range(cRange):Value = "'" + t-report.coddoc.
           cRange = "H" + cColumn.
           chWorkSheet:Range(cRange):Value = t-report.nrodoc.
           cRange = "I" + cColumn.
           chWorkSheet:Range(cRange):Value = t-report.fchvto.
           cRange = "J" + cColumn.
           chWorkSheet:Range(cRange):Value = t-report.nroref.
           cRange = "K" + cColumn.
           chWorkSheet:Range(cRange):Value = t-report.glodoc.
           cRange = "L" + cColumn.
           chWorkSheet:Range(cRange):Value = (IF t-importe <> 0 THEN t-report.t-importe ELSE 0).
           cRange = "M" + cColumn.
           chWorkSheet:Range(cRange):Value = (IF t-debe <> 0 THEN t-report.t-debe ELSE 0).
           cRange = "N" + cColumn.
           chWorkSheet:Range(cRange):Value = (IF t-haber <> 0 THEN t-report.t-haber ELSE 0).
           cRange = "O" + cColumn.
           chWorkSheet:Range(cRange):Value = y-saldo.
           iCount = iCount + 1.
           X-CONDOC = X-CONDOC + 1.
        END.
        ELSE DO:
            iCount = iCount + 1.
            cColumn = STRING(iCount).
            cRange = "A" + cColumn.
            chWorkSheet:Range(cRange):Value = t-report.fchdoc.
            cRange = "B" + cColumn.
            chWorkSheet:Range(cRange):Value = "'" + t-report.coddiv.
            cRange = "C" + cColumn.                
            chWorkSheet:Range(cRange):Value = "'" + t-report.cco.
            cRange = "D" + cColumn.                
            chWorkSheet:Range(cRange):Value = "'" + t-report.nroast.
            cRange = "E" + cColumn.                
            chWorkSheet:Range(cRange):Value = "'" + t-report.codope.
            cRange = "F" + cColumn.                
            chWorkSheet:Range(cRange):Value = t-report.nromes.
            cRange = "G" + cColumn.                
            chWorkSheet:Range(cRange):Value = "'" + t-report.coddoc.
            cRange = "H" + cColumn.
            chWorkSheet:Range(cRange):Value = t-report.nrodoc.
            cRange = "I" + cColumn.
            chWorkSheet:Range(cRange):Value = t-report.fchvto.
            cRange = "J" + cColumn.
            chWorkSheet:Range(cRange):Value = t-report.nroref.
            cRange = "K" + cColumn.
            chWorkSheet:Range(cRange):Value = t-report.glodoc.
            cRange = "L" + cColumn.
            chWorkSheet:Range(cRange):Value = (IF t-importe <> 0 THEN t-report.t-importe ELSE 0).
            cRange = "M" + cColumn.
            chWorkSheet:Range(cRange):Value = (IF t-debe <> 0 THEN t-report.t-debe ELSE 0).
            cRange = "N" + cColumn.
            chWorkSheet:Range(cRange):Value = (IF t-haber <> 0 THEN t-report.t-haber ELSE 0).
        END.
        IF LAST-OF(T-REPORT.CODCTA) THEN DO:
           Y-SALDO = (ACCUMULATE    SUB-TOTAL BY T-REPORT.CODCTA T-debe ) -
                     (ACCUMULATE    SUB-TOTAL BY T-REPORT.CODCTA T-haber) .
           iCount = iCount + 1.
           cColumn = STRING(iCount).
           cRange = "K" + cColumn.
           chWorkSheet:Range(cRange):Value = "Total Cuenta".
           cRange = "M" + cColumn.
           chWorkSheet:Range(cRange):Value = (ACCUMULATE SUB-TOTAL BY T-REPORT.CODCTA T-debe).
           cRange = "N" + cColumn.
           chWorkSheet:Range(cRange):Value = (ACCUMULATE SUB-TOTAL BY T-REPORT.CODCTA T-haber).
           cRange = "O" + cColumn.
           chWorkSheet:Range(cRange):Value = y-saldo.
           iCount = iCount + 1.
        END.
        IF LAST-OF(T-REPORT.CODAUX) AND X-CONDOC > 0 THEN DO:
           Y-SALDO = (ACCUMULATE    SUB-TOTAL BY T-REPORT.CODAUX T-debe ) -
                     (ACCUMULATE    SUB-TOTAL BY T-REPORT.CODAUX T-haber) .
           iCount = iCount + 1.
           cColumn = STRING(iCount).
           cRange = "K" + cColumn.
           chWorkSheet:Range(cRange):Value = "Total Auxiliar".
           cRange = "M" + cColumn.
           chWorkSheet:Range(cRange):Value = (ACCUMULATE SUB-TOTAL BY T-REPORT.CODAUX T-debe).
           cRange = "N" + cColumn.
           chWorkSheet:Range(cRange):Value = (ACCUMULATE SUB-TOTAL BY T-REPORT.CODAUX T-haber).
           cRange = "O" + cColumn.
           chWorkSheet:Range(cRange):Value = y-saldo.
           iCount = iCount + 1.
        END.
    END.
    IF CON-CTA > 1 THEN DO:
       Y-SALDO = (ACCUMULATE    TOTAL T-debe ) -
                 (ACCUMULATE    TOTAL T-haber) .
       iCount = iCount + 1.
       cColumn = STRING(iCount).
       cRange = "K" + cColumn.
       chWorkSheet:Range(cRange):Value = "Total General".
       cRange = "M" + cColumn.
       chWorkSheet:Range(cRange):Value = (ACCUMULATE TOTAL T-debe).
       cRange = "N" + cColumn.
       chWorkSheet:Range(cRange):Value = (ACCUMULATE TOTAL T-haber).
       cRange = "O" + cColumn.
       chWorkSheet:Range(cRange):Value = y-saldo.
       iCount = iCount + 1.
    END. /* FIN DEL CON-CTA */

    chExcelApplication:Visible = TRUE.

    /* release com-handles */
    RELEASE OBJECT chExcelApplication.      
    RELEASE OBJECT chWorkbook.
    RELEASE OBJECT chWorksheet.

    HIDE FRAME F-Mensaje.   
    
    MESSAGE
        "Proceso Terminado con suceso"
        VIEW-AS ALERT-BOX INFORMA.
    HIDE FRAME F-Auxiliar.  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Excel-4 DIALOG-1 
PROCEDURE Excel-4 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR X-MONCTA AS INTEGER.
    DEF VAR K AS INTEGER.
    DEF VAR CON-CTA  AS INTEGER.
    DEF VAR Y-SALDO  AS DECIMAL.

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

    /* titulos */
    iCount = iCount + 1.
    cColumn = STRING(iCount).
    cRange = "J" + cColumn.
    chWorkSheet:Range(cRange):Value = "ANALISIS DE CUENTAS".
    iCount = iCount + 1.
    cColumn = STRING(iCount).
    cRange = "J" + cColumn.
    chWorkSheet:Range(cRange):Value = pinta-mes.
    iCount = iCount + 1.
    cColumn = STRING(iCount).
    cRange = "J" + cColumn.
    chWorkSheet:Range(cRange):Value = x-nombala.
    iCount = iCount + 1.
    cColumn = STRING(iCount).
    cRange = "J" + cColumn.
    chWorkSheet:Range(cRange):Value = x-expres.
    iCount = iCount + 2.
    /* set the column names for the Worksheet */
    iCount = iCount + 1.
    cColumn = STRING(iCount).
    cRange = "A" + cColumn.
    chWorkSheet:Range(cRange):Value = "Fecha Doc.".
    cRange = "B" + cColumn.
    chWorkSheet:Range(cRange):Value = "Cuenta".
    cRange = "C" + cColumn.
    chWorkSheet:Range(cRange):Value = "Division".
    cRange = "D" + cColumn.
    chWorkSheet:Range(cRange):Value = "Centro Costo".
    cRange = "E" + cColumn.
    chWorkSheet:Range(cRange):Value = "Comprobante".
    cRange = "F" + cColumn.
    chWorkSheet:Range(cRange):Value = "Operacion".
    cRange = "G" + cColumn.
    chWorkSheet:Range(cRange):Value = "Nro. Mes".
    cRange = "H" + cColumn.
    chWorkSheet:Range(cRange):Value = "Cod Doc".
    cRange = "I" + cColumn.
    chWorkSheet:Range(cRange):Value = "Nro Doc".
    cRange = "J" + cColumn.
    chWorkSheet:Range(cRange):Value = "Fecha Vto.".
    cRange = "K" + cColumn.
    chWorkSheet:Range(cRange):Value = "Auxiliar".
    cRange = "L" + cColumn.
    chWorkSheet:Range(cRange):Value = "Detalle".
    cRange = "M" + cColumn.
    chWorkSheet:Range(cRange):Value = "Importe".
    cRange = "N" + cColumn.
    chWorkSheet:Range(cRange):Value = "Cargos".
    cRange = "O" + cColumn.
    chWorkSheet:Range(cRange):Value = "Abonos".

    FOR EACH T-REPORT NO-LOCK USE-INDEX IDX03
        BREAK  BY T-REPORT.CODCIA
            BY T-REPORT.FCHVTO:
        K = K + 1.
        IF K = 1 THEN DO:
           VIEW FRAME F-Mensaje.   
           PAUSE 0.           
        END.          
        
        ACCUMULATE T-debe  (TOTAL BY T-REPORT.CODCIA).
        ACCUMULATE T-haber (TOTAL BY T-REPORT.CODCIA).
        iCount = iCount + 1.
        cColumn = STRING(iCount).
        cRange = "A" + cColumn.
        chWorkSheet:Range(cRange):Value = t-report.fchdoc.
        cRange = "B" + cColumn.
        chWorkSheet:Range(cRange):Value = "'" + t-report.codcta.
        cRange = "C" + cColumn.
        chWorkSheet:Range(cRange):Value = "'" + t-report.coddiv.
        cRange = "D" + cColumn.                
        chWorkSheet:Range(cRange):Value = "'" + t-report.cco.
        cRange = "E" + cColumn.                
        chWorkSheet:Range(cRange):Value = "'" + t-report.nroast.
        cRange = "F" + cColumn.                
        chWorkSheet:Range(cRange):Value = "'" + t-report.codope.
        cRange = "G" + cColumn.                
        chWorkSheet:Range(cRange):Value = t-report.nromes.
        cRange = "H" + cColumn.                
        chWorkSheet:Range(cRange):Value = "'" + t-report.coddoc.
        cRange = "I" + cColumn.
        chWorkSheet:Range(cRange):Value = t-report.nrodoc.
        cRange = "J" + cColumn.
        chWorkSheet:Range(cRange):Value = t-report.fchvto.
        cRange = "K" + cColumn.
        chWorkSheet:Range(cRange):Value = t-report.codaux.
        cRange = "L" + cColumn.
        chWorkSheet:Range(cRange):Value = t-report.glodoc.
        cRange = "M" + cColumn.
        chWorkSheet:Range(cRange):Value = (IF t-importe <> 0 THEN t-report.t-importe ELSE 0).
        cRange = "N" + cColumn.
        chWorkSheet:Range(cRange):Value = (IF t-debe <> 0 THEN t-report.t-debe ELSE 0).
        cRange = "O" + cColumn.
        chWorkSheet:Range(cRange):Value = (IF t-haber <> 0 THEN t-report.t-haber ELSE 0).        
        
        IF LAST-OF(T-REPORT.CODCIA) THEN DO:
           iCount = iCount + 1.
           cColumn = STRING(iCount).
           cRange = "K" + cColumn.
           chWorkSheet:Range(cRange):Value = "Total General".
           cRange = "M" + cColumn.
           chWorkSheet:Range(cRange):Value = (ACCUMULATE TOTAL T-debe).
           cRange = "N" + cColumn.
           chWorkSheet:Range(cRange):Value = (ACCUMULATE TOTAL T-haber).
           iCount = iCount + 1.
        END. /* FIN DEL CON-CTA */
    END.

    chExcelApplication:Visible = TRUE.

    /* release com-handles */
    RELEASE OBJECT chExcelApplication.      
    RELEASE OBJECT chWorkbook.
    RELEASE OBJECT chWorksheet.

    HIDE FRAME F-Mensaje.   
    
    MESSAGE
        "Proceso Terminado con suceso"
        VIEW-AS ALERT-BOX INFORMA.
    HIDE FRAME F-Auxiliar.  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE IMP-FECHAS DIALOG-1 
PROCEDURE IMP-FECHAS :
DEF VAR X-MONCTA  AS INTEGER NO-UNDO.
DEF VAR CON-CTA   AS INTEGER NO-UNDO.
DEF VAR Y-SALDO   AS DECIMAL NO-UNDO.
DEF VAR K         AS INTEGER NO-UNDO.
DEF VAR T-FECHA   AS DATE    NO-UNDO.
DEF VAR X-VOUCHER AS CHAR    NO-UNDO.   
DEF VAR X-S1      AS DECIMAL NO-UNDO.
DEF VAR X-S2      AS DECIMAL NO-UNDO.
DEF VAR X-MES1    AS INTEGER NO-UNDO.
DEF VAR X-MES2    AS INTEGER NO-UNDO.
X-MES1 = MONTH(F-FECHA1).
X-MES2 = MONTH(F-FECHA2).
IF C-COPIAS = 1 THEN  DO:
    K = 0.
    Y-SALDO = 0.
    DISPLAY "Ordenando Informaci�n Requerida" @ x-mensaje with frame f-auxiliar.
    pause 0.
    {cbd/cbdr401a10.i}  
END.
p-config =  p-20cpi.
DO WITH FRAME T-REP :
    IF R-TIPREP <= 2 THEN DO:
        {cbd/cbdr401a12.i}  
    END. 
    /*
    ELSE DO:
        {cbd/cbdr401a03.i}  
    END. */
    ELSE IF R-TIPREP = 3 THEN DO: 
        {cbd/cbdr401a03.i} 
    END. /* FIN DEL ELSE */
    ELSE IF R-TIPREP = 4 THEN DO:
        {cbd/cbdr401a04.i}
    END.
    
END. /* FIN DEL DO WITH */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE IMPRIMIR DIALOG-1 
PROCEDURE IMPRIMIR :
DEF VAR X-MONCTA AS INTEGER.
     DEF VAR K AS INTEGER.
     DEF VAR CON-CTA  AS INTEGER.
     DEF VAR Y-SALDO  AS DECIMAL.

IF C-COPIAS = 1 THEN  DO:
    RUN Carga-Temporal.
END.

p-config =  p-20cpi.
DO WITH FRAME T-REP :
    CASE R-TIPREP:
        WHEN 1 OR WHEN 2 THEN DO:
            {cbd/cbdr401a12.i}
        END.
        WHEN 3 THEN DO:        
            {cbd/cbdr401a03.i}
        END. /* FIN DEL ELSE */
        WHEN 4 THEN DO: 
            {cbd/cbdr401a04.i}
        END.        
    END CASE. 
END. /* FIN DEL DO WITH */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE imprimir-2 DIALOG-1 
PROCEDURE imprimir-2 :
DEF VAR X-MONCTA AS INTEGER.
    DEF VAR MES-1 AS INTEGER.
    DEF VAR MES-2 AS INTEGER.
    DEF VAR CON-CTA  AS INTEGER.
    DEF VAR Y-SALDO  AS DECIMAL.
    DEF VAR K AS INTEGER.
    K = 0.
    Y-SALDO = 0.
         
    IF X-FORMATO = 1 THEN DO:
       MES-1 = 0.
       MES-2 = c-mes.
    END.
    ELSE DO:
       MES-1 = c-mes.
       MES-2 = c-mes.
    END.
    
    p-config =  p-20cpi.
    
    DISPLAY "Seleccionando informaci�n requerida" @ x-mensaje with frame f-auxiliar.
    pause 0.

     FOR EACH cb-dmov NO-LOCK WHERE
                            cb-dmov.codcia    =      s-codcia
                        AND cb-dmov.periodo   =      s-periodo 
                        AND cb-dmov.coddiv   BEGINS  (x-div)
                        AND cb-dmov.codaux   BEGINS  (x-auxiliar)
                        AND cb-dmov.codcta   BEGINS  (x-cuenta)
                        AND cb-dmov.nromes   >=      mes-1 
                        AND cb-dmov.nromes   <=      mes-2 
                        AND cb-dmov.Clfaux   BEGINS  (x-Clasificacion)
            BREAK BY cb-dmov.codcia
                  BY cb-dmov.periodo
                  BY cb-dmov.Codcta
                  BY cb-dmov.codaux
                  BY cb-dmov.coddoc
                  BY cb-dmov.nrodoc 
                  BY cb-dmov.nromes
                  BY cb-dmov.fchdoc :
        K = K + 1.
        IF K = 1 THEN DO:
            hide frame f-auxiliar .
            VIEW FRAME F-Mensaje.   
            PAUSE 0.           
        END.          
        X-MonCta = 1.    
        IF FIRST-OF (cb-dmov.codcta) THEN DO:
            i = 0.
            CON-CTA = CON-CTA + 1.
            find cb-ctas WHERE cb-ctas.codcia = cb-codcia
                            AND cb-ctas.codcta = cb-dmov.codcta
                            NO-LOCK NO-ERROR.
            IF AVAILABLE cb-ctas THEN DO:
                x-MoNcta = cb-ctas.codmon.
                x-nomcta = cb-dmov.codcta + " " + cb-ctas.nomcta.
                {&NEW-PAGE}.            
                DISPLAY STREAM report WITH FRAME f-cab.
                PUT STREAM report CONTROL P-dobleon.
                PUT STREAM report x-nomcta.
                PUT STREAM report CONTROL P-dobleoff.
                DOWN STREAM REPORT WITH FRAME F-CAB.    
            END.
        END.
        IF FIRST-OF (cb-dmov.codaux) THEN DO:
            run nom-aux.
            x-nomaux = cb-dmov.codaux + " " + x-nomaux.
            {&NEW-PAGE}.            
             DISPLAY STREAM report WITH FRAME f-cab.
             PUT STREAM report CONTROL P-dobleon.
             PUT STREAM report x-nomaux.
             PUT STREAM report CONTROL P-dobleoff.
             DOWN STREAM REPORT WITH FRAME F-CAB.
             x-condoc = 0.
        END.
        
        IF FIRST-OF(CB-DMOV.NRODOC) THEN DO:
           x-conreg = 0.
           Y-SALDO  = 0.
           if x-formato = 2 and  q-saldo then do:
              run verifica.
              if imprime then do:
                  {&NEW-PAGE}.          
                   DISPLAY STREAM report 
                       
                        "                Saldo Anterior" @ cb-dmov.glodoc
                        x-saldo  WITH FRAME f-cab.
                  down stream report with frame f-cab.      
                 y-saldo = x-saldo.
                 ACCUMULATE y-saldo  (SUB-TOTAL BY cb-dmov.codcta).
                 ACCUMULATE y-saldo  (SUB-TOTAL BY cb-dmov.codaux).
                 ACCUMULATE y-saldo  (TOTAL) .
                 x-saldo = 0.         
              end.
              
           end.   
           
           
        END.
        
        x-glodoc = glodoc.
        /* Buscando la cabezera correspondiente */
        IF x-glodoc = "" THEN DO:
            FIND cb-cmov WHERE cb-cmov.codcia   = cb-dmov.codcia
                            AND cb-cmov.periodo = cb-dmov.periodo 
                            AND cb-cmov.nromes  = cb-dmov.nromes
                            AND cb-cmov.codope  = cb-dmov.codope
                            AND cb-cmov.nroast  = cb-dmov.nroast
                        NO-LOCK NO-ERROR.
            IF AVAILABLE cb-cmov THEN
                x-glodoc = cb-cmov.notast.
        END.
        IF x-glodoc = "" THEN DO:
            find cb-ctas WHERE cb-ctas.codcta = cb-dmov.codcta
                                AND cb-ctas.CodCia = cb-codcia
                            NO-LOCK NO-ERROR.                      
                IF AVAILABLE cb-ctas THEN 
                    x-glodoc = cb-ctas.nomcta.
        END.
        
        IF NOT tpomov THEN DO:
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
        
        IF cb-dmov.codmon = x-codmon THEN 
            x-importe = 0.
        ELSE DO:
            CASE cb-dmov.codmon:
            WHEN 1 THEN 
                x-importe = cb-dmov.ImpMn1.
            WHEN 2 THEN 
                x-importe = cb-dmov.ImpMn2.
            END CASE.
        END.
     
        IF  ( x-haber <>  ? AND  x-debe <> ? )
           THEN DO:
            ACCUMULATE x-debe  (SUB-TOTAL BY cb-dmov.nrodoc).
            ACCUMULATE x-haber (SUB-TOTAL BY cb-dmov.nrodoc).
            ACCUMULATE x-debe  (SUB-TOTAL BY cb-dmov.codcta).
            ACCUMULATE x-haber (SUB-TOTAL BY cb-dmov.codcta).
            ACCUMULATE x-debe  (SUB-TOTAL BY cb-dmov.codaux).
            ACCUMULATE x-haber (SUB-TOTAL BY cb-dmov.codaux).
            ACCUMULATE x-debe  (TOTAL) .
            ACCUMULATE x-haber (TOTAL) .
        END.       
            {&NEW-PAGE}.
            DOWN STREAM report WITH FRAME f-cab.
            DISPLAY STREAM report 
                                cb-dmov.fchdoc  WHEN (cb-dmov.fchdoc <> ?)
                                cb-dmov.coddiv
                                cb-dmov.cco
                                cb-dmov.nroast
                                cb-dmov.codope
                                cb-dmov.nromes
                                cb-dmov.coddoc   
                                cb-dmov.nrodoc
                                cb-dmov.nroref
                                cb-dmov.fchvto    WHEN (cb-dmov.fchvto <> ?)
                                x-glodoc @ cb-dmov.glodoc           
                                x-importe WHEN (x-importe <> 0) 
                                x-debe    WHEN (x-debe    <> 0)
                                x-haber   WHEN (x-haber   <> 0)
                  WITH FRAME f-cab.
                  x-conreg = x-conreg + 1.
        
        
        
        IF LAST-OF (cb-dmov.nrodoc) AND x-conreg > 0 THEN DO:
            x-saldo = (ACCUM SUB-TOTAL BY cb-dmov.nrodoc x-debe) -
                      (ACCUM SUB-TOTAL BY cb-dmov.nrodoc x-haber) +
                      y-saldo.
            {&NEW-PAGE}.          
            DISPLAY STREAM report x-saldo  WITH FRAME f-cab.
            DOWN STREAM report 1 WITH FRAME f-cab.
            DISPLAY STREAM report "........" @ cb-dmov.fchdoc
                                "....."      @ cb-dmov.coddiv
                                "....."      @ cb-dmov.cco
                                "......"     @ cb-dmov.nroast
                                "..."        @ cb-dmov.nromes
                                "..."        @ cb-dmov.codope
                                "......"     @ cb-dmov.coddoc
                                ".........." @ cb-dmov.nrodoc
                                ".........." @ cb-dmov.nroref
                                "........"   @ cb-dmov.fchvto
                                ".............................." @ cb-dmov.glodoc
                                "..............." @ x-importe  
                                "..............." @ x-debe 
                                "..............." @ x-haber
                                "..............." @ x-saldo
                    WITH FRAME f-cab.
            x-conreg = 0.
            x-condoc = x-condoc + 1.
        END.
        
        IF LAST-OF (cb-dmov.codaux) AND x-condoc > 0 THEN DO:
            x-glodoc = "** TOTAL AUXILIAR " + cb-dmov.codaux + " **".
            {&NEW-PAGE}.
            DOWN STREAM report 1 WITH FRAME f-cab.
            DISPLAY STREAM report
                        x-glodoc                                   @ cb-dmov.glodoc
                        ACCUM SUB-TOTAL BY cb-dmov.codaux x-debe   @ x-debe
                        ACCUM SUB-TOTAL BY cb-dmov.codaux x-haber  @ x-haber
                       (ACCUM SUB-TOTAL BY cb-dmov.codaux x-debe) -
                       (ACCUM SUB-TOTAL BY cb-dmov.codaux x-haber) + 
                       (ACCUM SUB-TOTAL BY cb-dmov.codaux y-saldo) @ x-saldo
                WITH FRAME f-cab.
            {&NEW-PAGE}.    
            DOWN STREAM report 1 WITH FRAME f-cab.
            DISPLAY STREAM report "--------" @ cb-dmov.fchdoc
                                "------"     @ cb-dmov.nroast
                                "-----"      @ cb-dmov.coddiv
                                "-----"      @ cb-dmov.cco
                                "---"        @ cb-dmov.nromes
                                "---"        @ cb-dmov.codope
                                "------"     @ cb-dmov.coddoc
                                "----------" @ cb-dmov.nrodoc
                                "----------" @ cb-dmov.nroref
                                "--------"   @ cb-dmov.fchvto
                                "------------------------------" @ cb-dmov.glodoc
                                "---------------" @ x-importe  
                                "---------------" @ x-debe 
                                "---------------" @ x-haber
                                "---------------" @ x-saldo
                    WITH FRAME f-cab.
            x-condoc = 0.
            i = i + 1.
        END.
        IF LAST-OF (cb-dmov.codcta) AND i > 0 THEN DO:
            x-glodoc = "** TOTAL CUENTA " + cb-dmov.codcta + " **".
            {&NEW-PAGE}.
            DOWN STREAM report 1 WITH FRAME f-cab.
            DISPLAY STREAM report
                        x-glodoc                                   @ cb-dmov.glodoc
                        ACCUM SUB-TOTAL BY cb-dmov.codcta x-debe   @ x-debe
                        ACCUM SUB-TOTAL BY cb-dmov.codcta x-haber  @ x-haber
                       (ACCUM SUB-TOTAL BY cb-dmov.codcta x-debe) -
                       (ACCUM SUB-TOTAL BY cb-dmov.codcta x-haber) + 
                       (ACCUM SUB-TOTAL BY cb-dmov.codcta y-saldo) @ x-saldo
                WITH FRAME f-cab.
            {&NEW-PAGE}.    
            DOWN STREAM report 1 WITH FRAME f-cab.
            DISPLAY STREAM report "========" @ cb-dmov.fchdoc
                                "======"     @ cb-dmov.nroast
                                "==="        @ cb-dmov.nromes
                                "==="        @ cb-dmov.codope
                                "======"     @ cb-dmov.coddoc
                                "==========" @ cb-dmov.nrodoc
                                "==========" @ cb-dmov.nroref
                                "========"   @ cb-dmov.fchvto
                                "==============================" @ cb-dmov.glodoc
                                "===============" @ x-importe  
                                "===============" @ x-debe 
                                "===============" @ x-haber
                                "===============" @ x-saldo
                    WITH FRAME f-cab.
            i = 0.
        END.

    END. /* FIN DEL FOR EACH */
    
    IF CON-CTA > 1 THEN DO:    
            x-glodoc = "**** TOTAL GENERAL ****".
            {&NEW-PAGE}.
            DOWN STREAM report 1 WITH FRAME f-cab.
            DISPLAY STREAM report
                        x-glodoc                                   @ cb-dmov.glodoc
                        ACCUM TOTAL x-debe   @ x-debe
                        ACCUM TOTAL x-haber  @ x-haber
                       (ACCUM TOTAL x-debe)  -
                       (ACCUM TOTAL x-haber) + 
                       (ACCUM TOTAL y-saldo)  @ x-saldo
                WITH FRAME f-cab.
    
   END.    

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
    IF c-Pagina > P-pagfin
    THEN RETURN ERROR.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE nom-aux DIALOG-1 
PROCEDURE nom-aux :
CASE cb-dmov.clfaux:
            WHEN "@CL" THEN DO:
                FIND gn-clie WHERE gn-clie.codcli  = cb-dmov.codaux
                                AND gn-clie.CodCia = cl-codcia
                            NO-LOCK NO-ERROR. 
                IF AVAILABLE gn-clie THEN
                    x-nomaux = gn-clie.nomcli.
            END.
            WHEN "@PV" THEN DO:
                FIND gn-prov WHERE gn-prov.codpro = cb-dmov.codaux
                                AND gn-prov.CodCia = pv-codcia
                            NO-LOCK NO-ERROR.                      
                IF AVAILABLE gn-prov THEN 
                    x-nomaux = gn-prov.nompro.
            END.
            WHEN "@CT" THEN DO:
                find cb-ctas WHERE cb-ctas.codcta = cb-dmov.codaux
                                AND cb-ctas.CodCia = cb-codcia
                            NO-LOCK NO-ERROR.                      
                IF AVAILABLE cb-ctas THEN 
                    x-nomaux = cb-ctas.nomcta.
            END.
            WHEN "" THEN DO:
                x-nomaux = "".
            END.
            OTHERWISE DO:
                FIND cb-auxi WHERE cb-auxi.clfaux = cb-dmov.clfaux
                                AND cb-auxi.codaux = cb-dmov.codaux
                                AND cb-auxi.CodCia = cb-codcia
                            NO-LOCK NO-ERROR.                      
                IF AVAILABLE cb-auxi THEN 
                    x-nomaux = cb-auxi.nomaux.
                ELSE
                    x-nomaux = "".
            END.
            END CASE.
            
     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE p-nom-aux DIALOG-1 
PROCEDURE p-nom-aux :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
            CASE cb-dmov.clfaux:
            WHEN "@CL" THEN DO:
                FIND gn-clie WHERE  
                     gn-clie.CodCia = cl-codcia AND
                     gn-clie.codcli = cb-dmov.codaux
                     NO-LOCK NO-ERROR. 
                IF AVAILABLE gn-clie THEN
                    x-nomaux = gn-clie.nomcli.
            END.
            WHEN "@PV" THEN DO:
                FIND gn-prov WHERE 
                     gn-prov.codpro = cb-dmov.codaux AND
                     gn-prov.CodCia = pv-codcia
                     NO-LOCK NO-ERROR.                      
                IF AVAILABLE gn-prov THEN 
                    x-nomaux = gn-prov.nompro.
            END.
            WHEN "@CT" THEN DO:
                find cb-ctas WHERE 
                     cb-ctas.CodCia = cb-codcia AND
                     cb-ctas.codcta = cb-dmov.codaux
                     NO-LOCK NO-ERROR.                      
                IF AVAILABLE cb-ctas THEN 
                    x-nomaux = cb-ctas.nomcta.
            END.
            OTHERWISE DO:
                FIND cb-auxi WHERE 
                     cb-auxi.CodCia = cb-codcia AND
                     cb-auxi.clfaux = cb-dmov.clfaux AND
                     cb-auxi.codaux = cb-dmov.codaux
                     NO-LOCK NO-ERROR.                      
                IF AVAILABLE cb-auxi THEN 
                    x-nomaux = cb-auxi.nomaux.
            END.
            END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Procesa-Parametros DIALOG-1 
PROCEDURE Procesa-Parametros :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Recoge-Parametros DIALOG-1 
PROCEDURE Recoge-Parametros :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE T-NOM-AUX DIALOG-1 
PROCEDURE T-NOM-AUX :
CASE T-REPORT.clfaux:
            WHEN "@CL" THEN DO:
                FIND gn-clie WHERE 
                     gn-clie.codcli  = T-REPORT.codaux AND
                     gn-clie.CodCia = cl-codcia
                     NO-LOCK NO-ERROR. 
                IF AVAILABLE gn-clie THEN
                    x-nomaux = gn-clie.nomcli.
            END.
            WHEN "@PV" THEN DO:
                FIND gn-prov WHERE 
                     gn-prov.CodCia = pv-codcia AND
                     gn-prov.codpro = T-REPORT.codaux
                     NO-LOCK NO-ERROR.                      
                IF AVAILABLE gn-prov THEN 
                    x-nomaux = gn-prov.nompro.
            END.
            WHEN "@CT" THEN DO:
                find cb-ctas WHERE 
                     cb-ctas.CodCia = cb-codcia AND
                     cb-ctas.codcta = T-REPORT.codaux 
                     NO-LOCK NO-ERROR.                      
                IF AVAILABLE cb-ctas THEN 
                    x-nomaux = cb-ctas.nomcta.
            END.
            WHEN "" THEN DO:
                x-nomaux = "".
            END.
            OTHERWISE DO:
                FIND cb-auxi WHERE 
                     cb-auxi.CodCia = cb-codcia AND
                     cb-auxi.clfaux = T-REPORT.clfaux AND
                     cb-auxi.codaux = T-REPORT.codaux
                     NO-LOCK NO-ERROR.                      
                IF AVAILABLE cb-auxi THEN 
                    x-nomaux = cb-auxi.nomaux.
                ELSE
                    x-nomaux = "".
            END.
            END CASE.
            

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE verifica DIALOG-1 
PROCEDURE verifica :
X-saldo = 0.
    x-debe  = 0.
    x-haber = 0.
    imprime = yes.

    FOR EACH detalle WHERE detalle.codcia   = cb-dmov.CodCia
                        AND detalle.periodo = cb-dmov.periodo
                        AND detalle.codaux  = cb-dmov.codaux
                        AND detalle.codcta  = cb-dmov.codcta
                        AND detalle.coddoc  = cb-dmov.coddoc
                        AND detalle.nrodoc  = cb-dmov.nrodoc
                        
                        AND detalle.nromes  < c-mes :
                                               
            IF NOT detalle.tpomov THEN DO:
                CASE x-codmon:
                WHEN 1 THEN
                    x-debe = x-debe + detalle.ImpMn1.
                WHEN 2 THEN
                    x-debe = x-debe + detalle.ImpMn2.
               END CASE.         
            END.
            ELSE DO:      
                CASE x-codmon:
                WHEN 1 THEN
                    x-haber = x-haber + detalle.ImpMn1.
                WHEN 2 THEN
                    x-haber = x-haber + detalle.ImpMn2.
                END CASE.    
            END.
        
            
     
        
    END. /* FIN DEL FOR EACH */
    
    x-saldo = x-debe - x-haber.
    IF (x-saldo = 0 ) THEN imprime = NO.
    
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
