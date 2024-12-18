&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wWin
{adecomm/appserv.i}
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: New V9 Version - January 15, 1998
          
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AB.              */
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

{src/adm2/widgetprto.i}

DEF SHARED VAR s-codcia AS INT.
DEF SHARED VAR cl-codcia AS INT.
DEF SHARED VAR pv-codcia AS INT.
DEF NEW SHARED VAR input-var-1 AS CHAR.
DEF NEW SHARED VAR input-var-2 AS CHAR.
DEF NEW SHARED VAR input-var-3 AS CHAR.
DEF NEW SHARED VAR output-var-1 AS ROWID.
DEF NEW SHARED VAR output-var-2 AS CHAR.
DEF NEW SHARED VAR output-var-3 AS CHAR.

DEFINE VAR T-Vtamn   AS DECI INIT 0 EXTENT 4.
DEFINE VAR T-Vtame   AS DECI INIT 0 EXTENT 4.
DEFINE VAR T-Ctomn   AS DECI INIT 0 EXTENT 4.
DEFINE VAR T-Ctome   AS DECI INIT 0 EXTENT 4.
DEFINE VAR T-Promn   AS DECI INIT 0 EXTENT 4.
DEFINE VAR T-Prome   AS DECI INIT 0 EXTENT 4.
DEFINE VAR F-Salida  AS DECI INIT 0 EXTENT 4.
DEFINE VAR X-FECHA AS DATE.

/* VARIABLES PARA EL RESUMEN */
DEF VAR x-CodDiv LIKE dwh_Division.coddiv   NO-UNDO.
DEF VAR x-CodCli LIKE dwh_Cliente.codcli   NO-UNDO.
DEF VAR x-ClfCli LIKE dwh_Cliente.clfcli   NO-UNDO.
DEF VAR x-CodMat LIKE dwh_Producto.codmat  NO-UNDO.
DEF VAR x-CodPro LIKE dwh_Proveedor.codpro   NO-UNDO.
DEF VAR x-CodVen LIKE dwh_Vendedor.codven    NO-UNDO.
DEF VAR x-CodFam LIKE dwh_Producto.codfam  NO-UNDO.
DEF VAR x-SubFam LIKE dwh_Producto.subfam  NO-UNDO.
DEF VAR x-CanalVenta LIKE dwh_Division.CanalVenta NO-UNDO.
DEF VAR x-Canal  LIKE dwh_Cliente.canal    NO-UNDO.
DEF VAR x-Giro   LIKE dwh_Cliente.gircli   NO-UNDO.
DEF VAR x-NroCard LIKE dwh_Cliente.nrocard NO-UNDO.
DEF VAR x-Zona   AS CHAR               NO-UNDO.
DEF VAR x-CodDept LIKE dwh_Cliente.coddept NO-UNDO.
DEF VAR x-CodProv LIKE dwh_Cliente.codprov NO-UNDO.
DEF VAR x-CodDist LIKE dwh_Cliente.coddist NO-UNDO.
DEF VAR x-CuentaReg  AS INT             NO-UNDO.    /* Contador de registros */
DEF VAR x-MuestraReg AS INT             NO-UNDO.    /* Tope para mostrar registros */

DEF VAR iContador AS INT NO-UNDO.
DEFINE VAR x-NroFchR AS INT NO-UNDO.
DEFINE VAR x-NroFchE AS INT NO-UNDO.

DEFINE VAR x-Llave AS CHAR.
DEF STREAM REPORTE.

DEF INPUT PARAMETER pParametro AS CHAR.
/* Sistaxis de pParamtero
+COSTO    Imprimir el valor del costo
-COSTO    Imprimir sin el valor del costo
************************** */

/*DEF VAR pParametro AS CHAR INIT "+COSTO".*/
/*DEF INPUT PARAMETER pParametro AS CHAR.*/
/* Sistaxis de pParamtero
+COSTO    Imprimir el valor del costo
-COSTO    Imprimir sin el valor del costo
************************** */

/*Tabla Clientes*/
DEFINE TEMP-TABLE tt-cliente
    FIELDS tt-codcli LIKE dwh_Cliente.codcli
    FIELDS tt-nomcli LIKE dwh_Cliente.nomcli
    INDEX idx01 IS PRIMARY tt-codcli.

/*Tabla Clientes*/
DEFINE TEMP-TABLE tt-articulo
    FIELDS tt-codmat LIKE dwh_Producto.codmat
    FIELDS tt-desmat LIKE dwh_Producto.desmat
    INDEX idx01 IS PRIMARY tt-codmat.

DEFINE TEMP-TABLE tt-datos
    FIELDS tt-codigo AS CHAR.

/* TABLA GENERAL ACUMULADOS */
{lib/tt-file.i}

DEF TEMP-TABLE Detalle
    FIELD Llave     AS CHAR
    FIELD Campania  AS CHAR FORMAT 'x(20)'
    FIELD Periodo   AS INT  FORMAT 'ZZZ9' LABEL 'A�o'
    FIELD NroMes    AS INT  FORMAT 'Z9' LABEL 'Mes'
    FIELD Dia       AS DATE FORMAT {&tt-fmt-dmy} LABEL 'Dia'
    FIELD Division AS CHAR  FORMAT 'x(60)'
    FIELD CanalVenta AS CHAR    LABEL "Canal Venta" FORMAT 'x(60)'
    FIELD Producto  AS CHAR FORMAT 'x(60)'
    FIELD Linea     AS CHAR FORMAT 'x(60)'
    FIELD Sublinea  AS CHAR FORMAT 'x(60)'
    FIELD Marca     AS CHAR FORMAT 'x(20)'
    FIELD Unidad    AS CHAR FORMAT 'x(10)'
    FIELD Licencia  AS CHAR FORMAT 'x(60)'
    FIELD Proveedor AS CHAR FORMAT 'x(60)'
    FIELD Cliente   AS CHAR FORMAT 'x(60)'
    FIELD Canal     AS CHAR FORMAT 'x(30)'
    FIELD Tarjeta   AS CHAR FORMAT 'x(60)'
    FIELD Departamento AS CHAR FORMAT 'x(20)'
    FIELD Provincia AS CHAR FORMAT 'x(20)'
    FIELD Distrito  AS CHAR FORMAT 'x(20)'
    FIELD Zona      AS CHAR FORMAT 'x(20)'
    FIELD Clasificacion AS CHAR FORMAT 'x(20)'
    FIELD Tipo      AS CHAR FORMAT 'x(20)'
    FIELD Vendedor  AS CHAR FORMAT 'x(60)'
    FIELD CanxMes-1     AS DEC  LABEL "CANTIDAD-ACTUAL"             FORMAT '->>>,>>>,>>>,>>9.99'
    FIELD VtaxMesMe-1   AS DEC  LABEL "VENTA-DOLARES-ACTUAL"        FORMAT '->>>,>>>,>>>,>>9.99'
    FIELD VtaxMesMn-1   AS DEC  LABEL "VENTA-SOLES-ACTUAL"          FORMAT '->>>,>>>,>>>,>>9.99'
    FIELD CtoxMesMe-1   AS DEC  LABEL "CTO-DOLAR-ACTUAL"            FORMAT '->>>,>>>,>>>,>>9.99'
    FIELD CtoxMesMn-1   AS DEC  LABEL "CTO-SOLES-ACTUAL"            FORMAT '->>>,>>>,>>>,>>9.99'
    FIELD ProxMesMe-1   AS DEC  LABEL "PROM-DOLAR-ACTUAL"           FORMAT '->>>,>>>,>>>,>>9.99'
    FIELD ProxMesMn-1   AS DEC  LABEL "PROM-SOLES-ACTUAL"           FORMAT '->>>,>>>,>>>,>>9.99'
    FIELD CanxMes-2     AS DEC  LABEL "CANTIDAD-ACUM-ACTUAL"        FORMAT '->>>,>>>,>>>,>>9.99'
    FIELD VtaxMesMe-2   AS DEC  LABEL "VENTA-DOLARES-ACUM-ACTUAL"   FORMAT '->>>,>>>,>>>,>>9.99'
    FIELD VtaxMesMn-2   AS DEC  LABEL "VENTA-SOLES-ACUM-ACTUAL"     FORMAT '->>>,>>>,>>>,>>9.99'
    FIELD CtoxMesMe-2   AS DEC  LABEL "CTO-DOLAR-ACUM-ACTUAL"       FORMAT '->>>,>>>,>>>,>>9.99'
    FIELD CtoxMesMn-2   AS DEC  LABEL "CTO-SOLES-ACUM-ACTUAL"       FORMAT '->>>,>>>,>>>,>>9.99'
    FIELD ProxMesMe-2   AS DEC  LABEL "PROM-DOLAR-ACUM-ACTUAL"      FORMAT '->>>,>>>,>>>,>>9.99'
    FIELD ProxMesMn-2   AS DEC  LABEL "PROM-SOLES-ACUM-ACTUAL"      FORMAT '->>>,>>>,>>>,>>9.99'
    FIELD CanxMes-3     AS DEC  LABEL "CANTIDAD-ANTERIOR"           FORMAT '->>>,>>>,>>>,>>9.99'
    FIELD VtaxMesMe-3   AS DEC  LABEL "VENTA-DOLARES-ANTERIOR"      FORMAT '->>>,>>>,>>>,>>9.99'
    FIELD VtaxMesMn-3   AS DEC  LABEL "VENTA-SOLES-ANTERIOR"        FORMAT '->>>,>>>,>>>,>>9.99'
    FIELD CtoxMesMe-3   AS DEC  LABEL "CTO-DOLAR-ANTERIOR"          FORMAT '->>>,>>>,>>>,>>9.99'
    FIELD CtoxMesMn-3   AS DEC  LABEL "CTO-SOLES-ANTERIOR"          FORMAT '->>>,>>>,>>>,>>9.99'
    FIELD ProxMesMe-3   AS DEC  LABEL "PROM-DOLAR-ANTERIOR"         FORMAT '->>>,>>>,>>>,>>9.99'
    FIELD ProxMesMn-3   AS DEC  LABEL "PROM-SOLES-ANTERIOR"         FORMAT '->>>,>>>,>>>,>>9.99'
    FIELD CanxMes-4     AS DEC  LABEL "CANTIDAD-ACUM-ANTERIOR"      FORMAT '->>>,>>>,>>>,>>9.99'
    FIELD VtaxMesMe-4   AS DEC  LABEL "VENTA-DOLARES-ACUM-ANTERIOR" FORMAT '->>>,>>>,>>>,>>9.99'
    FIELD VtaxMesMn-4   AS DEC  LABEL "VENTA-SOLES-ACUM-ANTERIOR"   FORMAT '->>>,>>>,>>>,>>9.99'
    FIELD CtoxMesMe-4   AS DEC  LABEL "CTO-DOLAR-ACUM-ANTERIOR"     FORMAT '->>>,>>>,>>>,>>9.99'
    FIELD CtoxMesMn-4   AS DEC  LABEL "CTO-SOLES-ACUM-ANTERIOR"     FORMAT '->>>,>>>,>>>,>>9.99'
    FIELD ProxMesMe-4   AS DEC  LABEL "PROM-DOLAR-ACUM-ANTERIOR"    FORMAT '->>>,>>>,>>>,>>9.99'
    FIELD ProxMesMn-4   AS DEC  LABEL "PROM-SOLES-ACUM-ANTERIOR"    FORMAT '->>>,>>>,>>>,>>9.99'
    INDEX Indice01 AS PRIMARY Llave.

DEF VAR cCampania LIKE Detalle.Campania.
DEF VAR cPeriodo LIKE Detalle.Periodo.
DEF var cNroMes   LIKE Detalle.NroMes.
DEF VAR cDia      LIKE Detalle.Dia.
DEF VAR cDivision LIKE Detalle.Division.
DEF VAR cCanalVenta LIKE Detalle.CanalVenta.
DEF VAR cProducto  LIKE Detalle.Producto.
DEF VAR cLinea     LIKE Detalle.Linea.
DEF VAR cSublinea  LIKE Detalle.Sublinea.
DEF VAR cMarca     LIKE Detalle.Marca.
DEF VAR cUnidad    LIKE Detalle.Unidad.
DEF VAR cLicencia  LIKE Detalle.Licencia.
DEF VAR cProveedor LIKE Detalle.Proveedor.
DEF VAR cCliente   LIKE Detalle.Cliente.
DEF VAR cCanal     LIKE Detalle.Canal.
DEF VAR cTarjeta   LIKE Detalle.Tarjeta.
DEF VAR cDepartamento LIKE Detalle.Departamento.
DEF VAR cProvincia LIKE Detalle.Provincia.
DEF VAR cDistrito  LIKE Detalle.Distrito.
DEF VAR cZona      LIKE Detalle.Zona.
DEF VAR cClasificacion LIKE Detalle.Clasificacion.
DEF VAR cTipo      LIKE Detalle.Tipo.
DEF VAR cVendedor  LIKE Detalle.Vendedor.

DEF VAR pOptions AS CHAR.
DEF VAR pArchivo AS CHAR.
DEF VAR lOptions AS CHAR.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fMain

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 RECT-3 RECT-4 RECT-5 BUTTON-1 ~
BtnDone DesdeF HastaF COMBO-BOX-Tipo TOGGLE-CodDiv TOGGLE-CodCli ~
TOGGLE-CodMat TOGGLE-CodVen 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-Mensaje DesdeF HastaF ~
RADIO-SET-Tipo COMBO-BOX-Tipo TOGGLE-CodDiv COMBO-BOX-CodDiv TOGGLE-CodCli ~
FILL-IN-CodCli FILL-IN-NomCli FILL-IN-file TOGGLE-Resumen-Depto ~
TOGGLE-CodMat COMBO-BOX-CodFam TOGGLE-Resumen-Linea COMBO-BOX-SubFam ~
TOGGLE-Resumen-Marca FILL-IN-CodMat FILL-IN-DesMat FILL-IN-file-2 ~
FILL-IN-CodPro FILL-IN-NomPro TOGGLE-CodVen FILL-IN-CodVen FILL-IN-NomVen 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnDone DEFAULT 
     IMAGE-UP FILE "img/exit.ico":U
     LABEL "&Done" 
     SIZE 6 BY 1.62
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-1 
     IMAGE-UP FILE "img/excel.bmp":U
     LABEL "Button 1" 
     SIZE 6 BY 1.62.

DEFINE BUTTON BUTTON-5 
     LABEL "..." 
     SIZE 5 BY 1.

DEFINE BUTTON BUTTON-6 
     LABEL "..." 
     SIZE 5 BY 1.

DEFINE VARIABLE COMBO-BOX-CodDiv AS CHARACTER FORMAT "X(256)":U INITIAL "Todos" 
     LABEL "Division" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Todos" 
     DROP-DOWN-LIST
     SIZE 70 BY 1 NO-UNDO.

DEFINE VARIABLE COMBO-BOX-CodFam AS CHARACTER FORMAT "X(256)":U INITIAL "Todos" 
     LABEL "Linea" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Todos" 
     DROP-DOWN-LIST
     SIZE 43 BY 1 NO-UNDO.

DEFINE VARIABLE COMBO-BOX-SubFam AS CHARACTER FORMAT "X(256)":U INITIAL "Todos" 
     LABEL "Sub-linea" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Todos" 
     DROP-DOWN-LIST
     SIZE 43 BY 1 NO-UNDO.

DEFINE VARIABLE COMBO-BOX-Tipo AS CHARACTER FORMAT "X(256)":U INITIAL "Cantidades" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Cantidades" 
     DROP-DOWN-LIST
     SIZE 33 BY 1 NO-UNDO.

DEFINE VARIABLE DesdeF AS DATE FORMAT "99/99/9999":U 
     LABEL "Desde" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-CodCli AS CHARACTER FORMAT "X(11)":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-CodMat AS CHARACTER FORMAT "X(6)":U 
     LABEL "Codigo" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-CodPro AS CHARACTER FORMAT "X(11)":U 
     LABEL "Proveedor" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-CodVen AS CHARACTER FORMAT "X(3)":U 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-DesMat AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 75 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-file AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 76 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-file-2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 76 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-Mensaje AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 103 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-NomCli AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 75 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-NomPro AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 75 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-NomVen AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 83 BY 1 NO-UNDO.

DEFINE VARIABLE HastaF AS DATE FORMAT "99/99/9999":U 
     LABEL "Hasta" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE RADIO-SET-Tipo AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Resumido", 1,
"Mensual", 2
     SIZE 25 BY 1.08 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 116 BY 1.88.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 116 BY 3.5.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 116 BY 5.92.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 116 BY 1.62.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 116 BY 4.04.

DEFINE VARIABLE TOGGLE-CodCli AS LOGICAL INITIAL no 
     LABEL "Cliente" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.29 BY .77 NO-UNDO.

DEFINE VARIABLE TOGGLE-CodDiv AS LOGICAL INITIAL no 
     LABEL "Division" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.29 BY .77 NO-UNDO.

DEFINE VARIABLE TOGGLE-CodMat AS LOGICAL INITIAL no 
     LABEL "Art�culo" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.29 BY .77 NO-UNDO.

DEFINE VARIABLE TOGGLE-CodVen AS LOGICAL INITIAL no 
     LABEL "Vendedor" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.29 BY .77 NO-UNDO.

DEFINE VARIABLE TOGGLE-Resumen-Depto AS LOGICAL INITIAL no 
     LABEL "Resumido por Departamento-Provincia" 
     VIEW-AS TOGGLE-BOX
     SIZE 37 BY .77 NO-UNDO.

DEFINE VARIABLE TOGGLE-Resumen-Linea AS LOGICAL INITIAL yes 
     LABEL "Resumido por Linea y Sub-Linea" 
     VIEW-AS TOGGLE-BOX
     SIZE 32 BY .77 NO-UNDO.

DEFINE VARIABLE TOGGLE-Resumen-Marca AS LOGICAL INITIAL no 
     LABEL "Resumido por Marca" 
     VIEW-AS TOGGLE-BOX
     SIZE 32 BY .77 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     BUTTON-1 AT ROW 1 COL 112 WIDGET-ID 24
     BtnDone AT ROW 1 COL 118 WIDGET-ID 28
     FILL-IN-Mensaje AT ROW 1.27 COL 6 COLON-ALIGNED NO-LABEL WIDGET-ID 22
     DesdeF AT ROW 3.15 COL 29 COLON-ALIGNED WIDGET-ID 10
     HastaF AT ROW 3.15 COL 49 COLON-ALIGNED WIDGET-ID 12
     RADIO-SET-Tipo AT ROW 4.23 COL 31 NO-LABEL WIDGET-ID 56
     COMBO-BOX-Tipo AT ROW 5.58 COL 29 COLON-ALIGNED NO-LABEL WIDGET-ID 88
     TOGGLE-CodDiv AT ROW 7.46 COL 11 WIDGET-ID 2
     COMBO-BOX-CodDiv AT ROW 7.46 COL 29 COLON-ALIGNED WIDGET-ID 30
     TOGGLE-CodCli AT ROW 9.08 COL 11 WIDGET-ID 6
     FILL-IN-CodCli AT ROW 9.08 COL 29 COLON-ALIGNED NO-LABEL WIDGET-ID 32
     FILL-IN-NomCli AT ROW 9.08 COL 44 COLON-ALIGNED NO-LABEL WIDGET-ID 34
     BUTTON-5 AT ROW 10.15 COL 108 WIDGET-ID 66
     FILL-IN-file AT ROW 10.19 COL 29 COLON-ALIGNED NO-LABEL WIDGET-ID 68
     TOGGLE-Resumen-Depto AT ROW 11.23 COL 31 WIDGET-ID 64
     TOGGLE-CodMat AT ROW 12.58 COL 11 WIDGET-ID 14
     COMBO-BOX-CodFam AT ROW 12.58 COL 29 COLON-ALIGNED WIDGET-ID 36
     TOGGLE-Resumen-Linea AT ROW 12.58 COL 81 WIDGET-ID 60
     COMBO-BOX-SubFam AT ROW 13.65 COL 29 COLON-ALIGNED WIDGET-ID 38
     TOGGLE-Resumen-Marca AT ROW 13.65 COL 81 WIDGET-ID 62
     FILL-IN-CodMat AT ROW 14.73 COL 29.14 COLON-ALIGNED WIDGET-ID 70
     FILL-IN-DesMat AT ROW 14.73 COL 44.14 COLON-ALIGNED NO-LABEL WIDGET-ID 72
     BUTTON-6 AT ROW 15.77 COL 108 WIDGET-ID 74
     FILL-IN-file-2 AT ROW 15.81 COL 29 COLON-ALIGNED NO-LABEL WIDGET-ID 76
     FILL-IN-CodPro AT ROW 16.88 COL 29 COLON-ALIGNED WIDGET-ID 40
     FILL-IN-NomPro AT ROW 16.88 COL 44 COLON-ALIGNED NO-LABEL WIDGET-ID 42
     TOGGLE-CodVen AT ROW 18.5 COL 11 WIDGET-ID 18
     FILL-IN-CodVen AT ROW 18.5 COL 29 COLON-ALIGNED NO-LABEL WIDGET-ID 44
     FILL-IN-NomVen AT ROW 18.5 COL 36 COLON-ALIGNED NO-LABEL WIDGET-ID 46
     RECT-1 AT ROW 6.92 COL 8 WIDGET-ID 78
     RECT-2 AT ROW 8.81 COL 8 WIDGET-ID 80
     RECT-3 AT ROW 12.31 COL 8 WIDGET-ID 82
     RECT-4 AT ROW 18.23 COL 8 WIDGET-ID 84
     RECT-5 AT ROW 2.88 COL 8 WIDGET-ID 86
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 128.72 BY 19.5 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Other Settings: COMPILE APPSERVER
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "ESTADISTICAS DE VENTAS COMPARATIVAS"
         HEIGHT             = 19.5
         WIDTH              = 128.72
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.14
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.14
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME fMain
   FRAME-NAME                                                           */
/* SETTINGS FOR BUTTON BUTTON-5 IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-6 IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX COMBO-BOX-CodDiv IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX COMBO-BOX-CodFam IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX COMBO-BOX-SubFam IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-CodCli IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-CodMat IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-CodPro IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-CodVen IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-DesMat IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-file IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-file-2 IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-Mensaje IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-NomCli IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-NomPro IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-NomVen IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET RADIO-SET-Tipo IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TOGGLE-Resumen-Depto IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TOGGLE-Resumen-Linea IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TOGGLE-Resumen-Marca IN FRAME fMain
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* ESTADISTICAS DE VENTAS COMPARATIVAS */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* ESTADISTICAS DE VENTAS COMPARATIVAS */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnDone
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnDone wWin
ON CHOOSE OF BtnDone IN FRAME fMain /* Done */
DO:
  &IF "{&PROCEDURE-TYPE}" EQ "SmartPanel" &THEN
    &IF "{&ADM-VERSION}" EQ "ADM1.1" &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
    &ELSE
      RUN exitObject.
    &ENDIF
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 wWin
ON CHOOSE OF BUTTON-1 IN FRAME fMain /* Button 1 */
DO:
    ASSIGN
        TOGGLE-CodCli 
        TOGGLE-CodDiv 
        TOGGLE-CodMat 
        TOGGLE-CodVen
        TOGGLE-Resumen-Linea 
        TOGGLE-Resumen-Marca
        TOGGLE-Resumen-Depto.
    ASSIGN
        COMBO-BOX-CodDiv 
        COMBO-BOX-CodFam 
        COMBO-BOX-SubFam 
        COMBO-BOX-Tipo
        FILL-IN-CodCli 
        FILL-IN-CodPro 
        FILL-IN-CodVen 
        FILL-IN-file 
        FILL-IN-file-2.
    ASSIGN
        RADIO-SET-Tipo
        DesdeF HastaF.
    /* CONSISTENCIA */
    IF (TOGGLE-CodCli OR 
        TOGGLE-CodDiv OR
        TOGGLE-CodMat OR
        TOGGLE-CodVen
        ) = NO
        THEN DO:
        MESSAGE 'Debe seleccionar por lo menos uno' VIEW-AS ALERT-BOX WARNING.
        RETURN NO-APPLY.
    END.
    IF HastaF - DesdeF > 30 AND RADIO-SET-Tipo = 3 THEN DO:
        MESSAGE 'La diferencia de fechas es de m�s de 30 d�as' SKIP
            'La informaci�n puede ser excesiva para cargarla en una hoja Excel' SKIP
            '�Continuamos con el proceso?' 
            VIEW-AS ALERT-BOX WARNING BUTTONS YES-NO UPDATE rpta AS LOG.
        IF rpta = NO THEN RETURN NO-APPLY.
    END.
    RUN lib/tt-file-to-text (OUTPUT pOptions, OUTPUT pArchivo).
    IF pOptions = "" THEN RETURN NO-APPLY.

    SESSION:SET-WAIT-STATE('GENERAL').
    RUN Carga-Temporal.
    SESSION:SET-WAIT-STATE('GENERAL').

    FIND FIRST Detalle NO-ERROR.
    IF NOT AVAILABLE Detalle THEN DO:
        MESSAGE 'No hay registros' VIEW-AS ALERT-BOX WARNING.
        RETURN NO-APPLY.
    END.
    pOptions = pOptions + CHR(1) + "SkipList:Llave".

    RUN lib/tt-file (TEMP-TABLE Detalle:HANDLE, pArchivo, pOptions).

    ASSIGN
       FILL-IN-file = ''
       FILL-IN-file-2 = ''.

    DISPLAY FILL-IN-file FILL-IN-file-2 WITH FRAME {&FRAME-NAME}.
    MESSAGE 'Proceso terminado' VIEW-AS ALERT-BOX INFORMATION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-5 wWin
ON CHOOSE OF BUTTON-5 IN FRAME fMain /* ... */
DO:

    DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.

    SYSTEM-DIALOG GET-FILE FILL-IN-file
        FILTERS
            "Archivos Texto (*.txt)" "*.txt"
        TITLE
            "Archivo(s) de Carga..."
        MUST-EXIST
        USE-FILENAME
        UPDATE OKpressed.

    IF OKpressed = TRUE THEN
        FILL-IN-file:SCREEN-VALUE = FILL-IN-file.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-6 wWin
ON CHOOSE OF BUTTON-6 IN FRAME fMain /* ... */
DO:

    DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.

    SYSTEM-DIALOG GET-FILE FILL-IN-file-2
        FILTERS
            "Archivos Texto (*.txt)" "*.txt"
        TITLE
            "Archivo(s) de Carga..."
        MUST-EXIST
        USE-FILENAME
        UPDATE OKpressed.

    IF OKpressed = TRUE THEN
        FILL-IN-file-2:SCREEN-VALUE = FILL-IN-file-2.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME COMBO-BOX-CodFam
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL COMBO-BOX-CodFam wWin
ON VALUE-CHANGED OF COMBO-BOX-CodFam IN FRAME fMain /* Linea */
DO:
    COMBO-BOX-SubFam:DELETE(COMBO-BOX-SubFam:LIST-ITEMS).
    COMBO-BOX-SubFam:ADD-LAST('Todos').
    COMBO-BOX-SubFam:SCREEN-VALUE = 'Todos'.
    IF SELF:SCREEN-VALUE <> 'Todos' THEN DO:
        FOR EACH dwh_lineas NO-LOCK WHERE dwh_lineas.codcia = s-codcia
            AND dwh_lineas.codfam = ENTRY(1, SELF:SCREEN-VALUE, ' - '):
            COMBO-BOX-SubFam:ADD-LAST(dwh_lineas.subfam + ' - '+ dwh_Lineas.NomSubFam).
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-CodCli
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-CodCli wWin
ON LEAVE OF FILL-IN-CodCli IN FRAME fMain
DO:
  IF SELF:SCREEN-VALUE = '' THEN RETURN.
  FIND dwh_Cliente WHERE dwh_Cliente.codcia = cl-codcia
      AND dwh_Cliente.codcli = SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
  IF NOT AVAILABLE dwh_Cliente THEN DO:
      MESSAGE 'Cliente no registrado' VIEW-AS ALERT-BOX ERROR.
      SELF:SCREEN-VALUE = ''.
      RETURN NO-APPLY.
  END.
  FILL-IN-NomCli:SCREEN-VALUE = dwh_Cliente.nomcli.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-CodCli wWin
ON LEFT-MOUSE-DBLCLICK OF FILL-IN-CodCli IN FRAME fMain
OR F8 OF FILL-IN-CodCli
DO:
    RUN lkup/c-client ('Clientes').
    IF output-var-1 <> ? THEN SELF:SCREEN-VALUE = output-var-2.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-CodMat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-CodMat wWin
ON LEAVE OF FILL-IN-CodMat IN FRAME fMain /* Codigo */
DO:
  IF SELF:SCREEN-VALUE = '' THEN RETURN.
  FIND dwh_Producto WHERE dwh_Producto.codcia = s-codcia
      AND dwh_Producto.codmat = SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
  IF NOT AVAILABLE dwh_Producto THEN DO:
      MESSAGE 'Articulo Inv�lido' VIEW-AS ALERT-BOX ERROR.
      SELF:SCREEN-VALUE = ''.
      RETURN NO-APPLY.
  END.
  FILL-IN-DesMat:SCREEN-VALUE = dwh_Producto.desmat.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-CodPro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-CodPro wWin
ON LEAVE OF FILL-IN-CodPro IN FRAME fMain /* Proveedor */
DO:
    IF SELF:SCREEN-VALUE = '' THEN RETURN.
    FIND dwh_Proveedor WHERE dwh_Proveedor.codcia = pv-codcia
        AND dwh_Proveedor.codpro = SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
    IF NOT AVAILABLE dwh_Proveedor THEN DO:
        MESSAGE 'Proveedor no registrado' VIEW-AS ALERT-BOX ERROR.
        SELF:SCREEN-VALUE = ''.
        RETURN NO-APPLY.
    END.
    FILL-IN-NomPro:SCREEN-VALUE = dwh_Proveedor.nompro.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-CodPro wWin
ON LEFT-MOUSE-DBLCLICK OF FILL-IN-CodPro IN FRAME fMain /* Proveedor */
DO:
    RUN lkup/c-provee ('Proveedores').
    IF output-var-1 <> ? THEN SELF:SCREEN-VALUE = output-var-2.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-CodVen
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-CodVen wWin
ON LEAVE OF FILL-IN-CodVen IN FRAME fMain
DO:
    IF SELF:SCREEN-VALUE = '' THEN RETURN.
    FIND dwh_Vendedor WHERE dwh_Vendedor.codcia = s-codcia
        AND dwh_Vendedor.codven = SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
    IF NOT AVAILABLE dwh_Vendedor THEN DO:
        MESSAGE 'Vendedor no registrado' VIEW-AS ALERT-BOX ERROR.
        SELF:SCREEN-VALUE = ''.
        RETURN NO-APPLY.
    END.
    FILL-IN-NomVen:SCREEN-VALUE = dwh_Vendedor.nomven.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOGGLE-CodCli
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOGGLE-CodCli wWin
ON VALUE-CHANGED OF TOGGLE-CodCli IN FRAME fMain /* Cliente */
DO:
    FILL-IN-CodCli:SENSITIVE = NOT FILL-IN-CodCli:SENSITIVE.
    TOGGLE-Resumen-Depto:SENSITIVE = NOT TOGGLE-Resumen-Depto:SENSITIVE.
    IF INPUT {&self-name} = YES 
        THEN ASSIGN
                BUTTON-5:SENSITIVE = YES.
        ELSE ASSIGN     
                BUTTON-5:SENSITIVE = NO
                FILL-IN-file:SCREEN-VALUE = ''.
    APPLY 'VALUE-CHANGED' TO TOGGLE-Resumen-Linea.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOGGLE-CodDiv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOGGLE-CodDiv wWin
ON VALUE-CHANGED OF TOGGLE-CodDiv IN FRAME fMain /* Division */
DO:
  COMBO-BOX-CodDiv:SENSITIVE = NOT COMBO-BOX-CodDiv:SENSITIVE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOGGLE-CodMat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOGGLE-CodMat wWin
ON VALUE-CHANGED OF TOGGLE-CodMat IN FRAME fMain /* Art�culo */
DO:
    COMBO-BOX-CodFam:SENSITIVE = NOT COMBO-BOX-CodFam:SENSITIVE.
    COMBO-BOX-SubFam:SENSITIVE = NOT COMBO-BOX-SubFam:SENSITIVE.
    FILL-IN-CodPro:SENSITIVE = NOT FILL-IN-CodPro:SENSITIVE.
    FILL-IN-CodMat:SENSITIVE = NOT FILL-IN-CodMat:SENSITIVE.
    TOGGLE-Resumen-Linea:SENSITIVE = NOT TOGGLE-Resumen-Linea:SENSITIVE.
    TOGGLE-Resumen-Marca:SENSITIVE = NOT TOGGLE-Resumen-Marca:SENSITIVE.
    APPLY 'VALUE-CHANGED' TO TOGGLE-Resumen-Linea.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOGGLE-CodVen
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOGGLE-CodVen wWin
ON VALUE-CHANGED OF TOGGLE-CodVen IN FRAME fMain /* Vendedor */
DO:
    FILL-IN-CodVen:SENSITIVE = NOT FILL-IN-CodVen:SENSITIVE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOGGLE-Resumen-Linea
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOGGLE-Resumen-Linea wWin
ON VALUE-CHANGED OF TOGGLE-Resumen-Linea IN FRAME fMain /* Resumido por Linea y Sub-Linea */
DO:
  ASSIGN
      TOGGLE-CodCli TOGGLE-CodMat TOGGLE-Resumen-Linea TOGGLE-Resumen-Marca.
  IF ( TOGGLE-CodCli = YES AND TOGGLE-CodMat = YES 
       AND ( TOGGLE-Resumen-Linea = YES OR TOGGLE-Resumen-Marca = YES ) )
  THEN ASSIGN 
            FILL-IN-CodPro:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ''
            FILL-IN-CodPro:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
  ELSE ASSIGN
            FILL-IN-CodPro:SENSITIVE IN FRAME {&FRAME-NAME} = YES.

  IF TOGGLE-Resumen-Linea = YES OR TOGGLE-Resumen-Marca = YES 
      THEN ASSIGN
                FILL-IN-file-2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ''
                BUTTON-6:SENSITIVE IN FRAME {&FRAME-NAME} = NO
                FILL-IN-CodMat:SENSITIVE IN FRAME {&FRAME-NAME} = NO
                FILL-IN-CodMat:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ''.
  ELSE ASSIGN
            BUTTON-6:SENSITIVE IN FRAME {&FRAME-NAME} = YES
            FILL-IN-CodMat:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOGGLE-Resumen-Marca
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOGGLE-Resumen-Marca wWin
ON VALUE-CHANGED OF TOGGLE-Resumen-Marca IN FRAME fMain /* Resumido por Marca */
DO:
    APPLY 'VALUE-CHANGED' TO TOGGLE-Resumen-Linea.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm2/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects wWin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Carga-Lista-Articulos wWin 
PROCEDURE Carga-Lista-Articulos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    EMPTY TEMP-TABLE tt-datos.
    EMPTY TEMP-TABLE tt-articulo.

    /* Carga de Excel */
    IF SEARCH(FILL-IN-file-2) <> ? THEN DO:
        INPUT FROM VALUE(FILL-IN-file-2).
        REPEAT:
            CREATE tt-datos.
            IMPORT tt-codigo.
        END.
        INPUT CLOSE.
    END.
    FOR EACH tt-datos WHERE tt-datos.tt-codigo = '':
        DELETE tt-datos.
    END.
    FIND FIRST tt-datos NO-LOCK NO-ERROR.
    IF NOT AVAIL tt-datos THEN DO:
        CREATE tt-datos.        
        ASSIGN tt-codigo = FILL-IN-CodMat.
    END.

    /*Carga Tabla Articulos*/
    FOR EACH tt-datos:
        FIND dwh_Producto WHERE dwh_Producto.codcia = s-codcia
            AND dwh_Producto.codmat = tt-datos.tt-codigo
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE dwh_Producto THEN NEXT.
        FIND FIRST tt-articulo WHERE tt-articulo.tt-codmat = tt-codigo 
            NO-LOCK NO-ERROR.
        IF NOT AVAIL tt-articulo THEN  DO:
            CREATE tt-articulo.
            ASSIGN tt-articulo.tt-codmat = tt-datos.tt-codigo.
        END.
    END.

    x-CodMat = ''.
    FOR EACH tt-articulo:
        IF x-CodMat = '' THEN x-CodMat = tt-articulo.tt-codmat.
        ELSE x-CodMat = x-CodMat + ',' + tt-articulo.tt-codmat.
    END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Carga-Lista-Clientes wWin 
PROCEDURE Carga-Lista-Clientes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEF VAR x-linea LIKE dwh_Cliente.codcli.

    EMPTY TEMP-TABLE tt-cliente.

    /* Carga de Excel */
    IF SEARCH(FILL-IN-file) <> ? THEN DO:
        INPUT FROM VALUE(FILL-IN-file).
        REPEAT:
            IMPORT UNFORMATTED x-linea.
            FIND tt-cliente WHERE tt-cliente.tt-codcli = x-linea NO-ERROR.
            IF NOT AVAILABLE tt-cliente THEN CREATE tt-cliente.
            tt-cliente.tt-codcli = x-linea.
        END.
        INPUT CLOSE.
    END.
    
    FOR EACH tt-cliente WHERE tt-cliente.tt-codcli = '':
        DELETE tt-cliente.
    END.

    FIND FIRST tt-cliente NO-LOCK NO-ERROR.
    IF NOT AVAIL tt-cliente THEN DO:
        CREATE tt-cliente.        
        ASSIGN tt-codcli = fill-in-codcli.
    END.

    FOR EACH tt-cliente:
        FIND dwh_Cliente WHERE dwh_Cliente.codcia = cl-codcia
            AND dwh_Cliente.codcli = tt-cliente.tt-codcli
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE dwh_Cliente THEN DELETE tt-cliente.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Carga-Temporal wWin 
PROCEDURE Carga-Temporal :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR Control-Resumen AS LOG INIT NO NO-UNDO.

EMPTY TEMP-TABLE Detalle.

/* INFORMACION RESUMIDA */
ASSIGN
    x-CodDiv = ''
    x-CodCli = ''
    x-CodPro = ''
    x-CodVen = ''
    x-CodFam = ''
    x-SubFam = ''
    x-CodMat = ''
    x-CuentaReg = 0
    x-MuestraReg = 1000.    /* cada 1000 registros */

IF TOGGLE-CodDiv AND NOT COMBO-BOX-CodDiv BEGINS 'Todos' THEN x-CodDiv = ENTRY(1, COMBO-BOX-CodDiv, ' - ').
IF TOGGLE-CodCli THEN x-CodCli = FILL-IN-CodCli.
IF TOGGLE-CodMat THEN x-CodMat = FILL-IN-CodMat.
IF TOGGLE-CodMat AND NOT COMBO-BOX-CodFam BEGINS 'Todos' THEN x-CodFam = ENTRY(1, COMBO-BOX-CodFam, ' - ').
IF TOGGLE-CodMat AND NOT COMBO-BOX-SubFam BEGINS 'Todos' THEN x-SubFam = ENTRY(1, COMBO-BOX-SubFam, ' - ').
IF TOGGLE-CodMat THEN x-CodPro = FILL-IN-CodPro.
IF TOGGLE-CodVen THEN x-CodVen = FILL-IN-CodVen.

RUN Carga-Lista-Articulos.
RUN Carga-Lista-Clientes.

/* FILTROS */
/* Por Divisi�n y/o Vendedor */
IF (TOGGLE-CodDiv = YES OR TOGGLE-CodVen = YES) AND ( TOGGLE-CodCli = NO AND TOGGLE-CodMat = NO)
    THEN DO:
    RUN Resumen-por-division.
    Control-Resumen = YES.
END.
/* Por Clientes */
IF TOGGLE-CodCli = YES AND TOGGLE-CodMat = NO AND TOGGLE-CodVen = NO
    THEN DO:
    RUN Resumen-por-cliente.
    Control-Resumen = YES.
END.
/* Por Clientes y Productos Resumidos por Linea y/o Sublinea */
IF TOGGLE-CodCli = YES AND TOGGLE-CodMat = YES AND TOGGLE-CodVen = NO
    AND (TOGGLE-Resumen-Linea = YES OR TOGGLE-Resumen-Marca = YES)
    THEN DO:
    RUN Resumen-por-climat.
    Control-Resumen = YES.
END.
/* Por Clientes y Vendedor */
IF TOGGLE-CodCli = YES AND TOGGLE-CodMat = NO AND TOGGLE-CodVen = YES
    THEN DO:
    RUN Resumen-por-vendcli.
    Control-Resumen = YES.
END.
/* Por Producto */
IF TOGGLE-CodCli = NO AND TOGGLE-CodMat = YES AND TOGGLE-CodVen = NO
    AND TOGGLE-Resumen-Linea = NO AND TOGGLE-Resumen-Marca = NO
    THEN DO:
    RUN Resumen-por-producto.
    Control-Resumen = YES.
END.
IF TOGGLE-CodCli = NO AND TOGGLE-CodMat = YES 
    AND (TOGGLE-Resumen-Linea = YES OR TOGGLE-Resumen-Marca = YES)
    THEN DO:
    RUN Resumen-por-linea.
    Control-Resumen = YES.
END.

IF Control-Resumen = NO THEN DO:
    ASSIGN
        iContador = 1       /* Valor por defecto */
        x-CodCli = ''.
    FOR EACH tt-cliente NO-LOCK:
        iContador = iContador + 1.
        IF x-CodCli = '' THEN x-CodCli = tt-cliente.tt-codcli.
        ELSE x-CodCli = x-CodCli + ',' + tt-cliente.tt-codcli.
        IF iContador = 1000 THEN DO:
            {est/estad102.i}
            ASSIGN
                iContador = 0
                x-CodCli = ''.
        END.
    END.
    IF iContador > 0 THEN DO:
        {est/estad102.i}
    END.
END.
IF COMBO-BOX-Tipo = "Cantidades" THEN pOptions = pOptions + ',CanxMes-1,CanxMes-3,CanxMes-2,CanxMes-4'.
IF COMBO-BOX-Tipo BEGINS "Ventas" THEN pOptions = pOptions + ',VtaxMesMe-1,VtaxMesMn-1,' +
    'VtaxMesMe-3,VtaxMesMn-3,VtaxMesMe-2,VtaxMesMn-2,VtaxMesMe-4,VtaxMesMn-4'.
IF COMBO-BOX-Tipo = "Ventas vs Costo de Reposicion" THEN pOptions = pOptions + ',CtoxMesMe-1,CtoxMesMn-1,' +
    'CtoxMesMe-3,CtoxMesMn-3,CtoxMesMe-2,CtoxMesMn-2,CtoxMesMe-4,CtoxMesMn-4'.
IF COMBO-BOX-Tipo = "Ventas vs Costo Promedio" THEN pOptions = pOptions + ',ProxMesMe-1,ProxMesMn-1,' +
    'ProxMesMe-3,ProxMesMn-3,ProxMesMe-2,ProxMesMn-2,ProxMesMe-4,ProxMesMn-4'.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI wWin  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
  THEN DELETE WIDGET wWin.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI wWin  _DEFAULT-ENABLE
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
  DISPLAY FILL-IN-Mensaje DesdeF HastaF RADIO-SET-Tipo COMBO-BOX-Tipo 
          TOGGLE-CodDiv COMBO-BOX-CodDiv TOGGLE-CodCli FILL-IN-CodCli 
          FILL-IN-NomCli FILL-IN-file TOGGLE-Resumen-Depto TOGGLE-CodMat 
          COMBO-BOX-CodFam TOGGLE-Resumen-Linea COMBO-BOX-SubFam 
          TOGGLE-Resumen-Marca FILL-IN-CodMat FILL-IN-DesMat FILL-IN-file-2 
          FILL-IN-CodPro FILL-IN-NomPro TOGGLE-CodVen FILL-IN-CodVen 
          FILL-IN-NomVen 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE RECT-1 RECT-2 RECT-3 RECT-4 RECT-5 BUTTON-1 BtnDone DesdeF HastaF 
         COMBO-BOX-Tipo TOGGLE-CodDiv TOGGLE-CodCli TOGGLE-CodMat TOGGLE-CodVen 
      WITH FRAME fMain IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  VIEW wWin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exitObject wWin 
PROCEDURE exitObject :
/*------------------------------------------------------------------------------
  Purpose:  Window-specific override of this procedure which destroys 
            its contents and itself.
    Notes:  
------------------------------------------------------------------------------*/

  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  ASSIGN
    DesdeF = TODAY - DAY(TODAY) + 1
    HastaF = TODAY.
  FOR EACH dwh_Division NO-LOCK WHERE dwh_Division.codcia = s-codcia:
      COMBO-BOX-CodDiv:ADD-LAST(dwh_Division.coddiv + ' - ' + dwh_Division.DesDiv) IN FRAME {&FRAME-NAME}.
  END.
  FOR EACH dwh_lineas NO-LOCK WHERE dwh_lineas.CodCia = s-codcia
      BREAK BY dwh_Lineas.codfam:
      IF FIRST-OF(dwh_Lineas.codfam) THEN
      COMBO-BOX-CodFam:ADD-LAST(dwh_lineas.codfam + ' - ' + dwh_lineas.nomfam) IN FRAME {&FRAME-NAME}.
  END.
  CASE pParametro:
      WHEN "+COSTO" THEN COMBO-BOX-Tipo:ADD-LAST("Ventas vs Costo de Reposicion,Ventas vs Costo Promedio") IN FRAME {&FRAME-NAME}.
      WHEN "-COSTO" THEN COMBO-BOX-Tipo:ADD-LAST("Ventas") IN FRAME {&FRAME-NAME}.
  END CASE.

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Resumen-por-cliente wWin 
PROCEDURE Resumen-por-cliente :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

ASSIGN
    iContador = 1       /* Valor por defecto */
    x-CodCli = ''.
FOR EACH tt-cliente NO-LOCK:
    iContador = iContador + 1.
    IF x-CodCli = '' THEN x-CodCli = tt-cliente.tt-codcli.
    ELSE x-CodCli = x-CodCli + ',' + tt-cliente.tt-codcli.
    IF iContador = 1000 THEN DO:
        {est/estad102-cliente.i}
        ASSIGN
            iContador = 0
            x-CodCli = ''.
    END.
END.
IF iContador > 0 THEN DO:
    {est/estad102-cliente.i}
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Resumen-por-climat wWin 
PROCEDURE Resumen-por-climat :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

ASSIGN
    iContador = 1       /* Valor por defecto */
    x-CodCli = ''.
FOR EACH tt-cliente NO-LOCK:
    iContador = iContador + 1.
    IF x-CodCli = '' THEN x-CodCli = tt-cliente.tt-codcli.
    ELSE x-CodCli = x-CodCli + ',' + tt-cliente.tt-codcli.
    IF iContador = 1000 THEN DO:
        {est/estad102-cliente.i}
        ASSIGN
            iContador = 0
            x-CodCli = ''.
    END.
END.
IF iContador > 0 THEN DO:
    {est/estad102-cliente.i}
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Resumen-por-division wWin 
PROCEDURE Resumen-por-division :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

ASSIGN
    x-NroFchR = INTEGER( STRING(YEAR(DesdeF) - 1, '9999') + '0101')
    x-NroFchE = INTEGER( STRING(YEAR(HastaF), '9999') + STRING(MONTH(HastaF), '99') + '31').

FOR EACH dwh_ventas_vend NO-LOCK WHERE dwh_ventas_vend.codcia = s-codcia
    AND dwh_ventas_vend.fecha >= x-NroFchR
    AND dwh_ventas_vend.fecha <= x-NroFchE
    AND dwh_ventas_vend.coddiv BEGINS x-CodDiv
    AND dwh_ventas_vend.codven BEGINS x-CodVen,
    FIRST dwh_Division OF dwh_ventas_vend NO-LOCK,
    FIRST dwh_Vendedor OF dwh_ventas_vend NO-LOCK,
    FIRST dwh_tiempo NO-LOCK WHERE dwh_tiempo.codcia = s-codcia AND dwh_tiempo.fecha = dwh_ventas_vend.fecha:
    IF x-CuentaReg = 0 OR ( x-CuentaReg MODULO x-MuestraReg ) = 0 THEN
        FILL-IN-Mensaje:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '** TABLA GENERAL ' + 
        ' FECHA ' + STRING (dwh_Tiempo.NroDia, '99') +
        ' ' + dwh_Tiempo.NombreMes + ' ' + STRING (dwh_Tiempo.Periodo, '9999') +
        ' DIVISION ' + dwh_ventas_vend.coddiv + ' **'.
    x-CuentaReg = x-CuentaReg + 1.
    /* ARMAMOS LA LLAVE */
    ASSIGN
        x-Llave = ''
        cCampania = ''
        cPeriodo = 0
        cNroMes = 0
        cDia = ?
        cDivision = ''
        cCanalVenta = ''
        cProducto = ''
        cLinea = ''
        cSublinea = ''
        cMarca = ''
        cUnidad = ''
        cLicencia = ''
        cProveedor = ''
        cCliente = ''
        cCanal = ''
        cTarjeta = ''
        cDepartamento = ''
        cProvincia = ''
        cDistrito = ''
        cZona = ''
        cClasificacion = ''
        cTipo = ''
        cVendedor = ''
        lOptions = "FieldList:".
    /* LLAVE INICIAL */
    IF RADIO-SET-Tipo = 2 THEN DO:
        IF x-Llave = '' THEN x-Llave = dwh_Tiempo.Campania.
        ELSE x-Llave = x-Llave + dwh_Tiempo.Campania.
        x-Llave = x-Llave + '|'.
        x-Llave = x-Llave + STRING(dwh_Tiempo.Periodo, '9999').
        x-Llave = x-Llave + '|'.
        x-Llave = x-Llave + STRING(dwh_Tiempo.NroMes, '99').      /*dwh_Tiempo.NombreMes. */
        x-Llave = x-LLave + '|'.
        ASSIGN
            cCampania = dwh_Tiempo.Campania
            cPeriodo  = dwh_Tiempo.Periodo
            cNroMes   = dwh_Tiempo.NroMes
            lOptions = lOptions + 'Campania,Periodo,Nromes'.
    END.
    IF TOGGLE-CodDiv = YES THEN DO:
         IF x-Llave = '' THEN x-Llave = dwh_ventas_vend.coddiv + '|'.
         ELSE x-Llave = x-LLave + dwh_ventas_vend.coddiv + '|'.
         x-Llave = x-Llave + dwh_Division.CanalVenta + '|'.
         ASSIGN
             cDivision = dwh_ventas_vend.coddiv + ' ' + dwh_Division.DesDiv
             cCanalVenta = dwh_Division.CanalVenta + ' ' + dwh_Division.NomCanalVenta
             lOptions = lOptions + (IF SUBSTRING(lOptions, LENGTH(lOptions), 1) = ':' THEN '' ELSE ',') +
                        'Division,CanalVenta'.
                                
    END.
     /* ******************************************** */
     IF TOGGLE-CodVen = YES THEN DO:
         IF x-Llave = '' THEN x-Llave = dwh_ventas_vend.codven + '|'.
         ELSE x-Llave = x-Llave + dwh_ventas_vend.codven + '|'.
         ASSIGN
             cVendedor = dwh_ventas_vend.codven + ' ' + dwh_Vendedor.NomVen
             lOptions = lOptions + (IF SUBSTRING(lOptions, LENGTH(lOptions), 1) = ':' THEN '' ELSE ',') +
                        'Vendedor'.
     END.
     /* ******************************************** */
     FIND Detalle WHERE Detalle.llave = x-Llave NO-ERROR.
     IF NOT AVAILABLE Detalle THEN DO:
         CREATE Detalle.
         Detalle.llave = x-Llave.
     END.
     ASSIGN
         Detalle.Campania = cCampania
         Detalle.Periodo = cPeriodo
         Detalle.NroMes = cNroMes
         Detalle.Dia = cDia
         Detalle.Division = cDivision
         Detalle.CanalVenta = cCanalVenta
         Detalle.Producto = cProducto
         Detalle.Linea = cLinea
         Detalle.Sublinea = cSublinea
         Detalle.Marca = cMarca
         Detalle.Unidad = cUnidad
         Detalle.Licencia = cLicencia
         Detalle.Proveedor = cProveedor
         Detalle.Cliente = cCliente
         Detalle.Canal = cCanal
         Detalle.Tarjeta = cTarjeta
         Detalle.Departamento = cDepartamento
         Detalle.Provincia = cProvincia
         Detalle.Distrito = cDistrito
         Detalle.Zona = cZona 
         Detalle.Clasificacion = cClasificacion
         Detalle.Tipo = cTipo
         Detalle.Vendedor = cVendedor.
     x-Fecha = DATE(SUBSTRING(STRING(dwh_ventas_vend.fecha, '99999999'),7,2) + '/' +
               SUBSTRING(STRING(dwh_ventas_vend.fecha, '99999999'),5,2) + '/' +
               SUBSTRING(STRING(dwh_ventas_vend.fecha, '99999999'),1,4)).
     /* PERIODO ACTUAL */
     IF x-Fecha >= DesdeF AND x-Fecha <= HastaF THEN DO:
         ASSIGN
             /*Detalle.CanxMes-1   = Detalle.CanxMes-1   + dwh_ventas_vend.Cantidad*/
             Detalle.VtaxMesMe-1 = Detalle.VtaxMesMe-1 + dwh_ventas_vend.ImpExtCIGV
             Detalle.VtaxMesMn-1 = Detalle.VtaxMesMn-1 + dwh_ventas_vend.ImpNacCIGV
             Detalle.CtoxMesMe-1 = Detalle.CtoxMesMe-1 + dwh_ventas_vend.CostoExtCIGV
             Detalle.CtoxMesMn-1 = Detalle.CtoxMesMn-1 + dwh_ventas_vend.CostoNacCIGV
             Detalle.ProxMesMe-1 = Detalle.ProxMesMe-1 + dwh_ventas_vend.PromExtCIGV
             Detalle.ProxMesMn-1 = Detalle.ProxMesMn-1 + dwh_ventas_vend.PromNacCIGV.
     END.
     /* ACUMULADO PERIODO ACTUAL */
     IF x-Fecha >= DATE(01,01,YEAR(DesdeF)) AND x-Fecha <= HastaF THEN DO:
         ASSIGN
             /*Detalle.CanxMes-2   = Detalle.CanxMes-2   + dwh_ventas_vend.Cantidad*/
             Detalle.VtaxMesMe-2 = Detalle.VtaxMesMe-2 + dwh_ventas_vend.ImpExtCIGV
             Detalle.VtaxMesMn-2 = Detalle.VtaxMesMn-2 + dwh_ventas_vend.ImpNacCIGV
             Detalle.CtoxMesMe-2 = Detalle.CtoxMesMe-2 + dwh_ventas_vend.CostoExtCIGV
             Detalle.CtoxMesMn-2 = Detalle.CtoxMesMn-2 + dwh_ventas_vend.CostoNacCIGV
             Detalle.ProxMesMe-2 = Detalle.ProxMesMe-2 + dwh_ventas_vend.PromExtCIGV
             Detalle.ProxMesMn-2 = Detalle.ProxMesMn-2 + dwh_ventas_vend.PromNacCIGV.
     END.
     /* PERIODO ANTERIOR */
     IF x-Fecha >= DATE(MONTH(DesdeF),DAY(DesdeF),YEAR(DesdeF) - 1) AND x-Fecha <= DATE(MONTH(HastaF),DAY(HastaF),YEAR(HastaF) - 1) THEN DO:
         ASSIGN
             /*Detalle.CanxMes-3   = Detalle.CanxMes-3   + dwh_ventas_vend.Cantidad*/
             Detalle.VtaxMesMe-3 = Detalle.VtaxMesMe-3 + dwh_ventas_vend.ImpExtCIGV
             Detalle.VtaxMesMn-3 = Detalle.VtaxMesMn-3 + dwh_ventas_vend.ImpNacCIGV
             Detalle.CtoxMesMe-3 = Detalle.CtoxMesMe-3 + dwh_ventas_vend.CostoExtCIGV
             Detalle.CtoxMesMn-3 = Detalle.CtoxMesMn-3 + dwh_ventas_vend.CostoNacCIGV
             Detalle.ProxMesMe-3 = Detalle.ProxMesMe-3 + dwh_ventas_vend.PromExtCIGV
             Detalle.ProxMesMn-3 = Detalle.ProxMesMn-3 + dwh_ventas_vend.PromNacCIGV.
     END.
     /* ACUMULADO PERIODO ANTERIOR */
     IF x-Fecha >= DATE(01,01,YEAR(DesdeF) - 1) AND x-Fecha <= DATE(MONTH(HastaF),DAY(HastaF),YEAR(HastaF) - 1) THEN DO:
         ASSIGN
             /*Detalle.CanxMes-4   = Detalle.CanxMes-4   + dwh_ventas_vend.Cantidad*/
             Detalle.VtaxMesMe-4 = Detalle.VtaxMesMe-4 + dwh_ventas_vend.ImpExtCIGV
             Detalle.VtaxMesMn-4 = Detalle.VtaxMesMn-4 + dwh_ventas_vend.ImpNacCIGV
             Detalle.CtoxMesMe-4 = Detalle.CtoxMesMe-4 + dwh_ventas_vend.CostoExtCIGV
             Detalle.CtoxMesMn-4 = Detalle.CtoxMesMn-4 + dwh_ventas_vend.CostoNacCIGV
             Detalle.ProxMesMe-4 = Detalle.ProxMesMe-4 + dwh_ventas_vend.PromExtCIGV
             Detalle.ProxMesMn-4 = Detalle.ProxMesMn-4 + dwh_ventas_vend.PromNacCIGV.
     END.
END.

ASSIGN
    pOptions = pOptions + CHR(1) + lOptions.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Resumen-por-linea wWin 
PROCEDURE Resumen-por-linea :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

ASSIGN
    x-NroFchR = INTEGER( STRING(YEAR(DesdeF) - 1, '9999') + '0101')
    x-NroFchE = INTEGER( STRING(YEAR(HastaF), '9999') + STRING(MONTH(HastaF), '99') + '31').

FOR EACH dwh_ventas_resmat NO-LOCK WHERE dwh_ventas_resmat.codcia = s-codcia
    AND dwh_ventas_resmat.fecha >= x-NroFchR
    AND dwh_ventas_resmat.fecha <= x-NroFchE
    AND dwh_ventas_resmat.coddiv BEGINS x-CodDiv
    AND dwh_ventas_resmat.codven BEGINS x-CodVen,
    FIRST dwh_Division OF dwh_ventas_resmat NO-LOCK,
    FIRST dwh_Vendedor OF dwh_ventas_resmat NO-LOCK,
    FIRST dwh_tiempo NO-LOCK WHERE dwh_tiempo.codcia = s-codcia AND dwh_tiempo.fecha = dwh_ventas_resmat.fecha:
    IF x-CuentaReg = 0 OR ( x-CuentaReg MODULO x-MuestraReg ) = 0 THEN
        FILL-IN-Mensaje:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '** TABLA GENERAL ' + 
        ' FECHA ' + STRING (dwh_Tiempo.NroDia, '99') +
        ' ' + dwh_Tiempo.NombreMes + ' ' + STRING (dwh_Tiempo.Periodo, '9999') +
        ' DIVISION ' + dwh_ventas_resmat.coddiv + ' **'.
    x-CuentaReg = x-CuentaReg + 1.
    /* ARMAMOS LA LLAVE */
    ASSIGN
        x-Llave = ''
        cCampania = ''
        cPeriodo = 0
        cNroMes = 0
        cDia = ?
        cDivision = ''
        cCanalVenta = ''
        cProducto = ''
        cLinea = ''
        cSublinea = ''
        cMarca = ''
        cUnidad = ''
        cLicencia = ''
        cProveedor = ''
        cCliente = ''
        cCanal = ''
        cTarjeta = ''
        cDepartamento = ''
        cProvincia = ''
        cDistrito = ''
        cZona = ''
        cClasificacion = ''
        cTipo = ''
        cVendedor = ''
        lOptions = "FieldList:".
    /* LLAVE INICIAL */
    IF RADIO-SET-Tipo = 2 THEN DO:
        IF x-Llave = '' THEN x-Llave = dwh_Tiempo.Campania.
        ELSE x-Llave = x-Llave + dwh_Tiempo.Campania.
        x-Llave = x-Llave + '|'.
        x-Llave = x-Llave + STRING(dwh_Tiempo.Periodo, '9999').
        x-Llave = x-Llave + '|'.
        x-Llave = x-Llave + STRING(dwh_Tiempo.NroMes, '99').      /*dwh_Tiempo.NombreMes. */
        x-Llave = x-LLave + '|'.
        ASSIGN
            cCampania = dwh_Tiempo.Campania
            cPeriodo  = dwh_Tiempo.Periodo
            cNroMes   = dwh_Tiempo.NroMes
            lOptions = lOptions + 'Campania,Periodo,Nromes'.
    END.
    IF TOGGLE-CodDiv = YES THEN DO:
         IF x-Llave = '' THEN x-Llave = dwh_ventas_resmat.coddiv + '|'.
         ELSE x-Llave = x-LLave + dwh_ventas_resmat.coddiv + '|'.
         x-Llave = x-Llave + dwh_Division.CanalVenta + '|'.
         ASSIGN
             cDivision = dwh_ventas_resmat.coddiv + ' ' + dwh_Division.DesDiv
             cCanalVenta = dwh_Division.CanalVenta + ' ' + dwh_Division.NomCanalVenta
             lOptions = lOptions + (IF SUBSTRING(lOptions, LENGTH(lOptions), 1) = ':' THEN '' ELSE ',') +
                        'Division,CanalVenta'.
                                
    END.
     /* ******************************************** */
     IF TOGGLE-CodVen = YES THEN DO:
         IF x-Llave = '' THEN x-Llave = dwh_ventas_resmat.codven + '|'.
         ELSE x-Llave = x-Llave + dwh_ventas_resmat.codven + '|'.
         ASSIGN
             cVendedor = dwh_ventas_resmat.codven + ' ' + dwh_Vendedor.NomVen
             lOptions = lOptions + (IF SUBSTRING(lOptions, LENGTH(lOptions), 1) = ':' THEN '' ELSE ',') +
                        'Vendedor'.
     END.
     /* ******************************************** */
     FIND Detalle WHERE Detalle.llave = x-Llave NO-ERROR.
     IF NOT AVAILABLE Detalle THEN DO:
         CREATE Detalle.
         Detalle.llave = x-Llave.
     END.
     ASSIGN
         Detalle.Campania = cCampania
         Detalle.Periodo = cPeriodo
         Detalle.NroMes = cNroMes
         Detalle.Dia = cDia
         Detalle.Division = cDivision
         Detalle.CanalVenta = cCanalVenta
         Detalle.Producto = cProducto
         Detalle.Linea = cLinea
         Detalle.Sublinea = cSublinea
         Detalle.Marca = cMarca
         Detalle.Unidad = cUnidad
         Detalle.Licencia = cLicencia
         Detalle.Proveedor = cProveedor
         Detalle.Cliente = cCliente
         Detalle.Canal = cCanal
         Detalle.Tarjeta = cTarjeta
         Detalle.Departamento = cDepartamento
         Detalle.Provincia = cProvincia
         Detalle.Distrito = cDistrito
         Detalle.Zona = cZona 
         Detalle.Clasificacion = cClasificacion
         Detalle.Tipo = cTipo
         Detalle.Vendedor = cVendedor.
     x-Fecha = DATE(SUBSTRING(STRING(dwh_ventas_resmat.fecha, '99999999'),7,2) + '/' +
               SUBSTRING(STRING(dwh_ventas_resmat.fecha, '99999999'),5,2) + '/' +
               SUBSTRING(STRING(dwh_ventas_resmat.fecha, '99999999'),1,4)).
     /* PERIODO ACTUAL */
     IF x-Fecha >= DesdeF AND x-Fecha <= HastaF THEN DO:
         ASSIGN
             /*Detalle.CanxMes-1   = Detalle.CanxMes-1   + dwh_ventas_resmat.Cantidad*/
             Detalle.VtaxMesMe-1 = Detalle.VtaxMesMe-1 + dwh_ventas_resmat.ImpExtCIGV
             Detalle.VtaxMesMn-1 = Detalle.VtaxMesMn-1 + dwh_ventas_resmat.ImpNacCIGV
             Detalle.CtoxMesMe-1 = Detalle.CtoxMesMe-1 + dwh_ventas_resmat.CostoExtCIGV
             Detalle.CtoxMesMn-1 = Detalle.CtoxMesMn-1 + dwh_ventas_resmat.CostoNacCIGV
             Detalle.ProxMesMe-1 = Detalle.ProxMesMe-1 + dwh_ventas_resmat.PromExtCIGV
             Detalle.ProxMesMn-1 = Detalle.ProxMesMn-1 + dwh_ventas_resmat.PromNacCIGV.
     END.
     /* ACUMULADO PERIODO ACTUAL */
     IF x-Fecha >= DATE(01,01,YEAR(DesdeF)) AND x-Fecha <= HastaF THEN DO:
         ASSIGN
             /*Detalle.CanxMes-2   = Detalle.CanxMes-2   + dwh_ventas_resmat.Cantidad*/
             Detalle.VtaxMesMe-2 = Detalle.VtaxMesMe-2 + dwh_ventas_resmat.ImpExtCIGV
             Detalle.VtaxMesMn-2 = Detalle.VtaxMesMn-2 + dwh_ventas_resmat.ImpNacCIGV
             Detalle.CtoxMesMe-2 = Detalle.CtoxMesMe-2 + dwh_ventas_resmat.CostoExtCIGV
             Detalle.CtoxMesMn-2 = Detalle.CtoxMesMn-2 + dwh_ventas_resmat.CostoNacCIGV
             Detalle.ProxMesMe-2 = Detalle.ProxMesMe-2 + dwh_ventas_resmat.PromExtCIGV
             Detalle.ProxMesMn-2 = Detalle.ProxMesMn-2 + dwh_ventas_resmat.PromNacCIGV.
     END.
     /* PERIODO ANTERIOR */
     IF x-Fecha >= DATE(MONTH(DesdeF),DAY(DesdeF),YEAR(DesdeF) - 1) AND x-Fecha <= DATE(MONTH(HastaF),DAY(HastaF),YEAR(HastaF) - 1) THEN DO:
         ASSIGN
             /*Detalle.CanxMes-3   = Detalle.CanxMes-3   + dwh_ventas_resmat.Cantidad*/
             Detalle.VtaxMesMe-3 = Detalle.VtaxMesMe-3 + dwh_ventas_resmat.ImpExtCIGV
             Detalle.VtaxMesMn-3 = Detalle.VtaxMesMn-3 + dwh_ventas_resmat.ImpNacCIGV
             Detalle.CtoxMesMe-3 = Detalle.CtoxMesMe-3 + dwh_ventas_resmat.CostoExtCIGV
             Detalle.CtoxMesMn-3 = Detalle.CtoxMesMn-3 + dwh_ventas_resmat.CostoNacCIGV
             Detalle.ProxMesMe-3 = Detalle.ProxMesMe-3 + dwh_ventas_resmat.PromExtCIGV
             Detalle.ProxMesMn-3 = Detalle.ProxMesMn-3 + dwh_ventas_resmat.PromNacCIGV.
     END.
     /* ACUMULADO PERIODO ANTERIOR */
     IF x-Fecha >= DATE(01,01,YEAR(DesdeF) - 1) AND x-Fecha <= DATE(MONTH(HastaF),DAY(HastaF),YEAR(HastaF) - 1) THEN DO:
         ASSIGN
             /*Detalle.CanxMes-4   = Detalle.CanxMes-4   + dwh_ventas_resmat.Cantidad*/
             Detalle.VtaxMesMe-4 = Detalle.VtaxMesMe-4 + dwh_ventas_resmat.ImpExtCIGV
             Detalle.VtaxMesMn-4 = Detalle.VtaxMesMn-4 + dwh_ventas_resmat.ImpNacCIGV
             Detalle.CtoxMesMe-4 = Detalle.CtoxMesMe-4 + dwh_ventas_resmat.CostoExtCIGV
             Detalle.CtoxMesMn-4 = Detalle.CtoxMesMn-4 + dwh_ventas_resmat.CostoNacCIGV
             Detalle.ProxMesMe-4 = Detalle.ProxMesMe-4 + dwh_ventas_resmat.PromExtCIGV
             Detalle.ProxMesMn-4 = Detalle.ProxMesMn-4 + dwh_ventas_resmat.PromNacCIGV.
     END.

END.

ASSIGN
    pOptions = pOptions + CHR(1) + lOptions.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Resumen-por-producto wWin 
PROCEDURE Resumen-por-producto :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

ASSIGN
    x-NroFchR = INTEGER( STRING(YEAR(DesdeF) - 1, '9999') + '0101')
    x-NroFchE = INTEGER( STRING(YEAR(HastaF), '9999') + STRING(MONTH(HastaF), '99') + '31').

FOR EACH dwh_ventas_mat NO-LOCK WHERE dwh_ventas_mat.codcia = s-codcia
    AND dwh_ventas_mat.fecha >= x-NroFchR
    AND dwh_ventas_mat.fecha <= x-NroFchE
    AND ( x-CodMat = '' OR LOOKUP (dwh_ventas_mat.codmat, x-CodMat) > 0 )
    AND dwh_ventas_mat.coddiv BEGINS x-CodDiv,
    FIRST dwh_Division OF dwh_ventas_mat NO-LOCK,
    FIRST dwh_tiempo NO-LOCK WHERE dwh_tiempo.codcia = s-codcia AND dwh_tiempo.fecha = dwh_ventas_mat.fecha,
    FIRST dwh_Productos OF dwh_ventas_mat NO-LOCK WHERE dwh_Productos.codfam BEGINS x-CodFam
        AND dwh_Productos.CodPro[1] BEGINS x-CodPro
        AND dwh_Productos.subfam BEGINS x-SubFam:
    IF x-CuentaReg = 0 OR ( x-CuentaReg MODULO x-MuestraReg ) = 0 THEN
        FILL-IN-Mensaje:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '** TABLA GENERAL ' + 
        ' FECHA ' + STRING (dwh_Tiempo.NroDia, '99') +
        ' ' + dwh_Tiempo.NombreMes + ' ' + STRING (dwh_Tiempo.Periodo, '9999') +
        ' DIVISION ' + dwh_ventas_mat.coddiv + ' PRODUCTO ' + dwh_ventas_mat.codmat + ' **'.
    x-CuentaReg = x-CuentaReg + 1.
    /* ARMAMOS LA LLAVE */
    ASSIGN
        x-Llave = ''
        cCampania = ''
        cPeriodo = 0
        cNroMes = 0
        cDia = ?
        cDivision = ''
        cCanalVenta = ''
        cProducto = ''
        cLinea = ''
        cSublinea = ''
        cMarca = ''
        cUnidad = ''
        cLicencia = ''
        cProveedor = ''
        cCliente = ''
        cCanal = ''
        cTarjeta = ''
        cDepartamento = ''
        cProvincia = ''
        cDistrito = ''
        cZona = ''
        cClasificacion = ''
        cTipo = ''
        cVendedor = ''
        lOptions = "FieldList:".
    /* LLAVE INICIAL */
    IF RADIO-SET-Tipo = 2 THEN DO:
        IF x-Llave = '' THEN x-Llave = dwh_Tiempo.Campania.
        ELSE x-Llave = x-Llave + dwh_Tiempo.Campania.
        x-Llave = x-Llave + '|'.
        x-Llave = x-Llave + STRING(dwh_Tiempo.Periodo, '9999').
        x-Llave = x-Llave + '|'.
        x-Llave = x-Llave + STRING(dwh_Tiempo.NroMes, '99').      /*dwh_Tiempo.NombreMes. */
        x-Llave = x-LLave + '|'.
        ASSIGN
            cCampania = dwh_Tiempo.Campania
            cPeriodo  = dwh_Tiempo.Periodo
            cNroMes   = dwh_Tiempo.NroMes
            lOptions = lOptions + 'Campania,Periodo,Nromes'.
    END.
    IF TOGGLE-CodDiv = YES THEN DO:
         IF x-Llave = '' THEN x-Llave = dwh_ventas_mat.coddiv + '|'.
         ELSE x-Llave = x-LLave + dwh_ventas_mat.coddiv + '|'.
         x-Llave = x-Llave + dwh_Division.CanalVenta + '|'.
         ASSIGN
             cDivision = dwh_ventas_mat.coddiv + ' ' + dwh_Division.DesDiv
             cCanalVenta = dwh_Division.CanalVenta + ' ' + dwh_Division.NomCanalVenta
             lOptions = lOptions + (IF SUBSTRING(lOptions, LENGTH(lOptions), 1) = ':' THEN '' ELSE ',') +
                        'Division,CanalVenta'.
                                
    END.
    IF TOGGLE-CodMat = YES THEN DO:
        IF (TOGGLE-Resumen-Linea = NO AND TOGGLE-Resumen-Marca = NO) THEN DO:
             IF x-Llave = '' THEN x-Llave = dwh_ventas_mat.codmat.
             ELSE x-Llave = x-Llave + dwh_ventas_mat.codmat.
             x-Llave = x-Llave + dwh_Productos.codfam + '|'.
             x-Llave = x-Llave + dwh_Productos.subfam + '|'.
             x-Llave = x-Llave + dwh_Productos.desmar + '|'.
             x-Llave = x-Llave + dwh_Productos.undbas + '|'.
             ASSIGN
                 cProducto = dwh_ventas_mat.codmat + ' ' + dwh_Productos.DesMat
                 cLinea = dwh_Productos.codfam + ' ' + dwh_Productos.NomFam 
                 cSublinea = dwh_Productos.subfam + ' ' + dwh_Productos.NomSubFam 
                 cMarca = dwh_Productos.desmar
                 cUnidad = dwh_Productos.undbas
                 lOptions = lOptions + (IF SUBSTRING(lOptions, LENGTH(lOptions), 1) = ':' THEN '' ELSE ',') +
                            'Producto,Linea,Sublinea,Marca,Unidad'.
         END.
         ELSE DO:
             IF TOGGLE-Resumen-Linea = YES THEN DO:
                 IF x-Llave = '' THEN x-Llave = dwh_Productos.codfam + '|'.
                 ELSE x-Llave = x-Llave + dwh_Productos.codfam + '|'.
                 x-Llave = x-Llave + dwh_Productos.subfam + '|'.
                 ASSIGN
                     cLinea = dwh_Productos.codfam + ' ' + dwh_Productos.NomFam 
                     cSublinea = dwh_Productos.subfam + ' ' + dwh_Productos.NomSubFam
                     lOptions = lOptions + (IF SUBSTRING(lOptions, LENGTH(lOptions), 1) = ':' THEN '' ELSE ',') +
                                'Linea,Sublinea'.

             END.
             IF TOGGLE-Resumen-Marca = YES THEN DO:
                 IF x-Llave = '' THEN x-Llave = dwh_Productos.desmar + '|'.
                 ELSE x-Llave = x-Llave + dwh_Productos.desmar + '|'.
                 ASSIGN
                     cMarca = dwh_Productos.desmar
                     lOptions = lOptions + (IF SUBSTRING(lOptions, LENGTH(lOptions), 1) = ':' THEN '' ELSE ',') +
                                'Marca'.
             END.
         END.
         x-Llave = x-Llave + dwh_Productos.licencia + '|'.
         x-Llave = x-Llave + dwh_Productos.codpro[1] + '|'.
         ASSIGN
             cLicencia = dwh_Productos.licencia + ' ' + dwh_Productos.NomLicencia
             cProveedor = dwh_Productos.codpro[1] + ' ' + dwh_Productos.NomPro[1]
             lOptions = lOptions + (IF SUBSTRING(lOptions, LENGTH(lOptions), 1) = ':' THEN '' ELSE ',') +
                        'Licencia,Proveedor'.

     END.
     /* ******************************************** */
     FIND Detalle WHERE Detalle.llave = x-Llave NO-ERROR.
     IF NOT AVAILABLE Detalle THEN DO:
         CREATE Detalle.
         Detalle.llave = x-Llave.
     END.
     ASSIGN
         Detalle.Campania = cCampania
         Detalle.Periodo = cPeriodo
         Detalle.NroMes = cNroMes
         Detalle.Dia = cDia
         Detalle.Division = cDivision
         Detalle.CanalVenta = cCanalVenta
         Detalle.Producto = cProducto
         Detalle.Linea = cLinea
         Detalle.Sublinea = cSublinea
         Detalle.Marca = cMarca
         Detalle.Unidad = cUnidad
         Detalle.Licencia = cLicencia
         Detalle.Proveedor = cProveedor
         Detalle.Cliente = cCliente
         Detalle.Canal = cCanal
         Detalle.Tarjeta = cTarjeta
         Detalle.Departamento = cDepartamento
         Detalle.Provincia = cProvincia
         Detalle.Distrito = cDistrito
         Detalle.Zona = cZona 
         Detalle.Clasificacion = cClasificacion
         Detalle.Tipo = cTipo
         Detalle.Vendedor = cVendedor.
     x-Fecha = DATE(SUBSTRING(STRING(dwh_ventas_mat.fecha, '99999999'),7,2) + '/' +
               SUBSTRING(STRING(dwh_ventas_mat.fecha, '99999999'),5,2) + '/' +
               SUBSTRING(STRING(dwh_ventas_mat.fecha, '99999999'),1,4)).
     /* PERIODO ACTUAL */
     IF x-Fecha >= DesdeF AND x-Fecha <= HastaF THEN DO:
         ASSIGN
             Detalle.CanxMes-1   = Detalle.CanxMes-1   + dwh_ventas_mat.Cantidad
             Detalle.VtaxMesMe-1 = Detalle.VtaxMesMe-1 + dwh_ventas_mat.ImpExtCIGV
             Detalle.VtaxMesMn-1 = Detalle.VtaxMesMn-1 + dwh_ventas_mat.ImpNacCIGV
             Detalle.CtoxMesMe-1 = Detalle.CtoxMesMe-1 + dwh_ventas_mat.CostoExtCIGV
             Detalle.CtoxMesMn-1 = Detalle.CtoxMesMn-1 + dwh_ventas_mat.CostoNacCIGV
             Detalle.ProxMesMe-1 = Detalle.ProxMesMe-1 + dwh_ventas_mat.PromExtCIGV
             Detalle.ProxMesMn-1 = Detalle.ProxMesMn-1 + dwh_ventas_mat.PromNacCIGV.
     END.
     /* ACUMULADO PERIODO ACTUAL */
     IF x-Fecha >= DATE(01,01,YEAR(DesdeF)) AND x-Fecha <= HastaF THEN DO:
         ASSIGN
             Detalle.CanxMes-2   = Detalle.CanxMes-2   + dwh_ventas_mat.Cantidad
             Detalle.VtaxMesMe-2 = Detalle.VtaxMesMe-2 + dwh_ventas_mat.ImpExtCIGV
             Detalle.VtaxMesMn-2 = Detalle.VtaxMesMn-2 + dwh_ventas_mat.ImpNacCIGV
             Detalle.CtoxMesMe-2 = Detalle.CtoxMesMe-2 + dwh_ventas_mat.CostoExtCIGV
             Detalle.CtoxMesMn-2 = Detalle.CtoxMesMn-2 + dwh_ventas_mat.CostoNacCIGV
             Detalle.ProxMesMe-2 = Detalle.ProxMesMe-2 + dwh_ventas_mat.PromExtCIGV
             Detalle.ProxMesMn-2 = Detalle.ProxMesMn-2 + dwh_ventas_mat.PromNacCIGV.
     END.
     /* PERIODO ANTERIOR */
     IF x-Fecha >= DATE(MONTH(DesdeF),DAY(DesdeF),YEAR(DesdeF) - 1) AND x-Fecha <= DATE(MONTH(HastaF),DAY(HastaF),YEAR(HastaF) - 1) THEN DO:
         ASSIGN
             Detalle.CanxMes-3   = Detalle.CanxMes-3   + dwh_ventas_mat.Cantidad
             Detalle.VtaxMesMe-3 = Detalle.VtaxMesMe-3 + dwh_ventas_mat.ImpExtCIGV
             Detalle.VtaxMesMn-3 = Detalle.VtaxMesMn-3 + dwh_ventas_mat.ImpNacCIGV
             Detalle.CtoxMesMe-3 = Detalle.CtoxMesMe-3 + dwh_ventas_mat.CostoExtCIGV
             Detalle.CtoxMesMn-3 = Detalle.CtoxMesMn-3 + dwh_ventas_mat.CostoNacCIGV
             Detalle.ProxMesMe-3 = Detalle.ProxMesMe-3 + dwh_ventas_mat.PromExtCIGV
             Detalle.ProxMesMn-3 = Detalle.ProxMesMn-3 + dwh_ventas_mat.PromNacCIGV.
     END.
     /* ACUMULADO PERIODO ANTERIOR */
     IF x-Fecha >= DATE(01,01,YEAR(DesdeF) - 1) AND x-Fecha <= DATE(MONTH(HastaF),DAY(HastaF),YEAR(HastaF) - 1) THEN DO:
         ASSIGN
             Detalle.CanxMes-4   = Detalle.CanxMes-4   + dwh_ventas_mat.Cantidad
             Detalle.VtaxMesMe-4 = Detalle.VtaxMesMe-4 + dwh_ventas_mat.ImpExtCIGV
             Detalle.VtaxMesMn-4 = Detalle.VtaxMesMn-4 + dwh_ventas_mat.ImpNacCIGV
             Detalle.CtoxMesMe-4 = Detalle.CtoxMesMe-4 + dwh_ventas_mat.CostoExtCIGV
             Detalle.CtoxMesMn-4 = Detalle.CtoxMesMn-4 + dwh_ventas_mat.CostoNacCIGV
             Detalle.ProxMesMe-4 = Detalle.ProxMesMe-4 + dwh_ventas_mat.PromExtCIGV
             Detalle.ProxMesMn-4 = Detalle.ProxMesMn-4 + dwh_ventas_mat.PromNacCIGV.
     END.

END.

ASSIGN
    pOptions = pOptions + CHR(1) + lOptions.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Resumen-por-vendcli wWin 
PROCEDURE Resumen-por-vendcli :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

ASSIGN
    iContador = 1       /* Valor por defecto */
    x-CodCli = ''.
FOR EACH tt-cliente NO-LOCK:
    iContador = iContador + 1.
    IF x-CodCli = '' THEN x-CodCli = tt-cliente.tt-codcli.
    ELSE x-CodCli = x-CodCli + ',' + tt-cliente.tt-codcli.
    IF iContador = 1000 THEN DO:
        {est/estad102-vendcli.i}
        ASSIGN
            iContador = 0
            x-CodCli = ''.
    END.
END.
IF iContador > 0 THEN DO:
    {est/estad102-vendcli.i}
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

