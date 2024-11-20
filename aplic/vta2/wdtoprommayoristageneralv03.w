&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          integral         PROGRESS
*/
&Scoped-define WINDOW-NAME W-Win


/* Temp-Table and Buffer definitions                                    */
DEFINE NEW SHARED TEMP-TABLE E-MATG NO-UNDO LIKE Almmmatg.
DEFINE NEW SHARED TEMP-TABLE T-MATG LIKE Almmmatg.
DEFINE NEW SHARED TEMP-TABLE T-TABLA LIKE VtaTabla.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
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

DEF SHARED VAR s-codcia AS INT.
DEF SHARED VAR s-user-id AS CHAR.
DEF SHARED VAR s-coddiv AS CHAR.

DEFINE VARIABLE FILL-IN-Archivo AS CHAR NO-UNDO.
DEFINE VARIABLE OKpressed AS LOG NO-UNDO.

DEFINE VARIABLE chExcelApplication  AS COM-HANDLE.
DEFINE VARIABLE chWorkbook          AS COM-HANDLE.
DEFINE VARIABLE chWorksheet         AS COM-HANDLE.
DEFINE VARIABLE cRange          AS CHARACTER    NO-UNDO.
DEFINE VARIABLE iCountLine      AS INTEGER      NO-UNDO.
DEFINE VARIABLE iTotalColumn    AS INTEGER      NO-UNDO.
DEFINE VARIABLE cValue          AS CHARACTER    NO-UNDO.
DEFINE VARIABLE iValue          AS INTEGER      NO-UNDO.
DEFINE VARIABLE dValue          AS DECIMAL      NO-UNDO.
DEFINE VARIABLE t-Column        AS INTEGER INIT 1.
DEFINE VARIABLE t-Row           AS INTEGER INIT 1.
DEFINE VARIABLE k               AS INTEGER NO-UNDO.

DEFINE VARIABLE RADIO-SET-1 AS INTEGER NO-UNDO.
DEFINE VARIABLE RADIO-SET-2 AS INTEGER NO-UNDO.

DEF VAR pMensaje AS CHAR NO-UNDO.

/* VARIABLE PARA CONTROL LA ACTUALIZACION DE UNA DIVISION O TODAS LAS DIVISIONES */
DEF VAR x-MetodoActualizacion AS INT INIT 1 NO-UNDO.
/* 
1: Todas las divisiones
2: Divisi�n seleccionada
*/

DEF VAR f-Division AS CHAR NO-UNDO.
DEF VAR f-FechaD AS DATE NO-UNDO FORMAT '99/99/9999'.
DEF VAR f-FechaH AS DATE NO-UNDO FORMAT '99/99/9999'.

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
&Scoped-Define ENABLED-OBJECTS RECT-3 BtnDone COMBO-BOX-Division BUTTON-1 
&Scoped-Define DISPLAYED-OBJECTS COMBO-BOX-Division txtCodMat 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_butilexdctoprom-02 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_butilexdctoprom-02-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_butilexdctoprom-02a AS HANDLE NO-UNDO.
DEFINE VARIABLE h_butilexdctoprom-02b AS HANDLE NO-UNDO.
DEFINE VARIABLE h_t-lima-dtopromv2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_t-utilex-dtopromv31 AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnDone DEFAULT 
     IMAGE-UP FILE "IMG/exit.ico":U
     LABEL "&Done" 
     SIZE 6 BY 1.54
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-1 
     LABEL "CAPTURAR EXCEL" 
     SIZE 25 BY 1.12.

DEFINE BUTTON BUTTON-2 
     LABEL "GRABAR DESCUENTOS" 
     SIZE 25 BY 1.12.

DEFINE VARIABLE COMBO-BOX-Division AS CHARACTER FORMAT "X(256)":U 
     LABEL "Divisi�n" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 67 BY 1
     BGCOLOR 11 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE txtCodMat AS CHARACTER FORMAT "X(10)":U 
     LABEL "Art" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 141 BY 1.92
     BGCOLOR 11 FGCOLOR 0 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     BtnDone AT ROW 1.19 COL 132 WIDGET-ID 6
     COMBO-BOX-Division AT ROW 1.38 COL 12 COLON-ALIGNED WIDGET-ID 32
     BUTTON-1 AT ROW 1.38 COL 82 WIDGET-ID 2
     BUTTON-2 AT ROW 1.38 COL 107 WIDGET-ID 4
     txtCodMat AT ROW 17.73 COL 95 COLON-ALIGNED WIDGET-ID 34
     RECT-3 AT ROW 1 COL 1 WIDGET-ID 30
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 141.14 BY 21.08 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Design Page: 4
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: E-MATG T "NEW SHARED" NO-UNDO INTEGRAL Almmmatg
      TABLE: T-MATG T "NEW SHARED" ? INTEGRAL Almmmatg
      TABLE: T-TABLA T "NEW SHARED" ? INTEGRAL VtaTabla
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "DESCUENTOS PROMOCIONALES"
         HEIGHT             = 21.08
         WIDTH              = 141.14
         MAX-HEIGHT         = 21.54
         MAX-WIDTH          = 142.29
         VIRTUAL-HEIGHT     = 21.54
         VIRTUAL-WIDTH      = 142.29
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   FRAME-NAME                                                           */
/* SETTINGS FOR BUTTON BUTTON-2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN txtCodMat IN FRAME F-Main
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* DESCUENTOS PROMOCIONALES */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* DESCUENTOS PROMOCIONALES */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnDone
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnDone W-Win
ON CHOOSE OF BtnDone IN FRAME F-Main /* Done */
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 W-Win
ON CHOOSE OF BUTTON-1 IN FRAME F-Main /* CAPTURAR EXCEL */
DO:
    SYSTEM-DIALOG GET-FILE FILL-IN-Archivo
        FILTERS "Archivos Excel (*.xls)" "*.xls,*xlsx", "Todos (*.*)" "*.*"
        TITLE "Archivo(s) de Carga..."
        MUST-EXIST
        /*RETURN-TO-START-DIR */
        USE-FILENAME
        UPDATE OKpressed.
    IF OKpressed = FALSE THEN RETURN NO-APPLY.

    /* CREAMOS LA HOJA EXCEL */
    CREATE "Excel.Application" chExcelApplication.
    chWorkbook = chExcelApplication:Workbooks:OPEN(FILL-IN-Archivo).
    chWorkSheet = chExcelApplication:Sheets:ITEM(1).

    SESSION:SET-WAIT-STATE('GENERAL').
    pMensaje = ''.
    RUN Carga-Temporal.
    SESSION:SET-WAIT-STATE('').

    /* CERRAMOS EL EXCEL */
    chExcelApplication:QUIT().
    RELEASE OBJECT chExcelApplication.      
    RELEASE OBJECT chWorkbook.
    RELEASE OBJECT chWorksheet. 

    /* Mensaje de error de carga */
    IF pMensaje <> "" THEN DO:
        MESSAGE pMensaje VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY.
    END.

    ASSIGN
        COMBO-BOX-Division:SENSITIVE IN FRAME {&FRAME-NAME} = NO
        BUTTON-2:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
    MESSAGE 'Carga terminada' VIEW-AS ALERT-BOX INFORMATION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 W-Win
ON CHOOSE OF BUTTON-2 IN FRAME F-Main /* GRABAR DESCUENTOS */
DO:
    RUN Grabar-Descuentos.
    IF h_butilexdctoprom-02-2 <> ? THEN RUN dispatch IN h_butilexdctoprom-02-2 ('open-query':U).
    IF h_butilexdctoprom-02 <> ? THEN RUN dispatch IN h_butilexdctoprom-02 ('open-query':U).
    IF h_butilexdctoprom-02a <> ? THEN RUN dispatch IN h_butilexdctoprom-02a ('open-query':U).
    IF h_butilexdctoprom-02b <> ? THEN RUN dispatch IN h_butilexdctoprom-02b ('open-query':U).
    ASSIGN
        BUTTON-1:SENSITIVE IN FRAME {&FRAME-NAME} = YES
        BUTTON-2:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
    RUN Excel-de-Errores.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME COMBO-BOX-Division
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL COMBO-BOX-Division W-Win
ON VALUE-CHANGED OF COMBO-BOX-Division IN FRAME F-Main /* Divisi�n */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */

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
  DEFINE VARIABLE adm-current-page  AS INTEGER NO-UNDO.

  RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
  ASSIGN adm-current-page = INTEGER(RETURN-VALUE).

  CASE adm-current-page: 

    WHEN 1 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'aplic/vta2/butilexdctoprom-02.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_butilexdctoprom-02-2 ).
       RUN set-position IN h_butilexdctoprom-02-2 ( 2.92 , 1.00 ) NO-ERROR.
       RUN set-size IN h_butilexdctoprom-02-2 ( 19.04 , 86.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'aplic/vta2/t-lima-dtopromv2.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_t-lima-dtopromv2 ).
       /* Position in AB:  ( 2.92 , 88.00 ) */
       /* Size in UIB:  ( 12.88 , 45.00 ) */

       /* Links to  h_t-lima-dtopromv2. */
       RUN add-link IN adm-broker-hdl ( h_butilexdctoprom-02-2 , 'Record':U , h_t-lima-dtopromv2 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_butilexdctoprom-02-2 ,
             BUTTON-2:HANDLE IN FRAME F-Main , 'AFTER':U ).
    END. /* Page 1 */
    WHEN 2 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'aplic/vta2/butilexdctoprom-02a.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_butilexdctoprom-02a ).
       /* Position in AB:  ( 2.92 , 1.00 ) */
       /* Size in UIB:  ( 19.04 , 141.00 ) */

    END. /* Page 2 */
    WHEN 3 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'aplic/vta2/butilexdctoprom-02b.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_butilexdctoprom-02b ).
       /* Position in AB:  ( 2.92 , 1.00 ) */
       /* Size in UIB:  ( 19.04 , 141.00 ) */

    END. /* Page 3 */
    WHEN 4 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'aplic/vta2/butilexdctoprom-02.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_butilexdctoprom-02 ).
       RUN set-position IN h_butilexdctoprom-02 ( 2.92 , 1.00 ) NO-ERROR.
       RUN set-size IN h_butilexdctoprom-02 ( 19.04 , 86.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'aplic/vta2/t-utilex-dtopromv31.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_t-utilex-dtopromv31 ).
       RUN set-position IN h_t-utilex-dtopromv31 ( 2.92 , 87.00 ) NO-ERROR.
       RUN set-size IN h_t-utilex-dtopromv31 ( 12.88 , 45.00 ) NO-ERROR.

       /* Links to SmartBrowser h_t-utilex-dtopromv31. */
       RUN add-link IN adm-broker-hdl ( h_butilexdctoprom-02 , 'Record':U , h_t-utilex-dtopromv31 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_butilexdctoprom-02 ,
             BUTTON-2:HANDLE IN FRAME F-Main , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_t-utilex-dtopromv31 ,
             h_butilexdctoprom-02 , 'AFTER':U ).
    END. /* Page 4 */

  END CASE.
  /* Select a Startup page. */
  IF adm-current-page eq 0 
  THEN RUN select-page IN THIS-PROCEDURE ( 1 ).

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Carga-Temporal W-Win 
PROCEDURE Carga-Temporal PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR cNombreLista AS CHAR NO-UNDO.

/* CHEQUEAMOS LA INTEGRIDAD DEL ARCHIVO EXCEL */
ASSIGN
    t-Column = 0
    t-Row = 1.     /* Saltamos 1ra linea */
cValue = chWorkSheet:Cells(1,1):VALUE.
IF cValue = "" OR cValue = ? THEN DO:
    MESSAGE 'Formato del archivo Excel errado' VIEW-AS ALERT-BOX ERROR.
    RETURN.
END.

IF NOT (cValue BEGINS "CONTINENTAL - DESCUENTOS PROMOCIONALES"
        OR cValue BEGINS "UTILEX - DESCUENTOS PROMOCIONALES"
        OR cValue BEGINS COMBO-BOX-Division + " - DESCUENTOS PROMOCIONALES"
        OR cValue BEGINS "CONTRATO MARCO - DESCUENTOS PROMOCIONALES"
/*         OR cValue BEGINS "PROVINCIAS - DESCUENTOS PROMOCIONALES DE TERCEROS" */
/*         OR cValue BEGINS "PROVINCIAS - DESCUENTOS PROMOCIONALES PROPIOS")    */
        )
    THEN DO:
    pMensaje = 'Formato del archivo Excel errado'.
    RETURN.
END.
cNombreLista = cValue.

ASSIGN
    t-Row    = t-Row + 1
    t-column = t-Column + 1.
cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
IF cValue = "" OR cValue = ? OR cValue <> "CODIGO" THEN DO:
    pmensaje = 'Formato del archivo Excel errado'.
    RETURN.
END.
t-column = t-column + 1.
cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
IF cValue = "" OR cValue = ? OR cValue <> "DESCRIPCION" THEN DO:
    pMensaje =  'Formato del archivo Excel errado'.
    RETURN.
END.

CASE TRUE:
    WHEN cNombreLista BEGINS "CONTINENTAL - DESCUENTOS PROMOCIONALES"       THEN RADIO-SET-1 = 1.
    WHEN cNombreLista BEGINS "UTILEX - DESCUENTOS PROMOCIONALES"            THEN RADIO-SET-1 = 2.
    WHEN cNombreLista BEGINS COMBO-BOX-Division + " - DESCUENTOS PROMOCIONALES" THEN RADIO-SET-1 = 3.
    WHEN cNombreLista BEGINS "CONTRATO MARCO - DESCUENTOS PROMOCIONALES"    THEN RADIO-SET-1 = 4.
    WHEN cNombreLista BEGINS "PROVINCIAS - DESCUENTOS PROMOCIONALES DE TERCEROS" THEN RADIO-SET-1 = 5.
    WHEN cNombreLista BEGINS "PROVINCIAS - DESCUENTOS PROMOCIONALES PROPIOS"     THEN RADIO-SET-1 = 6.
    OTHERWISE RETURN.
END CASE.
CASE TRUE:
    WHEN INDEX(cNombreLista, "- PORCENTAJES") > 0 THEN RADIO-SET-2 = 1.
    WHEN INDEX(cNombreLista, "- IMPORTES") > 0 THEN RADIO-SET-2 = 2.
    OTHERWISE RETURN.
END CASE.
/* CASE RADIO-SET-1:                                                                                                 */
/*     WHEN 1 THEN RUN Pinta-Titulo IN h_butilexdctoprom-02 ( "CONTINENTAL - DESCUENTOS PROMOCIONALES" ).            */
/*     WHEN 2 THEN RUN Pinta-Titulo IN h_butilexdctoprom-02 ( "UTILEX - DESCUENTOS PROMOCIONALES" ).                 */
/*     WHEN 3 THEN RUN Pinta-Titulo IN h_butilexdctoprom-02 ( s-coddiv + " - DESCUENTOS PROMOCIONALES" ).            */
/*     WHEN 4 THEN RUN Pinta-Titulo IN h_butilexdctoprom-02 ( "CONTRATO MARCO - DESCUENTOS PROMOCIONALES" ).         */
/*     WHEN 5 THEN RUN Pinta-Titulo IN h_butilexdctoprom-02 ( "PROVINCIAS - DESCUENTOS PROMOCIONALES DE TERCEROS" ). */
/*     WHEN 6 THEN RUN Pinta-Titulo IN h_butilexdctoprom-02 ( "PROVINCIAS - DESCUENTOS PROMOCIONALES PROPIOS" ).     */
/* END CASE.                                                                                                         */
CASE RADIO-SET-1:
    WHEN 1 THEN RUN select-page("1").
    WHEN 2 THEN RUN select-page("4").
    WHEN 5 THEN RUN select-page("2").
    WHEN 6 THEN RUN select-page("3").
    OTHERWISE   RUN select-page("4").
END CASE.

/* Verificamos y Cargamos Divisiones */
DEF VAR x-Division AS CHAR NO-UNDO.
ASSIGN
    t-column = t-column + 7
    f-Division = ""
    x-Division = ""
    f-FechaD = ?
    f-FechaH = ?.
/* Cargamos las divisiones inscritas en la hoja Excel */
REPEAT WHILE RADIO-SET-1 <> 5:
    t-column = t-column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    IF cValue = "" OR cValue = ? THEN LEAVE.
    FIND gn-divi WHERE gn-divi.codcia = s-codcia
        AND gn-divi.coddiv = cValue
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE gn-divi THEN DO:
        pMensaje = 'Divisi�n ' + cValue + ' errada'.
        RETURN.
    END.
    IF TRUE <> (f-Division > "") THEN f-Division = cValue.
    ELSE f-Division = f-Division + ',' + cValue.
    /*
    IF RADIO-SET-1 <> 1 THEN IF NUM-ENTRIES(f-Division) = 10 THEN LEAVE.     /* NO mas de 10 (por ahora) */
    */
END.
IF RADIO-SET-1 = 3 AND f-Division <> COMBO-BOX-Division THEN DO:
    pMensaje = 'No puede actualizar la divisi�n ' + f-Division + CHR(10) +
        'La divisi�n que le corresponde es la ' + COMBO-BOX-Division.
    RETURN.
END.
IF RADIO-SET-1 = 5 OR RADIO-SET-1 = 6 THEN f-Division = "00018".

/* CONTROL DE UNA O VARIAS DIVISIONES */
x-MetodoActualizacion = 1.  /* Todas */
x-Division = f-Division.
CASE RADIO-SET-1:
    WHEN 1 THEN DO:
        RUN vta2/dutilexdctoprom-02 (INPUT-OUTPUT x-Division, OUTPUT x-MetodoActualizacion).
        IF x-Division = '' THEN RETURN.
        /*RUN Pinta-Titulo IN h_butilexdctoprom-02 ( "CONTINENTAL - DESCUENTOS PROMOCIONALES - DIVISION ESPECIFICA" ).*/
    END.
    WHEN 2 THEN DO:
        RUN vta2/dutilexdctoprom-02 (INPUT-OUTPUT x-Division, OUTPUT x-MetodoActualizacion).
        IF x-Division = '' THEN RETURN.
        RUN Pinta-Titulo IN h_butilexdctoprom-02 ( "UTILEX - DESCUENTOS PROMOCIONALES - DIVISION ESPECIFICA" ).
    END.
END CASE.
/* ********************************** */
/* Cargamos temporal */
EMPTY TEMP-TABLE T-MATG.
EMPTY TEMP-TABLE T-TABLA.
ASSIGN
    pMensaje = ""
    t-Column = 0
    t-Row = 2.     /* Saltamos el encabezado de los campos */
REPEAT:
    ASSIGN
        t-column = 0
        t-Row    = t-Row + 1.
    t-column = t-column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    IF cValue = "" OR cValue = ? THEN LEAVE.    /* FIN DE DATOS */ 

    ASSIGN
        cValue = STRING(DECIMAL(cValue), '999999') NO-ERROR.
    IF ERROR-STATUS:ERROR THEN NEXT.

    CREATE T-MATG.
    ASSIGN
        T-MATG.codcia = s-codcia
        T-MATG.codmat = cValue.

    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        T-MATG.desmat = cValue.
    IF cValue = '' OR cValue = '' THEN DO:
        FIND Almmmatg OF T-MATG NO-LOCK NO-ERROR.
        IF AVAILABLE Almmmatg THEN T-MATG.desmat = Almmmatg.desmat.
    END.
    
    /* FECHAS */
    t-Column = t-Column + 6.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        f-FechaD = DATE(cValue)
        NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        pMensaje = "Error en la linea " + TRIM(STRING(t-Row, '>>>9')) + ":"  + CHR(10) 
            + "Fecha Inicio".
        LEAVE.
    END.

    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        f-FechaH = DATE(cValue)
        NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        pMensaje = "Error en la linea " + TRIM(STRING(t-Row, '>>>9')) + ":"  + CHR(10) 
            + "Fecha Fin".
        LEAVE.
    END.
    
    IF f-FechaD <> ? AND f-FechaH <> ? AND f-FechaD > f-FechaH THEN DO:
        pMensaje = "Error en la linea " + TRIM(STRING(t-Row, '>>>9')) + ":"  + CHR(10) 
            + "Fecha Inicial es mayor que la Fecha Fin".
        LEAVE.
    END.

    /* DESCUENTOS PROMOCIONALES */
    CASE RADIO-SET-1:
        WHEN 1 THEN DO:
            DO iCountLine = 1 TO NUM-ENTRIES(f-Division):
                t-Column = t-Column + 1.
                cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
                IF cValue = ? OR DECIMAL(cValue) = 0 THEN NEXT.
                /* En caso de seleccionar 1 division */
                IF x-MetodoActualizacion = 2 AND x-Division <> ENTRY(iCountLine, f-Division)  THEN NEXT.     
                CREATE T-TABLA.
                ASSIGN
                    T-TABLA.CodCia = s-codcia
                    T-TABLA.Tabla = "DTOPROLIMA"
                    T-TABLA.Llave_c1 = T-MATG.codmat
                    T-TABLA.Llave_c2 = ENTRY(iCountLine, f-Division) 
                    T-TABLA.Rango_fecha[1] = f-FechaD
                    T-TABLA.Rango_fecha[2] = f-FechaH
                    T-TABLA.Valor[1] = DECIMAL(cValue).
                IF cValue = ? THEN T-TABLA.Valor[1] = 0.
                
            END.
        END.
        WHEN 2 THEN DO:
            DO iCountLine = 1 TO NUM-ENTRIES(f-Division):
                t-Column = t-Column + 1.
                cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
                IF cValue = ? OR DECIMAL(cValue) = 0 THEN NEXT.
                /* En caso de seleccionar 1 division */
                IF x-MetodoActualizacion = 2 AND x-Division <> ENTRY(iCountLine, f-Division)  THEN NEXT.     
                CREATE T-TABLA.
                ASSIGN
                    T-TABLA.CodCia = s-codcia
                    T-TABLA.Tabla = "DTOPROUTILEX"
                    T-TABLA.Llave_c1 = T-MATG.codmat
                    T-TABLA.Llave_c2 = ENTRY(iCountLine, f-Division) 
                    T-TABLA.Rango_fecha[1] = f-FechaD
                    T-TABLA.Rango_fecha[2] = f-FechaH
                    T-TABLA.Valor[1] = DECIMAL(cValue).
                IF cValue = ? THEN T-TABLA.Valor[1] = 0.
            END.
        END.
        WHEN 5 THEN DO:
            ASSIGN
                T-MATG.PromMinDivi[1] = f-Division
                T-MATG.PromMinFchD[1] = f-FechaD
                T-MATG.PromMinFchH[1] = f-FechaH.
            k = 1.
            DO iCountLine = 1 TO 2:
                t-Column = t-Column + 1.
                cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
                IF cValue = ? OR DECIMAL(cValue) = 0 THEN NEXT.
                ASSIGN
                    T-MATG.PromMinDto[k]  = DECIMAL(cValue).
                IF cValue = ? THEN T-MATG.PromMinDto[k] = 0.
                k = k + 1.
            END.
        END.
        WHEN 6 THEN DO:
            ASSIGN
                T-MATG.PromMinDivi[1] = f-Division
                T-MATG.PromMinFchD[1] = f-FechaD
                T-MATG.PromMinFchH[1] = f-FechaH.
            k = 1.
            DO iCountLine = 1 TO 1:
                t-Column = t-Column + 1.
                cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
                IF cValue = ? OR DECIMAL(cValue) = 0 THEN NEXT.
                ASSIGN
                    T-MATG.PromMinDto[k]  = DECIMAL(cValue).
                IF cValue = ? THEN T-MATG.PromMinDto[k] = 0.
                k = k + 1.
            END.
        END.
        OTHERWISE DO:
            k = 1.
            DO iCountLine = 1 TO NUM-ENTRIES(f-Division):
                t-Column = t-Column + 1.
                cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
                IF cValue = ? OR DECIMAL(cValue) = 0 THEN NEXT.
                IF x-MetodoActualizacion = 2 AND x-Division <> ENTRY(iCountLine, f-Division)  THEN NEXT.     /* En caso de seleccionar 1 division */
                ASSIGN
                    T-MATG.PromDivi[k] = ENTRY(iCountLine, f-Division) 
                    T-MATG.PromDto[k]  = DECIMAL(cValue)
                    T-MATG.PromFchD[k] = f-FechaD
                    T-MATG.PromFchH[k] = f-FechaH.
                IF cValue = ? THEN T-MATG.PromDto[k] = 0.
                k = k + 1.
            END.
        END.
    END CASE.
END.
/* EN CASO DE ERROR */
IF pMensaje > "" THEN DO:
    EMPTY TEMP-TABLE T-MATG.
    RETURN.
END.
f-Division = x-Division.    /* OJO: NOS QUEDAMOS CON UNA O TODAS LAS DIVISIONES */
/* Cargamos precios actuales */
FOR EACH T-MATG:
    FIND Almmmatg WHERE Almmmatg.codcia = s-codcia
        AND Almmmatg.codmat = T-MATG.codmat
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Almmmatg THEN DO:
        DELETE T-MATG.
        NEXT.
    END.
    IF TRUE <> (T-MATG.DesMat > '') THEN T-MATG.DesMat = Almmmatg.DesMat.
    FIND VtaListaMinGn WHERE VtaListaMinGn.codcia = s-codcia
        AND VtaListaMinGn.codmat = T-MATG.codmat
        NO-LOCK NO-ERROR.
    FIND VtaListaMay WHERE VtaListaMay.codcia = s-codcia
        AND VtaListaMay.coddiv = COMBO-BOX-Division
        AND VtaListaMay.codmat = T-MATG.codmat
        NO-LOCK NO-ERROR.
    FIND Almmmatp WHERE Almmmatp.codcia = s-codcia
        AND Almmmatp.codmat = T-MATG.codmat
        NO-LOCK NO-ERROR.
    IF RADIO-SET-1 = 2 AND NOT AVAILABLE VtaListaMinGn THEN NEXT.
    IF RADIO-SET-1 = 3 AND NOT AVAILABLE VtaListaMay   THEN NEXT.
    IF RADIO-SET-1 = 4 AND NOT AVAILABLE Almmmatp      THEN NEXT.
    ASSIGN
        T-MATG.MonVta = Almmmatg.MonVta 
        T-MATG.TpoCmb = Almmmatg.TpoCmb
        T-MATG.CtoTot = Almmmatg.CtoTot
        T-MATG.UndBas = Almmmatg.UndBas
        T-MATG.CHR__02 = Almmmatg.CHR__02.
    CASE RADIO-SET-1:
        WHEN 1 OR WHEN 5 OR WHEN 6 THEN DO:
            ASSIGN
                T-MATG.Chr__01   = Almmmatg.Chr__01 
                T-MATG.PreOfi    = Almmmatg.PreOfi 
                T-MATG.Prevta[1] = Almmmatg.PreVta[1].
        END.
        WHEN 2 THEN DO:
            ASSIGN
                T-MATG.Chr__01   = VtaListaMinGn.Chr__01 
                T-MATG.PreOfi    = VtaListaMinGn.PreOfi 
                T-MATG.Prevta[1] = VtaListaMinGn.PreOfi.
        END.
        WHEN 3 THEN DO:
            ASSIGN
                T-MATG.Chr__01   = VtaListaMay.Chr__01 
                T-MATG.PreOfi    = VtaListaMay.PreOfi 
                T-MATG.Prevta[1] = VtaListaMay.PreOfi.
        END.
        WHEN 4 THEN DO:
            ASSIGN
                T-MATG.CtoTot    = Almmmatg.CtoTotMarco
                T-MATG.Chr__01   = Almmmatp.Chr__01 
                T-MATG.PreOfi    = Almmmatp.PreOfi 
                T-MATG.Prevta[1] = Almmmatp.PreOfi.
        END.
    END CASE.
    CASE RADIO-SET-1:
        WHEN 4  THEN DO:    /* Los precios est�n siempre en soles */
            IF T-MATG.MonVta = 2 THEN
                ASSIGN
                T-MATG.CtoTot    = T-MATG.CtoTot * T-MATG.TpoCmb.
        END.
        OTHERWISE DO:
            IF T-MATG.MonVta = 2 THEN
                ASSIGN
                T-MATG.PreOfi    = T-MATG.PreOfi * T-MATG.TpoCmb
                T-MATG.Prevta[1] = T-MATG.PreVta[1] * T-MATG.TpoCmb
                T-MATG.CtoTot    = T-MATG.CtoTot * T-MATG.TpoCmb.
        END.
    END CASE.

    /* Corregimos de acuerdo al m�todo de c�lculo */
    IF RADIO-SET-1 = 1 OR RADIO-SET-1 = 2 THEN DO:
        IF RADIO-SET-2 = 1 THEN DO:     /* PORCENTAJES */
            FOR EACH T-TABLA WHERE T-TABLA.Llave_c1 = T-MATG.CodMat:
                T-TABLA.Valor[2] = ROUND(T-MATG.Prevta[1] * ( 1 - ( T-TABLA.Valor[1] / 100 ) ),4).
            END.
        END.
        IF RADIO-SET-2 = 2 THEN DO:     /* VALORES */
            FOR EACH T-TABLA WHERE T-TABLA.Llave_c1 = T-MATG.CodMat:
                T-TABLA.Valor[2] = T-TABLA.Valor[1].
                T-TABLA.Valor[1] = ROUND ( ( 1 - T-TABLA.Valor[1] / T-MATG.PreVta[1] ) * 100, 4).
            END.
        END.
    END.
    IF RADIO-SET-2 = 2 THEN DO:     /* VALORES */
        DO iValue = 1 TO 10:
            IF T-MATG.PromDto[iValue] > 0 THEN
            T-MATG.PromDto[iValue] = ROUND ( ( 1 -  T-MATG.PromDto[iValue] / T-MATG.PreVta[1]  ) * 100, 4 ).
        END.
        IF RADIO-SET-1 = 6 THEN DO:     /* PROVINCIAS PROPIOS */
            DO iValue = 1 TO 1:
                IF T-MATG.PromMinDto[iValue] > 0 THEN
                    T-MATG.PromMinDto[iValue] = ROUND ( ( 1 -  T-MATG.PromMinDto[iValue] / T-MATG.PreVta[1]  ) * 100, 4 ).
            END.
        END.
    END.
END.
/* Filtramos solo lineas autorizadas */
FOR EACH T-MATG, FIRST Almmmatg OF T-MATG NO-LOCK:
    FIND Vtatabla WHERE Vtatabla.codcia = s-codcia
        AND Vtatabla.tabla = "LP"
        AND Vtatabla.llave_c1 = s-user-id
        AND Vtatabla.llave_c2 = Almmmatg.codfam
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Vtatabla THEN DELETE T-MATG.
END.
/* Filtramos solo productos de terceros */
IF RADIO-SET-1 = 5 THEN FOR EACH T-MATG WHERE T-MATG.CHR__02 <> "T":
    DELETE T-MATG.
END.
IF RADIO-SET-1 = 6 THEN FOR EACH T-MATG WHERE T-MATG.CHR__02 <> "P":
    DELETE T-MATG.
END.
/* ******************************************** */
/* ****** RHC 29/02/2016 NO % negativos ******* */
/* ******************************************** */
/* FOR EACH T-TABLA WHERE T-TABLA.Valor[1] < 0: */
/*     DELETE T-TABLA.                          */
/* END.                                         */
CICLO:
FOR EACH T-MATG:
    DO k = 1 TO 2:
        IF T-MATG.PromMinDto[k] < 0 THEN DO:
            DELETE T-MATG.
            NEXT CICLO.
        END.
    END.
    DO k = 1 TO 10:
        IF T-MATG.PromDto[k] < 0 THEN DO:
            DELETE T-MATG.
            NEXT CICLO.
        END.
    END.
END.
/* ******************************************** */

FIND FIRST T-MATG NO-LOCK NO-ERROR.
IF NOT AVAILABLE T-MATG THEN MESSAGE 'No hay registros que procesar' VIEW-AS ALERT-BOX ERROR.

ASSIGN
    BUTTON-1:SENSITIVE IN FRAME {&FRAME-NAME} = NO.

IF h_butilexdctoprom-02-2 <> ? THEN RUN dispatch IN h_butilexdctoprom-02-2 ('open-query':U).
IF h_butilexdctoprom-02 <> ? THEN RUN dispatch IN h_butilexdctoprom-02 ('open-query':U).
IF h_butilexdctoprom-02a <> ? THEN RUN dispatch IN h_butilexdctoprom-02a ('open-query':U).
IF h_butilexdctoprom-02b <> ? THEN RUN dispatch IN h_butilexdctoprom-02b ('open-query':U).

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
  DISPLAY COMBO-BOX-Division txtCodMat 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE RECT-3 BtnDone COMBO-BOX-Division BUTTON-1 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Excel-de-Errores W-Win 
PROCEDURE Excel-de-Errores :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FIND FIRST E-MATG NO-LOCK NO-ERROR.
IF NOT AVAILABLE E-MATG THEN RETURN.
MESSAGE 'Los siguientes producto NO se han actualizado' SKIP
    'Se mostrar� una lista de errores en Excel'
    VIEW-AS ALERT-BOX WARNING.

DEFINE VARIABLE chExcelApplication      AS COM-HANDLE.
DEFINE VARIABLE chWorkbook              AS COM-HANDLE.
DEFINE VARIABLE chWorksheet             AS COM-HANDLE.
DEFINE VARIABLE chChart                 AS COM-HANDLE.
DEFINE VARIABLE chWorksheetRange        AS COM-HANDLE.
DEFINE VARIABLE cColumn                 AS CHARACTER.
DEFINE VARIABLE cRange                  AS CHARACTER.
DEFINE VARIABLE t-Column                AS INTEGER INIT 1.
DEFINE VARIABLE t-Row                   AS INTEGER INIT 1.

/* create a new Excel Application object */
CREATE "Excel.Application" chExcelApplication.

/* create a new Workbook */
chWorkbook = chExcelApplication:Workbooks:Add().

/* get the active Worksheet */
chWorkSheet = chExcelApplication:Sheets:Item(1).

/* set the column names for the Worksheet */
DEF VAR x-CtoTot LIKE Almmmatg.ctotot NO-UNDO.
DEF VAR x-PreVta LIKE Almmmatg.prevta NO-UNDO.
DEF VAR x-PreOfi LIKE Almmmatg.preofi NO-UNDO.

ASSIGN
    chWorkSheet:Range("A1"):Value = "ERRORES - PRECIOS"
    chWorkSheet:Range("A2"):Value = "CODIGO"
    chWorkSheet:Columns("A"):NumberFormat = "@"
    chWorkSheet:Range("B2"):Value = "DESCRIPCION"
    chWorkSheet:Range("C2"):Value = "ERROR".
ASSIGN
    t-Row = 2.
FOR EACH E-MATG:
    ASSIGN
        t-Column = 0
        t-Row    = t-Row + 1.
    ASSIGN
        t-Column = t-Column + 1
        chWorkSheet:Cells(t-Row, t-Column):VALUE = E-MATG.codmat.
    ASSIGN
        t-Column = t-Column + 1
        chWorkSheet:Cells(t-Row, t-Column):VALUE = E-MATG.desmat.
    ASSIGN
        t-Column = t-Column + 1
        chWorkSheet:Cells(t-Row, t-Column):VALUE = E-MATG.Libre_c01.
END.
chExcelApplication:VISIBLE = TRUE.
/* release com-handles */
RELEASE OBJECT chExcelApplication.      
RELEASE OBJECT chWorkbook.
RELEASE OBJECT chWorksheet.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Grabar-Descuentos W-Win 
PROCEDURE Grabar-Descuentos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

MESSAGE 'Procedemos a grabar los descuentos promocionales?'
    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
    UPDATE rpta AS LOG.
IF rpta = NO THEN RETURN.

/* Verificamos si hay un margen negativo */
FOR FIRST T-TABLA WHERE T-TABLA.Valor[1] < 0:
    MESSAGE 'Hay un margen negativo:' SKIP
        'C�digo:' T-TABLA.llave_c1 SKIP
        'Divisi�n:' T-TABLA.llave_c2 SKIP(2)
        'Proceso abortado'
        VIEW-AS ALERT-BOX WARNING.
    RETURN.
END.
/* ************************************* */
DEF VAR pError AS CHAR NO-UNDO.
DEF VAR X-MARGEN AS DEC NO-UNDO.
DEF VAR X-LIMITE AS DEC NO-UNDO.
DEF VAR x-PreUni AS DEC NO-UNDO.

DEF VAR k AS INT NO-UNDO.
DEF VAR j AS INT NO-UNDO.


CASE RADIO-SET-1:
    WHEN 1 THEN DO:
        {vta2/wdctoprommayoristav02.i &Tabla=Almmmatg}
    END.
    WHEN 2 THEN DO:

        /*{vta2/wutilexdctoprom-02.i &Tabla=Vtalistamingn}*/
        {vta2/wutilexdctoprom-03.i &Tabla=Vtalistamingn}
    END.
    WHEN 3 THEN DO:
        /* ERRORES */
        EMPTY TEMP-TABLE E-MATG.
        trloop:
        FOR EACH T-MATG, FIRST VtaListaMay OF T-MATG WHERE VtaListaMay.CodDiv = COMBO-BOX-Division NO-LOCK:
            /* Cargamos informacion */
            DO k = 1 TO 10:
                IF T-MATG.PromDto[k] <> 0 THEN DO:
                    /* Versi�n nueva NO aprobada */
/*                     RUN Margen-de-Utilidad (INPUT s-CodDiv,                                       */
/*                                             INPUT T-MATG.CodMat,                                  */
/*                                             T-MATG.PreVta[1] * ( 1 - (T-MATG.PromDto[k] / 100) ), */
/*                                             INPUT T-MATG.CHR__01,                                 */
/*                                             INPUT T-MATG.TpoCmb,                                  */
/*                                             OUTPUT x-Limite,                                      */
/*                                             OUTPUT pError).                                       */
                    RUN vtagn/p-margen-utilidad (
                        T-MATG.CodMat,
                        T-MATG.PreVta[1] * ( 1 - (T-MATG.PromDto[k] / 100) ),
                        T-MATG.CHR__01,
                        1,                      /* Moneda */
                        T-MATG.TpoCmb,
                        NO,                     /* Muestra error? */
                        "",                     /* Almac�n */
                        OUTPUT x-Margen,        /* Margen de utilidad */
                        OUTPUT x-Limite,        /* Margen m�nimo de utilidad */
                        OUTPUT pError           /* Control de errores: "OK" "ADM-ERROR" */
                        ).
                    IF pError = "ADM-ERROR" THEN DO:
                        CREATE E-MATG.
                        BUFFER-COPY T-MATG 
                            TO E-MATG
                            ASSIGN 
                            E-MATG.Libre_c01 = "Margen de utilidad NO debe ser menor a " + TRIM(STRING(x-Limite, ">>>,>>9.99")).
                        DELETE T-MATG.
                        NEXT trloop.
                    END.
                END.
            END.
        END.

        FOR EACH T-MATG ON ERROR UNDO, RETURN ON STOP UNDO, RETURN:
            FIND VtaListaMay OF T-MATG WHERE VtaListaMay.CodDiv = COMBO-BOX-Division EXCLUSIVE-LOCK NO-ERROR.
            IF NOT AVAILABLE VtaListaMay THEN DO:
                MESSAGE 'NO se pudo actualizar el art�culo' T-MATG.codmat SKIP
                    'Proceso abortado' VIEW-AS ALERT-BOX WARNING.
                NEXT.
            END.
            /* Limpiamos informacion */
            ASSIGN
                VtaListaMay.PromDto = 0
                VtaListaMay.PromFchD = ?
                VtaListaMay.PromFchH = ?.
            /* Cargamos informacion */
            j = 0.
            DO k = 1 TO 1:
                IF T-MATG.PromDto[k] <> 0 THEN DO:
                    j = j + 1.
                    ASSIGN
                        VtaListaMay.PromDto = T-MATG.PromDto[k]
                        VtaListaMay.PromFchD = T-MATG.PromFchD[k]
                        VtaListaMay.PromFchH = T-MATG.PromFchH[k].
                END.
            END.
            VtaListaMay.fchact = TODAY.
        END.
        EMPTY TEMP-TABLE T-MATG.
    END.
    WHEN 4 THEN DO:
        {vta2/wutilexdctoprom-02.i &Tabla=Almmmatp}
    END.
    WHEN 5 THEN DO:     /* PROVINCIAS TERCEROS SIEMPRE ES IMPORTE */
        /* CONSISTENCIA DE DATOS */
        EMPTY TEMP-TABLE E-MATG.
        trloop:
        FOR EACH T-MATG NO-LOCK:
            /* Cargamos informacion "A" y "B" */
            DO k = 1 TO 2:
                IF T-MATG.PromMinDto[k] <> 0 THEN DO:
                    RUN vtagn/p-margen-utilidad (
                        T-MATG.CodMat,
                        T-MATG.PromMinDto[k],
                        T-MATG.CHR__01,
                        1,                      /* Moneda */
                        T-MATG.TpoCmb,
                        NO,                     /* Muestra error? */
                        "",                     /* Almac�n */
                        OUTPUT x-Margen,        /* Margen de utilidad */
                        OUTPUT x-Limite,        /* Margen m�nimo de utilidad */
                        OUTPUT pError           /* Control de errores: "OK" "ADM-ERROR" */
                        ).
                    IF pError = "ADM-ERROR" THEN DO:
                        CREATE E-MATG.
                        BUFFER-COPY T-MATG 
                            TO E-MATG
                            ASSIGN 
                            E-MATG.Libre_c01 = "Margen de utilidad NO debe ser menor a " + TRIM(STRING(x-Limite, ">>>,>>9.99")).
                        DELETE T-MATG.
                        NEXT trloop.
                    END.
                END.
            END.
        END.

        FOR EACH T-MATG ON STOP UNDO, RETURN ON ERROR UNDO, RETURN:
            FIND Almmmatg OF T-MATG EXCLUSIVE-LOCK NO-ERROR.
            IF NOT AVAILABLE Almmmatg THEN UNDO, RETURN.
            /* Limpiamos informacion */
            ASSIGN
                Almmmatg.PromMinDivi[1] = ""
                Almmmatg.PromMinDto[1] = 0
                Almmmatg.PromMinDto[2] = 0
                Almmmatg.PromMinFchD[1] = ?
                Almmmatg.PromMinFchH[1] = ?.
            /* Cargamos informacion */
            IF T-MATG.PromMinDto[1] <> 0 OR T-MATG.PromMinDto[2] <> 0 
                THEN DO:
                ASSIGN
                    Almmmatg.PromMinDivi[1] = "00018"
                    Almmmatg.PromMinDto[1] = T-MATG.PromMinDto[1]
                    Almmmatg.PromMinDto[2] = T-MATG.PromMinDto[2]
                    Almmmatg.PromMinFchD[1] = T-MATG.PromMinFchD[1]
                    Almmmatg.PromMinFchH[1] = T-MATG.PromMinFchH[1].
            END.
            Almmmatg.fchact = TODAY.
        END.
        EMPTY TEMP-TABLE T-MATG.
    END.
    WHEN 6 THEN DO:
        /* CONSISTENCIA DE DATOS */
        EMPTY TEMP-TABLE E-MATG.
        trloop:
        FOR EACH T-MATG NO-LOCK:
            /* Cargamos informacion */
            DO k = 1 TO 1:
                IF T-MATG.PromMinDto[k] <> 0 THEN DO:
                    /* Versi�n nueva NO aprobada */
/*                     RUN Margen-de-Utilidad (INPUT T-MATG.CodMat,                                     */
/*                                             T-MATG.PreVta[1] * ( 1 - (T-MATG.PromMinDto[k] / 100) ), */
/*                                             INPUT T-MATG.CHR__01,                                    */
/*                                             INPUT T-MATG.TpoCmb,                                     */
/*                                             OUTPUT x-Limite,                                         */
/*                                             OUTPUT pError).                                          */
                    RUN vtagn/p-margen-utilidad (
                        T-MATG.CodMat,
                        T-MATG.PreVta[1] * ( 1 - (T-MATG.PromMinDto[k] / 100) ),
                        T-MATG.CHR__01,
                        1,                      /* Moneda */
                        T-MATG.TpoCmb,
                        NO,                     /* Muestra error? */
                        "",                     /* Almac�n */
                        OUTPUT x-Margen,        /* Margen de utilidad */
                        OUTPUT x-Limite,        /* Margen m�nimo de utilidad */
                        OUTPUT pError           /* Control de errores: "OK" "ADM-ERROR" */
                        ).
                    IF pError = "ADM-ERROR" THEN DO:
                        CREATE E-MATG.
                        BUFFER-COPY T-MATG 
                            TO E-MATG
                            ASSIGN 
                            E-MATG.Libre_c01 = "Margen de utilidad NO debe ser menor a " + TRIM(STRING(x-Limite, ">>>,>>9.99")).
                        DELETE T-MATG.
                        NEXT trloop.
                    END.
                END.
            END.
        END.

        FOR EACH T-MATG ON STOP UNDO, RETURN ON ERROR UNDO, RETURN:
            FIND Almmmatg OF T-MATG EXCLUSIVE-LOCK NO-ERROR.
            IF NOT AVAILABLE Almmmatg THEN UNDO, RETURN.
            /* Limpiamos informacion */
            ASSIGN
                Almmmatg.PromMinDivi[1] = ""
                Almmmatg.PromMinDto[1] = 0
                Almmmatg.PromMinDto[2] = 0
                Almmmatg.PromMinFchD[1] = ?
                Almmmatg.PromMinFchH[1] = ?.
            /* Cargamos informacion */
            IF T-MATG.PromMinDto[1] <> 0 OR T-MATG.PromMinDto[2] <> 0 
                THEN DO:
                ASSIGN
                    Almmmatg.PromMinDivi[1] = "00018"
                    Almmmatg.PromMinDto[1] = T-MATG.PromMinDto[1]
                    Almmmatg.PromMinDto[2] = T-MATG.PromMinDto[2]
                    Almmmatg.PromMinFchD[1] = T-MATG.PromMinFchD[1]
                    Almmmatg.PromMinFchH[1] = T-MATG.PromMinFchH[1].
            END.
            Almmmatg.fchact = TODAY.
        END.
        EMPTY TEMP-TABLE T-MATG.
    END.
END CASE.
IF AVAILABLE(VtaListaMay) THEN RELEASE VtaListaMay.

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
  DO WITH FRAME {&FRAME-NAME}:
      COMBO-BOX-Division:DELETE(1).
      FOR EACH gn-divi NO-LOCK WHERE gn-divi.codcia = s-codcia:
          COMBO-BOX-Division:ADD-LAST(gn-divi.coddiv + ' - ' + gn-divi.desdiv, gn-divi.coddiv).
      END.
      COMBO-BOX-Division = s-coddiv.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Margen-de-Utilidad W-Win 
PROCEDURE Margen-de-Utilidad :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF INPUT PARAMETER pCodDiv AS CHAR.
DEF INPUT PARAMETER pCodMat AS CHAR.
DEF INPUT PARAMETER pPreUni AS DEC.
DEF INPUT PARAMETER pUndVta AS CHAR.
DEF INPUT PARAMETER pTpoCmb AS DEC.
DEF OUTPUT PARAMETER x-Limite AS DEC.
DEF OUTPUT PARAMETER pError AS CHAR.

DEF VAR x-Margen AS DEC NO-UNDO.    /* Margen de utilidad */

pError = ''.
    RUN vtagn/p-margen-utilidad-v2 (pCodDiv,
                                    pCodMat,
                                    pPreUni,
                                    pUndVta,
                                    1,                      /* Moneda */
                                    pTpoCmb,
                                    NO,                     /* Muestra error? */
                                    "",                     /* Almac�n */
                                    OUTPUT x-Margen,        /* Margen de utilidad */
                                    OUTPUT x-Limite,        /* Margen m�nimo de utilidad */
                                    OUTPUT pError           /* Control de errores: "OK" "ADM-ERROR" */
                                    ).
IF RETURN-VALUE = 'ADM-ERROR' THEN pError = 'ADM-ERROR'.

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

