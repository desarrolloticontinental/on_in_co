&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          integral         PROGRESS
*/
&Scoped-define WINDOW-NAME W-Win


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE T-MATG NO-UNDO LIKE Almmmatg
       field codalm like almmmate.codalm
       field stkact like almmmate.stkact
       field minimo as dec
       field maximo as dec
       field dias   as dec
       field cantidad as dec
       field costos as dec
       field compras as dec
       index llave01 codcia codalm codmat
       index llave02 codcia codmat.



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
DEF SHARED VAR pv-codcia AS INT.
DEF STREAM REPORTE.

/* Buscamos los valores generales */
FIND FIRST AlmCfgGn WHERE almcfggn.codcia = s-codcia NO-LOCK NO-ERROR.
IF NOT AVAILABLE Almcfggn THEN DO:
    MESSAGE 'Debe configurar los par�metros generales' VIEW-AS ALERT-BOX WARNING.
    RETURN ERROR.
END.

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
&Scoped-Define ENABLED-OBJECTS txt-codalm BUTTON-1 x-CodFam x-CodPro ~
TOGGLE-Resumen BUTTON-6 BUTTON-7 
&Scoped-Define DISPLAYED-OBJECTS txt-codalm x-CodFam x-desFam x-CodPro ~
x-NomPro FILL-IN-DiasMinimo FILL-IN-DiasMaximo TOGGLE-Resumen x-mensaje 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-1 
     LABEL "..." 
     SIZE 3 BY .88.

DEFINE BUTTON BUTTON-6 
     IMAGE-UP FILE "img/excel.bmp":U
     LABEL "Button 6" 
     SIZE 6 BY 1.62.

DEFINE BUTTON BUTTON-7 
     IMAGE-UP FILE "img/exit.ico":U
     LABEL "Button 7" 
     SIZE 6 BY 1.62.

DEFINE VARIABLE FILL-IN-DiasMaximo AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Dias m�ximo" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-DiasMinimo AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Dias m�nimo" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE txt-codalm AS CHARACTER FORMAT "X(256)":U 
     LABEL "Almacen" 
     VIEW-AS FILL-IN 
     SIZE 58 BY .88
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE x-CodFam AS CHARACTER FORMAT "X(3)":U 
     LABEL "Familia" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 NO-UNDO.

DEFINE VARIABLE x-CodPro AS CHARACTER FORMAT "X(11)":U 
     LABEL "Proveedor" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE x-desFam AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 55 BY .88 NO-UNDO.

DEFINE VARIABLE x-mensaje AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 46.29 BY .88 NO-UNDO.

DEFINE VARIABLE x-NomPro AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 48 BY .88 NO-UNDO.

DEFINE VARIABLE TOGGLE-Resumen AS LOGICAL INITIAL no 
     LABEL "Resumido?" 
     VIEW-AS TOGGLE-BOX
     SIZE 13 BY .77 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     txt-codalm AT ROW 1.27 COL 11 COLON-ALIGNED WIDGET-ID 30
     BUTTON-1 AT ROW 1.27 COL 71 WIDGET-ID 62
     x-CodFam AT ROW 2.35 COL 11 COLON-ALIGNED WIDGET-ID 70
     x-desFam AT ROW 2.35 COL 19 COLON-ALIGNED NO-LABEL WIDGET-ID 72
     x-CodPro AT ROW 3.42 COL 11 COLON-ALIGNED WIDGET-ID 74
     x-NomPro AT ROW 3.42 COL 26 COLON-ALIGNED NO-LABEL WIDGET-ID 76
     FILL-IN-DiasMinimo AT ROW 4.77 COL 19 COLON-ALIGNED WIDGET-ID 86
     FILL-IN-DiasMaximo AT ROW 5.85 COL 19 COLON-ALIGNED WIDGET-ID 88
     TOGGLE-Resumen AT ROW 7.19 COL 12 WIDGET-ID 82
     BUTTON-6 AT ROW 8.54 COL 2 WIDGET-ID 64
     BUTTON-7 AT ROW 8.54 COL 9 WIDGET-ID 66
     x-mensaje AT ROW 9.08 COL 17 COLON-ALIGNED NO-LABEL WIDGET-ID 68
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 82.86 BY 9.92 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: T-MATG T "?" NO-UNDO INTEGRAL Almmmatg
      ADDITIONAL-FIELDS:
          field codalm like almmmate.codalm
          field stkact like almmmate.stkact
          field minimo as dec
          field maximo as dec
          field dias   as dec
          field cantidad as dec
          field costos as dec
          field compras as dec
          index llave01 codcia codalm codmat
          index llave02 codcia codmat
      END-FIELDS.
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "STOCK PROYECTADO POR ALMACEN"
         HEIGHT             = 9.92
         WIDTH              = 82.86
         MAX-HEIGHT         = 17
         MAX-WIDTH          = 82.86
         VIRTUAL-HEIGHT     = 17
         VIRTUAL-WIDTH      = 82.86
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

{src/adm-vm/method/vmviewer.i}
{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN FILL-IN-DiasMaximo IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-DiasMinimo IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN x-desFam IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN x-mensaje IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN x-NomPro IN FRAME F-Main
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* STOCK PROYECTADO POR ALMACEN */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* STOCK PROYECTADO POR ALMACEN */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 W-Win
ON CHOOSE OF BUTTON-1 IN FRAME F-Main /* ... */
DO:

    DEF VAR x-Almacenes AS CHAR.
    x-Almacenes = txt-CodAlm:SCREEN-VALUE.
    RUN alm/d-repalm (INPUT-OUTPUT x-Almacenes).
    txt-CodAlm:SCREEN-VALUE = x-Almacenes.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-6 W-Win
ON CHOOSE OF BUTTON-6 IN FRAME F-Main /* Button 6 */
DO:
    ASSIGN txt-codalm x-codfam x-codpro FILL-IN-DiasMaximo FILL-IN-DiasMinimo TOGGLE-Resumen.
    IF txt-codalm = "" THEN DO:
        MESSAGE "Debe seleccionar al menos un almacen" VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY.
    END.
    IF FILL-IN-DiasMaximo < FILL-IN-DiasMinimo THEN DO:
        MESSAGE 'Ingrese correctamente los d�as m�nimo y m�ximo' 
            VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY.
    END.
    IF TOGGLE-Resumen = YES THEN RUN Excel2.
    ELSE RUN Excel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-7 W-Win
ON CHOOSE OF BUTTON-7 IN FRAME F-Main /* Button 7 */
DO:
    APPLY "CLOSE":U TO THIS-PROCEDURE.
    RETURN NO-APPLY.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME x-CodFam
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL x-CodFam W-Win
ON LEAVE OF x-CodFam IN FRAME F-Main /* Familia */
DO:
  IF SELF:SCREEN-VALUE = '' THEN RETURN.
  FIND Almtfami WHERE Almtfami.codcia = s-codcia
      AND Almtfami.codfam = SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
  IF NOT AVAILABLE Almtfami THEN DO:
      MESSAGE "Familia no registrada" VIEW-AS ALERT-BOX ERROR.
      RETURN NO-APPLY.
  END.
  x-desfam:SCREEN-VALUE = Almtfami.desfam. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME x-CodPro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL x-CodPro W-Win
ON LEAVE OF x-CodPro IN FRAME F-Main /* Proveedor */
DO:
    IF SELF:SCREEN-VALUE = '' THEN DO:
        DISPLAY
            AlmCfgGn.DiasMinimo @ FILL-IN-DiasMinimo
            AlmCfgGn.DiasMaximo @ FILL-IN-DiasMaximo
            WITH FRAME {&FRAME-NAME}.
        ASSIGN
/*             FILL-IN-DiasMinimo:HIDDEN IN FRAME {&FRAME-NAME} = YES */
/*             FILL-IN-DiasMaximo:HIDDEN IN FRAME {&FRAME-NAME} = YES */
            FILL-IN-DiasMinimo:SENSITIVE IN FRAME {&FRAME-NAME} = NO
            FILL-IN-DiasMaximo:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
        RETURN.
    END.
    FIND gn-prov WHERE gn-prov.codcia = pv-codcia
        AND gn-prov.codpro = SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
    IF NOT AVAILABLE gn-prov THEN DO:
        MESSAGE "Proveedor no registrado" VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY.
    END.
    x-nompro:SCREEN-VALUE = gn-prov.nompro.
    DISPLAY
        gn-prov.StkMin @ FILL-IN-DiasMinimo
        gn-prov.StkMax @ FILL-IN-DiasMaximo
        WITH FRAME {&FRAME-NAME}.
    ASSIGN
/*         FILL-IN-DiasMinimo:HIDDEN IN FRAME {&FRAME-NAME} = NO */
/*         FILL-IN-DiasMaximo:HIDDEN IN FRAME {&FRAME-NAME} = NO */
        FILL-IN-DiasMinimo:SENSITIVE IN FRAME {&FRAME-NAME} = YES
        FILL-IN-DiasMaximo:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
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
PROCEDURE Carga-Temporal :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR i AS INT NO-UNDO.
DEF VAR x-CodAlm AS CHAR NO-UNDO.
DEF VAR pVentaDiaria AS DEC NO-UNDO.
DEF VAR pDiasUtiles AS INT.
DEF VAR pDiasMaximo AS INT.
DEF VAR pDiasMinimo AS INT.

FOR EACH T-MATG:
    DELETE T-MATG.
END.

/* Cargamos stocks */
FOR EACH Almmmatg NO-LOCK WHERE Almmmatg.codcia = s-codcia
    AND Almmmatg.codfam BEGINS x-CodFam
    AND Almmmatg.codpr1 BEGINS x-CodPro
    AND Almmmatg.TpoArt <> "D",
    FIRST Almtfami OF Almmmatg NO-LOCK WHERE Almtfami.SwComercial = YES
    BY Almmmatg.codmat:
    x-Mensaje:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "CALCULANDO STOCK: " + Almmmatg.codmat.
    DO i = 1 TO NUM-ENTRIES(txt-codalm):
        FIND Almmmate WHERE Almmmate.codcia = s-codcia
            AND Almmmate.codalm = ENTRY(I, txt-codalm)
            AND Almmmate.codmat = Almmmatg.codmat
            NO-LOCK NO-ERROR.
        IF AVAILABLE Almmmate THEN DO:
            CREATE T-MATG.
            BUFFER-COPY Almmmatg TO T-MATG
                ASSIGN
                    T-MATG.CodAlm = Almmmate.CodAlm
                    T-MATG.StkAct = Almmmate.StkAct
                    T-MATG.StkMin = Almmmate.StkMin
                    T-MATG.StkMax = Almmmate.StkMax
                    T-MATG.StkRep = Almmmate.StkRep.
            IF Almmmatg.CanEmp = 0 THEN T-MATG.CanEmp = 1.
            IF Almmmatg.MonVta = 2 THEN T-MATG.CtoTot = T-MATG.CtoTot * T-MATG.TpoCmb.
        END.
    END.
END.

/* Cargamos dias de stock */
FOR EACH T-MATG:
    x-Mensaje:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "CALCULANDO VENTAS: " + T-MATG.codalm + " " + T-MATG.codmat.
    /* Valores por defecto */
    ASSIGN
        pDiasMaximo = AlmCfgGn.DiasMaximo
        pDiasMinimo = AlmCfgGn.DiasMinimo
        pDiasUtiles = AlmCfgGn.DiasUtiles.
    /* Buscamos parametros del proveedor */
    FIND FIRST gn-prov WHERE gn-prov.codcia = pv-codcia
        AND gn-prov.codpro = T-MATG.codpr1
        NO-LOCK NO-ERROR.
    IF AVAILABLE gn-prov AND (gn-prov.stkmin > 0 AND gn-prov.stkmax > 0) THEN DO:
        ASSIGN
            pDiasMaximo = gn-prov.stkmax
            pDiasMinimo = gn-prov.stkmin.
    END.
    IF x-CodPro <> "" 
    THEN ASSIGN
            pDiasMinimo = FILL-IN-DiasMinimo
            pDiasMaximo = FILL-IN-DiasMaximo.
    /* Venta Diaria */
    ASSIGN
        T-MATG.Dias   = ( IF T-MATG.StkRep > 0 THEN (T-MATG.StkAct / T-MATG.StkRep) ELSE 0 )
        T-MATG.Minimo = T-MATG.StkRep * pDiasMinimo
        T-MATG.Maximo = T-MATG.StkRep * pDiasMaximo.
    /* Cantidad */
    IF T-MATG.StkAct < T-MATG.Minimo THEN T-MATG.Cantidad = T-MATG.StkAct - T-MATG.Minimo.
    IF T-MATG.StkAct > T-MATG.Maximo THEN T-MATG.Cantidad = T-MATG.StkAct - T-MATG.Maximo.
    T-MATG.Costo = T-MATG.CtoTot * T-MATG.Cantidad.
END.
x-Mensaje:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Carga-Temporal-2 W-Win 
PROCEDURE Carga-Temporal-2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR i AS INT NO-UNDO.
DEF VAR x-CodAlm AS CHAR NO-UNDO.
DEF VAR pVentaDiaria AS DEC NO-UNDO.
DEF VAR pDiasUtiles AS INT.
DEF VAR pDiasMaximo AS INT.
DEF VAR pDiasMinimo AS INT.

FOR EACH T-MATG:
    DELETE T-MATG.
END.

/* Cargamos stocks */
FOR EACH Almmmatg NO-LOCK WHERE Almmmatg.codcia = s-codcia
    AND Almmmatg.codfam BEGINS x-CodFam
    AND Almmmatg.codpr1 BEGINS x-CodPro
    AND Almmmatg.TpoArt <> "D",
    FIRST Almtfami OF Almmmatg NO-LOCK WHERE Almtfami.SwComercial = YES
    BY Almmmatg.codmat:
    x-Mensaje:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "CALCULANDO STOCK: " + Almmmatg.codmat.
    DO i = 1 TO NUM-ENTRIES(txt-codalm):
        FIND Almmmate WHERE Almmmate.codcia = s-codcia
            AND Almmmate.codalm = ENTRY(I, txt-codalm)
            AND Almmmate.codmat = Almmmatg.codmat
            NO-LOCK NO-ERROR.
        IF AVAILABLE Almmmate THEN DO:
            FIND T-MATG WHERE T-MATG.codcia = Almmmate.codcia
                AND T-MATG.codmat = Almmmate.codmat NO-ERROR.
            IF NOT AVAILABLE T-MATG THEN CREATE T-MATG.
            BUFFER-COPY Almmmatg 
                EXCEPT Almmmatg.StkMin Almmmatg.StkMax Almmmatg.StkRep
                TO T-MATG
                ASSIGN
                    T-MATG.StkAct = T-MATG.StkAct + Almmmate.StkAct
                    T-MATG.StkMin = T-MATG.StkMin + Almmmate.StkMin
                    T-MATG.StkMax = T-MATG.StkMax + Almmmate.StkMax
                    T-MATG.StkRep = T-MATG.StkRep + Almmmate.StkRep.
            IF Almmmatg.CanEmp = 0 THEN T-MATG.CanEmp = 1.
            IF Almmmatg.MonVta = 2 THEN T-MATG.CtoTot = T-MATG.CtoTot * T-MATG.TpoCmb.
        END.
    END.
END.

/* Cargamos dias de stock */
FOR EACH T-MATG:
    x-Mensaje:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "CALCULANDO VENTAS: " + T-MATG.codmat.
    /* Valores por defecto */
    ASSIGN
        pDiasMaximo = AlmCfgGn.DiasMaximo
        pDiasMinimo = AlmCfgGn.DiasMinimo
        pDiasUtiles = AlmCfgGn.DiasUtiles.
    /* Buscamos parametros del proveedor */
    FIND FIRST gn-prov WHERE gn-prov.codcia = pv-codcia
        AND gn-prov.codpro = T-MATG.codpr1
        NO-LOCK NO-ERROR.
    IF AVAILABLE gn-prov AND (gn-prov.stkmin > 0 AND gn-prov.stkmax > 0) THEN DO:
        ASSIGN
            pDiasMaximo = gn-prov.stkmax
            pDiasMinimo = gn-prov.stkmin.
    END.
    IF x-CodPro <> "" 
    THEN ASSIGN
            pDiasMinimo = FILL-IN-DiasMinimo
            pDiasMaximo = FILL-IN-DiasMaximo.
    /* Venta Diaria */
    ASSIGN
        T-MATG.Dias   = ( IF T-MATG.StkRep > 0 THEN (T-MATG.StkAct / T-MATG.StkRep) ELSE 0 )
        T-MATG.Minimo = T-MATG.StkRep * pDiasMinimo
        T-MATG.Maximo = T-MATG.StkRep * pDiasMaximo.
    /* Cantidad */
    IF T-MATG.StkAct < T-MATG.Minimo THEN T-MATG.Cantidad = T-MATG.StkAct - T-MATG.Minimo.
    IF T-MATG.StkAct > T-MATG.Maximo THEN T-MATG.Cantidad = T-MATG.StkAct - T-MATG.Maximo.
    T-MATG.Costo = T-MATG.CtoTot * T-MATG.Cantidad.
    IF T-MATG.StkAct < T-MATG.Minimo AND T-MATG.CanEmp > 0 THEN DO:
        T-MATG.Compras = T-MATG.CanEmp * ROUND(T-MATG.Minimo / T-MATG.CanEmp, 0).
        IF T-MATG.Compras <= 0 THEN T-MATG.Compras = T-MATG.CanEmp.
    END.
END.
x-Mensaje:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".

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
  DISPLAY txt-codalm x-CodFam x-desFam x-CodPro x-NomPro FILL-IN-DiasMinimo 
          FILL-IN-DiasMaximo TOGGLE-Resumen x-mensaje 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE txt-codalm BUTTON-1 x-CodFam x-CodPro TOGGLE-Resumen BUTTON-6 BUTTON-7 
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

SESSION:SET-WAIT-STATE('GENERAL').

/* Cargamos el temporal */
RUN Carga-Temporal.

/* Generamos el archivo texto */
DEF VAR x-Archivo AS CHAR NO-UNDO.
DEF VAR x-Llave AS CHAR FORMAT 'x(500)' NO-UNDO.

x-Archivo = SESSION:TEMP-DIRECTORY + STRING(NEXT-VALUE(sec-arc,integral)) + ".txt".
OUTPUT STREAM REPORTE TO VALUE (x-Archivo).
x-Llave = ''.
x-Llave = x-Llave + "Almacen" + CHR(9).
x-Llave = x-Llave + "Codigo" + CHR(9).
x-Llave = x-Llave + "Descripcion" + CHR(9).
x-LLave = x-Llave + "Marca" + CHR(9).
x-Llave = x-Llave + "Familia" + CHR(9).
x-LLave = x-LLave + "Sub-familia" + CHR(9).
x-Llave = x-Llave + "Stock" + CHR(9).
x-Llave = x-Llave + "Unidad" + CHR(9).
x-Llave = x-Llave + "Venta Diaria" + CHR(9).
x-Llave = x-Llave + "Stock minimo" + CHR(9).
x-Llave = x-LLave + "Stock maximo" + CHR(9).
/*x-Llave = x-Llave + "Costo" + CHR(9).*/
x-Llave = x-Llave + "Empaque" + CHR(9).
x-Llave = x-Llave + "Numero de Dias" + CHR(9).
x-Llave = x-Llave + "Minimo" + CHR(9).
x-Llave = x-Llave + "Maximo" + CHR(9).
x-Llave = x-Llave + "Cantidad" + CHR(9).
/*x-Llave = x-Llave + "Costo Saldo" + CHR(9).*/
x-Llave = x-Llave + "Proveedor" + CHR(9).
x-Llave = x-Llave + "Nombre Proveedor" + CHR(9).
x-Llave = x-Llave + "".
PUT STREAM REPORTE x-LLave SKIP.
FOR EACH T-MATG:
    x-Llave = "".
    x-Llave = x-Llave + STRING(T-MATG.codalm, "999") + CHR(9).
    x-Llave = x-Llave + STRING(T-MATG.codmat, "999999") + CHR(9).
    x-Llave = x-Llave + T-MATG.desmat + CHR(9).
    x-Llave = x-Llave + T-MATG.desmar + CHR(9).
    x-LLave = x-LLave + T-MATG.codfam + CHR(9).
    x-Llave = x-LLave + T-MATG.subfam + CHR(9).
    x-LLave = x-Llave + STRING(T-MATG.stkact) + CHR(9).
    x-LLave = x-Llave + T-MATG.undbas + CHR(9).
    x-Llave = x-LLave + STRING(T-MATG.stkrep) + CHR(9).
    x-Llave = x-Llave + STRING(T-MATG.stkmin) + CHR(9).
    x-Llave = x-Llave + STRING(T-MATG.stkmax) + CHR(9).
    /*x-Llave = x-Llave + STRING(T-MATG.ctotot) + CHR(9).*/
    x-Llave = x-Llave + STRING(T-MATG.canemp) + CHR(9).
    x-Llave = x-Llave + STRING(T-MATG.dias) + CHR(9).
    x-Llave = x-Llave + STRING(T-MATG.minimo) + CHR(9).
    x-Llave = x-Llave + STRING(T-MATG.maximo) + CHR(9).
    x-Llave = x-Llave + STRING(T-MATG.cantidad) + CHR(9).
    /*x-Llave = x-Llave + STRING(T-MATG.costo) + CHR(9).*/
    x-Llave = x-Llave + T-MATG.codpr1 + CHR(9).
    FIND gn-prov WHERE gn-prov.codcia = pv-codcia
        AND gn-prov.codpro = T-MATG.codpr1
        NO-LOCK NO-ERROR.
    IF AVAILABLE gn-prov THEN x-Llave = x-Llave + gn-prov.nompro + CHR(9).
    ELSE x-Llave = x-Llave + " " + CHR(9).
    x-Llave = x-Llave + "".
    PUT STREAM REPORTE x-LLave SKIP.
END.            
OUTPUT STREAM REPORTE CLOSE.
/* CARGAMOS EL EXCEL */
RUN lib/filetext-to-excel(x-Archivo, 'Por Almacen', YES).

SESSION:SET-WAIT-STATE('').


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Excel2 W-Win 
PROCEDURE Excel2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

SESSION:SET-WAIT-STATE('GENERAL').

/* Cargamos el temporal */
RUN Carga-Temporal-2.

/* Generamos el archivo texto */
DEF VAR x-Archivo AS CHAR NO-UNDO.
DEF VAR x-Llave AS CHAR FORMAT 'x(500)' NO-UNDO.

x-Archivo = SESSION:TEMP-DIRECTORY + STRING(NEXT-VALUE(sec-arc,integral)) + ".txt".
OUTPUT STREAM REPORTE TO VALUE (x-Archivo).
x-Llave = ''.
x-Llave = x-Llave + "Codigo" + CHR(9).
x-Llave = x-Llave + "Descripcion" + CHR(9).
x-LLave = x-Llave + "Marca" + CHR(9).
x-Llave = x-Llave + "Familia" + CHR(9).
x-LLave = x-LLave + "Sub-familia" + CHR(9).
x-Llave = x-Llave + "Stock" + CHR(9).
x-Llave = x-Llave + "Unidad" + CHR(9).
x-Llave = x-Llave + "Venta Diaria" + CHR(9).
x-Llave = x-Llave + "Stock minimo" + CHR(9).
x-Llave = x-LLave + "Stock maximo" + CHR(9).
/*x-Llave = x-Llave + "Costo" + CHR(9).*/
x-Llave = x-Llave + "Empaque" + CHR(9).
x-Llave = x-Llave + "Numero de Dias" + CHR(9).
x-Llave = x-Llave + "Minimo" + CHR(9).
x-Llave = x-Llave + "Maximo" + CHR(9).
x-Llave = x-Llave + "Cantidad" + CHR(9).
/*x-Llave = x-Llave + "Costo Saldo" + CHR(9).*/
x-Llave = x-Llave + "Compras" + CHR(9).
x-Llave = x-Llave + "Proveedor" + CHR(9).
x-Llave = x-Llave + "Nombre Proveedor" + CHR(9).
x-Llave = x-Llave + "".
PUT STREAM REPORTE x-LLave SKIP.
FOR EACH T-MATG:
    x-Llave = "".
    x-Llave = x-Llave + STRING(T-MATG.codmat, "999999")  + CHR(9).
    x-Llave = x-Llave + T-MATG.desmat + CHR(9).
    x-Llave = x-Llave + T-MATG.desmar + CHR(9).
    x-LLave = x-LLave + T-MATG.codfam + CHR(9).
    x-Llave = x-LLave + T-MATG.subfam + CHR(9).
    x-LLave = x-Llave + STRING(T-MATG.stkact) + CHR(9).
    x-LLave = x-Llave + T-MATG.undbas + CHR(9).
    x-Llave = x-LLave + STRING(T-MATG.stkrep) + CHR(9).
    x-Llave = x-Llave + STRING(T-MATG.stkmin) + CHR(9).
    x-Llave = x-Llave + STRING(T-MATG.stkmax) + CHR(9).
    /*x-Llave = x-Llave + STRING(T-MATG.ctotot) + CHR(9).*/
    x-Llave = x-Llave + STRING(T-MATG.canemp) + CHR(9).
    x-Llave = x-Llave + STRING(T-MATG.dias) + CHR(9).
    x-Llave = x-Llave + STRING(T-MATG.minimo) + CHR(9).
    x-Llave = x-Llave + STRING(T-MATG.maximo) + CHR(9).
    x-Llave = x-Llave + STRING(T-MATG.cantidad) + CHR(9).
    /*x-Llave = x-Llave + STRING(T-MATG.costo) + CHR(9).*/
    x-Llave = x-Llave + STRING(T-MATG.compras) + CHR(9).
    x-Llave = x-Llave + T-MATG.codpr1 + CHR(9).
    FIND gn-prov WHERE gn-prov.codcia = pv-codcia
        AND gn-prov.codpro = T-MATG.codpr1
        NO-LOCK NO-ERROR.
    IF AVAILABLE gn-prov THEN x-Llave = x-Llave + gn-prov.nompro + CHR(9).
    ELSE x-Llave = x-Llave + " " + CHR(9).
    x-Llave = x-Llave + "".
    PUT STREAM REPORTE x-LLave SKIP.
END.            
OUTPUT STREAM REPORTE CLOSE.
/* CARGAMOS EL EXCEL */
RUN lib/filetext-to-excel(x-Archivo, 'Resumido', YES).

SESSION:SET-WAIT-STATE('').

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
  /* Valores por defecto */
  ASSIGN
      FILL-IN-DiasMaximo = AlmCfgGn.DiasMaximo
      FILL-IN-DiasMinimo = AlmCfgGn.DiasMinimo.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE procesa-parametros W-Win 
PROCEDURE procesa-parametros :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

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

