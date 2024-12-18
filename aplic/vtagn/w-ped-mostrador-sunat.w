&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          integral         PROGRESS
*/
&Scoped-define WINDOW-NAME W-Win


/* Temp-Table and Buffer definitions                                    */
DEFINE NEW SHARED TEMP-TABLE ITEM LIKE FacDPedi.
DEFINE NEW SHARED TEMP-TABLE ITEM-3 LIKE FacDPedi.



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
DEF INPUT PARAMETER pCodDoc AS CHAR.
/* Valores: p1,p2,p3
p1: PM = P/M
p2: TpoPed = CO
p3: s-acceso-semaforos: SI,NO,TRU,FALSE,YES
*/

DEF NEW SHARED VAR s-NroSer AS INT.
DEF NEW SHARED VAR s-CodDoc AS CHAR.
DEF NEW SHARED VAR s-tpoped AS CHAR.
DEF NEW SHARED VAR s-CodMon AS INT.
DEF NEW SHARED VAR s-CodCli AS CHAR.
DEF NEW SHARED VAR s-cndvta AS CHAR.
DEF NEW SHARED VAR s-tpocmb AS DEC.
DEF NEW SHARED VAR lh_handle AS HANDLE.
DEF NEW SHARED VAR s-porigv AS DEC.
DEF NEW SHARED VAR s-nrodec AS INT.
DEF NEW SHARED VAR s-flgigv AS LOG.
DEF NEW SHARED VAR S-FLGSIT AS CHAR.
DEF NEW SHARED VAR s-adm-new-record AS CHAR.
DEF NEW SHARED VAR s-nroped AS CHAR.
DEF NEW SHARED VAR s-FmaPgo AS CHAR INIT "000".

DEFINE NEW SHARED VAR s-DiasVtoPed LIKE GN-DIVI.DiasVtoPed.

DEF NEW SHARED TEMP-TABLE T-DPEDI LIKE FacDPedi.

DEF SHARED VAR s-codcia AS INT.
DEF SHARED VAR s-coddiv AS CHAR.
DEF SHARED VAR s-codven AS CHAR.

s-CodDoc = ENTRY(1, pCodDoc).
CASE s-CodDoc:
    WHEN "PM" THEN s-CodDoc = "P/M".
END CASE.
IF NUM-ENTRIES(pCodDoc) > 1 THEN s-TpoPed = ENTRY(2, pCodDoc).
/* Posibles valores para s-TpoPed:
CO : Contado
*/

DEFINE NEW SHARED VARIABLE s-acceso-semaforos AS LOG.
DEFINE VAR x-TpoPed AS CHAR NO-UNDO.
s-acceso-semaforos = NO.
IF NUM-ENTRIES(pCodDoc) > 2 THEN DO:
    x-TpoPed = ENTRY(3, pCodDoc).
    IF LOOKUP(x-TpoPed, 'SI,YES,TRUE,S�') > 0 THEN
        ASSIGN s-acceso-semaforos = YES.
END.
ELSE s-acceso-semaforos = NO.

/* CONTROL DE ALMACENES DE DESCARGA */
DEF NEW SHARED VAR s-CodAlm AS CHAR.

FIND FIRST VtaAlmDiv WHERE Vtaalmdiv.codcia = s-codcia
    AND Vtaalmdiv.coddiv = s-coddiv
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE VtaAlmDiv THEN DO:
    MESSAGE 'NO se han definido los almacenes de ventas para la divisi�n' s-coddiv VIEW-AS ALERT-BOX WARNING.
    RETURN ERROR.
END.

CASE s-TpoPed:
    WHEN "R" THEN DO:
        /* Solo Almacenes de Remate */
        FOR EACH VtaAlmDiv NO-LOCK WHERE Vtaalmdiv.codcia = s-codcia
            AND Vtaalmdiv.coddiv = s-coddiv,
            FIRST Almacen OF Vtaalmdiv NO-LOCK WHERE Almacen.Campo-C[3] = 'Si'
            BY VtaAlmDiv.Orden:
            IF s-CodAlm = "" THEN s-CodAlm = TRIM(VtaAlmDiv.CodAlm).
            ELSE s-CodAlm = s-CodAlm + "," + TRIM(VtaAlmDiv.CodAlm).
        END.
    END.
    OTHERWISE DO:
        /* TODOS los Almacenes, menos los de remate */
        FOR EACH VtaAlmDiv NO-LOCK WHERE Vtaalmdiv.codcia = s-codcia
            AND Vtaalmdiv.coddiv = s-coddiv,
            FIRST Almacen OF Vtaalmdiv NO-LOCK WHERE Almacen.Campo-C[3] <> 'Si'
            BY VtaAlmDiv.Orden:
            IF s-CodAlm = "" THEN s-CodAlm = TRIM(VtaAlmDiv.CodAlm).
            ELSE s-CodAlm = s-CodAlm + "," + TRIM(VtaAlmDiv.CodAlm).
        END.
    END.
END CASE.
IF s-CodAlm = "" THEN DO:
    MESSAGE 'NO se han definido los almacenes de ventas para la divisi�n' s-coddiv VIEW-AS ALERT-BOX WARNING.
    RETURN ERROR.
END.

/* PARAMETROS DE COTIZACION PARA LA DIVISION */
DEF NEW SHARED VAR s-FlgEmpaque LIKE GN-DIVI.FlgEmpaque.
DEF NEW SHARED VAR s-FlgMinVenta LIKE GN-DIVI.FlgMinVenta.
DEF NEW SHARED VAR s-FlgRotacion LIKE GN-DIVI.FlgRotacion.
DEF NEW SHARED VAR s-VentaMayorista LIKE GN-DIVI.VentaMayorista.
DEF NEW SHARED VAR s-FlgTipoVenta LIKE GN-DIVI.FlgPreVta.

FIND gn-divi WHERE gn-divi.codcia = s-codcia
    AND gn-divi.coddiv = s-coddiv
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE gn-divi THEN DO:
    MESSAGE 'Divisi�n' s-coddiv 'NO configurada' VIEW-AS ALERT-BOX WARNING.
    RETURN ERROR.
END.
ASSIGN
    s-FlgEmpaque = GN-DIVI.FlgEmpaque
    s-FlgMinVenta = GN-DIVI.FlgMinVenta
    s-FlgRotacion = GN-DIVI.FlgRotacion
    s-VentaMayorista = GN-DIVI.VentaMayorista
    s-FlgTipoVenta = GN-DIVI.FlgPreVta.

DEF NEW SHARED VAR s-Sunat-Activo AS LOG INIT NO.
s-Sunat-Activo = gn-divi.campo-log[10].

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
&Scoped-Define ENABLED-OBJECTS RECT-24 COMBO-NroSer BTN-Excel 
&Scoped-Define DISPLAYED-OBJECTS COMBO-NroSer 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_b-pedido-cont AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-navico-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updv03 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updv12 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updv95 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_q-pedido AS HANDLE NO-UNDO.
DEFINE VARIABLE h_t-ped-mostrador-unificado AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-ped-mostrador-unificado AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN-Excel 
     IMAGE-UP FILE "img\excel":U
     LABEL "Button 1" 
     SIZE 10 BY 1.35.

DEFINE BUTTON Btn-Grupos-2 
     LABEL "MATRIZ DE VENTAS" 
     SIZE 20 BY .96.

DEFINE VARIABLE COMBO-NroSer AS CHARACTER FORMAT "X(3)":U INITIAL "0" 
     LABEL "Serie" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     DROP-DOWN-LIST
     SIZE 6.72 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-24
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 170 BY 8.88.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     COMBO-NroSer AT ROW 1.27 COL 79.71 WIDGET-ID 4
     BTN-Excel AT ROW 2.35 COL 173 WIDGET-ID 16
     Btn-Grupos-2 AT ROW 25.5 COL 38 WIDGET-ID 20
     "Buscar el n�mero:" VIEW-AS TEXT
          SIZE 13 BY .5 AT ROW 1.54 COL 92 WIDGET-ID 6
     RECT-24 AT ROW 2.35 COL 2 WIDGET-ID 8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 183.43 BY 25.96
         FONT 4 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Design Page: 1
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: ITEM T "NEW SHARED" ? INTEGRAL FacDPedi
      TABLE: ITEM-3 T "NEW SHARED" ? INTEGRAL FacDPedi
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "*** PEDIDOS MOSTRADOR SUNAT ***"
         HEIGHT             = 25.96
         WIDTH              = 183.43
         MAX-HEIGHT         = 31.62
         MAX-WIDTH          = 195.14
         VIRTUAL-HEIGHT     = 31.62
         VIRTUAL-WIDTH      = 195.14
         MAX-BUTTON         = no
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
/* SETTINGS FOR BUTTON Btn-Grupos-2 IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       Btn-Grupos-2:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR COMBO-BOX COMBO-NroSer IN FRAME F-Main
   ALIGN-L                                                              */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* *** PEDIDOS MOSTRADOR SUNAT *** */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* *** PEDIDOS MOSTRADOR SUNAT *** */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-Excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-Excel W-Win
ON CHOOSE OF BTN-Excel IN FRAME F-Main /* Button 1 */
DO:
  RUN Genera-Excel IN h_v-ped-mostrador-unificado.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Grupos-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Grupos-2 W-Win
ON CHOOSE OF Btn-Grupos-2 IN FRAME F-Main /* MATRIZ DE VENTAS */
DO:
    RUN vta2/d-matriz-venta-02.
    RUN Recalcular-Precios IN h_t-ped-mostrador-unificado.
    RUN dispatch IN h_t-ped-mostrador-unificado ('open-query').
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME COMBO-NroSer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL COMBO-NroSer W-Win
ON VALUE-CHANGED OF COMBO-NroSer IN FRAME F-Main /* Serie */
DO:
    ASSIGN COMBO-NroSer.
    s-NroSer = INTEGER(COMBO-NroSer).
    RUN dispatch IN h_q-pedido ('open-query':U).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */
lh_handle = THIS-PROCEDURE.
CASE s-CodDoc:
    WHEN "P/M" THEN W-Win:TITLE = "PEDIDOS MAYORISTAS - SUNAT".
END CASE.
CASE s-TpoPed:
    WHEN "CO" THEN W-Win:TITLE = W-Win:TITLE + " - MOSTRADOR".
END CASE.

/* 07/09/2022: Cambio de t�tulo */
W-Win:TITLE = ">>> PEDIDO MOSTRADOR".
IF AVAILABLE gn-divi THEN DO:
    W-Win:TITLE = W-Win:TITLE + " - " + GN-DIVI.CodDiv + " " + TRIM(GN-DIVI.DesDiv).
    FIND FacTabla WHERE FacTabla.CodCia = s-CodCia AND
        FacTabla.Tabla = 'GRUPO_DIVGG' AND
        FacTabla.Codigo = GN-DIVI.Grupo_Divi_GG NO-LOCK NO-ERROR.
    IF AVAILABLE FacTabla THEN W-Win:TITLE = W-Win:TITLE + " - " + FacTabla.Nombre.
END.
W-Win:TITLE = W-Win:TITLE + " <<<".


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

    WHEN 0 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm-vm/objects/p-updv03.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updv03 ).
       RUN set-position IN h_p-updv03 ( 1.00 , 2.00 ) NO-ERROR.
       RUN set-size IN h_p-updv03 ( 1.42 , 73.57 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'src/adm/objects/p-navico.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_p-navico-2 ).
       RUN set-position IN h_p-navico-2 ( 1.00 , 115.00 ) NO-ERROR.
       RUN set-size IN h_p-navico-2 ( 1.35 , 21.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'vtagn/v-ped-mostrador-unificado.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-ped-mostrador-unificado ).
       RUN set-position IN h_v-ped-mostrador-unificado ( 2.62 , 3.00 ) NO-ERROR.
       /* Size in UIB:  ( 8.35 , 117.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'vta2/qpedidocontadomay.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_q-pedido ).
       RUN set-position IN h_q-pedido ( 1.00 , 105.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.35 , 9.00 ) */

       /* Links to SmartViewer h_v-ped-mostrador-unificado. */
       RUN add-link IN adm-broker-hdl ( h_p-updv03 , 'TableIO':U , h_v-ped-mostrador-unificado ).
       RUN add-link IN adm-broker-hdl ( h_q-pedido , 'Record':U , h_v-ped-mostrador-unificado ).

       /* Links to SmartQuery h_q-pedido. */
       RUN add-link IN adm-broker-hdl ( h_p-navico-2 , 'Navigation':U , h_q-pedido ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-updv03 ,
             COMBO-NroSer:HANDLE IN FRAME F-Main , 'BEFORE':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-navico-2 ,
             h_p-updv03 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-ped-mostrador-unificado ,
             BTN-Excel:HANDLE IN FRAME F-Main , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_q-pedido ,
             Btn-Grupos-2:HANDLE IN FRAME F-Main , 'AFTER':U ).
    END. /* Page 0 */
    WHEN 1 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'vtagn/b-ped-mostrador-sunat.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-pedido-cont ).
       RUN set-position IN h_b-pedido-cont ( 11.23 , 2.00 ) NO-ERROR.
       RUN set-size IN h_b-pedido-cont ( 15.08 , 182.00 ) NO-ERROR.

       /* Links to SmartBrowser h_b-pedido-cont. */
       RUN add-link IN adm-broker-hdl ( h_q-pedido , 'Record':U , h_b-pedido-cont ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-pedido-cont ,
             h_v-ped-mostrador-unificado , 'AFTER':U ).
    END. /* Page 1 */
    WHEN 2 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'vtagn/t-ped-mostrador-sunat.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_t-ped-mostrador-unificado ).
       RUN set-position IN h_t-ped-mostrador-unificado ( 11.23 , 2.00 ) NO-ERROR.
       RUN set-size IN h_t-ped-mostrador-unificado ( 15.62 , 181.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'src/adm-vm/objects/p-updv12.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updv12 ).
       RUN set-position IN h_p-updv12 ( 25.23 , 3.00 ) NO-ERROR.
       RUN set-size IN h_p-updv12 ( 1.42 , 34.14 ) NO-ERROR.

       /* Links to SmartBrowser h_t-ped-mostrador-unificado. */
       RUN add-link IN adm-broker-hdl ( h_p-updv12 , 'TableIO':U , h_t-ped-mostrador-unificado ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_t-ped-mostrador-unificado ,
             h_v-ped-mostrador-unificado , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-updv12 ,
             h_t-ped-mostrador-unificado , 'AFTER':U ).
    END. /* Page 2 */
    WHEN 3 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm-vm/objects/p-updv95.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updv95 ).
       RUN set-position IN h_p-updv95 ( 22.54 , 2.00 ) NO-ERROR.
       RUN set-size IN h_p-updv95 ( 1.54 , 26.14 ) NO-ERROR.

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-updv95 ,
             h_v-ped-mostrador-unificado , 'AFTER':U ).
    END. /* Page 3 */

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
  DISPLAY COMBO-NroSer 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE RECT-24 COMBO-NroSer BTN-Excel 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW W-Win.
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
  DEFINE VARIABLE cListItems AS CHARACTER NO-UNDO.

  FIND FacCfgGn WHERE FacCfgGn.CodCia = s-CodCia NO-LOCK NO-ERROR.
  FOR EACH FacCorre NO-LOCK WHERE 
      FacCorre.CodCia = s-CodCia AND
      FacCorre.CodDoc = s-CodDoc AND
      FacCorre.CodDiv = s-CodDiv AND
      FacCorre.FlgEst = YES:
      IF cListItems = "" THEN cListItems = STRING(FacCorre.NroSer,"999").
      ELSE cListItems = cListItems + "," + STRING(FacCorre.NroSer,"999").
  END.
  DO WITH FRAME {&FRAME-NAME}:
      COMBO-NroSer:LIST-ITEMS = cListItems.
      COMBO-NroSer = ENTRY(1,COMBO-NroSer:LIST-ITEMS).
      s-NroSer = INTEGER(COMBO-NroSer).
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Procesa-Handle W-Win 
PROCEDURE Procesa-Handle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER L-Handle AS CHAR.
CASE L-Handle:
    WHEN "Pagina1"  THEN DO WITH FRAME {&FRAME-NAME}:
         RUN select-page(1).
         ASSIGN
             COMBO-NroSer:SENSITIVE = YES
             Btn-Grupos-2:SENSITIVE = NO
             Btn-Grupos-2:VISIBLE = NO
             Btn-Excel:SENSITIVE = YES.
         RUN dispatch IN h_q-pedido ('enable':U).
      END.
    WHEN "Pagina2"  THEN DO WITH FRAME {&FRAME-NAME}:
         RUN select-page(2).
         ASSIGN
             COMBO-NroSer:SENSITIVE = NO
             Btn-Grupos-2:SENSITIVE = YES
             Btn-Grupos-2:VISIBLE = YES
             Btn-Excel:SENSITIVE    = NO.
         RUN dispatch IN h_q-pedido ('disable':U).
         RUN dispatch IN h_t-ped-mostrador-unificado ('open-query':U).
      END.
    WHEN "Disable-Head" THEN DO:
        RUN dispatch IN h_p-updv03 ('disable':U).
      END.
    WHEN "Enable-Head" THEN DO:
        RUN dispatch IN h_p-updv03 ('enable':U).
      END.
    WHEN "Add-Record" THEN DO:
        RUN dispatch IN h_t-ped-mostrador-unificado ('add-record':U).
      END.
    WHEN "browse" THEN DO:
          IF h_b-pedido-cont <> ? THEN RUN dispatch IN h_b-pedido-cont ('open-query':U).
          IF h_t-ped-mostrador-unificado <> ? THEN RUN dispatch IN h_t-ped-mostrador-unificado ('open-query':U). 
      END.
    WHEN "browse2" THEN DO:
          RUN dispatch IN h_t-ped-mostrador-unificado ('open-query':U). 
      END.
    WHEN "Recalculo" THEN DO WITH FRAME {&FRAME-NAME}:
         RUN Recalcular-Precios IN h_t-ped-mostrador-unificado.
         IF h_t-ped-mostrador-unificado <> ? THEN RUN dispatch IN h_t-ped-mostrador-unificado ('open-query':U). 
      END.
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

