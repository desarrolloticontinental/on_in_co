&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          integral         PROGRESS
*/
&Scoped-define WINDOW-NAME W-Win


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE E-MATG NO-UNDO LIKE Almmmatg.
DEFINE NEW SHARED TEMP-TABLE T-MATG LIKE Almmmatg.



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

DEFINE VARIABLE RADIO-SET-1 AS INTEGER NO-UNDO.
DEFINE VARIABLE RADIO-SET-2 AS INTEGER NO-UNDO.

DEF VAR pMensaje AS CHAR NO-UNDO.

DEFINE BUFFER b-VtaListaMay FOR VtaListaMay.

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
&Scoped-Define ENABLED-OBJECTS RECT-3 BtnDone COMBO-BOX-Division BUTTON-1 ~
BUTTON-2 
&Scoped-Define DISPLAYED-OBJECTS COMBO-BOX-Division 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_butilexdctoprom-02 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vutilexdctoxvol-02 AS HANDLE NO-UNDO.

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
     LABEL "ELIMINAR DESCUENTOS" 
     SIZE 25 BY 1.12
     BGCOLOR 14 FGCOLOR 0 .

DEFINE VARIABLE COMBO-BOX-Division AS CHARACTER FORMAT "X(256)":U 
     LABEL "Divisi�n" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 67 BY 1
     BGCOLOR 11 FGCOLOR 0  NO-UNDO.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 141 BY 1.92
     BGCOLOR 12 FGCOLOR 14 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     BtnDone AT ROW 1.19 COL 135 WIDGET-ID 6
     COMBO-BOX-Division AT ROW 1.38 COL 12 COLON-ALIGNED WIDGET-ID 32
     BUTTON-1 AT ROW 1.38 COL 82 WIDGET-ID 2
     BUTTON-2 AT ROW 1.38 COL 107 WIDGET-ID 4
     RECT-3 AT ROW 1 COL 1 WIDGET-ID 30
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 141.29 BY 21.42 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Design Page: 3
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: E-MATG T "?" NO-UNDO INTEGRAL Almmmatg
      TABLE: T-MATG T "NEW SHARED" ? INTEGRAL Almmmatg
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "DESCUENTOS POR VOLUMEN"
         HEIGHT             = 21.42
         WIDTH              = 141.29
         MAX-HEIGHT         = 21.42
         MAX-WIDTH          = 142.29
         VIRTUAL-HEIGHT     = 21.42
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
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* DESCUENTOS POR VOLUMEN */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* DESCUENTOS POR VOLUMEN */
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
    IF OKpressed = FALSE THEN RETURN.

    CREATE "Excel.Application" chExcelApplication.
    chWorkbook = chExcelApplication:Workbooks:OPEN(FILL-IN-Archivo).
    chWorkSheet = chExcelApplication:Sheets:ITEM(1).

    SESSION:SET-WAIT-STATE('GENERAL').
    pMensaje = ''.
    RUN Carga-Temporal.
    SESSION:SET-WAIT-STATE('').

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

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 W-Win
ON CHOOSE OF BUTTON-2 IN FRAME F-Main /* ELIMINAR DESCUENTOS */
DO:
   RUN Grabar-Descuentos.
   ASSIGN
       BUTTON-1:SENSITIVE IN FRAME {&FRAME-NAME} = YES
       BUTTON-2:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
   /*RUN Excel-de-Errores.*/
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

    WHEN 0 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'aplic/vta2/butilexdctoprom-02.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_butilexdctoprom-02 ).
       RUN set-position IN h_butilexdctoprom-02 ( 2.92 , 1.00 ) NO-ERROR.
       RUN set-size IN h_butilexdctoprom-02 ( 19.04 , 86.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'vutilexdctoxvol-02-eliminar.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_vutilexdctoxvol-02 ).
       RUN set-position IN h_vutilexdctoxvol-02 ( 2.92 , 88.00 ) NO-ERROR.
       /* Size in UIB:  ( 10.96 , 42.00 ) */

       /* Links to SmartViewer h_vutilexdctoxvol-02. */
       RUN add-link IN adm-broker-hdl ( h_butilexdctoprom-02 , 'Record':U , h_vutilexdctoxvol-02 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_butilexdctoprom-02 ,
             BUTTON-2:HANDLE IN FRAME F-Main , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_vutilexdctoxvol-02 ,
             h_butilexdctoprom-02 , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.

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

DEF VAR cNombreLista AS CHAR NO-UNDO.
DEF VAR k         AS INT NO-UNDO.
DEF VAR x-DtoVolR AS DEC NO-UNDO.
DEF VAR x-DtoVolD AS DEC NO-UNDO.

/* CHEQUEAMOS LA INTEGRIDAD DEL ARCHIVO EXCEL */
ASSIGN
    t-Column = 0
    t-Row = 1.     /* Saltamos 1ra linea */

cValue = chWorkSheet:Cells(1,1):VALUE.
IF cValue = "" OR cValue = ? THEN DO:
    pMensaje = 'Formato del archivo Excel errado #1'.
    RETURN.
END.

IF NOT (cValue BEGINS "CONTINENTAL - DESCUENTOS POR VOLUMEN - ELIMINAR"
        OR cValue BEGINS "UTILEX - DESCUENTOS POR VOLUMEN - ELIMINAR"
        OR cValue BEGINS COMBO-BOX-Division + " - DESCUENTOS POR VOLUMEN - ELIMINAR"
        OR cValue BEGINS "CONTRATO MARCO - DESCUENTOS POR VOLUMEN - ELIMINAR")
    THEN DO:
    pMensaje = 'Formato del archivo Excel errado #2'.
    RETURN.
END.
cNombreLista = cValue.

ASSIGN
    t-Row    = t-Row + 1
    t-column = t-column + 1.
cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
IF cValue = "" OR cValue = ? OR cValue <> "CODIGO" THEN DO:
    pMensaje = 'Formato del archivo Excel errado #3'.
    RETURN.
END.
t-column = t-column + 1.
cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
IF cValue = "" OR cValue = ? OR cValue <> "DESCRIPCION" THEN DO:
    pMensaje = 'Formato del archivo Excel errado #4'.
    RETURN.
END.

CASE TRUE:
    WHEN cNombreLista BEGINS "CONTINENTAL - DESCUENTOS POR VOLUMEN - ELIMINAR" THEN RADIO-SET-1 = 1.
    WHEN cNombreLista BEGINS "UTILEX - DESCUENTOS POR VOLUMEN - ELIMINAR"      THEN RADIO-SET-1 = 2.
    WHEN cNombreLista BEGINS COMBO-BOX-Division + " - DESCUENTOS POR VOLUMEN - ELIMINAR" THEN RADIO-SET-1 = 3.
    WHEN cNombreLista BEGINS "CONTRATO MARCO - DESCUENTOS POR VOLUMEN - ELIMINAR" THEN RADIO-SET-1 = 4.
    OTHERWISE RETURN.
END CASE.
/*
CASE TRUE:
    WHEN INDEX(cNombreLista, "- PORCENTAJES") > 0 THEN RADIO-SET-2 = 1.
    WHEN INDEX(cNombreLista, "- IMPORTES") > 0 THEN RADIO-SET-2 = 2.
    OTHERWISE RETURN.
END CASE.
*/
RUN Pinta-Titulo IN h_butilexdctoprom-02 ( INPUT cNombreLista /* CHARACTER */).

/* Cargamos temporal */
EMPTY TEMP-TABLE T-MATG.
ASSIGN
    t-Column = 0
    t-Row = 2.     /* Saltamos el encabezado de los campos */
REPEAT:
    ASSIGN
        t-column = 0
        t-Row    = t-Row + 1.
    t-column = t-column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    IF cValue = "" OR cValue = ? THEN LEAVE.    /* FIN DE DATOS */ 

    CREATE T-MATG.
    ASSIGN
        T-MATG.codcia = s-codcia
        T-MATG.codmat = cValue.

    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        T-MATG.desmat = cValue.
    
    t-Column = t-Column + 6.
    /* Cargamos Descuentos por Volumen */
    k = 1.
    /*
    DO iCountLine = 1 TO 5:
        t-Column = t-Column + 1.
        cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
        ASSIGN
            x-DtoVolR = DECIMAL(cValue)
            NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DO:
            pMensaje = "Error en la linea " + TRIM(STRING(t-Row, '>>>9')) + ":"  + CHR(10) 
                + "Rango".
            LEAVE.
        END.

        t-Column = t-Column + 1.
        cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
        ASSIGN
            x-DtoVolD = DECIMAL(cValue)
            NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DO:
            pMensaje = "Error en la linea " + TRIM(STRING(t-Row, '>>>9')) + ":"  + CHR(10) 
                + "Descuento".
            LEAVE.
        END.

        IF x-DtoVolR = ? OR x-DtoVolR = 0 OR x-DtoVolR < 0
            OR x-DtoVolD = ? OR x-DtoVolD = 0 OR x-DtoVolD < 0 THEN NEXT.
        ASSIGN
            T-MATG.DtoVolR[k]  = x-DtoVolR
            T-MATG.DtoVolD[k]  = x-DtoVolD.
        k = k + 1.
    END.
    */
END.
/* EN CASO DE ERROR */
IF pMensaje <> "" THEN DO:
    EMPTY TEMP-TABLE T-MATG.
    RETURN.
END.
/* Cargamos precios actuales */
FOR EACH T-MATG:
    FIND Almmmatg WHERE Almmmatg.codcia = s-codcia
        AND Almmmatg.codmat = T-MATG.codmat
        NO-LOCK.
    FIND VtaListaMinGn WHERE VtaListaMinGn.codcia = s-codcia
        AND VtaListaMinGn.codmat = T-MATG.codmat
        NO-LOCK NO-ERROR.
    FIND VtaListaMay WHERE VtaListaMay.codcia = s-codcia
        AND VtaListaMay.codmat = T-MATG.codmat
        AND VtaListaMay.coddiv = COMBO-BOX-Division
        NO-LOCK NO-ERROR.
    FIND Almmmatp OF Almmmatg WHERE NO-LOCK NO-ERROR.
    ASSIGN
        T-MATG.CtoTot = Almmmatg.CtoTot
        T-MATG.MonVta = Almmmatg.MonVta
        T-MATG.TpoCmb = Almmmatg.TpoCmb.
    IF RADIO-SET-1 = 4 THEN T-MATG.CtoTot =  Almmmatg.CtoTotMarco.
    CASE RADIO-SET-1:
        WHEN 1 THEN DO:
            ASSIGN
                T-MATG.Chr__01 = Almmmatg.Chr__01 
                T-MATG.PreOfi = Almmmatg.PreOfi 
                T-MATG.Prevta[1] = Almmmatg.PreVta[1].
        END.
        WHEN 2 THEN DO:
            ASSIGN
                T-MATG.Chr__01 = VtaListaMinGn.Chr__01 
                T-MATG.PreOfi = VtaListaMinGn.PreOfi 
                T-MATG.Prevta[1] = VtaListaMinGn.PreOfi.
        END.
        WHEN 3 THEN DO:
            IF NOT AVAILABLE VtaListaMay THEN DO:
                DELETE T-MATG.
                NEXT.
            END.
            ASSIGN
                T-MATG.Chr__01 = VtaListaMay.Chr__01 
                T-MATG.PreOfi = VtaListaMay.PreOfi 
                T-MATG.Prevta[1] = VtaListaMay.PreOfi.
        END.
        WHEN 4 THEN DO:
            IF NOT AVAILABLE Almmmatp THEN DO:
                DELETE T-MATG.
                NEXT.
            END.
            ASSIGN
                T-MATG.Chr__01 = Almmmatp.Chr__01 
                T-MATG.PreOfi = Almmmatp.PreOfi 
                T-MATG.Prevta[1] = Almmmatp.PreOfi.
        END.
    END CASE.
    CASE RADIO-SET-1:
        WHEN 4 THEN DO:
            IF T-MATG.MonVta = 2 THEN
                ASSIGN
                T-MATG.CtoTot = T-MATG.CtoTot * Almmmatg.TpoCmb.
        END.
        OTHERWISE DO:
            IF T-MATG.MonVta = 2 THEN
                ASSIGN
                T-MATG.CtoTot = T-MATG.CtoTot * Almmmatg.TpoCmb
                T-MATG.PreOfi = T-MATG.PreOfi * Almmmatg.TpoCmb
                T-MATG.Prevta[1] = T-MATG.Prevta[1] * Almmmatg.TpoCmb.
        END.
    END CASE.

    /* Corregimos de acuerdo al m�todo de c�lculo */
    IF RADIO-SET-2 = 2 THEN DO:
        DO iValue = 1 TO 5:
            IF T-MATG.DtoVolD[iValue] > 0 THEN
            T-MATG.DtoVolD[iValue] = ROUND ( ( 1 -  T-MATG.DtoVolD[iValue] / T-MATG.PreVta[1]  ) * 100, 4 ).
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
FIND FIRST T-MATG NO-LOCK NO-ERROR.
IF NOT AVAILABLE T-MATG THEN MESSAGE 'No hay registros que procesar' VIEW-AS ALERT-BOX ERROR.

ASSIGN
    BUTTON-1:SENSITIVE IN FRAME {&FRAME-NAME} = NO.

RUN dispatch IN h_butilexdctoprom-02 ('open-query':U).

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
  DISPLAY COMBO-BOX-Division 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE RECT-3 BtnDone COMBO-BOX-Division BUTTON-1 BUTTON-2 
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

DEF VAR pError AS CHAR NO-UNDO.
DEF VAR pError1 AS CHAR NO-UNDO.
DEF VAR X-MARGEN AS DEC NO-UNDO.
DEF VAR X-LIMITE AS DEC NO-UNDO.
DEF VAR x-PreUni AS DEC NO-UNDO.
DEF VAR iCuentaRangos AS INT NO-UNDO.

DEF VAR k AS INT NO-UNDO.
DEF VAR j AS INT NO-UNDO.

MESSAGE 'Procedemos a ELIMINAR los descuentos por volumen ?'
    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
    UPDATE rpta AS LOG.
IF rpta = NO THEN RETURN.

pError = "OK".
pError1 = "".

SESSION:SET-WAIT-STATE("GENERAL").

CASE RADIO-SET-1:
    WHEN 1 THEN DO:
        /* {vta2/wutilexdctoxvol-02.i &Tabla=Almmmatg} */        
        GRABAR_DATOS:
        DO TRANSACTION ON ERROR UNDO GRABAR_DATOS, LEAVE GRABAR_DATOS:
            FOR EACH T-MATG NO-LOCK:
                FIND FIRST almmmatg OF T-MATG EXCLUSIVE-LOCK NO-ERROR.
                IF LOCKED almmmatg THEN DO:
                   pError =  'La tabla Almmmatg esta siendo usado por otro usuario'.
                   pError1 = "Actualizacion Abortada".
                    UNDO GRABAR_DATOS, LEAVE GRABAR_DATOS.
                END.
                /* Limpiamos informacion */
                IF AVAILABLE almmmatg THEN DO:
                    DO k = 1 TO 10:
                        ASSIGN
                            almmmatg.DtoVolR[k] = 0
                            almmmatg.DtoVolD[k] = 0 NO-ERROR.
                        IF ERROR-STATUS:ERROR = YES THEN DO:
                            pError =  'Error al eliminar el descuento por Volumen'.
                            pError1 = "Actualizacion Abortada".
                            UNDO GRABAR_DATOS, LEAVE GRABAR_DATOS.
                        END.
                    END.
                END.
            END.
        END.

        RELEASE Almmmatg.
        EMPTY TEMP-TABLE T-MATG.
        
    END.

    WHEN 2 THEN DO:
        /*{vta2/wutilexdctoxvol-02.i &Tabla=Vtalistamingn}*/
        GRABAR_DATOS:
        DO TRANSACTION ON ERROR UNDO GRABAR_DATOS, LEAVE GRABAR_DATOS:
            FOR EACH T-MATG NO-LOCK :
                FIND FIRST Vtalistamingn OF T-MATG EXCLUSIVE-LOCK NO-ERROR.
                IF LOCKED Vtalistamingn THEN DO:
                   pError =  'La tabla Vtalistamingn esta siendo usado por otro usuario'.
                   pError1 = "Actualizacion Abortada".
                    UNDO GRABAR_DATOS, LEAVE GRABAR_DATOS.
                END.
                /* Limpiamos informacion */
                IF AVAILABLE Vtalistamingn THEN DO:
                    DO k = 1 TO 10:
                        ASSIGN
                            Vtalistamingn.DtoVolR[k] = 0
                            Vtalistamingn.DtoVolD[k] = 0 NO-ERROR.
                        IF ERROR-STATUS:ERROR = YES THEN DO:
                            pError =  'Error al eliminar el descuento por Volumen'.
                            pError1 = "Actualizacion Abortada".
                            UNDO GRABAR_DATOS, LEAVE GRABAR_DATOS.
                        END.
                    END.
                END.
            END.
        END.

        RELEASE Vtalistamingn.
        EMPTY TEMP-TABLE T-MATG.

    END.

    WHEN 3 THEN DO:
        GRABAR_DATOS:
        DO TRANSACTION ON ERROR UNDO GRABAR_DATOS, LEAVE GRABAR_DATOS:
            FOR EACH T-MATG NO-LOCK :
                FIND FIRST VtaListaMay OF T-MATG WHERE VtaListaMay.coddiv = COMBO-BOX-Division EXCLUSIVE-LOCK NO-ERROR.
                IF LOCKED VtaListaMay THEN DO:
                   pError =  'La tabla VtaListaMay esta siendo usado por otro usuario'.
                   pError1 = "Actualizacion Abortada".
                    UNDO GRABAR_DATOS, LEAVE GRABAR_DATOS.
                END.
                IF AVAILABLE VtaListaMay THEN DO:
                    /* Limpiamos informacion */
                    DO k = 1 TO 10:
                        ASSIGN
                            VtaListaMay.DtoVolR[k] = 0
                            VtaListaMay.DtoVolD[k] = 0 NO-ERROR.
                        IF ERROR-STATUS:ERROR = YES THEN DO:
                            pError =  'Error al eliminar el descuento por Volumen'.
                            pError1 = "Actualizacion Abortada".
                            UNDO GRABAR_DATOS, LEAVE GRABAR_DATOS.
                        END.
                    END.
                END.
            END.
        END.

        RELEASE VtaListaMay.
        EMPTY TEMP-TABLE T-MATG.

    END.

    WHEN 4 THEN DO:
        /*{vta2/wutilexdctoxvol-02.i &Tabla=Almmmatp}*/
        GRABAR_DATOS:
        DO TRANSACTION ON ERROR UNDO GRABAR_DATOS, LEAVE GRABAR_DATOS:
            FOR EACH T-MATG NO-LOCK :
                FIND FIRST Almmmatp OF T-MATG WHERE VtaListaMay.coddiv = COMBO-BOX-Division EXCLUSIVE-LOCK NO-ERROR.
                IF LOCKED Almmmatp THEN DO:
                   pError =  'La tabla Almmmatp esta siendo usado por otro usuario'.
                   pError1 = "Actualizacion Abortada".
                    UNDO GRABAR_DATOS, LEAVE GRABAR_DATOS.
                END.
                IF AVAILABLE Almmmatp THEN DO:
                    /* Limpiamos informacion */
                    DO k = 1 TO 10:
                        ASSIGN
                            Almmmatp.DtoVolR[k] = 0
                            Almmmatp.DtoVolD[k] = 0 NO-ERROR.
                        IF ERROR-STATUS:ERROR = YES THEN DO:
                            pError =  'Error al eliminar el descuento por Volumen'.
                            pError1 = "Actualizacion Abortada".
                            UNDO GRABAR_DATOS, LEAVE GRABAR_DATOS.
                        END.
                    END.
                END.
            END.
        END.

        RELEASE Almmmatp.
        EMPTY TEMP-TABLE T-MATG.

    END.
END CASE.

SESSION:SET-WAIT-STATE("").

MESSAGE pError SKIP
    pError1
    VIEW-AS ALERT-BOX WARNING.


RUN dispatch IN h_butilexdctoprom-02 ('open-query':U).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Grabar-Descuentos-Utilex W-Win 
PROCEDURE Grabar-Descuentos-Utilex :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR k AS INT NO-UNDO.
DEF VAR j AS INT NO-UNDO.

MESSAGE 'Procedemos a grabar los decuentos promocionales?'
    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
    UPDATE rpta AS LOG.
IF rpta = NO THEN RETURN.
FOR EACH T-MATG ON STOP UNDO, RETURN:
    FIND VtaListaMinGn OF T-MATG EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE VtaListaMinGn THEN UNDO, RETURN.
    /* Limpiamos informacion */
    DO k = 1 TO 10:
        ASSIGN
            VtaListaMinGn.PromDivi[k] = ""
            VtaListaMinGn.PromDto[k] = 0
            VtaListaMinGn.PromFchD[k] = ?
            VtaListaMinGn.PromFchH[k] = ?.
    END.
    /* Cargamos informacion */
    j = 0.
    DO k = 1 TO 10:
        IF T-MATG.PromDto[k] <> 0 THEN DO:
            j = j + 1.
            ASSIGN
                VtaListaMinGn.PromDivi[j] = T-MATG.PromDiv[k]
                VtaListaMinGn.PromDto[j] = T-MATG.PromDto[k]
                VtaListaMinGn.PromFchD[j] = T-MATG.PromFchD[k]
                VtaListaMinGn.PromFchH[j] = T-MATG.PromFchH[k].
        END.
    END.
END.
EMPTY TEMP-TABLE T-MATG.
RUN dispatch IN h_butilexdctoprom-02 ('open-query':U).

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

