&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          integral         PROGRESS
*/
&Scoped-define WINDOW-NAME W-Win
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
DEF SHARED VAR s-coddiv AS CHAR.
DEF SHARED VAR pv-codcia AS INT.
DEF SHARED VAR cl-codcia AS INT.

DEF STREAM REPORTE.

DEF TEMP-TABLE detalle
    FIELD nroped AS CHAR FORMAT '999999999'
    FIELD fchent AS DATE FORMAT '99/99/9999'
    FIELD codcia AS INT
    FIELD codmat LIKE almmmatg.codmat
    FIELD codpro LIKE gn-prov.codpro
    FIELD codfam LIKE almmmatg.codfam
    FIELD subfam LIKE almmmatg.subfam
    FIELD licencia AS CHAR
    FIELD codcli LIKE faccpedi.codcli
    FIELD codven LIKE faccpedi.codven
    FIELD canped LIKE facdpedi.canped
    FIELD canate LIKE facdpedi.canate
    FIELD implin LIKE facdpedi.implin
    FIELD flgest LIKE faccpedi.flgest
    FIELD coddept LIKE gn-clie.CodDept 
    FIELD codprov LIKE gn-clie.CodProv 
    FIELD coddist LIKE gn-clie.CodDist
    FIELD pesmat AS DEC FORMAT ">>,>>9.9999" LABEL "Peso Unitario"
    FIELD volume AS DEC FORMAT ">>,>>9.9999" LABEL "Vol.Unitario"
    FIELD listaprecio AS CHAR FORMAT "x(10)" LABEL "Lista de Precio"
    INDEX llave01 AS PRIMARY codcia nroped codmat
    INDEX llave02 codcia nroped codcli codfam subfam
    INDEX llave03 codcia nroped codcli
    INDEX llave04 codcia nroped codven codcli codmat.

DEF VAR x-CodDept AS CHAR NO-UNDO.
DEF VAR x-CodProv AS CHAR NO-UNDO.
DEF VAR x-CodDist AS CHAR NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME BROWSE-3

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES GN-DIVI

/* Definitions for BROWSE BROWSE-3                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-3 GN-DIVI.CodDiv GN-DIVI.DesDiv 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-3 
&Scoped-define QUERY-STRING-BROWSE-3 FOR EACH GN-DIVI ~
      WHERE GN-DIVI.CodCia = s-codcia NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-3 OPEN QUERY BROWSE-3 FOR EACH GN-DIVI ~
      WHERE GN-DIVI.CodCia = s-codcia NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-3 GN-DIVI
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-3 GN-DIVI


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-BROWSE-3}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN-Fecha-1 FILL-IN-Fecha-2 RADIO-SET-1 ~
BROWSE-3 BUTTON-2 BtnDone 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-Fecha-1 FILL-IN-Fecha-2 ~
RADIO-SET-1 FILL-IN-Mensaje 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnDone DEFAULT 
     IMAGE-UP FILE "img/exit.ico":U
     LABEL "&Salir" 
     SIZE 8 BY 1.62
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-2 
     IMAGE-UP FILE "img/excel.bmp":U
     LABEL "Button 2" 
     SIZE 8 BY 1.62.

DEFINE VARIABLE FILL-IN-Fecha-1 AS DATE FORMAT "99/99/9999":U 
     LABEL "Desde" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-Fecha-2 AS DATE FORMAT "99/99/9999":U 
     LABEL "Hasta" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-Mensaje AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 60 BY 1 NO-UNDO.

DEFINE VARIABLE RADIO-SET-1 AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Resumen por Producto", 1,
"Resumen por Cliente vs Lineas", 2,
"Resumen por Cliente", 3,
"Resumen por Producto vs Proveedor", 4,
"Resumen por Vendedor vs Cliente vs Producto", 5,
"Resumen por Proveedor", 6
     SIZE 45 BY 5.12 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-3 FOR 
      GN-DIVI SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-3 W-Win _STRUCTURED
  QUERY BROWSE-3 NO-LOCK DISPLAY
      GN-DIVI.CodDiv COLUMN-LABEL "Divisi�n" FORMAT "x(5)":U
      GN-DIVI.DesDiv FORMAT "X(40)":U WIDTH 72.29
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 83 BY 12.5 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     FILL-IN-Fecha-1 AT ROW 1.19 COL 9 COLON-ALIGNED WIDGET-ID 4
     FILL-IN-Fecha-2 AT ROW 1.19 COL 30 COLON-ALIGNED WIDGET-ID 6
     RADIO-SET-1 AT ROW 2.35 COL 11 NO-LABEL WIDGET-ID 12
     BROWSE-3 AT ROW 7.92 COL 4 WIDGET-ID 200
     BUTTON-2 AT ROW 1.38 COL 74 WIDGET-ID 8
     BtnDone AT ROW 1.38 COL 82 WIDGET-ID 10
     FILL-IN-Mensaje AT ROW 20.62 COL 2 COLON-ALIGNED NO-LABEL WIDGET-ID 2
     "Tipo:" VIEW-AS TEXT
          SIZE 5 BY .62 AT ROW 2.54 COL 6 WIDGET-ID 20
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 92.29 BY 20.88 WIDGET-ID 100.


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
         TITLE              = "ESTADISTICAS DE COTIZACIONES"
         HEIGHT             = 20.88
         WIDTH              = 92.29
         MAX-HEIGHT         = 21.38
         MAX-WIDTH          = 92.29
         VIRTUAL-HEIGHT     = 21.38
         VIRTUAL-WIDTH      = 92.29
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
   FRAME-NAME Custom                                                    */
/* BROWSE-TAB BROWSE-3 RADIO-SET-1 F-Main */
/* SETTINGS FOR FILL-IN FILL-IN-Mensaje IN FRAME F-Main
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-3
/* Query rebuild information for BROWSE BROWSE-3
     _TblList          = "INTEGRAL.GN-DIVI"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "INTEGRAL.GN-DIVI.CodCia = s-codcia"
     _FldNameList[1]   > INTEGRAL.GN-DIVI.CodDiv
"GN-DIVI.CodDiv" "Divisi�n" "x(5)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > INTEGRAL.GN-DIVI.DesDiv
"GN-DIVI.DesDiv" ? ? "character" ? ? ? ? ? ? no ? no no "72.29" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BROWSE-3 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* ESTADISTICAS DE COTIZACIONES */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* ESTADISTICAS DE COTIZACIONES */
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
ON CHOOSE OF BtnDone IN FRAME F-Main /* Salir */
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


&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 W-Win
ON CHOOSE OF BUTTON-2 IN FRAME F-Main /* Button 2 */
DO:
  ASSIGN
      FILL-IN-Fecha-1 FILL-IN-Fecha-2 RADIO-SET-1.
  IF {&BROWSE-NAME}:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME} = 0 THEN DO:
      MESSAGE 'Seleccione por lo menos una divisi�n' VIEW-AS ALERT-BOX WARNING.
      RETURN NO-APPLY.
  END.
  RUN Excel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-3
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
DEF VAR x-Cuenta AS INT NO-UNDO.
DEF VAR x-Divisiones AS CHAR NO-UNDO.
DEF VAR k AS INT NO-UNDO.

DEFINE VAR lDivErr AS CHAR.
lDivErr = '00015,10060,20015'.

DO k = {&BROWSE-NAME}:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME} TO 1 BY -1:
    IF {&BROWSE-NAME}:FETCH-SELECTED-ROW(k) IN FRAME {&FRAME-NAME} THEN DO:
        x-Divisiones = x-Divisiones + (IF x-Divisiones = '' THEN '' ELSE ',') + gn-divi.coddiv.
    END.
END.

EMPTY TEMP-TABLE Detalle.
x-Cuenta = 0.
ESTADISTICAS:
FOR EACH gn-divi NO-LOCK WHERE gn-divi.codcia = s-codcia
    AND LOOKUP(gn-divi.coddiv, x-Divisiones) > 0,
    EACH faccpedi NO-LOCK WHERE faccpedi.codcia = s-codcia
    AND coddoc = 'cot'
    AND faccpedi.coddiv = gn-divi.coddiv
    AND fchped >= FILL-IN-Fecha-1
    AND fchped <= FILL-IN-Fecha-2,
    FIRST gn-clie NO-LOCK WHERE gn-clie.codcia = cl-codcia
    AND gn-clie.codcli = faccpedi.codcli:

    /* 04Nov2014 - Ic */
    IF (faccpedi.fchped >= 09/23/2014 AND faccpedi.fchped < 11/01/2014) THEN DO:
        IF LOOKUP(faccpedi.coddiv,lDivErr) > 0  THEN DO:
            IF CAPS(faccpedi.usuario) = 'ADMIN' OR CAPS(SUBSTRING(faccpedi.codcli,1,3))="SYS" THEN NEXT.
        END.
    END.
    /* Fin 04Nov2014 - Ic */
    IF Faccpedi.flgest = "A" OR Faccpedi.flgest = "W" THEN NEXT.
    FILL-IN-Mensaje:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "** RESUMIENDO ** " +
        "COTIZACION " + faccpedi.nroped.
    FOR EACH facdpedi OF Faccpedi NO-LOCK, FIRST Almmmatg OF Facdpedi NO-LOCK:
        CASE RADIO-SET-1:
            WHEN 1 THEN DO:
                FIND detalle WHERE detalle.codcia = facdpedi.codcia
                    AND detalle.nroped = facdpedi.nroped
                    AND detalle.codmat = facdpedi.codmat
                    NO-ERROR.
                IF NOT AVAILABLE detalle THEN DO:
                    CREATE detalle.
                    ASSIGN
                        detalle.codcia = facdpedi.codcia
                        detalle.nroped = facdpedi.nroped
                        detalle.fchent = faccpedi.fchent
                        detalle.codmat = facdpedi.codmat
                        detalle.flgest = faccpedi.flgest
                        detalle.pesmat = almmmatg.pesmat
                        detalle.volume = almmmatg.libre_d02.
                    FIND almtabla WHERE almtabla.Tabla = "LC" AND
                         almtabla.Codigo = Almmmatg.Licencia[1] NO-LOCK NO-ERROR.
                    IF AVAILABLE almtabla THEN detalle.licencia = almtabla.Nombre.
                    x-Cuenta = x-Cuenta + 1.
                END.
                ASSIGN
                    detalle.canped = detalle.canped + facdpedi.canped * facdpedi.factor
                    detalle.implin = detalle.implin + facdpedi.implin.
            END.
            WHEN 2 THEN DO:
                FIND detalle WHERE detalle.codcia = facdpedi.codcia
                    AND detalle.nroped = facdpedi.nroped
                    AND detalle.codcli = faccpedi.codcli
                    AND detalle.codfam = almmmatg.codfam
                    AND detalle.subfam = almmmatg.subfam
                    NO-ERROR.
                IF NOT AVAILABLE detalle THEN DO:
                    CREATE detalle.
                    ASSIGN
                        detalle.codcia = facdpedi.codcia
                        detalle.nroped = facdpedi.nroped
                        detalle.fchent = faccpedi.fchent
                        detalle.codcli = faccpedi.codcli
                        detalle.codfam = almmmatg.codfam
                        detalle.subfam = almmmatg.subfam
                        detalle.flgest = faccpedi.flgest.
                    x-Cuenta = x-Cuenta + 1.
                END.
                ASSIGN
                    detalle.canped = detalle.canped + facdpedi.canped * facdpedi.factor
                    detalle.implin = detalle.implin + facdpedi.implin.
            END.
            WHEN 3 THEN DO:
                FIND detalle WHERE detalle.codcia = facdpedi.codcia
                    AND detalle.nroped = facdpedi.nroped
                    AND detalle.codcli = faccpedi.codcli
                    NO-ERROR.
                IF NOT AVAILABLE detalle THEN DO:
                    CREATE detalle.
                    ASSIGN
                        detalle.codcia = facdpedi.codcia
                        detalle.nroped = facdpedi.nroped
                        detalle.fchent = faccpedi.fchent
                        detalle.codcli = faccpedi.codcli
                        detalle.flgest = faccpedi.flgest.
                    x-Cuenta = x-Cuenta + 1.
                END.
                ASSIGN
                    detalle.implin = detalle.implin + facdpedi.implin.
            END.
        WHEN 4 THEN DO:
            FIND detalle WHERE detalle.codcia = facdpedi.codcia
                AND detalle.nroped = facdpedi.nroped
                AND detalle.codcli = faccpedi.codcli
                AND detalle.codmat = facdpedi.codmat
                NO-ERROR.
            IF NOT AVAILABLE detalle THEN DO:
                CREATE detalle.
                ASSIGN
                    detalle.codcia = facdpedi.codcia
                    detalle.nroped = facdpedi.nroped
                    detalle.fchent = faccpedi.fchent
                    detalle.codcli = faccpedi.codcli
                    detalle.codmat = facdpedi.codmat
                    detalle.codfam = almmmatg.codfam
                    detalle.subfam = almmmatg.subfam
                    detalle.flgest = faccpedi.flgest
                    detalle.pesmat = almmmatg.pesmat
                    detalle.volume = almmmatg.libre_d02.
                FIND almtabla WHERE almtabla.Tabla = "LC" AND
                     almtabla.Codigo = Almmmatg.Licencia[1] NO-LOCK NO-ERROR.
                IF AVAILABLE almtabla THEN detalle.licencia = almtabla.Nombre.
                x-Cuenta = x-Cuenta + 1.
            END.
            ASSIGN
                detalle.canped = detalle.canped + facdpedi.canped * facdpedi.factor
                detalle.implin = detalle.implin + facdpedi.implin.
        END.
        WHEN 5 THEN DO:
            FIND detalle WHERE detalle.codcia = facdpedi.codcia
                AND detalle.nroped = facdpedi.nroped
                AND detalle.codven = faccpedi.codven
                AND detalle.codcli = faccpedi.codcli
                AND detalle.codmat = facdpedi.codmat
                NO-ERROR.
            IF NOT AVAILABLE detalle THEN DO:
                CREATE detalle.
                ASSIGN
                    detalle.codcia = facdpedi.codcia
                    detalle.nroped = faccpedi.nroped
                    detalle.fchent = faccpedi.fchent
                    detalle.codven = faccpedi.codven
                    detalle.codcli = faccpedi.codcli
                    detalle.codmat = facdpedi.codmat
                    detalle.codfam = almmmatg.codfam
                    detalle.subfam = almmmatg.subfam
                    detalle.flgest = faccpedi.flgest
                    detalle.coddept = gn-clie.CodDept 
                    detalle.codprov = gn-clie.CodProv 
                    detalle.coddist = gn-clie.CodDist
                    detalle.pesmat = almmmatg.pesmat
                    detalle.volume = almmmatg.libre_d02
                    detalle.listaprecio = faccpedi.libre_c01.
                x-Cuenta = x-Cuenta + 1.
            END.
            ASSIGN
                detalle.canped = detalle.canped + facdpedi.canped * facdpedi.factor
                detalle.canate = detalle.canate + facdpedi.canate * facdpedi.factor
                detalle.implin = detalle.implin + facdpedi.implin.
        END.
        WHEN 6 THEN DO:
            FIND detalle WHERE detalle.codcia = facdpedi.codcia
                AND detalle.nroped = facdpedi.nroped
                AND detalle.codpro = almmmatg.codpr1
                NO-ERROR.
            IF NOT AVAILABLE detalle THEN DO:
                CREATE detalle.
                ASSIGN
                    detalle.codcia = facdpedi.codcia
                    detalle.nroped = facdpedi.nroped
                    detalle.fchent = faccpedi.fchent
                    detalle.codpro = almmmatg.codpr1
                    detalle.flgest = faccpedi.flgest.
                x-Cuenta = x-Cuenta + 1.
            END.
            ASSIGN
                detalle.canped = detalle.canped + facdpedi.canped * facdpedi.factor
                detalle.implin = detalle.implin + facdpedi.implin.
        END.
        END CASE.
    END.
END.
RETURN "OK".

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
  DISPLAY FILL-IN-Fecha-1 FILL-IN-Fecha-2 RADIO-SET-1 FILL-IN-Mensaje 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE FILL-IN-Fecha-1 FILL-IN-Fecha-2 RADIO-SET-1 BROWSE-3 BUTTON-2 BtnDone 
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
DEF VAR x-Llave AS CHAR FORMAT 'x(800)' NO-UNDO.
DEF VAR x-Titulo AS CHAR FORMAT 'x(800)' NO-UNDO.
DEF VAR x-NomPro LIKE gn-prov.nompro NO-UNDO.
DEF VAR x-NomVen LIKE gn-ven.nomven NO-UNDO.
DEF VAR x-Archivo AS CHAR NO-UNDO.

RUN Carga-Temporal.
IF RETURN-VALUE = "ADM-ERROR" THEN RETURN.

x-Archivo = SESSION:TEMP-DIRECTORY + STRING(NEXT-VALUE(sec-arc,integral)) + ".txt".

FILL-IN-Mensaje:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "** GENERANDO EL EXCEL **".
OUTPUT STREAM REPORTE TO VALUE (x-Archivo).
CASE RADIO-SET-1:
    WHEN 1 THEN DO:
        x-Titulo = 'Cotizacion|Entrega|Producto|Marca|Linea|Sublinea|Licencia|Proveedor|Cantidad|Unidad|Importe|Estado|Peso Unitario|Volumen Unitario'.
        x-Titulo = REPLACE(x-Titulo, '|', CHR(9) ).
        PUT STREAM REPORTE x-Titulo SKIP.
        FOR EACH detalle NO-LOCK,
            FIRST almmmatg OF detalle NO-LOCK,
            FIRST almtfami OF almmmatg NO-LOCK,
            FIRST almsfami OF almmmatg NO-LOCK:
            x-nompro = 'NN'.
            FIND gn-prov WHERE gn-prov.codcia = pv-codcia
                AND gn-prov.codpro = almmmatg.codpr1
                NO-LOCK NO-ERROR.
            IF AVAILABLE gn-prov THEN x-nompro = gn-prov.nompro.

            x-Llave = detalle.nroped + '|' + STRING(detalle.fchent, '99/99/9999') + '|' +
                detalle.codmat + ' ' + almmmatg.desmat + '|' +
                almmmatg.desmar + '|' +
                almmmatg.codfam + ' ' + Almtfami.desfam + '|' +
                almmmatg.subfam + ' ' + AlmSFami.dessub + '|' +
                detalle.licencia + '|' +
                almmmatg.codpr1 + ' ' + x-nompro + '|' +
                STRING (detalle.canped, '>>>,>>>,>>9.99') + '|' +
                Almmmatg.UndBas + '|' +
                STRING (detalle.implin, '>>>,>>>,>>9.99') + '|' +
                STRING (detalle.pesmat, '>>>,>>>,>>9.9999') + '|' +
                STRING (detalle.volume, '>>>,>>>,>>9.9999').
            x-Llave = REPLACE ( x-Llave, '|', CHR(9) ).
            PUT STREAM REPORTE x-LLave SKIP.
        END.
    END.
    WHEN 2 THEN DO:
        x-Titulo = 'Cotizacion|Entrega|Cliente|Linea|Sublinea|Importe|'.
        x-Titulo = REPLACE(x-Titulo, '|', CHR(9) ).
        PUT STREAM REPORTE x-Titulo SKIP.
        FOR EACH detalle NO-LOCK,
            FIRST gn-clie NO-LOCK WHERE gn-clie.codcia = cl-codcia
            AND gn-clie.codcli = detalle.codcli,
            FIRST almtfami OF detalle NO-LOCK,
            FIRST almsfami OF detalle NO-LOCK:
            x-nompro = 'NN'.
            FIND gn-prov WHERE gn-prov.codcia = pv-codcia
                AND gn-prov.codpro = almmmatg.codpr1
                NO-LOCK NO-ERROR.
            IF AVAILABLE gn-prov THEN x-nompro = gn-prov.nompro.

            x-Llave = detalle.nroped + '|' + STRING(detalle.fchent, '99/99/9999') + '|' +
                detalle.codcli + ' ' + gn-clie.nomcli + '|' +
                detalle.codfam + ' ' + Almtfami.desfam + '|' +
                detalle.subfam + ' ' + AlmSFami.dessub + '|' +
                STRING (detalle.implin, '>>>,>>>,>>9.99') + '|'.
            x-Llave = REPLACE ( x-Llave, '|', CHR(9) ).
            PUT STREAM REPORTE x-LLave SKIP.
        END.
    END.
    WHEN 3 THEN DO:
        x-Titulo = 'Cotizacion|Entrega|Cliente|Importe|'.
        x-Titulo = REPLACE(x-Titulo, '|', CHR(9) ).
        PUT STREAM REPORTE x-Titulo SKIP.
        FOR EACH detalle NO-LOCK,
            FIRST gn-clie NO-LOCK WHERE gn-clie.codcia = cl-codcia
            AND gn-clie.codcli = detalle.codcli:
            x-Llave = detalle.nroped + '|' + STRING(detalle.fchent, '99/99/9999') + '|' +
                detalle.codcli + ' ' + gn-clie.nomcli + '|' +
                STRING (detalle.implin, '>>>,>>>,>>9.99') + '|'.
            x-Llave = REPLACE ( x-Llave, '|', CHR(9) ).
            PUT STREAM REPORTE x-LLave SKIP.
        END.
    END.
/*     WHEN 4 THEN DO:                                                                                                */
/*         x-Titulo = 'Cotizacion|Cliente|Producto|Marca|Linea|Sublinea|Licencia|Proveedor|Cantidad|Unidad|Importe|'. */
/*         x-Titulo = REPLACE(x-Titulo, '|', CHR(9) ).                                                                */
/*         PUT STREAM REPORTE x-Titulo SKIP.                                                                          */
/*         FOR EACH detalle NO-LOCK,                                                                                  */
/*             FIRST gn-clie NO-LOCK WHERE gn-clie.codcia = cl-codcia                                                 */
/*             AND gn-clie.codcli = detalle.codcli,                                                                   */
/*             FIRST almmmatg OF detalle NO-LOCK,                                                                     */
/*             FIRST almtfami OF detalle NO-LOCK,                                                                     */
/*             FIRST almsfami OF detalle NO-LOCK:                                                                     */
/*             x-nompro = 'NN'.                                                                                       */
/*             FIND gn-prov WHERE gn-prov.codcia = pv-codcia                                                          */
/*                 AND gn-prov.codpro = almmmatg.codpr1                                                               */
/*                 NO-LOCK NO-ERROR.                                                                                  */
/*             IF AVAILABLE gn-prov THEN x-nompro = gn-prov.nompro.                                                   */
/*                                                                                                                    */
/*             x-Llave = detalle.nroped + '|' +                                                                       */
/*                 detalle.codcli + ' ' + gn-clie.nomcli + '|' +                                                      */
/*                 detalle.codmat + ' ' + Almmmatg.desmat + '|' +                                                     */
/*                 almmmatg.desmar + '|' +                                                                            */
/*                 detalle.codfam + ' ' + Almtfami.desfam + '|' +                                                     */
/*                 detalle.subfam + ' ' + AlmSFami.dessub + '|' +                                                     */
/*                 detalle.licencia + '|' +                                                                           */
/*                 x-nompro + '|' +                                                                                   */
/*                 STRING (detalle.canped, '>>>,>>>,>>9.99') + '|' +                                                  */
/*                 Almmmatg.UndBas + '|' +                                                                            */
/*                 STRING (detalle.implin, '>>>,>>>,>>9.99') + '|'.                                                   */
/*             x-Llave = REPLACE ( x-Llave, '|', CHR(9) ).                                                            */
/*             PUT STREAM REPORTE x-LLave SKIP.                                                                       */
/*         END.                                                                                                       */
/*     END.                                                                                                           */

    WHEN 4 THEN DO:
        x-Titulo = 'Cotizacion|Entrega|Producto|Marca|Linea|Sublinea|Licencia|Proveedor|Cantidad|Unidad|Importe|Peso Unitario|Volume Unitario|'.
        x-Titulo = REPLACE(x-Titulo, '|', CHR(9) ).
        PUT STREAM REPORTE x-Titulo SKIP.
        FOR EACH detalle NO-LOCK,
            FIRST almmmatg OF detalle NO-LOCK,
            FIRST almtfami OF detalle NO-LOCK,
            FIRST almsfami OF detalle NO-LOCK:
            x-nompro = 'NN'.
            FIND gn-prov WHERE gn-prov.codcia = pv-codcia
                AND gn-prov.codpro = almmmatg.codpr1
                NO-LOCK NO-ERROR.
            IF AVAILABLE gn-prov THEN x-nompro = gn-prov.nompro.

            x-Llave = detalle.nroped + '|' + STRING(detalle.fchent, '99/99/9999') + '|' +
                detalle.codmat + ' ' + Almmmatg.desmat + '|' + 
                almmmatg.desmar + '|' +
                detalle.codfam + ' ' + Almtfami.desfam + '|' +
                detalle.subfam + ' ' + AlmSFami.dessub + '|' +
                detalle.licencia + '|' +
                x-nompro + '|' +
                STRING (detalle.canped, '>>>,>>>,>>9.99') + '|' +
                Almmmatg.UndBas + '|' +
                STRING (detalle.implin, '>>>,>>>,>>9.99') + '|' +
                STRING (detalle.pesmat, '>>>,>>>,>>9.9999') + '|' +
                STRING (detalle.volume, '>>>,>>>,>>9.9999') + '|' .
            x-Llave = REPLACE ( x-Llave, '|', CHR(9) ).
            PUT STREAM REPORTE x-LLave SKIP.
        END.
    END.
    WHEN 5 THEN DO:
        x-Titulo = 'Cotizacion|Entrega|Vendedor|Cliente|Producto|Marca|Linea|Sublinea|Cantidad|Atendido|Unidad|Importe|Estado|' + 
            'Departamento|Provincia|Distrito|Peso Unitario|Volume Unitario|Lista de PRecio|'.
        x-Titulo = REPLACE(x-Titulo, '|', CHR(9) ).
        PUT STREAM REPORTE x-Titulo SKIP.
        FOR EACH detalle NO-LOCK,
            FIRST gn-clie NO-LOCK WHERE gn-clie.codcia = cl-codcia
            AND gn-clie.codcli = detalle.codcli,
            FIRST almmmatg OF detalle NO-LOCK,
            FIRST almtfami OF detalle NO-LOCK,
            FIRST almsfami OF detalle NO-LOCK:
            ASSIGN
                x-nompro = 'NN'
                x-nomven = 'NN'
                x-coddept = detalle.coddept
                x-codprov = detalle.codprov
                x-coddist = detalle.coddist.
            FIND gn-prov WHERE gn-prov.codcia = pv-codcia
                AND gn-prov.codpro = almmmatg.codpr1
                NO-LOCK NO-ERROR.
            IF AVAILABLE gn-prov THEN x-nompro = gn-prov.nompro.
            FIND gn-ven OF detalle NO-LOCK NO-ERROR.
            IF AVAILABLE gn-ven THEN x-nomven = gn-ven.nomven.
            FIND TabDepto WHERE TabDepto.CodDepto = detalle.coddept NO-LOCK NO-ERROR.
            IF AVAILABLE TabDepto THEN x-coddept = TRIM(x-coddept) + ' ' + TabDepto.NomDepto.
            FIND TabProvi WHERE TabProvi.CodDepto = detalle.coddept
                AND TabProvi.CodProvi = detalle.codprov NO-LOCK NO-ERROR.
            IF AVAILABLE TabProvi THEN x-codprov = TRIM(x-codprov) + ' ' + TabProvi.NomProvi.
            FIND TabDistr WHERE TabDistr.CodDepto = detalle.coddept
                AND TabDistr.CodProvi = detalle.codprov
                AND TabDistr.CodDistr = detalle.coddist NO-LOCK NO-ERROR.
            IF AVAILABLE TabDistr THEN x-coddist = TRIM(x-coddist) + ' ' + TabDistr.NomDistr.

            x-Llave = detalle.nroped + '|' + STRING(detalle.fchent, '99/99/9999') + '|' + 
                detalle.codven + ' ' + x-nomven + '|' + 
                detalle.codcli + ' ' + gn-clie.nomcli + '|' +
                detalle.codmat + ' ' + Almmmatg.desmat + '|' + 
                almmmatg.desmar + '|' +
                detalle.codfam + ' ' + Almtfami.desfam + '|' +
                detalle.subfam + ' ' + AlmSFami.dessub + '|' +
                STRING (detalle.canped, '>>>,>>>,>>9.99') + '|' +
                STRING (detalle.canate, '->>,>>>,>>9.99') + '|' +
                Almmmatg.UndBas + '|' +
                STRING (detalle.implin, '>>>,>>>,>>9.99') + '|' +
                STRING(detalle.flgest, 'X') + '|' +
                x-coddept + '|' +
                x-codprov + '|' +
                x-coddist + '|' + 
                STRING (detalle.pesmat, '>>>,>>>,>>9.99') + '|' + 
                STRING (detalle.volume, '>>>,>>>,>>9.99') + '|' +
                detalle.listaprecio.
            x-Llave = REPLACE ( x-Llave, '|', CHR(9) ).
            PUT STREAM REPORTE x-LLave SKIP.
        END.
    END.
    WHEN 6 THEN DO:
        x-Titulo = 'Cotizacion|Entrega|Proveedor|Importe|'.
        x-Titulo = REPLACE(x-Titulo, '|', CHR(9) ).
        PUT STREAM REPORTE x-Titulo SKIP.
        FOR EACH detalle NO-LOCK:
            x-nompro = 'NN'.
            FIND gn-prov WHERE gn-prov.codcia = pv-codcia
                AND gn-prov.codpro = detalle.codpro
                NO-LOCK NO-ERROR.
            IF AVAILABLE gn-prov THEN x-nompro = gn-prov.nompro.
            x-Llave = detalle.nroped + '|' + STRING(detalle.fchent, '99/99/9999') + '|' +
                detalle.codpro + ' ' + x-nompro + '|' +
                STRING (detalle.implin, '>>>,>>>,>>9.99') + '|'.
            x-Llave = REPLACE ( x-Llave, '|', CHR(9) ).
            PUT STREAM REPORTE x-LLave SKIP.
        END.
    END.
END CASE.
OUTPUT STREAM REPORTE CLOSE.

/* CARGAMOS EL EXCEL */
RUN lib/filetext-to-excel(x-Archivo, 'Estadistica', YES).
FILL-IN-Mensaje:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "** FIN DEL PROCESO **".

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
  ASSIGN
      FILL-IN-Fecha-1 = TODAY
      FILL-IN-Fecha-2 = TODAY.

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

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "GN-DIVI"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

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
