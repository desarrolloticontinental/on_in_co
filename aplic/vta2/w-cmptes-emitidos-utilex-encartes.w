&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
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

DEFINE SHARED VAR s-codcia AS INT.

DEFINE TEMP-TABLE tt-vta-utlx-encarte
    FIELD   coddiv  AS CHAR      FORMAT 'x(6)'   COLUMN-LABEL "Division"
    FIELD   coddoc  AS CHAR      FORMAT 'x(4)'   COLUMN-LABEL "Cod.Doc."
    FIELD   nrodoc  AS CHAR     FORMAT 'x(15)'  COLUMN-LABEL "Nro.Doc."
    FIELD   fchdoc  AS DATE                     COLUMN-LABEL "F.Emision"
    FIELD   codcli  AS CHAR     FORMAT 'x(11)'  COLUMN-LABEL "Cod.Cliente"
    FIELD   nomcli  AS CHAR     FORMAT 'x(80)'  COLUMN-LABEL "Nombre de Cliente"
    FIELD   encarte AS CHAR     FORMAT 'x(25)'  COLUMN-LABEL "Cod.Encarte"
    FIELD   dencarte AS CHAR     FORMAT 'x(80)'  COLUMN-LABEL "Nombre Encarte"
    FIELD   impexo  AS DEC      FORMAT "->>,>>>,>>9.99" COLUMN-LABEL "Impte Exonerado"
    FIELD   impvta  AS DEC      FORMAT "->>,>>>,>>9.99" COLUMN-LABEL "Impte VVenta"
    FIELD   impigv  AS DEC      FORMAT "->>,>>>,>>9.99" COLUMN-LABEL "Impte IGV"
    FIELD   impdto  AS DEC      FORMAT "->>,>>>,>>9.99" COLUMN-LABEL "Impte DSCTO"
    FIELD   imptot  AS DEC      FORMAT "->>,>>>,>>9.99" COLUMN-LABEL "Impte Total"
    FIELD   codmon  AS INT      FORMAT ">99" COLUMN-LABEL "Mone Vta"
    FIELD   tpocmb  AS DEC      FORMAT "->,>>9.9999"    COLUMN-LABEL "Tipo Cambio"
    .

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
&Scoped-Define ENABLED-OBJECTS txtDesde txtHasta BUTTON-1 
&Scoped-Define DISPLAYED-OBJECTS txtDesde txtHasta 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-1 
     LABEL "Procesar" 
     SIZE 15 BY 1.12.

DEFINE VARIABLE txtDesde AS DATE FORMAT "99/99/9999":U INITIAL ? 
     LABEL "Desde" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE txtHasta AS DATE FORMAT "99/99/9999":U 
     LABEL "Hasta" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     txtDesde AT ROW 2.92 COL 12 COLON-ALIGNED WIDGET-ID 2
     txtHasta AT ROW 2.92 COL 36 COLON-ALIGNED WIDGET-ID 4
     BUTTON-1 AT ROW 5.42 COL 46 WIDGET-ID 8
     "Comprobantes emitidos en UTILEX ( Solo ventas con ENCARTES )" VIEW-AS TEXT
          SIZE 61 BY .96 AT ROW 1.04 COL 7.14 WIDGET-ID 6
          BGCOLOR 15 FGCOLOR 9 FONT 10
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 76.29 BY 10.5 WIDGET-ID 100.


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
         TITLE              = "Comprobantes emitidos en UTILEX (x Encartes)"
         HEIGHT             = 6.88
         WIDTH              = 70
         MAX-HEIGHT         = 17
         MAX-WIDTH          = 105.43
         VIRTUAL-HEIGHT     = 17
         VIRTUAL-WIDTH      = 105.43
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "SmartWindowCues" W-Win _INLINE
/* Actions: adecomm/_so-cue.w ? adecomm/_so-cued.p ? adecomm/_so-cuew.p */
/* SmartWindow,ab,49271
Destroy on next read */
/* _UIB-CODE-BLOCK-END */
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
ON END-ERROR OF W-Win /* Comprobantes emitidos en UTILEX (x Encartes) */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Comprobantes emitidos en UTILEX (x Encartes) */
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
ON CHOOSE OF BUTTON-1 IN FRAME F-Main /* Procesar */
DO:
  ASSIGN txtDesde txtHasta.
  IF txtHasta < txtDesde THEN DO:
      MESSAGE "Rango de fechas erradas".
      RETURN NO-APPLY.
  END.

  RUN ue-procesar.
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
  DISPLAY txtDesde txtHasta 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE txtDesde txtHasta BUTTON-1 
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

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  txthasta:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(TODAY - 5,"99/99/9999") .
  txtDesde:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(TODAY,"99/99/9999") .

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ue-procesar W-Win 
PROCEDURE ue-procesar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE VAR lDia AS DATE.
DEFINE VAR lDocs AS CHAR.
DEFINE VAR lSec AS INT.
DEFINE VAR lCodDoc AS CHAR.
DEFINE VAR x-archivo AS CHAR.
DEFINE VAR rpta AS LOG.
DEFINE VAR tcmb AS DEC.

lDocs = "FAC,BOL,TCK,N/C,N/D".


SYSTEM-DIALOG GET-FILE x-Archivo
    FILTERS 'Archivo Excel (*.xlsx)' '*.xlsx'
    ASK-OVERWRITE
    CREATE-TEST-FILE
    DEFAULT-EXTENSION '.xlsx'
    RETURN-TO-START-DIR
    SAVE-AS
    TITLE 'Exportar a Excel'
    UPDATE rpta.
IF rpta = NO OR x-Archivo = '' THEN RETURN.

SESSION:SET-WAIT-STATE('GENERAL').

EMPTY TEMP-TABLE tt-vta-utlx-encarte.

FOR EACH gn-divi WHERE gn-divi.canalventa = 'MIN' NO-LOCK:
    REPEAT lDia = txtDesde TO txtHasta:
        REPEAT lSec = 1 TO NUM-ENTRIES(lDocs,","):
            lCodDOc = ENTRY(lSec,lDocs).
            /**/
            FOR EACH ccbcdocu USE-INDEX llave10 WHERE ccbcdocu.codcia = s-codcia AND
                                            ccbcdocu.coddiv = gn-divi.coddiv AND 
                                            ccbcdocu.fchdoc = lDia AND
                                            ccbcdocu.coddoc = lCodDoc AND 
                                            ccbcdocu.flgest <> "A" NO-LOCK:
                /* Solo venta con ENCARTE */
                IF NUM-ENTRIES(ccbcdocu.libre_c05,"|") > 1 AND CAPS(ENTRY(1,ccbcdocu.libre_c05,"|"))= 'CD' THEN DO:
                    tcmb = IF(ccbcdocu.codmon = 2) THEN  ccbcdocu.tpocmb ELSE 1.

                    FIND FIRST VtaCtabla WHERE VtaCtabla.codcia = s-codcia AND 
                                                VtaCtabla.tabla = 'UTILEX-ENCARTE' AND 
                                                VtaCtabla.llave = ENTRY(2,ccbcdocu.libre_c05,"|")
                                                NO-LOCK NO-ERROR.

                    CREATE tt-vta-utlx-encarte.
                        ASSIGN  tt-vta-utlx-encarte.coddiv = ccbcdocu.coddiv
                                tt-vta-utlx-encarte.coddoc = ccbcdocu.coddoc
                                tt-vta-utlx-encarte.nrodoc = ccbcdocu.nrodoc
                                tt-vta-utlx-encarte.fchdoc = ccbcdocu.fchdoc
                                tt-vta-utlx-encarte.codcli = ccbcdocu.codcli
                                tt-vta-utlx-encarte.nomcli = ccbcdocu.nomcli
                                tt-vta-utlx-encarte.encarte = ENTRY(2,ccbcdocu.libre_c05,"|")
                                tt-vta-utlx-encarte.dencarte = IF(AVAILABLE vtactabla) THEN vtactabla.descripcion ELSE "** ERROR ** (Encarte no EXISTE)"
                                tt-vta-utlx-encarte.impexo = ccbcdocu.impexo * tcmb
                                tt-vta-utlx-encarte.impvta = ccbcdocu.impvta * tcmb
                                tt-vta-utlx-encarte.impigv = ccbcdocu.impigv * tcmb
                                tt-vta-utlx-encarte.impdto = ccbcdocu.impdto * tcmb
                                tt-vta-utlx-encarte.imptot = ccbcdocu.imptot * tcmb
                                tt-vta-utlx-encarte.codmon = ccbcdocu.codmon * tcmb
                                tt-vta-utlx-encarte.tpocmb = ccbcdocu.tpocmb * tcmb.
                END.
            END.

        END.
    END.
END.

DEFINE VAR hProc AS HANDLE NO-UNDO.

RUN lib\Tools-to-excel PERSISTENT SET hProc.

def var c-csv-file as char no-undo.
def var c-xls-file as char no-undo. /* will contain the XLS file path created */

c-xls-file = x-Archivo.

run pi-crea-archivo-csv IN hProc (input  buffer tt-vta-utlx-encarte:handle,
                        /*input  session:temp-directory + "file"*/ c-xls-file,
                        output c-csv-file) .

run pi-crea-archivo-xls  IN hProc (input  buffer tt-vta-utlx-encarte:handle,
                        input  c-csv-file,
                        output c-xls-file) .

DELETE PROCEDURE hProc.

SESSION:SET-WAIT-STATE('').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
