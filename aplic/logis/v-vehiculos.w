&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          integral         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
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

  Description: from VIEWER.W - Template for SmartViewer Objects

  Input Parameters:
      <none>

  Output Parameters:
      <none>

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
DEF SHARED VAR s-user-id AS CHAR.

DEF VAR pGlosa AS CHAR NO-UNDO.

DEF STREAM REPORTE.

DEF SHARED VAR s-Create-Record AS LOG.
DEF SHARED VAR s-Write-Record AS LOG.
DEF SHARED VAR s-Delete-Record AS LOG.
DEF SHARED VAR s-Confirm-Record AS LOG.

/* DEF VAR s-Create-Record    AS LOG INIT YES. */
/* DEF VAR s-Write-Record     AS LOG INIT YES. */
/* DEF VAR s-Delete-Record    AS LOG INIT YES. */
/* DEF VAR s-Confirm-Record   AS LOG INIT YES. */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES gn-vehic
&Scoped-define FIRST-EXTERNAL-TABLE gn-vehic


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR gn-vehic.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS gn-vehic.Libre_c05 gn-vehic.Tipo ~
gn-vehic.placa gn-vehic.Marca gn-vehic.CodPro gn-vehic.Carga ~
gn-vehic.Libre_d01 gn-vehic.Volumen gn-vehic.Libre_c04 gn-vehic.Libre_c02 ~
gn-vehic.NroEstibadores gn-vehic.VtoRevTecnica gn-vehic.VtoExtintor ~
gn-vehic.VtoSoat gn-vehic.Libre_d02 
&Scoped-define ENABLED-TABLES gn-vehic
&Scoped-define FIRST-ENABLED-TABLE gn-vehic
&Scoped-Define ENABLED-OBJECTS RECT-8 RECT-9 
&Scoped-Define DISPLAYED-FIELDS gn-vehic.Libre_c05 gn-vehic.Tipo ~
gn-vehic.placa gn-vehic.Marca gn-vehic.Libre_c03 gn-vehic.CodPro ~
gn-vehic.Carga gn-vehic.Libre_d01 gn-vehic.Volumen gn-vehic.Libre_c04 ~
gn-vehic.Libre_c02 gn-vehic.NroEstibadores gn-vehic.VtoRevTecnica ~
gn-vehic.VtoExtintor gn-vehic.VtoSoat gn-vehic.Libre_d02 ~
gn-vehic.UsrModificacion gn-vehic.UsrAnulacion gn-vehic.FchModificacion ~
gn-vehic.FchAnulacion gn-vehic.HoraAnulacion gn-vehic.HoraModificacion 
&Scoped-define DISPLAYED-TABLES gn-vehic
&Scoped-define FIRST-DISPLAYED-TABLE gn-vehic
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-NomPro 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "",
     Keys-Supplied = ""':U).
/**************************
</EXECUTING-CODE> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE FILL-IN-NomPro AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 50 BY .81 NO-UNDO.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 30 BY 2.96.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 31 BY 2.96.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     gn-vehic.Libre_c05 AT ROW 1.12 COL 87.72 COLON-ALIGNED WIDGET-ID 32
          LABEL "Activo" FORMAT "x(60)"
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEMS "SI","NO" 
          DROP-DOWN-LIST
          SIZE 9 BY 1
     gn-vehic.Tipo AT ROW 1.19 COL 51.43 COLON-ALIGNED WIDGET-ID 72
          LABEL "Tarjeta unica Circulacion" FORMAT "x(25)"
          VIEW-AS FILL-IN 
          SIZE 20 BY .81
     gn-vehic.placa AT ROW 1.27 COL 22 COLON-ALIGNED WIDGET-ID 18
          VIEW-AS FILL-IN 
          SIZE 10 BY .81
     gn-vehic.Marca AT ROW 2.08 COL 22 COLON-ALIGNED WIDGET-ID 14
          VIEW-AS FILL-IN 
          SIZE 20 BY .81
     gn-vehic.Libre_c03 AT ROW 2.08 COL 69 COLON-ALIGNED WIDGET-ID 54 PASSWORD-FIELD 
          LABEL "Barras" FORMAT "x(15)"
          VIEW-AS FILL-IN 
          SIZE 22 BY .81
          BGCOLOR 14 FGCOLOR 0 
     gn-vehic.CodPro AT ROW 2.88 COL 22 COLON-ALIGNED WIDGET-ID 4
          LABEL "Proveedor"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
     FILL-IN-NomPro AT ROW 2.88 COL 33 COLON-ALIGNED NO-LABEL WIDGET-ID 36
     gn-vehic.Carga AT ROW 3.69 COL 22 COLON-ALIGNED WIDGET-ID 2
          LABEL "Carga Maxima" FORMAT ">>>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 10 BY .81
     gn-vehic.Libre_d01 AT ROW 3.69 COL 50 COLON-ALIGNED WIDGET-ID 12
          LABEL "Capacidad M�nima kg" FORMAT ">>>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 10 BY .81
     gn-vehic.Volumen AT ROW 4.5 COL 22 COLON-ALIGNED WIDGET-ID 70
          LABEL "Volumen (m3)" FORMAT ">,>>>,>>9.9999"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
     gn-vehic.Libre_c04 AT ROW 5.27 COL 48.72 COLON-ALIGNED WIDGET-ID 52
          LABEL "Nro. Registro del MTC" FORMAT "x(25)"
          VIEW-AS FILL-IN 
          SIZE 24.86 BY .81
     gn-vehic.Libre_c02 AT ROW 5.31 COL 22 COLON-ALIGNED WIDGET-ID 30
          LABEL "Tonelaje" FORMAT "x(60)"
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEMS "1TN","2TN","5TN","10TN","15TN","20TN","30TN" 
          DROP-DOWN-LIST
          SIZE 9 BY 1
     gn-vehic.NroEstibadores AT ROW 6.38 COL 22 COLON-ALIGNED WIDGET-ID 34
          LABEL "Nro. M�ximo de Estibadores" FORMAT "->,>>>,>>9"
          VIEW-AS COMBO-BOX INNER-LINES 7
          LIST-ITEMS "1","2","3","4","5","6","7" 
          DROP-DOWN-LIST
          SIZE 9 BY 1
     gn-vehic.VtoRevTecnica AT ROW 7.19 COL 22 COLON-ALIGNED WIDGET-ID 24
          LABEL "Vcto. Revisi�nTecnica"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
     gn-vehic.VtoExtintor AT ROW 7.19 COL 48.86 COLON-ALIGNED WIDGET-ID 22
          LABEL "Vencimiento extintor"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
     gn-vehic.VtoSoat AT ROW 7.19 COL 75 COLON-ALIGNED WIDGET-ID 26
          LABEL "Vencimiento soat"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
     gn-vehic.Libre_d02 AT ROW 8.12 COL 24.14 NO-LABEL WIDGET-ID 66
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Si", 1,
"No", 0
          SIZE 10 BY .81
     gn-vehic.UsrModificacion AT ROW 9.88 COL 39 COLON-ALIGNED WIDGET-ID 20
          LABEL "Usuario"
          VIEW-AS FILL-IN 
          SIZE 7.14 BY .81
     gn-vehic.UsrAnulacion AT ROW 9.88 COL 70 COLON-ALIGNED WIDGET-ID 50
          LABEL "Usuario"
          VIEW-AS FILL-IN 
          SIZE 7.14 BY .81
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 4 WIDGET-ID 100.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     gn-vehic.FchModificacion AT ROW 10.69 COL 39 COLON-ALIGNED WIDGET-ID 6
          LABEL "Fecha"
          VIEW-AS FILL-IN 
          SIZE 10 BY .81
     gn-vehic.FchAnulacion AT ROW 10.69 COL 70 COLON-ALIGNED WIDGET-ID 46
          LABEL "Fecha"
          VIEW-AS FILL-IN 
          SIZE 10 BY .81
     gn-vehic.HoraAnulacion AT ROW 11.5 COL 70 COLON-ALIGNED WIDGET-ID 48
          LABEL "Hora"
          VIEW-AS FILL-IN 
          SIZE 7.14 BY .81
     gn-vehic.HoraModificacion AT ROW 11.54 COL 39 COLON-ALIGNED WIDGET-ID 28
          LABEL "Hora"
          VIEW-AS FILL-IN 
          SIZE 7.14 BY .81
     "(Si el vehiculo es moto, carro de 4 puertas o de hasta 8 pasajeros)" VIEW-AS TEXT
          SIZE 55 BY .5 AT ROW 8.27 COL 34.29 WIDGET-ID 64
          FGCOLOR 4 FONT 4
     "Ultima Desactivaci�n" VIEW-AS TEXT
          SIZE 15 BY .5 AT ROW 9.35 COL 56 WIDGET-ID 44
          BGCOLOR 9 FGCOLOR 15 
     "Ultima Modificaci�n" VIEW-AS TEXT
          SIZE 14 BY .5 AT ROW 9.35 COL 25 WIDGET-ID 42
          BGCOLOR 9 FGCOLOR 15 
     "(Si el tonelaje es mayor a 2TN)" VIEW-AS TEXT
          SIZE 26.43 BY .5 AT ROW 4.73 COL 50.14 WIDGET-ID 56
          FGCOLOR 9 FONT 6
     "�Es categoria M1 o L?" VIEW-AS TEXT
          SIZE 19 BY .5 AT ROW 8.27 COL 4.86 WIDGET-ID 62
          FGCOLOR 9 FONT 6
     RECT-8 AT ROW 9.62 COL 24 WIDGET-ID 38
     RECT-9 AT ROW 9.62 COL 55 WIDGET-ID 40
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 4 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: INTEGRAL.gn-vehic
   Allow: Basic,DB-Fields
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY COMPILE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW V-table-Win ASSIGN
         HEIGHT             = 11.88
         WIDTH              = 103.72.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm-vm/method/vmviewer.i}
{src/adm/method/viewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN gn-vehic.Carga IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN gn-vehic.CodPro IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN gn-vehic.FchAnulacion IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN gn-vehic.FchModificacion IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN FILL-IN-NomPro IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN gn-vehic.HoraAnulacion IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN gn-vehic.HoraModificacion IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR COMBO-BOX gn-vehic.Libre_c02 IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN gn-vehic.Libre_c03 IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN gn-vehic.Libre_c04 IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR COMBO-BOX gn-vehic.Libre_c05 IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN gn-vehic.Libre_d01 IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR COMBO-BOX gn-vehic.NroEstibadores IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN gn-vehic.Tipo IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN gn-vehic.UsrAnulacion IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN gn-vehic.UsrModificacion IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN gn-vehic.Volumen IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN gn-vehic.VtoExtintor IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN gn-vehic.VtoRevTecnica IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN gn-vehic.VtoSoat IN FRAME F-Main
   EXP-LABEL                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME gn-vehic.CodPro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gn-vehic.CodPro V-table-Win
ON LEAVE OF gn-vehic.CodPro IN FRAME F-Main /* Proveedor */
DO:
  FIND gn-prov WHERE gn-prov.codcia = pv-codcia AND
      gn-prov.codpro = SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
  IF AVAILABLE gn-prov THEN DISPLAY gn-prov.NomPro @ FILL-IN-NomPro WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME gn-vehic.placa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gn-vehic.placa V-table-Win
ON LEAVE OF gn-vehic.placa IN FRAME F-Main /* No de Placa */
DO:
  SELF:SCREEN-VALUE = CAPS(SELF:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         
  
  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available V-table-Win  _ADM-ROW-AVAILABLE
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

  /* Create a list of all the tables that we need to get.            */
  {src/adm/template/row-list.i "gn-vehic"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "gn-vehic"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Barras V-table-Win 
PROCEDURE Barras :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

IF NOT AVAILABLE gn-vehic THEN RETURN.
IF gn-vehic.Libre_c05 = "NO" THEN RETURN.

DEFINE VAR x-Copias AS INT. 
DEFINE VAR lNada AS CHAR.
DEF VAR rpta AS LOG NO-UNDO.

SYSTEM-DIALOG PRINTER-SETUP NUM-COPIES x-Copias UPDATE rpta.
IF rpta = NO THEN RETURN.


lnada = gn-vehic.placa.
x-Copias = 1.

OUTPUT STREAM REPORTE TO PRINTER.
PUT STREAM REPORTE '^XA^LH000,012'                 SKIP.   /* Inicio de formato */
PUT STREAM REPORTE '^FO150,15'                     SKIP.   /* Coordenadas de origen campo1 */
PUT STREAM REPORTE '^ADN,30,15'                    SKIP.
PUT STREAM REPORTE '^FD'.
PUT STREAM REPORTE gn-vehic.Placa  FORMAT 'x(15)'            SKIP.
PUT STREAM REPORTE '^FS'                           SKIP.   /* Fin de Campo1 */
PUT STREAM REPORTE '^FO60,50'                      SKIP.   /* Coordenadas de origen barras */

PUT STREAM REPORTE '^BCN,100,N,N,N'            SKIP.   /* Codigo 128 */
PUT STREAM REPORTE '^FD'.
PUT STREAM REPORTE gn-vehic.Libre_c03 FORMAT 'x(20)'   SKIP.

PUT STREAM REPORTE '^FS'                       SKIP.   /* Fin de Campo2 */
PUT STREAM REPORTE '^PQ' + TRIM(STRING(x-Copias)) SKIP.  /* Cantidad a imprimir */
PUT STREAM REPORTE '^PR' + '6'                   SKIP.   /* Velocidad de impresion Pulg/seg */
PUT STREAM REPORTE '^XZ'                         SKIP.   /* Fin de formato */
OUTPUT STREAM REPORTE CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI V-table-Win  _DEFAULT-DISABLE
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
  HIDE FRAME F-Main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Generar-Barras V-table-Win 
PROCEDURE Generar-Barras :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF BUFFER B-VEHIC FOR gn-vehic.

IF NOT AVAILABLE gn-vehic OR gn-vehic.Libre_c05 = "NO" THEN DO:
    MESSAGE 'Acceso denegado' VIEW-AS ALERT-BOX WARNING.
    RETURN.
END.

DEF VAR tcPassword AS CHAR.
REPEAT:
    RUN lib/_gen_password (INPUT 15, OUTPUT tcPassword).
    IF NOT CAN-FIND(FIRST gn-vehic WHERE gn-vehic.CodCia = s-codcia AND
                    gn-vehic.Libre_c03 = tcPassword NO-LOCK)
        THEN LEAVE.
END.

{lib/lock-genericov3.i ~
    &Tabla="B-VEHIC" ~
    &Alcance="FIRST" ~
    &Condicion="ROWID(B-VEHIC) = ROWID(gn-vehic)" ~
    &Bloqueo="EXCLUSIVE-LOCK NO-ERROR" ~
    &Accion="RETRY" ~
    &Mensaje="YES" ~
    &TipoError="UNDO, RETURN ERROR"}
ASSIGN
    B-VEHIC.Libre_c03 = tcPassword.
RELEASE B-VEHIC.
RUN dispatch IN THIS-PROCEDURE ('display-fields':U).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-add-record V-table-Win 
PROCEDURE local-add-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  IF s-Create-Record = NO THEN DO:
      MESSAGE 'Usted no est� autorizado para agregar el registro' VIEW-AS ALERT-BOX INFORMATION.
      RETURN 'ADM-ERROR'.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  gn-vehic.Libre_c05:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "NO".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-statement V-table-Win 
PROCEDURE local-assign-statement :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-statement':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN GET-ATTRIBUTE('ADM-NEW-RECORD').
  IF RETURN-VALUE = 'YES' THEN DO:
      ASSIGN
          gn-vehic.CodCia = s-CodCia
          gn-vehic.FchCreacion = TODAY
          gn-vehic.HoraCreacion = STRING(TIME, 'HH:MM:SS')
          gn-vehic.UsrCreacion = s-user-id
          NO-ERROR.
      IF ERROR-STATUS:ERROR THEN DO:
           RUN dispatch IN THIS-PROCEDURE ('show-errors':U).
           UNDO, RETURN 'ADM-ERROR'.
      END.
  END.
  ELSE DO:
      ASSIGN
          /*gn-vehic.Libre_c04 = pGlosa*/
          gn-vehic.FchModificacion = TODAY
          gn-vehic.HoraModificacion = STRING(TIME, 'HH:MM:SS')
          gn-vehic.UsrModificacion = s-user-id.
  END.
  IF gn-vehic.Libre_c05 = "NO" THEN
      ASSIGN
      gn-vehic.FchAnulacion = TODAY
      gn-vehic.HoraAnulacion = STRING(TIME, 'HH:MM:SS')
      gn-vehic.UsrAnulacion = s-user-id.
  RUN dispatch IN THIS-PROCEDURE ('display-fields':U).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record V-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  IF s-Delete-Record = NO THEN DO:
      MESSAGE 'Usted no est� autorizado para eliminar el registro' VIEW-AS ALERT-BOX INFORMATION.
      RETURN 'ADM-ERROR'.
  END.

  FIND FIRST Di-RutaC WHERE DI-RutaC.CodCia = s-CodCia AND
              DI-RutaC.CodDoc = "H/R" AND
              DI-RutaC.CodVeh = gn-vehic.placa AND
              DI-RutaC.FlgEst <> "A" NO-LOCK NO-ERROR.
  IF AVAILABLE Di-RutaC THEN DO:      
      MESSAGE 'Acceso Denegado' SKIP 'Placa ha sido usada en la H/R ' Di-RutaC.NroDoc
          VIEW-AS ALERT-BOX ERROR.
      RETURN 'ADM-ERROR'.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF AVAILABLE gn-vehic THEN DO WITH FRAME {&FRAME-NAME}:
      FILL-IN-NomPro = ''.
      FIND gn-prov WHERE gn-prov.CodCia = pv-codcia AND
          gn-prov.CodPro = gn-vehic.CodPro
          NO-LOCK NO-ERROR.
      IF AVAILABLE gn-prov THEN FILL-IN-NomPro = gn-prov.NomPro.
      DISPLAY FILL-IN-NomPro.
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable-fields V-table-Win 
PROCEDURE local-enable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN GET-ATTRIBUTE('ADM-NEW-RECORD').
  IF RETURN-VALUE = 'YES' THEN DO:
      ENABLE gn-vehic.placa WITH FRAME {&FRAME-NAME}.
  END.
  ELSE DO:
      DISABLE gn-vehic.placa WITH FRAME {&FRAME-NAME}.
  END.
  /*
  /* Solo se puede modificar ciertas cosas el mismo d�a */
  IF s-Confirm-Record = YES THEN ENABLE gn-vehic.Libre_c05 WITH FRAME {&FRAME-NAME}.
  ELSE DISABLE gn-vehic.Libre_c05 WITH FRAME {&FRAME-NAME}.
  */
/*   IF RETURN-VALUE = 'NO' THEN DO:               */
/*       IF gn-vehic.FchCreacion <> TODAY THEN DO: */
/*           DISABLE                               */
/*               gn-vehic.Carga                    */
/*               gn-vehic.CodPro                   */
/*               gn-vehic.Marca                    */
/*               gn-vehic.Libre_d01                */
/*               gn-vehic.Libre_c02                */
/*               gn-vehic.NroEstibadores           */
/*               WITH FRAME {&FRAME-NAME}.         */
/*       END.                                      */
/*   END.                                          */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  RUN valida.
  IF RETURN-VALUE = "ADM-ERROR" THEN RETURN "ADM-ERROR".

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN dispatch IN THIS-PROCEDURE ('display-fields':U).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE procesa-parametros V-table-Win 
PROCEDURE procesa-parametros :
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE recoge-parametros V-table-Win 
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
        WHEN "Marca" THEN 
            ASSIGN
                input-var-1 = "MV"      /* Marca Vehiculo */
                input-var-2 = ""
                input-var-3 = "".
    END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records V-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "gn-vehic"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed V-table-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

  IF p-state = 'update-begin':U THEN DO:
     RUN valida-update.
     IF RETURN-VALUE = "ADM-ERROR" THEN RETURN.
  END.
  
  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
      {src/adm/template/vstates.i}
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valida V-table-Win 
PROCEDURE valida :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DO WITH FRAME {&FRAME-NAME} :
    IF TRUE <> (gn-vehic.placa:SCREEN-VALUE > '') THEN DO:
        MESSAGE 'Ingrese el n�mero de placa' VIEW-AS ALERT-BOX ERROR.
        APPLY 'ENTRY':U TO gn-vehic.placa.
        RETURN 'ADM-ERROR'.
    END.
    IF TRUE <> (gn-vehic.tipo:SCREEN-VALUE > '') THEN DO:
        /*
        MESSAGE 'Ingreso el numero de tarjeta unica de circulacion' VIEW-AS ALERT-BOX ERROR.
        APPLY 'ENTRY':U TO gn-vehic.tipo.
        RETURN 'ADM-ERROR'.
        */
    END.
    IF NOT CAN-FIND(almtabla WHERE almtabla.Tabla = 'MV' AND
                    almtabla.Codigo = gn-vehic.Marca:SCREEN-VALUE
                    NO-LOCK) THEN DO:
        MESSAGE 'Marca no registrada' VIEW-AS ALERT-BOX ERROR.
        APPLY 'ENTRY':U TO gn-vehic.marca.
        RETURN 'ADM-ERROR'.
    END.
    IF NOT CAN-FIND(gn-prov WHERE gn-prov.CodCia = pv-codcia AND
                    gn-prov.CodPro = gn-vehic.CodPro:SCREEN-VALUE
                    NO-LOCK) THEN DO:
        MESSAGE 'Proveedor no registrado' VIEW-AS ALERT-BOX ERROR.
        APPLY 'ENTRY':U TO gn-vehic.codpro.
        RETURN 'ADM-ERROR'.
    END.
    IF TRUE <> (gn-vehic.Libre_c02:SCREEN-VALUE  > '') THEN DO:
        MESSAGE 'Seleccione el tonelaje' VIEW-AS ALERT-BOX ERROR.
        APPLY 'ENTRY':U TO gn-vehic.Libre_c02.
        RETURN 'ADM-ERROR'.
    END.

    DEFINE VAR x-carga-max AS DEC.
    DEFINE VAR x-carga-min AS DEC.

    x-carga-max = INTEGER(gn-vehic.carga:SCREEN-VALUE).
    IF x-carga-max <= 0 THEN DO:
        MESSAGE 'Ingrese carga MAXIMA' VIEW-AS ALERT-BOX ERROR.
        APPLY 'ENTRY':U TO gn-vehic.carga.
        RETURN 'ADM-ERROR'.
    END.
    IF x-carga-min >= x-carga-max THEN DO:
        MESSAGE 'La carga MINIMA debe ser menor a la carga MAXIMA' VIEW-AS ALERT-BOX ERROR.
        APPLY 'ENTRY':U TO gn-vehic.libre_d01.
        RETURN 'ADM-ERROR'.
    END.

    /* Validar si la placa ya existe */
    DEFINE BUFFER y-gn-vehic FOR gn-vehic.

    RUN GET-ATTRIBUTE('ADM-NEW-RECORD').
    IF RETURN-VALUE = 'YES' THEN DO:
        FIND FIRST y-gn-vehic WHERE y-gn-vehic.codcia = s-codcia AND 
            y-gn-vehic.placa = gn-vehic.placa:SCREEN-VALUE
            NO-LOCK NO-ERROR.
        IF AVAILABLE y-gn-vehic THEN DO:
            MESSAGE 'Numero de Placa ya esta registrado' VIEW-AS ALERT-BOX ERROR.
            APPLY 'ENTRY':U TO gn-vehic.placa.
            RETURN 'ADM-ERROR'.
        END.
    END.
    ELSE DO:
        /**/
    END.

    /**/
    DEFINE VAR cTonelaje AS CHAR.

    cTonelaje = REPLACE(REPLACE(gn-vehic.libre_c02:SCREEN-VALUE,"TN","")," ","").

    IF DEC(cTonelaje) > 2 THEN DO:
        IF LENGTH(TRIM(gn-vehic.libre_c04:SCREEN-VALUE)) < 10 THEN DO:
            MESSAGE 'Registro del MTC esta errado' VIEW-AS ALERT-BOX ERROR.
            APPLY 'ENTRY':U TO gn-vehic.libre_c04.
            RETURN 'ADM-ERROR'.
        END.
    END.
END.

RETURN "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valida-update V-table-Win 
PROCEDURE valida-update :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  IF s-Write-Record = NO THEN DO:
      MESSAGE 'Usted no est� autorizado para modificar el registro' VIEW-AS ALERT-BOX INFORMATION.
      RETURN 'ADM-ERROR'.
  END.
                            
/*pGlosa = gn-vehic.Libre_c04.*/

RETURN "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

