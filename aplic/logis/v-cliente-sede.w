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

DEFINE SHARED VAR s-codcia AS INT.
DEFINE SHARED VAR s-user-id AS CHAR.

DEF SHARED VAR lh_handle AS HANDLE.

DEFINE SHARED VAR s-nivel-acceso AS INT NO-UNDO.

DEFINE BUFFER b-gn-clie FOR gn-clie.
DEFINE BUFFER x-gn-clieD FOR gn-clieD.

IF USERID("DICTDB") = "ADMIN" THEN DO:
    /*MESSAGE "v-cliente-sede".*/
END.

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
&Scoped-define EXTERNAL-TABLES Gn-ClieD gn-clie
&Scoped-define FIRST-EXTERNAL-TABLE Gn-ClieD


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR Gn-ClieD, gn-clie.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS Gn-ClieD.Sede Gn-ClieD.SedeClie ~
Gn-ClieD.DirCli Gn-ClieD.CodDept Gn-ClieD.Referencias Gn-ClieD.CodProv ~
Gn-ClieD.CodDist Gn-ClieD.Codpos Gn-ClieD.Longitud Gn-ClieD.Latitud ~
Gn-ClieD.Libre_c01 Gn-ClieD.Libre_c02 
&Scoped-define ENABLED-TABLES Gn-ClieD
&Scoped-define FIRST-ENABLED-TABLE Gn-ClieD
&Scoped-Define DISPLAYED-FIELDS Gn-ClieD.Sede Gn-ClieD.DomFiscal ~
Gn-ClieD.SwSedeSunat Gn-ClieD.SedeClie Gn-ClieD.DirCli Gn-ClieD.CodDept ~
Gn-ClieD.Referencias Gn-ClieD.CodProv Gn-ClieD.CodDist Gn-ClieD.Codpos ~
Gn-ClieD.Longitud Gn-ClieD.Latitud Gn-ClieD.Libre_c01 Gn-ClieD.Libre_c02 
&Scoped-define DISPLAYED-TABLES Gn-ClieD
&Scoped-define FIRST-DISPLAYED-TABLE Gn-ClieD
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-DEP FILL-IN-PROV FILL-IN-DIS ~
FILL-IN-POS FILL-IN-Cuadrante 

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
DEFINE VARIABLE FILL-IN-Cuadrante AS CHARACTER FORMAT "X(256)":U 
     LABEL "Cuadrante" 
     VIEW-AS FILL-IN 
     SIZE 11.72 BY .81 NO-UNDO.

DEFINE VARIABLE FILL-IN-DEP AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 22 BY .81 NO-UNDO.

DEFINE VARIABLE FILL-IN-DIS AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 22 BY .81 NO-UNDO.

DEFINE VARIABLE FILL-IN-POS AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 22 BY .81 NO-UNDO.

DEFINE VARIABLE FILL-IN-PROV AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 22 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Gn-ClieD.Sede AT ROW 1 COL 12 COLON-ALIGNED WIDGET-ID 66
          VIEW-AS FILL-IN 
          SIZE 6 BY .81
     Gn-ClieD.DomFiscal AT ROW 1 COL 37 NO-LABEL WIDGET-ID 76
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "SI", yes,
"NO", no
          SIZE 12 BY .81
     Gn-ClieD.SwSedeSunat AT ROW 1 COL 51 NO-LABEL WIDGET-ID 130
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "SUNAT", "S":U,
"MANUAL", "M":U
          SIZE 12 BY 1.62
     Gn-ClieD.SedeClie AT ROW 1.81 COL 12 COLON-ALIGNED WIDGET-ID 68
          VIEW-AS FILL-IN 
          SIZE 15.72 BY .81
     Gn-ClieD.DirCli AT ROW 2.62 COL 12 COLON-ALIGNED WIDGET-ID 124 FORMAT "x(200)"
          VIEW-AS FILL-IN 
          SIZE 108 BY .81
     Gn-ClieD.CodDept AT ROW 3.42 COL 12 COLON-ALIGNED WIDGET-ID 40
          VIEW-AS FILL-IN 
          SIZE 5 BY .81
     FILL-IN-DEP AT ROW 3.42 COL 17 COLON-ALIGNED NO-LABEL WIDGET-ID 84
     Gn-ClieD.Referencias AT ROW 3.5 COL 55.14 NO-LABEL WIDGET-ID 82
          VIEW-AS EDITOR SCROLLBAR-VERTICAL
          SIZE 60 BY 2.42
     Gn-ClieD.CodProv AT ROW 4.23 COL 12 COLON-ALIGNED WIDGET-ID 48
          LABEL "Provincia"
          VIEW-AS FILL-IN 
          SIZE 5 BY .81
     FILL-IN-PROV AT ROW 4.23 COL 17 COLON-ALIGNED NO-LABEL WIDGET-ID 90
     Gn-ClieD.CodDist AT ROW 5.04 COL 12 COLON-ALIGNED WIDGET-ID 42
          LABEL "Distrito" FORMAT "X(3)"
          VIEW-AS FILL-IN 
          SIZE 5 BY .81
     FILL-IN-DIS AT ROW 5.04 COL 17 COLON-ALIGNED NO-LABEL WIDGET-ID 86
     Gn-ClieD.Codpos AT ROW 5.85 COL 12 COLON-ALIGNED WIDGET-ID 46
          VIEW-AS FILL-IN 
          SIZE 5 BY .81
     FILL-IN-POS AT ROW 5.85 COL 17 COLON-ALIGNED NO-LABEL WIDGET-ID 88
     Gn-ClieD.Longitud AT ROW 6.65 COL 12 COLON-ALIGNED WIDGET-ID 58
          VIEW-AS FILL-IN 
          SIZE 10.72 BY .81
     Gn-ClieD.Latitud AT ROW 6.65 COL 29.57 COLON-ALIGNED WIDGET-ID 56
          VIEW-AS FILL-IN 
          SIZE 10.72 BY .81
     FILL-IN-Cuadrante AT ROW 6.65 COL 51.86 COLON-ALIGNED WIDGET-ID 128
     Gn-ClieD.Libre_c01 AT ROW 7.5 COL 12 COLON-ALIGNED WIDGET-ID 96
          LABEL "C�digo IBC" FORMAT "x(13)"
          VIEW-AS FILL-IN 
          SIZE 16 BY .81
     Gn-ClieD.Libre_c02 AT ROW 7.5 COL 34.86 COLON-ALIGNED WIDGET-ID 98
          LABEL "Barras" FORMAT "x(20)"
          VIEW-AS FILL-IN 
          SIZE 20 BY .81
     "Domicilio Fiscal:" VIEW-AS TEXT
          SIZE 11 BY .5 AT ROW 1.12 COL 26 WIDGET-ID 126
     "Referencias:" VIEW-AS TEXT
          SIZE 9 BY .5 AT ROW 3.5 COL 46 WIDGET-ID 94
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 4 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: INTEGRAL.Gn-ClieD,INTEGRAL.gn-clie
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
         HEIGHT             = 7.42
         WIDTH              = 123.43.
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

/* SETTINGS FOR FILL-IN Gn-ClieD.CodDist IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Gn-ClieD.CodProv IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Gn-ClieD.DirCli IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR RADIO-SET Gn-ClieD.DomFiscal IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-Cuadrante IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-DEP IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-DIS IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-POS IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-PROV IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Gn-ClieD.Libre_c01 IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Gn-ClieD.Libre_c02 IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR RADIO-SET Gn-ClieD.SwSedeSunat IN FRAME F-Main
   NO-ENABLE                                                            */
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

&Scoped-define SELF-NAME Gn-ClieD.CodDept
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Gn-ClieD.CodDept V-table-Win
ON LEAVE OF Gn-ClieD.CodDept IN FRAME F-Main /* Departamento */
DO:
  FILL-IN-DEP:SCREEN-VALUE = ''.
  FIND TabDepto WHERE TabDepto.CodDepto = Gn-ClieD.CodDept:SCREEN-VALUE NO-LOCK NO-ERROR.
  IF AVAILABLE TabDepto THEN FILL-IN-DEP:SCREEN-VALUE = TabDepto.NomDepto.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Gn-ClieD.CodDist
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Gn-ClieD.CodDist V-table-Win
ON LEAVE OF Gn-ClieD.CodDist IN FRAME F-Main /* Distrito */
DO:
  FILL-IN-DIS:SCREEN-VALUE = ''.
  FIND TabDistr WHERE TabDistr.CodDepto = Gn-ClieD.CodDept:SCREEN-VALUE AND 
      TabDistr.CodProvi = Gn-ClieD.CodProv:SCREEN-VALUE AND 
      TabDistr.CodDistr = Gn-ClieD.CodDist:SCREEN-VALUE NO-LOCK NO-ERROR.
  IF AVAILABLE TabDistr THEN DO:
      FILL-IN-DIS:SCREEN-VALUE = TabDistr.NomDistr.
      IF TRUE <> (Gn-ClieD.Codpos > '') THEN DO:
          Gn-ClieD.Codpos:SCREEN-VALUE = TabDistr.CodPos.
          APPLY 'LEAVE':U TO Gn-ClieD.Codpos.
      END.
  END.
      
      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Gn-ClieD.Codpos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Gn-ClieD.Codpos V-table-Win
ON LEAVE OF Gn-ClieD.Codpos IN FRAME F-Main /* Postal */
DO:
  FILL-IN-POS:SCREEN-VALUE = ''.
  FIND almtabla WHERE almtabla.Tabla = "CP" AND
      almtabla.Codigo = Gn-ClieD.Codpos:SCREEN-VALUE NO-LOCK NO-ERROR.
  IF AVAILABLE almtabla THEN FILL-IN-POS:SCREEN-VALUE = almtabla.Nombre.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Gn-ClieD.CodProv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Gn-ClieD.CodProv V-table-Win
ON LEAVE OF Gn-ClieD.CodProv IN FRAME F-Main /* Provincia */
DO:
  FILL-IN-PROV:SCREEN-VALUE = ''.
  FIND TabProvi WHERE TabProvi.CodDepto = Gn-ClieD.CodDept:SCREEN-VALUE AND 
      TabProvi.CodProvi = Gn-ClieD.CodProv:SCREEN-VALUE
      NO-LOCK NO-ERROR.
  IF AVAILABLE TabProvi THEN FILL-IN-PROV:SCREEN-VALUE = TabProvi.NomProvi.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Gn-ClieD.SedeClie
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Gn-ClieD.SedeClie V-table-Win
ON LEAVE OF Gn-ClieD.SedeClie IN FRAME F-Main /* Sede Cliente */
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
  {src/adm/template/row-list.i "Gn-ClieD"}
  {src/adm/template/row-list.i "gn-clie"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "Gn-ClieD"}
  {src/adm/template/row-find.i "gn-clie"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-add-record V-table-Win 
PROCEDURE local-add-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  
  IF NOT AVAILABLE gn-clie THEN DO:
      MESSAGE "No existe ningun cliente en Linea"
          VIEW-AS ALERT-BOX INFORMATION.
      RETURN "ADM-ERROR".
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  DEFINE VAR x-correlativo AS INT INIT 0.
  DEFINE VAR x-filer AS INT INIT 0.
  DEFINE VAR x-direccion-fiscal AS LOG INIT NO.
  DEFINE VAR x-sede AS CHAR.

  FOR EACH x-gn-clieD OF gn-clie NO-LOCK:
      x-filer = x-filer + 1.
      IF x-gn-clieD.sede = '@@@' THEN DO:
          x-direccion-fiscal = YES.
      END.
      ELSE DO:
          x-correlativo = MAXIMUM(INTEGER(TRIM(x-gn-clieD.sede)),x-correlativo).
      END.
  END.
  /*
  MESSAGE gn-clie.codcli SKIP
        x-direccion-fiscal SKIP
        x-filer.
  */
  x-correlativo = x-correlativo + 1.

  DO WITH FRAME {&FRAME-NAME} :
      IF x-direccion-fiscal = NO THEN DO:
        Gn-ClieD.Sede:SCREEN-VALUE = "@@@".
        gn-clieD.DomFiscal:SCREEN-VALUE = "SI".
      END.
      ELSE DO:
          Gn-ClieD.Sede:SCREEN-VALUE = STRING(x-correlativo,"9999").
      END.      
  END.
  

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
/*   /* Armamos la direcci�n */                                    */
/*   DEF VAR x-Direccion AS CHAR NO-UNDO.                          */
/*   IF Gn-ClieD.NomVia > '' THEN DO:                              */
/*       FIND PL-TABLA WHERE PL-TABLA.CodCia = 000 AND             */
/*           PL-TABLA.Tabla = "05" AND                             */
/*           PL-TABLA.Codigo = Gn-ClieD.TipoVia NO-LOCK NO-ERROR.  */
/*       IF AVAILABLE PL-TABLA THEN x-Direccion = x-Direccion +    */
/*           (IF TRUE <> (x-Direccion > '') THEN '' ELSE ' ') +    */
/*           PL-TABLA.Nombre + ' ' + Gn-ClieD.NomVia.              */
/*   END.                                                          */
/*   IF Gn-ClieD.Nro > '' THEN x-Direccion = x-Direccion +         */
/*           (IF TRUE <> (x-Direccion > '') THEN '' ELSE ' ') +    */
/*           'NRO. ' + Gn-ClieD.Nro.                               */
/*   IF Gn-ClieD.Km > '' THEN x-Direccion = x-Direccion +          */
/*       (IF TRUE <> (x-Direccion > '') THEN '' ELSE ' ') +        */
/*       'KM. ' + Gn-ClieD.Km.                                     */
/*   IF Gn-ClieD.Mza > '' THEN x-Direccion = x-Direccion +         */
/*       (IF TRUE <> (x-Direccion > '') THEN '' ELSE ' ') +        */
/*       'MZA. ' + Gn-ClieD.Mza.                                   */
/*   IF Gn-ClieD.Inter > '' THEN x-Direccion = x-Direccion +       */
/*       (IF TRUE <> (x-Direccion > '') THEN '' ELSE ' ') +        */
/*       'INTER. ' + Gn-ClieD.Inter.                               */
/*   IF Gn-ClieD.Dpto > '' THEN x-Direccion = x-Direccion +        */
/*       (IF TRUE <> (x-Direccion > '') THEN '' ELSE ' ') +        */
/*       'DPTO. ' + Gn-ClieD.Dpto.                                 */
/*   IF Gn-ClieD.Lote > '' THEN x-Direccion = x-Direccion +        */
/*       (IF TRUE <> (x-Direccion > '') THEN '' ELSE ' ') +        */
/*       'LOTE ' + Gn-ClieD.Lote.                                  */
/*   IF Gn-ClieD.NomZona > '' THEN DO:                             */
/*       FIND PL-TABLA WHERE PL-TABLA.CodCia = 000 AND             */
/*           PL-TABLA.Tabla = "06" AND                             */
/*           PL-TABLA.Codigo = Gn-ClieD.TipoZona NO-LOCK NO-ERROR. */
/*       IF AVAILABLE PL-TABLA THEN x-Direccion = x-Direccion +    */
/*           (IF TRUE <> (x-Direccion > '') THEN '' ELSE ' ') +    */
/*           PL-TABLA.Nombre + ' ' + Gn-ClieD.NomZona.             */
/*   END.                                                          */
/*   Gn-ClieD.DirCli = CAPS(x-Direccion).                          */

  IF Gn-ClieD.Sede <> '@@@' THEN Gn-ClieD.DomFiscal = NO.
    /*  */
  ELSE DO:  
      ASSIGN Gn-ClieD.DomFiscal = YES.
      /*
        @@@ = Direccion Fiscal 
        actualizamos los datos en el maestro de clientes - la direccion FISCAL 
      */
      /*FIND CURRENT gn-clie EXCLUSIVE-LOCK NO-ERROR.*/

      FIND FIRST b-gn-clie OF gn-clie EXCLUSIVE-LOCK NO-ERROR.
      IF NOT AVAILABLE b-gn-clie THEN UNDO, RETURN 'ADM-ERROR'.
      ASSIGN
          b-gn-clie.SwCargaSunat = Gn-ClieD.SwSedeSunat
          b-gn-clie.CodDept = Gn-ClieD.CodDept 
          b-gn-clie.CodDist = Gn-ClieD.CodDist 
          b-gn-clie.CodProv = Gn-ClieD.CodProv 
          b-gn-clie.Codpos  = Gn-ClieD.Codpos 
          b-gn-clie.CodIBC  = Gn-ClieD.Libre_c01
          b-gn-clie.DirCli  = Gn-ClieD.DirCli
          b-gn-clie.Referencias = Gn-ClieD.Referencias.
      RELEASE  b-gn-clie.
  END.
  RUN GET-ATTRIBUTE('ADM-NEW-RECORD').
  IF RETURN-VALUE = 'NO' THEN
      ASSIGN
      Gn-ClieD.FchModificacion = TODAY
      Gn-ClieD.UsrModificacion = s-User-Id.

  RUN dispatch IN THIS-PROCEDURE ('display-fields':U).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record V-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN
      Gn-ClieD.CodCia = Gn-Clie.CodCia
      Gn-ClieD.CodCli = Gn-Clie.CodCli
      Gn-ClieD.SwSedeSunat = "M".   /* MANUAL */
  ASSIGN
      Gn-ClieD.FchCreacion = TODAY
      Gn-ClieD.UsrCreacion = s-User-Id.

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
  IF AVAILABLE gn-clied AND gn-clied.sede = '@@@' THEN DO:
      MESSAGE 'NO puede eliminar la direcci�n fiscal' SKIP 'Proceso abortado'
          VIEW-AS ALERT-BOX WARNING.
      RETURN 'ADM-ERROR'.
  END.
  IF Gn-ClieD.SwSedeSunat = "S" THEN DO:
      MESSAGE 'NMO se puede anular una sede validada por SUNAT' SKIP 'Proceso abortado'
          VIEW-AS ALERT-BOX WARNING.
      RETURN 'ADM-ERROR'.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-disable-fields V-table-Win 
PROCEDURE local-disable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'disable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  /*RUN Procesa-Handle IN lh_handle('enable-fields').*/
  RUN Procesa-Handle IN lh_handle ('Enable-Header') NO-ERROR.

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
  IF AVAILABLE Gn-ClieD THEN DO WITH FRAME {&FRAME-NAME}:
      FILL-IN-DEP:SCREEN-VALUE = ''.
      FILL-IN-PROV:SCREEN-VALUE = ''.
      FILL-IN-DIS:SCREEN-VALUE = ''.
      FILL-IN-POS:SCREEN-VALUE = ''.
      FIND TabDepto WHERE TabDepto.CodDepto = Gn-ClieD.CodDept NO-LOCK NO-ERROR.
      IF AVAILABLE TabDepto THEN FILL-IN-DEP:SCREEN-VALUE = TabDepto.NomDepto.
      FIND TabProvi WHERE TabProvi.CodDepto = Gn-ClieD.CodDept AND 
          TabProvi.CodProvi = Gn-ClieD.CodProv
          NO-LOCK NO-ERROR.
      IF AVAILABLE TabProvi THEN FILL-IN-PROV:SCREEN-VALUE = TabProvi.NomProvi.
      FIND TabDistr WHERE TabDistr.CodDepto = Gn-ClieD.CodDept AND 
          TabDistr.CodProvi = Gn-ClieD.CodProv AND 
          TabDistr.CodDistr = Gn-ClieD.CodDist NO-LOCK NO-ERROR.
      IF AVAILABLE TabDistr THEN FILL-IN-DIS:SCREEN-VALUE = TabDistr.NomDistr.
      FIND almtabla WHERE almtabla.Tabla = "CP" AND 
          almtabla.Codigo = Gn-ClieD.Codpos NO-LOCK NO-ERROR.
      IF AVAILABLE almtabla THEN FILL-IN-POS:SCREEN-VALUE = almtabla.Nombre.
      RUN logis/p-cuadrante (Gn-ClieD.Longitud,
                             Gn-ClieD.Latitud,
                             OUTPUT FILL-IN-Cuadrante).
      DISPLAY FILL-IN-Cuadrante.
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
  DO WITH FRAME {&FRAME-NAME}:
      DISABLE Gn-ClieD.DomFiscal.
      RUN GET-ATTRIBUTE('ADM-NEW-RECORD').

      DISABLE Gn-ClieD.Sede.

      IF RETURN-VALUE = 'NO' THEN DO WITH FRAME {&FRAME-NAME}:
          IF Gn-ClieD.Sede = '@@@' THEN DO:
              DISABLE Gn-ClieD.Sede.
          END.
          IF Gn-ClieD.SwSedeSunat = "S" THEN
              DISABLE 
              Gn-ClieD.CodDept 
              Gn-ClieD.CodDist 
              Gn-ClieD.Codpos 
              Gn-ClieD.CodProv 
              Gn-ClieD.DirCli 
              Gn-ClieD.DomFiscal 
              Gn-ClieD.Sede 
              /*Gn-ClieD.SedeClie */
              Gn-ClieD.SwSedeSunat.
      END.
      /*RUN Procesa-Handle IN lh_handle('disable-fields').*/
  END.

  /* Restricciones */
  RUN restricciones(INPUT NO).

  RUN Procesa-Handle IN lh_handle ('Disable-Header') NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize V-table-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  DO WITH FRAME {&FRAME-NAME}:
/*       /* TIPO ZONA */                                                   */
/*       Gn-ClieD.TipoZona:DELETE(1).                                      */
/*       FOR EACH PL-TABLA NO-LOCK WHERE PL-TABLA.CodCia = 000 AND         */
/*           PL-TABLA.Tabla = "06":                                        */
/*           Gn-ClieD.TipoZona:ADD-LAST(PL-TABLA.Nombre, PL-TABLA.Codigo). */
/*       END.                                                              */
/*       /* TIPO VIA */                                                    */
/*       Gn-ClieD.TipoVia:DELETE(1).                                       */
/*       FOR EACH PL-TABLA NO-LOCK WHERE PL-TABLA.CodCia = 000 AND         */
/*           PL-TABLA.Tabla = "05":                                        */
/*           Gn-ClieD.TipoVia:ADD-LAST(PL-TABLA.Nombre, PL-TABLA.Codigo).  */
/*       END.                                                              */
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  /* Restricciones */
  RUN restricciones(INPUT YES).


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

    input-var-1 = ''.
    input-var-2 = ''.
    input-var-3 = ''.
    output-var-1 = ?.
    CASE HANDLE-CAMPO:name:
        WHEN "CodProv" THEN DO:
            input-var-1 = Gn-ClieD.CodDept:SCREEN-VALUE IN FRAME {&FRAME-NAME}.
        END.
        WHEN "CodDist" THEN DO:
            input-var-1 = Gn-ClieD.CodDept:SCREEN-VALUE IN FRAME {&FRAME-NAME}.
            input-var-2 = Gn-ClieD.CodProv:SCREEN-VALUE IN FRAME {&FRAME-NAME}.
        END.
        WHEN "CodPos" THEN DO:
            input-var-1 = "CP".
        END.
    END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE restricciones V-table-Win 
PROCEDURE restricciones :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE INPUT PARAMETER pInitialize AS LOG NO-UNDO.

    DO WITH FRAME {&FRAME-NAME} :
        IF pInitialize = NO AND s-nivel-acceso <> 0 THEN DO:
            RUN restricciones_objeto(INPUT gn-clied.swsedesunat:HANDLE, INPUT "gn-clied.swsedesunat").
            RUN restricciones_objeto(INPUT gn-clied.sedeclie:HANDLE, INPUT "gn-clied.sedeclie").
            RUN restricciones_objeto(INPUT gn-clied.dircli:HANDLE, INPUT "gn-clied.dircli").
            RUN restricciones_objeto(INPUT gn-clied.coddept:HANDLE, INPUT "gn-clied.coddept").
            RUN restricciones_objeto(INPUT gn-clied.codprov:HANDLE, INPUT "gn-clied.codprov").
            RUN restricciones_objeto(INPUT gn-clied.coddist:HANDLE, INPUT "gn-clied.coddist").
            RUN restricciones_objeto(INPUT gn-clied.codpos:HANDLE, INPUT "gn-clied.codpos").
            RUN restricciones_objeto(INPUT gn-clied.longitud:HANDLE, INPUT "gn-clied.longitud").
            RUN restricciones_objeto(INPUT gn-clied.latitud:HANDLE, INPUT "gn-clied.latitud").
            RUN restricciones_objeto(INPUT gn-clied.libre_c01:HANDLE, INPUT "gn-clied.libre_c01").
            RUN restricciones_objeto(INPUT gn-clied.libre_c02:HANDLE, INPUT "gn-clied.libre_c02").
            RUN restricciones_objeto(INPUT gn-clied.referencias:HANDLE, INPUT "gn-clied.referencias").
        END.
    END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE restricciones_objeto V-table-Win 
PROCEDURE restricciones_objeto :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER hwObjeto AS handle NO-UNDO.
DEFINE INPUT PARAMETER sFieldName AS CHAR NO-UNDO.

DEFINE VAR x-pos AS INT.

DEFINE BUFFER x-factabla FOR factabla.

FIND FIRST x-factabla WHERE x-factabla.codcia = s-codcia AND
                            x-factabla.tabla = 'MANTENIMIENTO-CLIENTE-SEDES' AND
                            x-factabla.codigo = sFieldName NO-LOCK NO-ERROR.

IF AVAILABLE x-factabla THEN DO:
    IF s-nivel-acceso <> 0 THEN DO:

        x-pos = (s-nivel-acceso * 2) - 1.

        IF x-factabla.campo-l[x-pos] = NO THEN DO:
            /* NO VISIBLE */
            hwObjeto:VISIBLE = NO NO-ERROR.
        END.
        ELSE DO:
            IF x-factabla.campo-l[x-pos + 1] = YES THEN DO:
                /* EDITABLE */
                ASSIGN hwObjeto:SENSITIVE = YES NO-ERROR.
            END.
            ELSE DO: 
                ASSIGN hwObjeto:SENSITIVE = NO NO-ERROR. 
            END.
                
                
        END.
        
    END.
    
END.

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
  {src/adm/template/snd-list.i "Gn-ClieD"}
  {src/adm/template/snd-list.i "gn-clie"}

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

DO WITH FRAME {&FRAME-NAME}:
    IF TRUE <> (Gn-ClieD.Sede:SCREEN-VALUE > '') THEN DO:
        MESSAGE 'Ingrese la sede' VIEW-AS ALERT-BOX ERROR.
        APPLY 'ENTRY':U TO Gn-ClieD.Sede.
        RETURN 'ADM-ERROR'.
    END.
    IF NOT CAN-FIND(TabDepto WHERE TabDepto.CodDepto = Gn-ClieD.CodDept:SCREEN-VALUE NO-LOCK)
        THEN DO:
        MESSAGE 'Error en el c�digo del Departamento' VIEW-AS ALERT-BOX ERROR.
        APPLY 'ENTRY':U TO Gn-ClieD.CodDept.
        RETURN 'ADM-ERROR'.
    END.
    IF NOT CAN-FIND(TabProvi WHERE TabProvi.CodDepto = Gn-ClieD.CodDept:SCREEN-VALUE AND 
                    TabProvi.CodProvi = Gn-ClieD.CodProv:SCREEN-VALUE NO-LOCK)
        THEN DO:
        MESSAGE 'Error en el c�digo de Provincia' VIEW-AS ALERT-BOX ERROR.
        APPLY 'ENTRY':U TO Gn-ClieD.CodProv.
        RETURN 'ADM-ERROR'.
    END.
    IF NOT CAN-FIND(TabDistr WHERE TabDistr.CodDepto = Gn-ClieD.CodDept:SCREEN-VALUE AND 
                    TabDistr.CodProvi = Gn-ClieD.CodProv:SCREEN-VALUE AND 
                    TabDistr.CodDistr = Gn-ClieD.CodDist:SCREEN-VALUE NO-LOCK)
        THEN DO:
        MESSAGE 'Error en el c�digo de Distrito' VIEW-AS ALERT-BOX ERROR.
        APPLY 'ENTRY':U TO Gn-ClieD.CodDist.
        RETURN 'ADM-ERROR'.
    END.
    IF NOT CAN-FIND(almtabla WHERE almtabla.Tabla = "CP" AND
                    almtabla.Codigo = Gn-ClieD.Codpos:SCREEN-VALUE NO-LOCK)
        THEN DO:
        MESSAGE 'Error en el c�digo Postal' VIEW-AS ALERT-BOX ERROR.
        APPLY 'ENTRY':U TO Gn-ClieD.CodPos.
        RETURN 'ADM-ERROR'.
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

IF NOT AVAILABLE Gn-ClieD THEN DO:
    MESSAGE 'NO hay registro que modificar' SKIP
        'Tiene que Adicionar un registro primero' VIEW-AS ALERT-BOX ERROR.
    RETURN 'ADM-ERROR'.
END.
/* IF Gn-ClieD.SwSedeSunat = "S" THEN DO:                   */
/*     MESSAGE 'Acceso Denegado' VIEW-AS ALERT-BOX WARNING. */
/*     RETURN 'ADM-ERROR'.                                  */
/* END.                                                     */
RETURN "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

