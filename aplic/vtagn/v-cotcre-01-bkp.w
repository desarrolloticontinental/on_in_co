&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          integral         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW


/* Temp-Table and Buffer definitions                                    */
DEFINE SHARED TEMP-TABLE T-DDocu NO-UNDO LIKE VtaDDocu.



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

DEFINE SHARED VARIABLE lh_Handle  AS HANDLE.

DEFINE SHARED VARIABLE s-codcia AS INT.
DEFINE SHARED VARIABLE s-coddiv AS CHAR.
DEFINE SHARED VARIABLE cl-codcia AS INT.
DEFINE SHARED VARIABLE cb-codcia AS INT.
DEFINE SHARED VARIABLE s-user-id AS CHAR.
DEFINE SHARED VARIABLE S-CODCLI   AS CHAR.
DEFINE SHARED VARIABLE S-CNDVTA   AS CHAR.
DEFINE SHARED VARIABLE S-TPOCMB AS DECIMAL.  
DEFINE SHARED VARIABLE s-codped AS CHAR.
DEFINE SHARED VARIABLE S-NROSER   AS INTEGER.
DEFINE SHARED VARIABLE s-codmon AS INT.
DEFINE SHARED VARIABLE S-FLGIGV AS LOGICAL.
DEFINE SHARED VARIABLE S-PORIGV AS DECIMAL.
DEFINE SHARED VAR s-adm-new-record AS CHAR.

DEFINE VAR s-cndvta-validos AS CHAR.

FIND FIRST Faccfggn WHERE codcia = s-codcia NO-LOCK.

/* CONFIGURACIONS GENERALES POR U.N. */
DEF VAR s-FlgAprCot LIKE gn-divi.flgaprcot NO-UNDO.
DEF VAR s-DiasVtoCot LIKE gn-divi.diasvtocot NO-UNDO.
DEF VAR s-DiasAmpCot LIKE gn-divi.diasampcot NO-UNDO.

FIND gn-divi WHERE gn-divi.codcia = s-codcia
    AND gn-divi.coddiv = s-coddiv NO-LOCK.
ASSIGN
    s-FlgAprCot = gn-divi.flgaprcot
    s-DiasVtoCot = gn-divi.diasvtocot
    s-DiasAmpCot = gn-divi.diasampcot.

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
&Scoped-define EXTERNAL-TABLES VtaCDocu
&Scoped-define FIRST-EXTERNAL-TABLE VtaCDocu


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR VtaCDocu.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS VtaCDocu.CodCli VtaCDocu.RucCli ~
VtaCDocu.DniCli VtaCDocu.NomCli VtaCDocu.DirCli VtaCDocu.Sede ~
VtaCDocu.LugEnt VtaCDocu.NroCard VtaCDocu.CodVen VtaCDocu.FmaPgo ~
VtaCDocu.FchPed VtaCDocu.FchVen VtaCDocu.FchEnt VtaCDocu.NroOrd ~
VtaCDocu.Cmpbnte VtaCDocu.FlgIgv VtaCDocu.CodMon VtaCDocu.TpoCmb 
&Scoped-define ENABLED-TABLES VtaCDocu
&Scoped-define FIRST-ENABLED-TABLE VtaCDocu
&Scoped-Define DISPLAYED-FIELDS VtaCDocu.NroPed VtaCDocu.CodCli ~
VtaCDocu.RucCli VtaCDocu.DniCli VtaCDocu.NomCli VtaCDocu.DirCli ~
VtaCDocu.Sede VtaCDocu.LugEnt VtaCDocu.NroCard VtaCDocu.CodVen ~
VtaCDocu.FmaPgo VtaCDocu.FchPed VtaCDocu.FchVen VtaCDocu.FchEnt ~
VtaCDocu.NroOrd VtaCDocu.Usuario VtaCDocu.Cmpbnte VtaCDocu.FlgIgv ~
VtaCDocu.CodMon VtaCDocu.TpoCmb 
&Scoped-define DISPLAYED-TABLES VtaCDocu
&Scoped-define FIRST-DISPLAYED-TABLE VtaCDocu
&Scoped-Define DISPLAYED-OBJECTS f-Estado f-NomVen f-CndVta 

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
DEFINE BUTTON BUTTON-Cliente 
     LABEL "Cliente:" 
     SIZE 8 BY 1.12.

DEFINE BUTTON BUTTON-Condicion 
     LABEL "Condicion de venta:" 
     SIZE 19 BY 1.12.

DEFINE BUTTON BUTTON-Sede 
     LABEL "Sede:" 
     SIZE 7 BY 1.12.

DEFINE BUTTON BUTTON-Tarjeta 
     LABEL "Tarjeta:" 
     SIZE 8 BY 1.12.

DEFINE BUTTON BUTTON-Vendedor 
     LABEL "Vendedor:" 
     SIZE 11 BY 1.12.

DEFINE VARIABLE f-CndVta AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 50 BY 1 NO-UNDO.

DEFINE VARIABLE f-Estado AS CHARACTER FORMAT "X(256)":U 
     LABEL "Estado" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE f-NomVen AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 50 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     f-Estado AT ROW 1 COL 50 COLON-ALIGNED WIDGET-ID 72
     VtaCDocu.NroPed AT ROW 1 COL 19 COLON-ALIGNED WIDGET-ID 32
          LABEL "No. Cotizacion"
          VIEW-AS FILL-IN 
          SIZE 13.43 BY 1
     BUTTON-Cliente AT ROW 2.08 COL 13 WIDGET-ID 58
     VtaCDocu.CodCli AT ROW 2.08 COL 19 COLON-ALIGNED NO-LABEL WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 15.43 BY 1
     VtaCDocu.RucCli AT ROW 2.08 COL 40 COLON-ALIGNED WIDGET-ID 34
          VIEW-AS FILL-IN 
          SIZE 15.43 BY 1
     VtaCDocu.DniCli AT ROW 2.08 COL 62 COLON-ALIGNED WIDGET-ID 10
          VIEW-AS FILL-IN 
          SIZE 12.43 BY 1
     VtaCDocu.NomCli AT ROW 3.15 COL 19 COLON-ALIGNED WIDGET-ID 26
          VIEW-AS FILL-IN 
          SIZE 61.43 BY 1
     VtaCDocu.DirCli AT ROW 4.23 COL 19 COLON-ALIGNED WIDGET-ID 8
          LABEL "Direcci�n"
          VIEW-AS FILL-IN 
          SIZE 61.43 BY 1
     BUTTON-Sede AT ROW 5.31 COL 14 WIDGET-ID 60
     VtaCDocu.Sede AT ROW 5.31 COL 19 COLON-ALIGNED NO-LABEL WIDGET-ID 36
          VIEW-AS FILL-IN 
          SIZE 9.43 BY 1
     VtaCDocu.LugEnt AT ROW 6.38 COL 19 COLON-ALIGNED WIDGET-ID 24
          VIEW-AS FILL-IN 
          SIZE 61.43 BY 1
     BUTTON-Tarjeta AT ROW 7.46 COL 13 WIDGET-ID 64
     VtaCDocu.NroCard AT ROW 7.46 COL 19 COLON-ALIGNED NO-LABEL WIDGET-ID 28
          VIEW-AS FILL-IN 
          SIZE 12.43 BY 1
     BUTTON-Vendedor AT ROW 8.54 COL 10 WIDGET-ID 66
     VtaCDocu.CodVen AT ROW 8.54 COL 19 COLON-ALIGNED NO-LABEL WIDGET-ID 42
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     BUTTON-Condicion AT ROW 9.62 COL 2 WIDGET-ID 68
     VtaCDocu.FmaPgo AT ROW 9.62 COL 19 COLON-ALIGNED NO-LABEL WIDGET-ID 20
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     f-NomVen AT ROW 8.54 COL 29 COLON-ALIGNED NO-LABEL WIDGET-ID 56
     f-CndVta AT ROW 9.62 COL 29 COLON-ALIGNED NO-LABEL WIDGET-ID 54
     VtaCDocu.FchPed AT ROW 1 COL 111 COLON-ALIGNED WIDGET-ID 14
          LABEL "Fecha de Emision"
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
     VtaCDocu.FchVen AT ROW 2.08 COL 111 COLON-ALIGNED WIDGET-ID 16
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
     VtaCDocu.FchEnt AT ROW 3.15 COL 111 COLON-ALIGNED WIDGET-ID 12
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
     VtaCDocu.NroOrd AT ROW 4.23 COL 111 COLON-ALIGNED WIDGET-ID 30
          VIEW-AS FILL-IN 
          SIZE 16.43 BY 1
     VtaCDocu.Usuario AT ROW 5.31 COL 111 COLON-ALIGNED WIDGET-ID 40
          LABEL "Digitado por"
          VIEW-AS FILL-IN 
          SIZE 16.43 BY 1
     VtaCDocu.Cmpbnte AT ROW 6.38 COL 111 COLON-ALIGNED WIDGET-ID 52
          LABEL "Comprobante"
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEMS "FAC","BOL" 
          DROP-DOWN-LIST
          SIZE 9 BY 1
     VtaCDocu.FlgIgv AT ROW 7.46 COL 113 WIDGET-ID 48
          LABEL "Incluido IGV"
          VIEW-AS TOGGLE-BOX
          SIZE 16 BY .77
     VtaCDocu.CodMon AT ROW 8.54 COL 113 NO-LABEL WIDGET-ID 44
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "S/.", 1,
"US$", 2
          SIZE 13 BY 1
     VtaCDocu.TpoCmb AT ROW 9.62 COL 111 COLON-ALIGNED WIDGET-ID 70
          VIEW-AS FILL-IN 
          SIZE 12.86 BY 1
     "Moneda:" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 8.81 COL 105 WIDGET-ID 50
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: INTEGRAL.VtaCDocu
   Allow: Basic,DB-Fields
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY COMPILE
   Temp-Tables and Buffers:
      TABLE: T-DDocu T "SHARED" NO-UNDO INTEGRAL VtaDDocu
   END-TABLES.
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
         HEIGHT             = 10.77
         WIDTH              = 140.43.
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
   NOT-VISIBLE FRAME-NAME Size-to-Fit Custom                            */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON BUTTON-Cliente IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-Condicion IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-Sede IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-Tarjeta IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-Vendedor IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX VtaCDocu.Cmpbnte IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN VtaCDocu.CodCli IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN VtaCDocu.DirCli IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN f-CndVta IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN f-Estado IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN f-NomVen IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN VtaCDocu.FchPed IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX VtaCDocu.FlgIgv IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN VtaCDocu.FmaPgo IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN VtaCDocu.NroPed IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN VtaCDocu.Usuario IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
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

&Scoped-define SELF-NAME BUTTON-Cliente
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Cliente V-table-Win
ON CHOOSE OF BUTTON-Cliente IN FRAME F-Main /* Cliente: */
DO:
  RUN vtagn/c-gn-clie-01 ('Clientes v�lidos').
  IF output-var-1 <> ? THEN DO:
      VtaCDocu.CodCli:SCREEN-VALUE = output-var-2.
      VtaCDocu.NomCli:SCREEN-VALUE = output-var-3.
      APPLY 'ENTRY':U TO VtaCDocu.CodCli.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Condicion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Condicion V-table-Win
ON CHOOSE OF BUTTON-Condicion IN FRAME F-Main /* Condicion de venta: */
DO:
    ASSIGN
        input-var-1 = s-cndvta-validos
        input-var-2 = ''
        input-var-3 = ''.
    RUN vta/d-cndvta.
    IF output-var-1 = ? THEN RETURN NO-APPLY.
    Vtacdocu.fmapgo:SCREEN-VALUE = output-var-2.
    f-cndvta:SCREEN-VALUE = output-var-3.
    APPLY 'ENTRY':U TO Vtacdocu.fmapgo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Sede
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Sede V-table-Win
ON CHOOSE OF BUTTON-Sede IN FRAME F-Main /* Sede: */
DO:
    ASSIGN
      input-var-1 = s-CodCli
      input-var-2 = Vtacdocu.NomCli:SCREEN-VALUE
      input-var-3 = ''
      output-var-1 = ?
      output-var-2 = ''
      output-var-3 = ''.
    RUN vta/c-clied.
    IF output-var-2 <> '' THEN DO:
        Vtacdocu.LugEnt:SCREEN-VALUE = output-var-2.
        VTacdocu.sede:SCREEN-VALUE = output-var-3.
        APPLY 'entry':U TO Vtacdocu.sede.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Tarjeta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Tarjeta V-table-Win
ON CHOOSE OF BUTTON-Tarjeta IN FRAME F-Main /* Tarjeta: */
DO:
    RUN lkup/c-gncard ('Tarjetas').
    IF output-var-1 <> ? THEN DO:
        VtaCDocu.NroCard:SCREEN-VALUE = output-var-2.
        APPLY 'ENTRY':U TO VtaCDocu.NroCard.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Vendedor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Vendedor V-table-Win
ON CHOOSE OF BUTTON-Vendedor IN FRAME F-Main /* Vendedor: */
DO:
    RUN lkup/c-vende ('Vendedor').
    IF output-var-1 <> ? THEN DO:
        VtaCDocu.CodVen:SCREEN-VALUE = output-var-2.
        f-NomVen:SCREEN-VALUE = output-var-3.
        APPLY 'ENTRY':U TO VtaCDocu.CodVen.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME VtaCDocu.Cmpbnte
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL VtaCDocu.Cmpbnte V-table-Win
ON VALUE-CHANGED OF VtaCDocu.Cmpbnte IN FRAME F-Main /* Comprobante */
DO:
  CASE INPUT {&self-name}:
      WHEN 'FAC' THEN DO:
          ASSIGN
              VtaCDocu.DniCli:SCREEN-VALUE = ''
              VtaCDocu.DniCli:SENSITIVE = NO
              VtaCDocu.NomCli:SENSITIVE = NO
              VtaCDocu.DirCli:SENSITIVE = NO.
          IF VtaCDocu.RucCli:SCREEN-VALUE = '' THEN DO:
              FIND gn-clie WHERE gn-clie.codcia = cl-codcia
                  AND gn-clie.codcli = Vtacdocu.codcli:SCREEN-VALUE 
                  NO-LOCK NO-ERROR.
              IF AVAILABLE gn-clie THEN VtaCDocu.RucCli:SCREEN-VALUE = gn-clie.ruc.
          END.
      END.
      WHEN 'BOL' THEN DO:
          ASSIGN
              VtaCDocu.RucCli:SCREEN-VALUE = ''
              VtaCDocu.DniCli:SENSITIVE = YES
              VtaCDocu.NomCli:SENSITIVE = YES
              VtaCDocu.DirCli:SENSITIVE = YES.
      END.
  END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME VtaCDocu.CodCli
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL VtaCDocu.CodCli V-table-Win
ON LEAVE OF VtaCDocu.CodCli IN FRAME F-Main /* Codigo */
DO:
    IF SELF:SCREEN-VALUE = '' THEN RETURN.
    FIND gn-clie WHERE gn-clie.CodCia = CL-CODCIA
      AND  gn-clie.CodCli = Vtacdocu.CodCli:SCREEN-VALUE 
      NO-LOCK NO-ERROR.
    IF NOT AVAILABLE gn-clie THEN DO:
       MESSAGE "Codigo de cliente no existe" VIEW-AS ALERT-BOX ERROR.
       RETURN NO-APPLY.
    END.
    /* BLOQUEO DEL CLIENTE */
    IF gn-clie.FlgSit = "I" THEN DO:
        MESSAGE "Cliente esta Inactivo" VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY.
    END.
    IF gn-clie.FlgSit = "C" THEN DO:
        MESSAGE "Cliente esta Cesado" VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY.
    END.
    /* Cargamos las condiciones de venta v�lidas */
    RUN vtagn/p-fmapgo-01 (gn-clie.codcli, OUTPUT s-cndvta-validos).

    DO WITH FRAME {&FRAME-NAME} :
       DISPLAY 
           gn-clie.NomCli @ Vtacdocu.NomCli
           gn-clie.Ruc    @ Vtacdocu.RucCli
           gn-clie.DirCli @ Vtacdocu.DirCli
           ENTRY(1, s-cndvta-validos) @ Vtacdocu.fmapgo. 
        ASSIGN
            S-CODCLI = Vtacdocu.CodCli:SCREEN-VALUE
            S-CNDVTA = Vtacdocu.FmaPgo:SCREEN-VALUE.
       IF gn-clie.CodCli <> FacCfgGn.CliVar THEN DO: 
           ASSIGN
               Vtacdocu.NomCli:SENSITIVE = NO
               Vtacdocu.RucCli:SENSITIVE = NO
               Vtacdocu.DirCli:SENSITIVE = NO.
       END.   
       ELSE DO: 
           ASSIGN
               Vtacdocu.NomCli:SENSITIVE = YES
               Vtacdocu.RucCli:SENSITIVE = YES
               Vtacdocu.DirCli:SENSITIVE = YES.
       END. 
       /* Ubica la Condicion Venta */
       FIND gn-convt WHERE gn-convt.Codig = Vtacdocu.FmaPgo:SCREEN-VALUE NO-LOCK NO-ERROR.
       IF AVAILABLE gn-convt 
       THEN F-CndVta:SCREEN-VALUE = gn-convt.Nombr.
       ELSE F-CndVta:SCREEN-VALUE = "".

       APPLY "VALUE-CHANGED":U TO VtaCDocu.Cmpbnte.
    END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME VtaCDocu.CodMon
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL VtaCDocu.CodMon V-table-Win
ON VALUE-CHANGED OF VtaCDocu.CodMon IN FRAME F-Main /* Cod!mon */
DO:
  s-codmon = INPUT {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME VtaCDocu.CodVen
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL VtaCDocu.CodVen V-table-Win
ON LEAVE OF VtaCDocu.CodVen IN FRAME F-Main
DO:
  FIND gn-ven WHERE gn-ven.codcia = s-codcia
      AND gn-ven.codven = SELF:SCREEN-VALUE 
      NO-LOCK NO-ERROR.
  IF AVAILABLE gn-ven 
  THEN f-nomven:SCREEN-VALUE = gn-ven.NomVen.
  ELSE f-nomven:SCREEN-VALUE = ''.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME VtaCDocu.FchEnt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL VtaCDocu.FchEnt V-table-Win
ON LEAVE OF VtaCDocu.FchEnt IN FRAME F-Main /* Fecha Entrega */
DO:
  IF INPUT {&self-name} < TODAY THEN RETURN NO-APPLY.
  IF ( INPUT {&self-name} - TODAY ) > 365 THEN DO:
      MESSAGE 'La fecha de entrega es de m�s de un a�o' SKIP
          'Continuamos?'
          VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
          UPDATE rpta AS LOG.
      IF rpta = NO THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME VtaCDocu.FlgIgv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL VtaCDocu.FlgIgv V-table-Win
ON VALUE-CHANGED OF VtaCDocu.FlgIgv IN FRAME F-Main /* Incluido IGV */
DO:
  s-FlgIgv = INPUT {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME VtaCDocu.FmaPgo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL VtaCDocu.FmaPgo V-table-Win
ON LEAVE OF VtaCDocu.FmaPgo IN FRAME F-Main /* Condicion de!venta */
DO:
  FIND gn-convt WHERE gn-convt.codig = SELF:SCREEN-VALUE 
      NO-LOCK NO-ERROR.
  IF AVAILABLE gn-convt
  THEN f-cndvta:SCREEN-VALUE = gn-ConVt.Nombr.
  ELSE f-cndvta:SCREEN-VALUE = ''.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME VtaCDocu.Sede
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL VtaCDocu.Sede V-table-Win
ON LEAVE OF VtaCDocu.Sede IN FRAME F-Main /* Sede */
DO:
  IF SELF:SCREEN-VALUE = '' THEN RETURN.
  FIND gn-clied WHERE gn-clied.codcia = cl-codcia
      AND Gn-ClieD.CodCli = s-codcli 
      AND gn-clied.sede = SELF:SCREEN-VALUE
      NO-LOCK NO-ERROR.
  IF AVAILABLE gn-clied
  THEN Vtacdocu.lugent:SCREEN-VALUE = Gn-ClieD.DirCli.
  ELSE Vtacdocu.lugent:SCREEN-VALUE = ''.
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
  {src/adm/template/row-list.i "VtaCDocu"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "VtaCDocu"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Borra-Detalle V-table-Win 
PROCEDURE Borra-Detalle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FOR EACH Vtaddocu OF Vtacdocu:
    DELETE Vtaddocu.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Borra-Temporal V-table-Win 
PROCEDURE Borra-Temporal :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FOR EACH T-DDOCU:
    DELETE T-DDOCU.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Carga-Temporal V-table-Win 
PROCEDURE Carga-Temporal :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FOR EACH Vtaddocu OF Vtacdocu NO-LOCK:
    CREATE T-DDOCU.
    BUFFER-COPY Vtaddocu TO T-DDOCU.
END.


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Graba-Detalle V-table-Win 
PROCEDURE Graba-Detalle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FOR EACH T-DDOCU:
    CREATE Vtaddocu.
    BUFFER-COPY T-DDOCU TO Vtaddocu
        ASSIGN
            Vtaddocu.codcia = Vtacdocu.codcia
            Vtaddocu.coddiv = Vtacdocu.coddiv
            Vtaddocu.codped = Vtacdocu.codped
            Vtaddocu.nroped = Vtacdocu.nroped.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Graba-Totales V-table-Win 
PROCEDURE Graba-Totales :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

ASSIGN
    Vtacdocu.impbrt = 0
    Vtacdocu.impdto = 0
    Vtacdocu.impexo = 0
    Vtacdocu.impvta = 0
    Vtacdocu.impigv = 0
    Vtacdocu.imptot = 0.

FOR EACH Vtaddocu OF Vtacdocu NO-LOCK:
    ASSIGN
        Vtacdocu.impbrt = Vtacdocu.impbrt + Vtaddocu.impbrt
        Vtacdocu.impdto = Vtacdocu.impdto + Vtaddocu.impdto
        Vtacdocu.impexo = Vtacdocu.impexo + Vtaddocu.impexo
        Vtacdocu.impvta = Vtacdocu.impvta + Vtaddocu.impvta
        Vtacdocu.impigv = Vtacdocu.impigv + Vtaddocu.impigv
        Vtacdocu.imptot = Vtacdocu.imptot + Vtaddocu.implin.
END.

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
  RUN Borra-Temporal.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  s-adm-new-record = 'YES'.

  {vtagn/i-v-cotcre-01.i}


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
  RUN GET-ATTRIBUTE ("ADM-NEW-RECORD").
  IF RETURN-VALUE = "YES" THEN DO:
      {vtagn/i-vtacordiv-01.i}
      ASSIGN
          Vtacdocu.codcia = s-codcia
          Vtacdocu.codped = s-codped
          Vtacdocu.coddiv = s-coddiv
          Vtacdocu.nroped = STRING(Vtacordiv.nroser, '999') + STRING(Vtacordiv.nrocor, '999999')
          VtaCDocu.FchCre = TODAY
          Vtacdocu.usuario = s-user-id.
      ASSIGN
          Vtacordiv.nrocor = Vtacordiv.nrocor + 1.
      Vtacdocu.PorIgv = IF s-FlgIgv THEN Faccfggn.porigv ELSE 0.
  END.
  ELSE DO:
      RUN Borra-Detalle.
  END.
  ASSIGN
      VtaCDocu.FchMod = TODAY
      VtaCDocu.UsrMod = s-user-id.

  RUN Graba-Detalle.
  RUN Graba-Totales.

  RUN Verifica-Cotizacion.

  RELEASE Vtaddocu.
  RELEASE Vtacordiv.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-cancel-record V-table-Win 
PROCEDURE local-cancel-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN Procesa-Handle IN lh_handle ('pagina1').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-copy-record V-table-Win 
PROCEDURE local-copy-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  RUN Borra-Temporal.
  RUN Carga-Temporal.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'copy-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  {vtagn/i-v-cotcre-01.i}

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
  IF Vtacdocu.FlgEst <> "X" THEN DO:
      MESSAGE "Acceso Denegado" VIEW-AS ALERT-BOX ERROR.
      RETURN "ADM-ERROR".
  END.

  /* Dispatch standard ADM method.                             */
  /*
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .
  */

  /* Code placed here will execute AFTER standard behavior.    */
  FIND CURRENT Vtacdocu EXCLUSIVE-LOCK NO-ERROR.
  IF NOT AVAILABLE Vtacdocu THEN RETURN "ADM-ERROR".
  ASSIGN
      VtaCDocu.FchAnu = TODAY
      VtaCDocu.FlgEst = "A"
      VtaCDocu.UsrAnu = s-user-id.
  FIND CURRENT Vtacdocu NO-LOCK NO-ERROR.
  RUN dispatch IN THIS-PROCEDURE ('display-fields').

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
  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN
          BUTTON-Cliente:SENSITIVE = NO
          BUTTON-Condicion:SENSITIVE = NO
          BUTTON-Sede:SENSITIVE = NO
          BUTTON-Tarjeta:SENSITIVE = NO
          BUTTON-Vendedor:SENSITIVE = NO.
  END.

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
  DEF VAR pEstado AS CHAR NO-UNDO.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF AVAILABLE Vtacdocu THEN DO WITH FRAME {&FRAME-NAME}:
      FIND gn-ven WHERE gn-ven.codcia = s-codcia
          AND gn-ven.codven = Vtacdocu.codven
          NO-LOCK NO-ERROR.
      IF AVAILABLE gn-ven 
      THEN f-nomven:SCREEN-VALUE = gn-ven.NomVen.
      ELSE f-nomven:SCREEN-VALUE = ''.
      FIND gn-convt WHERE gn-convt.codig = Vtacdocu.fmapgo
          NO-LOCK NO-ERROR.
      IF AVAILABLE gn-convt
      THEN f-cndvta:SCREEN-VALUE = gn-ConVt.Nombr.
      ELSE f-cndvta:SCREEN-VALUE = ''.
      RUN vtagn/p-vtacdocu-flgest (Vtacdocu.flgest, OUTPUT pEstado).
      f-Estado:SCREEN-VALUE = pEstado.
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
  IF AVAILABLE VtaCDocu THEN DO WITH FRAME {&FRAME-NAME}:
      ASSIGN
          VtaCDocu.FchPed:SENSITIVE = NO
          VtaCDocu.FchVen:SENSITIVE = NO
          VtaCDocu.CodVen:SENSITIVE = NO
          VtaCDocu.CodCli:SENSITIVE = NO
          VtaCDocu.DirCli:SENSITIVE = NO
          VtaCDocu.DniCli:SENSITIVE = NO
          VtaCDocu.FchPed:SENSITIVE = NO
          VtaCDocu.FchVen:SENSITIVE = NO
          VtaCDocu.NomCli:SENSITIVE = NO
          VtaCDocu.RucCli:SENSITIVE = NO
          VtaCDocu.TpoCmb:SENSITIVE = NO.
      ASSIGN
          BUTTON-Cliente:SENSITIVE = NO
          BUTTON-Vendedor:SENSITIVE = NO
          BUTTON-Condicion:SENSITIVE = YES
          BUTTON-Sede:SENSITIVE = YES
          BUTTON-Tarjeta:SENSITIVE = YES.
  END.

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
  IF RETURN-VALUE <> 'ADM-ERROR' THEN RUN Procesa-Handle IN lh_handle ('pagina1').

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
  {src/adm/template/snd-list.i "VtaCDocu"}

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

DEFINE VARIABLE F-TOT AS DECIMAL INIT 0 NO-UNDO.
DEFINE VARIABLE F-BOL AS DECIMAL INIT 0 NO-UNDO.

DO WITH FRAME {&FRAME-NAME} :
    FOR EACH T-DDOCU NO-LOCK:
        F-Tot = F-Tot + T-DDOCU.ImpLin.
    END.
    IF F-Tot = 0 THEN DO:
       MESSAGE "Importe total debe ser mayor a cero" VIEW-AS ALERT-BOX ERROR.
       RETURN "ADM-ERROR".   
    END.
    /* RHC 20.09.05 Transferencia gratuita */
    IF VtaCDocu.FmaPgo:SCREEN-VALUE = '900' AND VtaCDocu.NroCar:SCREEN-VALUE <> '' THEN DO:
        MESSAGE 'En caso de transferencia gratuita NO es v�lido el N� de Tarjeta' 
            VIEW-AS ALERT-BOX WARNING.
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
  IF Vtacdocu.FlgEst <> "X" THEN DO:
      MESSAGE "Acceso Denegado" VIEW-AS ALERT-BOX ERROR.
      RETURN "ADM-ERROR".
  END.
  RUN Carga-Temporal.
  RUN Procesa-Handle IN lh_handle ('pagina2').
  ASSIGN
      s-codcli = Vtacdocu.codcli
      s-codmon = Vtacdocu.codmon
      s-cndvta = Vtacdocu.fmapgo
      s-flgigv = Vtacdocu.flgigv
      s-porigv = Vtacdocu.porigv
      s-adm-new-record = 'NO'.
  RETURN "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Verifica-Cotizacion V-table-Win 
PROCEDURE Verifica-Cotizacion :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/* APROBACION AUTOMATICA */
VtaCDocu.FlgEst = IF s-FlgAprCot = YES THEN "P" ELSE "X".

/* CONSISTENCIAS FINALES */
IF Vtacdocu.flgest = "P" THEN DO:
    /* Margenes */
    FIND FIRST Vtaddocu OF Vtacdocu WHERE VtaDDocu.MrgUti <= 0 NO-LOCK NO-ERROR.
    IF AVAILABLE Vtaddocu THEN Vtacdocu.FlgEst = "X".
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

