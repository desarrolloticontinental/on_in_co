&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          integral         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog


/* Temp-Table and Buffer definitions                                    */
DEFINE BUFFER B-RutaC FOR DI-RutaC.
DEFINE BUFFER B-RutaD FOR DI-RutaD.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
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

  Description: from cntnrdlg.w - ADM SmartDialog Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
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

DEF INPUT PARAMETER pCodPHR AS CHAR.
DEF INPUT PARAMETER pNroPHR AS CHAR.
DEF OUTPUT PARAMETER pMensaje AS CHAR NO-UNDO.

FIND DI-RutaC WHERE DI-RutaC.CodCia = s-CodCia AND 
    DI-RutaC.CodDiv = s-CodDiv AND 
    DI-RutaC.CodDoc = pCodPHR AND 
    DI-RutaC.NroDoc = pNroPHR
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE DI-RutaC THEN RETURN.
IF NOT LOOKUP(DI-RutaC.FlgEst, 'PF,PX,PK') > 0 THEN RETURN.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME D-Dialog
&Scoped-define BROWSE-NAME BROWSE-7

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES AlmCDocu FacCPedi

/* Definitions for BROWSE BROWSE-7                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-7 FacCPedi.CodDoc FacCPedi.NroPed ~
FacCPedi.FchPed FacCPedi.CodCli FacCPedi.NomCli 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-7 
&Scoped-define QUERY-STRING-BROWSE-7 FOR EACH AlmCDocu ~
      WHERE AlmCDocu.CodCia = s-codcia ~
 AND AlmCDocu.CodDoc = COMBO-BOX_CodDoc ~
 AND AlmCDocu.FlgEst = "C" ~
 AND AlmCDocu.CodLlave = s-coddiv ~
 AND AlmCDocu.FchDoc >= (TODAY - 30) ~
 NO-LOCK, ~
      FIRST FacCPedi WHERE FacCPedi.CodCia = AlmCDocu.CodCia ~
  AND FacCPedi.CodDoc = AlmCDocu.CodDoc ~
  AND FacCPedi.NroPed = AlmCDocu.NroDoc NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-7 OPEN QUERY BROWSE-7 FOR EACH AlmCDocu ~
      WHERE AlmCDocu.CodCia = s-codcia ~
 AND AlmCDocu.CodDoc = COMBO-BOX_CodDoc ~
 AND AlmCDocu.FlgEst = "C" ~
 AND AlmCDocu.CodLlave = s-coddiv ~
 AND AlmCDocu.FchDoc >= (TODAY - 30) ~
 NO-LOCK, ~
      FIRST FacCPedi WHERE FacCPedi.CodCia = AlmCDocu.CodCia ~
  AND FacCPedi.CodDoc = AlmCDocu.CodDoc ~
  AND FacCPedi.NroPed = AlmCDocu.NroDoc NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-7 AlmCDocu FacCPedi
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-7 AlmCDocu
&Scoped-define SECOND-TABLE-IN-QUERY-BROWSE-7 FacCPedi


/* Definitions for DIALOG-BOX D-Dialog                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-D-Dialog ~
    ~{&OPEN-QUERY-BROWSE-7}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS COMBO-BOX_CodDoc BROWSE-7 Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS COMBO-BOX_CodDoc 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.15
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.15
     BGCOLOR 8 .

DEFINE VARIABLE COMBO-BOX_CodDoc AS CHARACTER FORMAT "X(256)":U INITIAL "O/D" 
     LABEL "Documento" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Orden de Despacho","O/D",
                     "Orden de Transferencia","OTR"
     DROP-DOWN-LIST
     SIZE 19 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-7 FOR 
      AlmCDocu, 
      FacCPedi SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-7 D-Dialog _STRUCTURED
  QUERY BROWSE-7 NO-LOCK DISPLAY
      FacCPedi.CodDoc FORMAT "x(3)":U
      FacCPedi.NroPed COLUMN-LABEL "Numero Pedido" FORMAT "X(12)":U
            WIDTH 11.14
      FacCPedi.FchPed COLUMN-LABEL "Fch. Pedido" FORMAT "99/99/9999":U
            WIDTH 9.43
      FacCPedi.CodCli COLUMN-LABEL "Cliente" FORMAT "x(11)":U WIDTH 11.43
      FacCPedi.NomCli FORMAT "x(100)":U WIDTH 66.72
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 109 BY 16.42
         FONT 4 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     COMBO-BOX_CodDoc AT ROW 1.27 COL 19 COLON-ALIGNED WIDGET-ID 2
     BROWSE-7 AT ROW 2.62 COL 3 WIDGET-ID 200
     Btn_OK AT ROW 19.58 COL 3
     Btn_Cancel AT ROW 19.58 COL 19
     SPACE(94.56) SKIP(0.57)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 4
         TITLE "SELECCIONE ORDENES A REPROGRAMAR"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: B-RutaC B "?" ? INTEGRAL DI-RutaC
      TABLE: B-RutaD B "?" ? INTEGRAL DI-RutaD
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB D-Dialog 
/* ************************* Included-Libraries *********************** */

{src/adm-vm/method/vmviewer.i}
{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX D-Dialog
   FRAME-NAME                                                           */
/* BROWSE-TAB BROWSE-7 COMBO-BOX_CodDoc D-Dialog */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-7
/* Query rebuild information for BROWSE BROWSE-7
     _TblList          = "INTEGRAL.AlmCDocu,INTEGRAL.FacCPedi WHERE INTEGRAL.AlmCDocu ..."
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _TblOptList       = ", FIRST"
     _Where[1]         = "INTEGRAL.AlmCDocu.CodCia = s-codcia
 AND INTEGRAL.AlmCDocu.CodDoc = COMBO-BOX_CodDoc
 AND AlmCDocu.FlgEst = ""C""
 AND INTEGRAL.AlmCDocu.CodLlave = s-coddiv
 AND INTEGRAL.AlmCDocu.FchDoc >= (TODAY - 30)
"
     _JoinCode[2]      = "FacCPedi.CodCia = AlmCDocu.CodCia
  AND FacCPedi.CodDoc = AlmCDocu.CodDoc
  AND FacCPedi.NroPed = AlmCDocu.NroDoc"
     _FldNameList[1]   = INTEGRAL.FacCPedi.CodDoc
     _FldNameList[2]   > INTEGRAL.FacCPedi.NroPed
"FacCPedi.NroPed" "Numero Pedido" ? "character" ? ? ? ? ? ? no ? no no "11.14" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > INTEGRAL.FacCPedi.FchPed
"FacCPedi.FchPed" "Fch. Pedido" ? "date" ? ? ? ? ? ? no ? no no "9.43" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > INTEGRAL.FacCPedi.CodCli
"FacCPedi.CodCli" "Cliente" ? "character" ? ? ? ? ? ? no ? no no "11.43" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > INTEGRAL.FacCPedi.NomCli
"FacCPedi.NomCli" ? ? "character" ? ? ? ? ? ? no ? no no "66.72" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BROWSE-7 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX D-Dialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* SELECCIONE ORDENES A REPROGRAMAR */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK D-Dialog
ON CHOOSE OF Btn_OK IN FRAME D-Dialog /* OK */
DO:

  RUN MASTER-TRANSACTION (OUTPUT pMensaje).
  IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
      MESSAGE pMensaje VIEW-AS ALERT-BOX ERROR.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME COMBO-BOX_CodDoc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL COMBO-BOX_CodDoc D-Dialog
ON VALUE-CHANGED OF COMBO-BOX_CodDoc IN FRAME D-Dialog /* Documento */
DO:
  ASSIGN {&self-name}.
  {&OPEN-QUERY-{&BROWSE-NAME}}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-7
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */
ON FIND OF Faccpedi DO:
    CASE Faccpedi.CodDoc:
        WHEN "O/D" THEN DO:
            DEF VAR LocalRastreo AS LOG NO-UNDO.
            IF NOT(faccpedi.divdes = s-coddiv AND faccpedi.flgest = 'C') THEN RETURN ERROR.
            LocalRastreo = NO.
            FOR EACH ccbcdocu NO-LOCK WHERE ccbcdocu.codcia = s-codcia
                AND ccbcdocu.codped = faccpedi.codref
                AND ccbcdocu.nroped = faccpedi.nroref
                AND ccbcdocu.coddoc = 'G/R'
                AND ccbcdocu.libre_c01 = faccpedi.coddoc
                AND ccbcdocu.libre_c02 = faccpedi.nroped
                AND ccbcdocu.flgest <> 'A':
                /* Rastreamos que NO est� en ninguna HR */
                FIND FIRST di-rutad WHERE di-rutad.codcia = s-codcia
                    AND di-rutad.coddoc = 'H/R'
                    AND di-rutad.codref = ccbcdocu.coddoc
                    AND di-rutad.nroref = ccbcdocu.nrodoc
                    AND CAN-FIND(FIRST di-rutac OF di-rutad WHERE di-rutac.flgest <> 'A' NO-LOCK)
                    NO-LOCK NO-ERROR.
                IF NOT AVAILABLE di-rutad THEN DO:
                    /* Buscamos que la FAC NO tenga PI */
                    IF CAN-FIND(FIRST Almcmov WHERE Almcmov.codcia = s-codcia
                                AND Almcmov.tipmov = "I"
                                AND Almcmov.codmov = 09
                                AND Almcmov.codref = Ccbcdocu.codref
                                AND Almcmov.nroref = Ccbcdocu.nroref
                                AND Almcmov.flgest <> 'A' NO-LOCK)
                        THEN NEXT.
                    LocalRastreo = YES.
                    LEAVE.
                END.
            END.
            IF LocalRastreo = NO THEN RETURN ERROR.
            /* NO debe tener P.I. */
            /* NO debe estar en ninguna PHR */
            FIND Di-RutaD WHERE Di-RutaD.codcia = s-codcia
                AND Di-RutaD.coddiv = s-coddiv
                AND Di-RutaD.coddoc = "PHR"
                AND Di-RutaD.codref = Faccpedi.coddoc
                AND Di-RutaD.nroref = Faccpedi.nroped
                AND CAN-FIND(FIRST Di-RutaC OF Di-RutaD WHERE LOOKUP(Di-RutaC.flgest, "PX,PK,PF") > 0 NO-LOCK)
                NO-LOCK NO-ERROR.
            IF AVAILABLE Di-RutaD THEN RETURN ERROR.

/*           FIND FIRST Almcdocu USE-INDEX Llave01 WHERE Almcdocu.codcia = s-codcia AND                     */
/*               Almcdocu.codllave = s-coddiv AND                                                           */
/*               Almcdocu.coddoc = Faccpedi.coddoc AND                                                      */
/*               Almcdocu.nrodoc = Faccpedi.nroped AND                                                      */
/*               Almcdocu.flgest = "C"                                                                      */
/*               NO-LOCK NO-ERROR.                                                                          */
/*           IF NOT AVAILABLE Almcdocu THEN RETURN ERROR.                                                   */
/*           FOR EACH Di-RutaD NO-LOCK WHERE DI-RutaD.CodCia = s-CodCia                                     */
/*                   AND DI-RutaD.CodDoc = AlmCDocu.Libre_c01    /* H/R */                                  */
/*                   AND DI-RutaD.NroDoc = AlmCDocu.Libre_c02                                               */
/*                   AND DI-RutaD.CodRef = "G/R"                                                            */
/*                   AND ( (DI-RutaD.FlgEst = "N" AND DI-RutaD.Libre_c02 = "R") OR DI-RutaD.FlgEst = "T" ), */
/*               FIRST Ccbcdocu NO-LOCK WHERE Ccbcdocu.codcia = s-CodCia                                    */
/*                   AND Ccbcdocu.coddoc = DI-RutaD.CodRef        /* G/R */                                 */
/*                   AND Ccbcdocu.nrodoc = DI-RutaD.NroRef                                                  */
/*                   AND Ccbcdocu.libre_c01 = AlmCDocu.CodDoc     /* O/D */                                 */
/*                   AND Ccbcdocu.libre_c02 = AlmCDocu.NroDoc:                                              */
/*               IF CAN-FIND(FIRST Di-RutaD WHERE Di-RutaD.codcia = s-codcia AND                            */
/*                           Di-RutaD.codref = Ccbcdocu.coddoc AND                                          */
/*                           Di-RutaD.nroref = Ccbcdocu.nrodoc AND                                          */
/*                           Di-RutaD.coddoc = AlmCDocu.Libre_c01 AND                                       */
/*                           Di-RutaD.nrodoc <> AlmCDocu.Libre_c02 NO-LOCK)                                 */
/*                   THEN RETURN ERROR.                                                                     */
/*           END.                                                                                           */
        END.
    END CASE.
    FIND FIRST B-RutaD WHERE B-RutaD.codcia = s-codcia AND
        B-RutaD.coddiv = s-coddiv AND
        B-RutaD.coddoc = pCodPHR AND
        B-RutaD.CodRef = Faccpedi.CodDoc AND
        B-RutaD.NroRef = Faccpedi.NroPed AND
        CAN-FIND(FIRST B-RutaC OF B-RutaD WHERE LOOKUP(B-RutaC.FlgEst, 'PX,PK,PF,P') > 0 NO-LOCK)
        NO-LOCK NO-ERROR.
    IF AVAILABLE B-RutaD THEN RETURN ERROR.
END.

{src/adm/template/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects D-Dialog  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available D-Dialog  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI D-Dialog  _DEFAULT-DISABLE
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
  HIDE FRAME D-Dialog.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI D-Dialog  _DEFAULT-ENABLE
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
  DISPLAY COMBO-BOX_CodDoc 
      WITH FRAME D-Dialog.
  ENABLE COMBO-BOX_CodDoc BROWSE-7 Btn_OK Btn_Cancel 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Graba-Registro D-Dialog 
PROCEDURE Graba-Registro :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF OUTPUT PARAMETER pMensaje AS CHAR NO-UNDO.

DO TRANSACTION ON ERROR UNDO, RETURN 'ADM-ERROR' ON STOP UNDO, RETURN 'ADM-ERROR':
    /* Verificamos que NO est� en otra PHR en tr�mite */
    FIND FIRST DI-RutaD OF DI-RutaC WHERE DI-RutaD.CodRef = Faccpedi.CodDoc AND
        DI-RutaD.NroDoc = Faccpedi.NroPed NO-LOCK NO-ERROR.
    IF AVAILABLE DI-RutaD THEN DO:
        pMensaje = "Documento Repetido".
        UNDO, RETURN 'ADM-ERROR'.
    END.
    /* NO debe reprogramarse mas de cuatro veces */
    DEF VAR iVeces AS INT NO-UNDO.
    FOR EACH B-RutaD NO-LOCK WHERE B-RutaD.codcia = s-codcia AND
        B-RutaD.coddiv = s-coddiv AND
        B-RutaD.coddoc = pCodPHR AND
        B-RutaD.CodRef = Faccpedi.CodDoc AND
        B-RutaD.NroRef = Faccpedi.NroPed AND
        CAN-FIND(FIRST B-RutaC OF B-RutaD WHERE B-RutaC.FlgEst = "C" NO-LOCK):
        iVeces = iVeces + 1.
    END.
    IF iVeces >= 4 THEN DO:
        MESSAGE 'NO se puede reprogramar mas de cuatro veces' VIEW-AS ALERT-BOX ERROR.
        UNDO, RETURN 'ADM-ERROR'.
    END.
    /* *************************************** */
    /* Grabamos */
    CREATE DI-RutaD.
    BUFFER-COPY DI-RutaC TO DI-RutaD
        ASSIGN
        DI-RutaD.CodRef = Faccpedi.CodDoc 
        DI-RutaD.NroRef = Faccpedi.NroPed.
    RELEASE DI-RutaD.
END.
RETURN 'OK'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MASTER-TRANSACTION D-Dialog 
PROCEDURE MASTER-TRANSACTION :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF OUTPUT PARAMETER pMensaje AS CHAR NO-UNDO.

DEF VAR k AS INTE NO-UNDO.

DO TRANSACTION ON ERROR UNDO, RETURN 'ADM-ERROR' ON STOP UNDO, RETURN 'ADM-ERROR' WITH FRAME {&FRAME-NAME}:
    /* Verificamos que la PHR sea la correcta */
    {lib/lock-genericov3.i ~
        &Tabla="DI-RutaC" ~
        &Condicion="DI-RutaC.CodCia = s-CodCia AND ~
        DI-RutaC.CodDiv = s-CodDiv AND ~
        DI-RutaC.CodDoc = pCodPHR AND ~
        DI-RutaC.NroDoc = pNroPHR" ~
        &Bloqueo="EXCLUSIVE-LOCK NO-ERROR" ~
        &Accion="RETRY" ~
        &Mensaje="NO" ~
        &TxtMensaje="pMensaje" ~
        &TipoError="UNDO, RETURN 'ADM-ERROR'"}
    IF NOT LOOKUP(DI-RutaC.FlgEst, 'PF,PX,PK') > 0 THEN DO:
        pMensaje = "La " + pCodPHR + " " + pNroPHR + " ya no es v�lida" + CHR(10) +
            "Estado = " + DI-RutaC.FlgEst.
        UNDO, RETURN 'ADM-ERROR'.
    END.
    /* LOGICA PRINCIPAL */
    DO k = 1 TO {&BROWSE-NAME}:NUM-SELECTED-ROWS:
        IF {&BROWSE-NAME}:FETCH-SELECTED-ROW(k) THEN DO:
            FIND FIRST B-RutaD WHERE B-RutaD.codcia = s-codcia AND
                B-RutaD.coddiv = s-coddiv AND
                B-RutaD.coddoc = pCodPHR AND
                B-RutaD.CodRef = Faccpedi.CodDoc AND
                B-RutaD.NroRef = Faccpedi.NroPed AND
                CAN-FIND(FIRST B-RutaC OF B-RutaD WHERE B-RutaC.FlgEst = "C" NO-LOCK)
                NO-LOCK NO-ERROR.
            IF AVAILABLE B-RutaD THEN DO:
                MESSAGE 'El documento'  B-RutaD.CodRef B-RutaD.NroRef 'YA ha sido reprogramado anteriormente' SKIP
                    B-RutaD.CodDoc B-RutaD.NroDoc SKIP
                    'Continuamos?' VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
                    UPDATE rpta AS LOG.
                IF rpta = NO THEN NEXT.
            END.
            RUN Graba-Registro (OUTPUT pMensaje).
            IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
                IF TRUE <> (pMensaje > '') THEN pMensaje = 'ERROR al grabar la orden'.
                UNDO, RETURN 'ADM-ERROR'.
            END.
        END.
    END.
END.

RETURN 'OK'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE procesa-parametros D-Dialog 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE recoge-parametros D-Dialog 
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
        WHEN "" THEN ASSIGN input-var-1 = "".
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records D-Dialog  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "AlmCDocu"}
  {src/adm/template/snd-list.i "FacCPedi"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed D-Dialog 
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

