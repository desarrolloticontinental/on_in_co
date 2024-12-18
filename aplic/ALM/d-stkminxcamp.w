&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
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
DEF TEMP-TABLE T-MATE LIKE Almmmate.

DEF INPUT PARAMETER pCodMat AS CHAR.
DEF INPUT-OUTPUT PARAMETER TABLE FOR T-MATE.
                                      
DEF SHARED VAR s-codcia AS INT.

FIND Almmmatg WHERE Almmmatg.codcia = s-codcia
    AND Almmmatg.codmat = pCodMat
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE Almmmatg THEN RETURN.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME D-Dialog

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN-1 FILL-IN-2 Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-1 FILL-IN-2 

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

DEFINE VARIABLE FILL-IN-1 AS DECIMAL FORMAT ">>>,>>9.99":U INITIAL 0 
     LABEL "Cantidad CAMPA�A" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 10 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE FILL-IN-2 AS DECIMAL FORMAT ">>>,>>9.99":U INITIAL 0 
     LABEL "Cantidad NO CAMPA�A" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 14 FGCOLOR 0  NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     FILL-IN-1 AT ROW 1.77 COL 24 COLON-ALIGNED WIDGET-ID 2
     FILL-IN-2 AT ROW 2.92 COL 24 COLON-ALIGNED WIDGET-ID 4
     Btn_OK AT ROW 4.65 COL 8
     Btn_Cancel AT ROW 4.65 COL 26
     SPACE(24.13) SKIP(0.61)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "<insert SmartDialog title>"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Other Settings: COMPILE
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
   FRAME-NAME Custom                                                    */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX D-Dialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* <insert SmartDialog title> */
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
    ASSIGN
        FILL-IN-1 FILL-IN-2.
   RUN Distribuye-cantidades.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Distribuye-cantidades D-Dialog 
PROCEDURE Distribuye-cantidades :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FOR EACH T-MATE:
    T-MATE.VCtMn1 = 0.
    T-MATE.VCtMn2 = 0.
END.
DEF VAR x-A-Repartir AS DEC NO-UNDO.
FOR EACH TabGener NO-LOCK WHERE TabGener.codcia = s-codcia
    AND TabGener.clave = '%REPOXLCNC'
    AND ENTRY(1,TabGener.codigo,'|') = Almmmatg.codfam
    AND ENTRY(2,TabGener.codigo,'|') = Almmmatg.subfam:
    /* Se reparte entre todos los almacenes y lo que falta se 
    agrega al almac�n que tiene la divisi�n 00000 */
    FIND FIRST T-MATE WHERE T-MATE.codmat = Almmmatg.codmat
        AND CAN-FIND(Almacen OF T-MATE WHERE Almacen.coddiv = ENTRY(3,TabGener.codigo,'|')
                     NO-LOCK)
        NO-ERROR.
    IF NOT AVAILABLE T-MATE THEN NEXT.
    CASE Almmmatg.TpoMrg:
        WHEN '1' THEN DO:       /* Mayorista */
            /* Campa�a */
            x-A-Repartir = INTEGER(FILL-IN-1 * TabGener.Parametro[2] / 100).
            T-MATE.VCtMn1 = x-A-repartir.
            /* No Campa�a */
            x-A-Repartir = INTEGER(FILL-IN-2 * TabGener.Parametro[5] / 100).
            T-MATE.VCtMn2 = x-A-repartir.
        END.
        WHEN '2' THEN DO:       /* Minorista */
            /* Campa�a */
            x-A-Repartir = INTEGER(FILL-IN-1 * TabGener.Parametro[3] / 100).
            T-MATE.VCtMn1 = x-A-repartir.
            /* No Campa�a */
            x-A-Repartir = INTEGER(FILL-IN-2 * TabGener.Parametro[6] / 100).
            T-MATE.VCtMn2 = x-A-repartir.
        END.
        OTHERWISE DO:           /* Ambos */
            /* Campa�a */
            x-A-Repartir = INTEGER(FILL-IN-1 * TabGener.Parametro[1] / 100).
            T-MATE.VCtMn1 = x-A-repartir.
            /* No Campa�a */
            x-A-Repartir = INTEGER(FILL-IN-2 * TabGener.Parametro[4] / 100).
            T-MATE.VCtMn2 = x-A-repartir.
        END.
    END CASE.
END.
/* SI queda un saldo se va a la divisi�n 00000 */
DEF VAR x-VCtMn1 AS DEC NO-UNDO.
DEF VAR x-VCtMn2 AS DEC NO-UNDO.
FOR EACH T-MATE:
    ASSIGN
        x-VCtMn1 = x-VCtMn1 + T-MATE.VCtMn1
        x-VCtMn2 = x-VCtMn2 + T-MATE.VCtMn2.
END.
FIND FIRST T-MATE WHERE CAN-FIND(FIRST Almacen WHERE Almacen.codcia = T-MATE.codcia
                                 AND Almacen.codalm = T-MATE.codalm
                                 AND Almacen.coddiv = '00000'
                                 NO-LOCK)
    NO-ERROR.
IF AVAILABLE T-MATE THEN
    ASSIGN
    T-MATE.VCtMn1 = T-MATE.VCtMn1 + (FILL-IN-1 - x-VCtMn1)
    T-MATE.VCtMn2 = T-MATE.VCtMn2 + (FILL-IN-2 - x-VCtMn2).

/* Actualizamos Tablas */
FOR EACH T-MATE:
  {lib/lock-generico.i &Tabla="Almmmate" &Condicion="Almmmate.codcia = T-MATE.codcia AND ~
      Almmmate.codalm = T-MATE.codalm AND Almmmate.codmat = T-MATE.codmat" ~
      &Bloqueo="EXCLUSIVE-LOCK" &Accion="RETRY" &Mensaje="YES" ~
      &TipoError=""ADM-ERROR""}

  ASSIGN
      Almmmate.VCtMn1 = T-MATE.VCtMn1 
      Almmmate.VCtMn2 = T-MATE.VCtMn2.

  /* Actualizamos el m�nimo de acuerdo a si estamos en Campa�a o no */
  DEF VAR EsCampana AS LOG NO-UNDO.
  /* Buscamos si es o no campa�a */
  EsCampana = NO.
  rloop:
  FOR EACH VtaTabla WHERE VtaTabla.CodCia = Almmmate.codcia
      AND VtaTabla.Tabla = "CAMPA�AS" NO-LOCK:
      IF TODAY >= VtaTabla.Rango_fecha[1] AND TODAY <= VtaTabla.Rango_fecha[2] THEN DO:
          EsCampana = YES.
          LEAVE rloop.
      END.
  END.
  IF EsCampana = NO  AND Almmmate.VCtMn2 > 0 THEN Almmmate.StkMin = Almmmate.VCtMn2.
  IF EsCampana = YES AND Almmmate.VCtMn1 > 0 THEN Almmmate.StkMin = Almmmate.VCtMn1.
  RELEASE Almmmate.
END.

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
  DISPLAY FILL-IN-1 FILL-IN-2 
      WITH FRAME D-Dialog.
  ENABLE FILL-IN-1 FILL-IN-2 Btn_OK Btn_Cancel 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
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

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartDialog, and there are no
     tables specified in any contained Browse, Query, or Frame. */

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

