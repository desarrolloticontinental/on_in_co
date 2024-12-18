&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          integral         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE T-GENER NO-UNDO LIKE TabGener.
DEFINE NEW SHARED TEMP-TABLE t-report NO-UNDO LIKE w-report.



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
DEF INPUT PARAMETER pCodMat AS CHAR.

/* Local Variable Definitions ---                                       */
DEF SHARED VAR s-codcia AS INT.

FIND Almmmatg WHERE Almmmatg.codcia = s-codcia
    AND Almmmatg.codmat = pCodMat
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE Almmmatg THEN RETURN ERROR.

DEF NEW SHARED VAR s-CodMat AS CHAR.

s-CodMat = pCodMat.

DEFINE TEMP-TABLE tmp-tabla
    FIELD t-CodAlm LIKE Almacen.codalm  FORMAT 'x(3)'
    FIELD t-CodDoc LIKE FacDPedi.CodDoc FORMAT "XXX"
    FIELD t-Nroped LIKE FacDPedi.NroPed FORMAT "XXX-XXXXXXXX"
    FIELD t-CodDiv LIKE FacCPedi.CodDiv FORMAT 'x(5)'
    FIELD t-FchPed LIKE FacDPedi.FchPed
    FIELD t-NomCli LIKE FacCPedi.NomCli COLUMN-LABEL "Cliente" FORMAT "x(35)"
    FIELD t-CodMat LIKE FacDPedi.codmat
    FIELD t-Canped LIKE FacDPedi.CanPed.

SESSION:SET-WAIT-STATE('GENERAL').
RUN Carga-Temporal.
SESSION:SET-WAIT-STATE('').

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME D-Dialog

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fComprasTransito D-Dialog 
FUNCTION fComprasTransito RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fStockComprometido D-Dialog 
FUNCTION fStockComprometido RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fStockTransito D-Dialog 
FUNCTION fStockTransito RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_b-asigna-res-zg AS HANDLE NO-UNDO.
DEFINE VARIABLE h_q-lima-dtopromv2 AS HANDLE NO-UNDO.

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     SPACE(117.18) SKIP(7.77)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "RESUMEN FALTANTE/SOBRANTE" WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: T-GENER T "?" NO-UNDO INTEGRAL TabGener
      TABLE: t-report T "NEW SHARED" NO-UNDO INTEGRAL w-report
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
ON WINDOW-CLOSE OF FRAME D-Dialog /* RESUMEN FALTANTE/SOBRANTE */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
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
  DEFINE VARIABLE adm-current-page  AS INTEGER NO-UNDO.

  RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
  ASSIGN adm-current-page = INTEGER(RETURN-VALUE).

  CASE adm-current-page: 

    WHEN 0 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'aplic/alm/b-asigna-res-zg.w':U ,
             INPUT  FRAME D-Dialog:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-asigna-res-zg ).
       RUN set-position IN h_b-asigna-res-zg ( 1.27 , 2.00 ) NO-ERROR.
       RUN set-size IN h_b-asigna-res-zg ( 7.00 , 114.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'aplic/vta2/q-lima-dtopromv2.w':U ,
             INPUT  FRAME D-Dialog:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_q-lima-dtopromv2 ).
       RUN set-position IN h_q-lima-dtopromv2 ( 1.00 , 7.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.50 , 7.72 ) */

       /* Links to SmartBrowser h_b-asigna-res-zg. */
       RUN add-link IN adm-broker-hdl ( h_q-lima-dtopromv2 , 'Record':U , h_b-asigna-res-zg ).

       /* Adjust the tab order of the smart objects. */
    END. /* Page 0 */

  END CASE.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Carga-Temporal D-Dialog 
PROCEDURE Carga-Temporal :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FOR EACH Almmmate NO-LOCK WHERE Almmmate.codcia = s-codcia 
    AND Almmmate.codmat = pCodMat,
    FIRST Almacen OF Almmmate NO-LOCK,
    FIRST TabGener WHERE TabGener.CodCia = Almmmate.CodCia
        AND TabGener.Clave = "ZG"
        AND TabGener.Libre_c01 = Almmmate.CodAlm,
    FIRST Almtabla NO-LOCK WHERE Almtabla.tabla = Tabgener.clave
        AND Almtabla.codigo = Tabgener.codigo:
    CREATE T-GENER.
    BUFFER-COPY Tabgener TO T-GENER.
    ASSIGN
        T-GENER.libre_c02 = Almacen.Descripcion
        T-GENER.libre_c03 = pCodMat
        T-GENER.libre_d01 = Almmmate.StkAct - fStockComprometido().     /*Almmmate.StkComprometido.*/
    ASSIGN
        T-GENER.libre_d02 = fStockTransito().
    ASSIGN
        T-GENER.Libre_d04 = fComprasTransito().
    /* Faltante/Sobrante */
    ASSIGN
        T-GENER.libre_d03 = (T-GENER.libre_d01 + T-GENER.libre_d02 + T-GENER.Libre_d04) - Almmmate.StkMin.
END.

DEF BUFFER B-AlmTabla FOR AlmTabla.
EMPTY TEMP-TABLE t-report.
FOR EACH T-GENER NO-LOCK WHERE T-GENER.CodCia = s-CodCia
    AND T-GENER.Clave = "ZG"
    AND T-GENER.Libre_c03 = pCodMat,
    FIRST Almmmate NO-LOCK WHERE Almmmate.codcia = T-GENER.CodCia
    AND Almmmate.CodAlm = T-GENER.Libre_c01
    AND Almmmate.CodMat = T-GENER.Libre_c03:
    FIND FIRST B-AlmTabla WHERE B-AlmTabla.Tabla = T-GENER.Clave
        AND B-AlmTabla.Codigo = T-GENER.Codigo
        NO-LOCK NO-ERROR.
    IF AVAILABLE B-AlmTabla THEN DO:
        FIND t-report WHERE t-report.Llave-C = B-AlmTabla.Codigo 
            AND t-report.Campo-C[10] = pCodMat
            NO-ERROR.
        IF NOT AVAILABLE t-report THEN CREATE t-report.
        ASSIGN
            t-report.Llave-C    = B-AlmTabla.Codigo
            t-report.Campo-C[10]= pCodMat 
            t-report.Campo-C[1] = CAPS(B-AlmTabla.Nombre)
            t-report.Campo-F[1] = t-report.Campo-F[1] + T-GENER.libre_d03.  /* Faltante/Sobrante */
        /* Datos Resumen */
        ASSIGN
            t-report.Campo-F[2] = t-report.Campo-F[2] + Almmmate.StockMax     /* Maximo */
            t-report.Campo-F[3] = t-report.Campo-F[3] + Almmmate.StockSeg     /* Seguridad */
            t-report.Campo-F[4] = t-report.Campo-F[4] + Almmmate.StkMin     /* Max + Seg */
            t-report.Campo-F[5] = t-report.Campo-F[5] + T-GENER.Libre_d01   /* Disponible */
            t-report.Campo-F[6] = t-report.Campo-F[6] + T-GENER.Libre_d02   /* Tr�nsito */
            t-report.Campo-F[7] = t-report.Campo-F[7] + T-GENER.Libre_d04   /* Compra Tr�nsito */
            .
    END.
END.
FOR EACH t-report :
    IF t-report.Llave-C = '' THEN DELETE t-report.
END.
FOR EACH t-report:
    FIND FIRST T-GENER WHERE T-GENER.Codigo = t-report.Llave-C
        AND T-GENER.Libre_L01 = YES
        AND T-GENER.Libre_C03 = t-report.Campo-C[10]
        NO-LOCK NO-ERROR.
    IF AVAILABLE T-GENER THEN DO:
        FIND Almmmate WHERE Almmmate.codcia = s-codcia
            AND Almmmate.codalm = T-GENER.Libre_C01
            AND Almmmate.codmat = T-GENER.Libre_C03
            NO-LOCK NO-ERROR.
        IF AVAILABLE Almmmate THEN t-report.Campo-F[10] = Almmmate.stkmax.
    END.
END.


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

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fComprasTransito D-Dialog 
FUNCTION fComprasTransito RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  FIND OOComPend WHERE OOComPend.CodAlm = Almmmate.codalm
      AND OOComPend.CodMat = Almmmate.codmat
      NO-LOCK NO-ERROR.
  IF AVAILABLE OOComPend THEN RETURN (OOComPend.CanPed - OOComPend.CanAte).
  ELSE RETURN 0.00.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fStockComprometido D-Dialog 
FUNCTION fStockComprometido RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

DEF VAR xStockComprometido AS DEC NO-UNDO.                                                                             
RUN gn/stock-comprometido-v2.p (Almmmate.CodMat, Almmmate.CodAlm, NO, OUTPUT xStockComprometido).
RETURN xStockComprometido.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fStockTransito D-Dialog 
FUNCTION fStockTransito RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    /* En Tr�nsito */
    DEF VAR x-Total AS DEC NO-UNDO.
    RUN alm\p-articulo-en-transito (
        Almmmate.CodCia,
        Almmmate.CodAlm,
        Almmmate.CodMat,
        INPUT-OUTPUT TABLE tmp-tabla,
        OUTPUT x-Total).

    RETURN x-Total.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

