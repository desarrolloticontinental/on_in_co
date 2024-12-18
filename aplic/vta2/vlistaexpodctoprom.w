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

DEF VAR x-PreOfi AS DEC NO-UNDO.

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
&Scoped-define EXTERNAL-TABLES VtaListaMay Almmmatg
&Scoped-define FIRST-EXTERNAL-TABLE VtaListaMay


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR VtaListaMay, Almmmatg.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS VtaListaMay.PromFchD VtaListaMay.PromFchH ~
VtaListaMay.Libre_d01 VtaListaMay.Libre_d03 
&Scoped-define ENABLED-TABLES VtaListaMay
&Scoped-define FIRST-ENABLED-TABLE VtaListaMay
&Scoped-Define ENABLED-OBJECTS RECT-66 RECT-68 
&Scoped-Define DISPLAYED-FIELDS VtaListaMay.CodDiv VtaListaMay.PromFchD ~
VtaListaMay.PromFchH VtaListaMay.Libre_d01 VtaListaMay.Libre_d02 ~
VtaListaMay.Libre_d03 
&Scoped-define DISPLAYED-TABLES VtaListaMay
&Scoped-define FIRST-DISPLAYED-TABLE VtaListaMay
&Scoped-Define DISPLAYED-OBJECTS f-Precio-1 f-Precio-2 f-Precio-3 

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
DEFINE VARIABLE f-Precio-1 AS DECIMAL FORMAT "->>,>>9.9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 11.72 BY .81
     BGCOLOR 15 FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE f-Precio-2 AS DECIMAL FORMAT "->>,>>9.9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 11.72 BY .81
     BGCOLOR 15 FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE f-Precio-3 AS DECIMAL FORMAT "->>,>>9.9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 11.72 BY .81
     BGCOLOR 15 FGCOLOR 1  NO-UNDO.

DEFINE RECTANGLE RECT-66
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 82 BY 1.08.

DEFINE RECTANGLE RECT-68
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 82 BY 3.23.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     VtaListaMay.CodDiv AT ROW 2.35 COL 1 COLON-ALIGNED NO-LABEL WIDGET-ID 22
          VIEW-AS FILL-IN 
          SIZE 9 BY .81
     VtaListaMay.PromFchD AT ROW 2.35 COL 11 COLON-ALIGNED NO-LABEL WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 12.86 BY .81
     VtaListaMay.PromFchH AT ROW 2.35 COL 26 COLON-ALIGNED NO-LABEL WIDGET-ID 6
          VIEW-AS FILL-IN 
          SIZE 12.86 BY .81
     VtaListaMay.Libre_d01 AT ROW 2.35 COL 52 COLON-ALIGNED WIDGET-ID 26
          LABEL "Cliente VIP" FORMAT ">>9.99"
          VIEW-AS FILL-IN 
          SIZE 8.57 BY .81
     f-Precio-1 AT ROW 2.35 COL 68 COLON-ALIGNED NO-LABEL WIDGET-ID 16
     VtaListaMay.Libre_d02 AT ROW 3.15 COL 52 COLON-ALIGNED WIDGET-ID 28
          LABEL "Mesa Redonda" FORMAT ">>9.99"
          VIEW-AS FILL-IN 
          SIZE 8.57 BY .81
     f-Precio-2 AT ROW 3.15 COL 68 COLON-ALIGNED NO-LABEL WIDGET-ID 34
     VtaListaMay.Libre_d03 AT ROW 3.96 COL 52 COLON-ALIGNED WIDGET-ID 30
          LABEL "Otros Clientes" FORMAT ">>9.99"
          VIEW-AS FILL-IN 
          SIZE 8.57 BY .81
     f-Precio-3 AT ROW 3.96 COL 68 COLON-ALIGNED NO-LABEL WIDGET-ID 36
     "Divisi�n" VIEW-AS TEXT
          SIZE 8 BY .5 AT ROW 1.27 COL 3 WIDGET-ID 24
     "Precio" VIEW-AS TEXT
          SIZE 8 BY .5 AT ROW 1.27 COL 70 WIDGET-ID 14
     "Inicio" VIEW-AS TEXT
          SIZE 8 BY .5 AT ROW 1.27 COL 13 WIDGET-ID 8
     "Fin" VIEW-AS TEXT
          SIZE 8 BY .5 AT ROW 1.27 COL 28 WIDGET-ID 10
     "% Descuento" VIEW-AS TEXT
          SIZE 10 BY .5 AT ROW 1.27 COL 53 WIDGET-ID 12
     RECT-66 AT ROW 1 COL 2 WIDGET-ID 18
     RECT-68 AT ROW 2.08 COL 2 WIDGET-ID 32
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 4 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: INTEGRAL.VtaListaMay,INTEGRAL.Almmmatg
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
         HEIGHT             = 4.65
         WIDTH              = 83.72.
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

/* SETTINGS FOR FILL-IN VtaListaMay.CodDiv IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN f-Precio-1 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN f-Precio-2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN f-Precio-3 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN VtaListaMay.Libre_d01 IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN VtaListaMay.Libre_d02 IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN VtaListaMay.Libre_d03 IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
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

&Scoped-define SELF-NAME f-Precio-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL f-Precio-1 V-table-Win
ON LEAVE OF f-Precio-1 IN FRAME F-Main
DO:
    IF DECIMAL(SELF:SCREEN-VALUE) = 0 THEN DO:
        VtaListaMay.Libre_d01:SCREEN-VALUE = '0'.
        RETURN.
    END.
    Libre_d01:SCREEN-VALUE = STRING ( ROUND ( ( 1 -  DECIMAL(SELF:SCREEN-VALUE) / x-PreOfi  ) * 100, 4 ) ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME f-Precio-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL f-Precio-2 V-table-Win
ON LEAVE OF f-Precio-2 IN FRAME F-Main
DO:
    IF DECIMAL(SELF:SCREEN-VALUE) = 0 THEN DO:
        VtaListaMay.Libre_d02:SCREEN-VALUE = '0'.
        RETURN.
    END.
    Libre_d02:SCREEN-VALUE = STRING ( ROUND ( ( 1 -  DECIMAL(SELF:SCREEN-VALUE) / x-PreOfi  ) * 100, 4 ) ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME f-Precio-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL f-Precio-3 V-table-Win
ON LEAVE OF f-Precio-3 IN FRAME F-Main
DO:
    IF DECIMAL(SELF:SCREEN-VALUE) = 0 THEN DO:
        VtaListaMay.Libre_d03:SCREEN-VALUE = '0'.
        RETURN.
    END.
    Libre_d03:SCREEN-VALUE = STRING ( ROUND ( ( 1 -  DECIMAL(SELF:SCREEN-VALUE) / x-PreOfi  ) * 100, 4 ) ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME VtaListaMay.Libre_d01
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL VtaListaMay.Libre_d01 V-table-Win
ON LEAVE OF VtaListaMay.Libre_d01 IN FRAME F-Main /* Cliente VIP */
DO:
    F-PRECIO-1 = ROUND(x-PreOfi * ( 1 - ( DECI(SELF:SCREEN-VALUE) / 100 ) ),4).
    IF DECIMAL(SELF:SCREEN-VALUE) = 0 THEN F-PRECIO-1 = 0.
    DISPLAY F-PRECIO-1 WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME VtaListaMay.Libre_d02
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL VtaListaMay.Libre_d02 V-table-Win
ON LEAVE OF VtaListaMay.Libre_d02 IN FRAME F-Main /* Mesa Redonda */
DO:
    F-PRECIO-2 = ROUND(x-PreOfi * ( 1 - ( DECI(SELF:SCREEN-VALUE) / 100 ) ),4).
    IF DECIMAL(SELF:SCREEN-VALUE) = 0 THEN F-PRECIO-2 = 0.
    DISPLAY F-PRECIO-2 WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME VtaListaMay.Libre_d03
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL VtaListaMay.Libre_d03 V-table-Win
ON LEAVE OF VtaListaMay.Libre_d03 IN FRAME F-Main /* Otros Clientes */
DO:
    F-PRECIO-3 = ROUND(x-PreOfi * ( 1 - ( DECI(SELF:SCREEN-VALUE) / 100 ) ),4).
    IF DECIMAL(SELF:SCREEN-VALUE) = 0 THEN F-PRECIO-3 = 0.
    DISPLAY F-PRECIO-3 WITH FRAME {&FRAME-NAME}.
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
  {src/adm/template/row-list.i "VtaListaMay"}
  {src/adm/template/row-list.i "Almmmatg"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "VtaListaMay"}
  {src/adm/template/row-find.i "Almmmatg"}

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
  IF (VtaListaMay.Libre_d01 = 0 AND VtaListaMay.Libre_d02 = 0 AND VtaListaMay.Libre_d03 = 0) OR
      VtaListaMay.PromFchD = ? OR 
      VtaListaMay.PromFchH = ? THEN 
      ASSIGN
            VtaListaMay.Libre_d01 = 0
            VtaListaMay.Libre_d02 = 0
            VtaListaMay.Libre_d03 = 0
            VtaListaMay.PromFchD = ?
            VtaListaMay.PromFchH = ?.
  ASSIGN
      VtaListaMay.PromDto = VtaListaMay.Libre_d03.      /* Valor por Defecto */
  RUN dispatch IN THIS-PROCEDURE ('display-fields':U).

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
      f-Precio-1:SENSITIVE = NO.
      f-Precio-2:SENSITIVE = NO.
      f-Precio-3:SENSITIVE = NO.
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
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF AVAILABLE VtaListaMay THEN DO WITH FRAME {&FRAME-NAME}:
      x-PreOfi = VtaListaMay.PreOfi.
      IF Almmmatg.MonVta = 2 THEN x-PreOfi = x-PreOfi * Almmmatg.TpoCmb.
      FIND Almmmatg OF VtaListaMay NO-LOCK NO-ERROR.
      F-PRECIO-1 = ROUND(x-PreOfi * ( 1 - ( VtaListaMay.Libre_d01 / 100 ) ),4).
      F-PRECIO-2 = ROUND(x-PreOfi * ( 1 - ( VtaListaMay.Libre_d02 / 100 ) ),4).
      F-PRECIO-3 = ROUND(x-PreOfi * ( 1 - ( VtaListaMay.Libre_d03 / 100 ) ),4).
      IF VtaListaMay.Libre_d01 = 0 THEN F-PRECIO-1 = 0.
      IF VtaListaMay.Libre_d02 = 0 THEN F-PRECIO-2 = 0.
      IF VtaListaMay.Libre_d03 = 0 THEN F-PRECIO-3 = 0.
      DISPLAY F-PRECIO-1 F-PRECIO-2 F-PRECIO-3 WITH FRAME {&FRAME-NAME}.
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
      f-Precio-1:SENSITIVE = YES.
      f-Precio-2:SENSITIVE = YES.
      f-Precio-3:SENSITIVE = YES.
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

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Margen-de-Utilidad V-table-Win 
PROCEDURE Margen-de-Utilidad :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF INPUT PARAMETER pCodDiv AS CHAR.
DEF INPUT PARAMETER pCodMat AS CHAR.
DEF INPUT PARAMETER pPreUni AS DEC.
DEF INPUT PARAMETER pUndVta AS CHAR.
DEF INPUT PARAMETER pTpoCmb AS DEC.
DEF OUTPUT PARAMETER x-Limite AS DEC.
DEF OUTPUT PARAMETER pError AS CHAR.

DEF VAR x-Margen AS DEC NO-UNDO.    /* Margen de utilidad */

pError = ''.

RUN vtagn/p-margen-utilidad-v2 (pCodDiv,
                                pCodMat,
                                pPreUni,
                                pUndVta,
                                1,                      /* Moneda */
                                pTpoCmb,
                                YES,                     /* Muestra error? */
                                "",                     /* Almac�n */
                                OUTPUT x-Margen,        /* Margen de utilidad */
                                OUTPUT x-Limite,        /* Margen m�nimo de utilidad */
                                OUTPUT pError           /* Control de errores: "OK" "ADM-ERROR" */
                                ).

IF RETURN-VALUE = 'ADM-ERROR' THEN pError = 'ADM-ERROR'.

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
  {src/adm/template/snd-list.i "VtaListaMay"}
  {src/adm/template/snd-list.i "Almmmatg"}

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
    DEF VAR x-Limite AS DEC NO-UNDO.
    DEF VAR pError AS CHAR NO-UNDO.
    RUN Margen-de-Utilidad (VtaListaMay.CodDiv,
                            VtaListaMay.codmat,
                            DECIMAL (f-Precio-1:SCREEN-VALUE),
                            VtaListaMay.Chr__01,
                            1,
                            OUTPUT x-Limite,        /* Margen m�nimo de utilidad */
                            OUTPUT pError           /* Control de errores: "OK" "ADM-ERROR" */
                            ).
    IF pError = "ADM-ERROR" THEN DO:
        APPLY 'entry' TO f-Precio-1.
        RETURN "ADM-ERROR".
    END.
    RUN Margen-de-Utilidad (VtaListaMay.CodDiv,
                            VtaListaMay.codmat,
                            DECIMAL (f-Precio-2:SCREEN-VALUE),
                            VtaListaMay.Chr__01,
                            1,
                            OUTPUT x-Limite,        /* Margen m�nimo de utilidad */
                            OUTPUT pError           /* Control de errores: "OK" "ADM-ERROR" */
                            ).
    IF pError = "ADM-ERROR" THEN DO:
        APPLY 'entry' TO f-Precio-2.
        RETURN "ADM-ERROR".
    END.
    RUN Margen-de-Utilidad (VtaListaMay.CodDiv,
                            VtaListaMay.codmat,
                            DECIMAL (f-Precio-3:SCREEN-VALUE),
                            VtaListaMay.Chr__01,
                            1,
                            OUTPUT x-Limite,        /* Margen m�nimo de utilidad */
                            OUTPUT pError           /* Control de errores: "OK" "ADM-ERROR" */
                            ).
    IF pError = "ADM-ERROR" THEN DO:
        APPLY 'entry' TO f-Precio-3.
        RETURN "ADM-ERROR".
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
RETURN "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

