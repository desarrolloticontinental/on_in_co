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
&Scoped-define EXTERNAL-TABLES PF-G004 PF-G003
&Scoped-define FIRST-EXTERNAL-TABLE PF-G004


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR PF-G004, PF-G003.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS PF-G004.User-Id 
&Scoped-define ENABLED-TABLES PF-G004
&Scoped-define FIRST-ENABLED-TABLE PF-G004
&Scoped-Define ENABLED-OBJECTS RECT-5 
&Scoped-Define DISPLAYED-FIELDS PF-G004.User-Id 
&Scoped-define DISPLAYED-TABLES PF-G004
&Scoped-define FIRST-DISPLAYED-TABLE PF-G004
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-Comentario FILL-IN-CodPer ~
FILL-IN-NomPer SELECT-grp-user SELECT-grp-aplic 

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
DEFINE BUTTON Btn-anade 
     LABEL "<< &Añadir" 
     SIZE 11 BY .85.

DEFINE BUTTON Btn-remove 
     LABEL "&Remover >>" 
     SIZE 11 BY .85.

DEFINE VARIABLE FILL-IN-CodPer AS CHARACTER FORMAT "x(6)":U 
     LABEL "Cod. en Planilla" 
     VIEW-AS FILL-IN 
     SIZE 11.72 BY .81 NO-UNDO.

DEFINE VARIABLE FILL-IN-Comentario AS CHARACTER FORMAT "X(256)":U 
     LABEL "Nombre" 
     VIEW-AS FILL-IN 
     SIZE 53.29 BY .81 NO-UNDO.

DEFINE VARIABLE FILL-IN-NomPer AS CHARACTER FORMAT "X(256)":U 
     LABEL "Nombre en Planilla" 
     VIEW-AS FILL-IN 
     SIZE 53 BY .81 NO-UNDO.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 67 BY 3.54.

DEFINE VARIABLE SELECT-grp-aplic AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 24.14 BY 2.69
     BGCOLOR 15 FGCOLOR 7  NO-UNDO.

DEFINE VARIABLE SELECT-grp-user AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 24.14 BY 2.69
     BGCOLOR 15 FGCOLOR 7  NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     PF-G004.User-Id AT ROW 1 COL 14 COLON-ALIGNED WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
     FILL-IN-Comentario AT ROW 1.77 COL 14 COLON-ALIGNED WIDGET-ID 10
     FILL-IN-CodPer AT ROW 2.54 COL 14 COLON-ALIGNED WIDGET-ID 6
     FILL-IN-NomPer AT ROW 3.31 COL 2.57 WIDGET-ID 8
     SELECT-grp-user AT ROW 5.04 COL 3 NO-LABEL WIDGET-ID 22
     SELECT-grp-aplic AT ROW 5.04 COL 40.29 NO-LABEL WIDGET-ID 20
     Btn-anade AT ROW 5.23 COL 28 WIDGET-ID 14
     Btn-remove AT ROW 6.19 COL 28 WIDGET-ID 16
     " Grupos de la Aplicación:" VIEW-AS TEXT
          SIZE 17.43 BY .5 AT ROW 4.27 COL 41 WIDGET-ID 24
     " Grupos Asignados:" VIEW-AS TEXT
          SIZE 13.72 BY .5 AT ROW 4.27 COL 3 WIDGET-ID 26
     RECT-5 AT ROW 4.46 COL 2 WIDGET-ID 18
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 4 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: INTEGRAL.PF-G004,INTEGRAL.PF-G003
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
         HEIGHT             = 11.54
         WIDTH              = 82.14.
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

/* SETTINGS FOR BUTTON Btn-anade IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON Btn-remove IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-CodPer IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-Comentario IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-NomPer IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR SELECTION-LIST SELECT-grp-aplic IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR SELECTION-LIST SELECT-grp-user IN FRAME F-Main
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

&Scoped-define SELF-NAME Btn-anade
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-anade V-table-Win
ON CHOOSE OF Btn-anade IN FRAME F-Main /* << Añadir */
DO:
    IF SELECT-grp-aplic:SCREEN-VALUE = ? THEN RETURN NO-APPLY.

    DEFINE VARIABLE estado AS LOGICAL.

    ASSIGN
        ESTADO = SELECT-grp-user:ADD-LAST(SELECT-grp-aplic:SCREEN-VALUE)
        ESTADO = SELECT-grp-aplic:DELETE(SELECT-grp-aplic:SCREEN-VALUE).

    IF Btn-remove:SENSITIVE = FALSE THEN ASSIGN Btn-remove:SENSITIVE = TRUE.
    IF SELECT-grp-aplic:LIST-ITEMS = ? THEN
        ASSIGN Btn-anade:SENSITIVE = FALSE.
    ELSE
        ASSIGN
            SELECT-grp-aplic:SCREEN-VALUE =
            SELECT-grp-aplic:ENTRY(SELECT-grp-aplic:NUM-ITEMS).
    ASSIGN
        SELECT-grp-user:SCREEN-VALUE =
        SELECT-grp-user:ENTRY(SELECT-grp-user:NUM-ITEMS).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-remove
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-remove V-table-Win
ON CHOOSE OF Btn-remove IN FRAME F-Main /* Remover >> */
DO:
    IF SELECT-grp-user:SCREEN-VALUE = ? THEN RETURN NO-APPLY.

    DEFINE VARIABLE estado AS LOGICAL.

    ASSIGN
        ESTADO = SELECT-grp-aplic:ADD-LAST(SELECT-grp-user:SCREEN-VALUE)
        ESTADO = SELECT-grp-user:DELETE(SELECT-grp-user:SCREEN-VALUE).

    IF Btn-anade:SENSITIVE = FALSE THEN ASSIGN Btn-anade:SENSITIVE = TRUE.
    IF SELECT-grp-user:LIST-ITEMS = ? THEN ASSIGN Btn-remove:SENSITIVE = FALSE.

    IF SELECT-grp-aplic:LIST-ITEMS = ? THEN
        ASSIGN Btn-remove:SENSITIVE = FALSE.
    ELSE
        ASSIGN
            SELECT-grp-user:SCREEN-VALUE =
            SELECT-grp-user:ENTRY(SELECT-grp-user:NUM-ITEMS).
    ASSIGN
        SELECT-grp-aplic:SCREEN-VALUE =
        SELECT-grp-aplic:ENTRY(SELECT-grp-aplic:NUM-ITEMS).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-CodPer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-CodPer V-table-Win
ON LEAVE OF FILL-IN-CodPer IN FRAME F-Main /* Cod. en Planilla */
DO:
    IF SELF:SCREEN-VALUE = '' THEN RETURN.
    ASSIGN
        SELF:SCREEN-VALUE = STRING(INTEGER(SELF:SCREEN-VALUE), '999999') NO-ERROR.
    FIND pl-pers WHERE pl-pers.codper = SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
    IF NOT AVAILABLE pl-pers THEN DO:
        MESSAGE 'Código de planilla NO registrado' VIEW-AS ALERT-BOX ERROR.
        SELF:SCREEN-VALUE = ''.
        RETURN NO-APPLY.
    END.
    FILL-IN-NomPer:SCREEN-VALUE = TRIM(pl-pers.patper) + ' ' +
        TRIM(pl-pers.matper) + ', ' + pl-pers.nomper.
    FIND FIRST gn-users WHERE gn-users.codcia = s-codcia
        AND gn-users.codper = SELF:SCREEN-VALUE
        AND gn-user.DISABLED = YES
        NO-LOCK NO-ERROR.
    IF AVAILABLE gn-user THEN DO:
        MESSAGE 'Personal ya no trabaja en la empresa' VIEW-AS ALERT-BOX ERROR.
        SELF:SCREEN-VALUE = ''.
        RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-CodPer V-table-Win
ON LEFT-MOUSE-DBLCLICK OF FILL-IN-CodPer IN FRAME F-Main /* Cod. en Planilla */
OR f8 OF FILL-IN-CodPer
DO:
  ASSIGN
      input-var-1 = ''
      input-var-2 = ''
      input-var-3 = ''
      output-var-1 = ?.
  RUN pln/c-plnper.
  IF output-var-1 <> ? THEN SELF:SCREEN-VALUE = output-var-2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME SELECT-grp-aplic
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SELECT-grp-aplic V-table-Win
ON MOUSE-SELECT-DBLCLICK OF SELECT-grp-aplic IN FRAME F-Main
DO:
    IF Btn-anade:SENSITIVE = TRUE THEN APPLY "CHOOSE" TO Btn-anade.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME SELECT-grp-user
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SELECT-grp-user V-table-Win
ON MOUSE-SELECT-DBLCLICK OF SELECT-grp-user IN FRAME F-Main
DO:
    IF Btn-remove:SENSITIVE = TRUE THEN APPLY "CHOOSE" TO Btn-remove.
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
  {src/adm/template/row-list.i "PF-G004"}
  {src/adm/template/row-list.i "PF-G003"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "PF-G004"}
  {src/adm/template/row-find.i "PF-G003"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Carga-Grupos V-table-Win 
PROCEDURE Carga-Grupos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    ASSIGN
        SELECT-grp-user:LIST-ITEMS IN FRAME {&FRAME-NAME} = ""
        SELECT-grp-aplic:LIST-ITEMS IN FRAME {&FRAME-NAME} = "".
    IF NOT AVAILABLE PF-G004 THEN RETURN.

    DEFINE VARIABLE i      AS INTEGER.
    DEFINE VARIABLE estado AS LOGICAL.

    ASSIGN
        SELECT-grp-user:LIST-ITEMS IN FRAME {&FRAME-NAME} = PF-G004.Seguridad
        SELECT-grp-aplic:LIST-ITEMS IN FRAME {&FRAME-NAME} = "".

    IF PF-G003.Grupos = "" THEN RETURN.
    IF SELECT-grp-user:LIST-ITEMS = ? THEN DO:
        ASSIGN
            SELECT-grp-aplic:LIST-ITEMS = PF-G003.Grupos
            SELECT-grp-aplic:SCREEN-VALUE = SELECT-grp-aplic:ENTRY(1).
    END.
    ELSE DO:
        DO i = 1 TO NUM-ENTRIES(PF-G003.Grupos):
            IF LOOKUP(ENTRY(i, PF-G003.Grupos), SELECT-grp-user:LIST-ITEMS) = 0 THEN DO:
                ESTADO = SELECT-grp-aplic:ADD-LAST(ENTRY(i, PF-G003.Grupos)).
            END.
        END.
        IF SELECT-grp-aplic:LIST-ITEMS <> ? THEN 
            SELECT-grp-aplic:SCREEN-VALUE = SELECT-grp-aplic:ENTRY(1).
        ASSIGN
            SELECT-grp-user:SCREEN-VALUE = SELECT-grp-user:ENTRY(1).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Carga-Usuario V-table-Win 
PROCEDURE Carga-Usuario :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    IF NOT AVAILABLE PF-G004 THEN RETURN.

    ASSIGN
        FILL-IN-Comentario = ""
        FILL-IN-CodPer = ''
        FILL-IN-NomPer = ''.
    FIND DICTDB._user WHERE DICTDB._user._userid = PF-G004.User-Id NO-LOCK NO-ERROR.
    IF AVAILABLE DICTDB._user THEN ASSIGN FILL-IN-Comentario = DICTDB._user._user-name.
    FIND FIRST gn-users WHERE gn-user.codcia = s-codcia
        AND gn-users.User-Id = PF-G004.User-Id
        NO-LOCK NO-ERROR.
    IF AVAILABLE gn-users THEN DO:
        FILL-IN-CodPer = gn-users.codper.
        FIND pl-pers WHERE pl-pers.codper = gn-users.codper NO-LOCK NO-ERROR.
        IF AVAILABLE pl-pers THEN FILL-IN-NomPer = TRIM(pl-pers.patper) + ' ' +
        TRIM(pl-pers.matper) + ', ' + pl-pers.nomper.
    END.
    DISPLAY
        FILL-IN-Comentario
        FILL-IN-CodPer FILL-IN-NomPer
        WITH FRAME F-Main.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Graba-Usuario V-table-Win 
PROCEDURE Graba-Usuario :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    FIND FIRST DICTDB._user WHERE DICTDB._user._userid = PF-G004.User-Id NO-ERROR.
    IF NOT AVAILABLE DICTDB._user THEN DO:
        CREATE DICTDB._user.
        ASSIGN
            DICTDB._user._userid = PF-G004.User-Id
            DICTDB._User._Password = ENCODE("").
    END.
    ASSIGN DICTDB._user._user-name = FILL-IN-Comentario.

    FIND FIRST gn-users WHERE gn-users.codcia = s-codcia
        AND gn-users.USER-ID = DICTDB._user._userid
        NO-ERROR.
    IF NOT AVAILABLE gn-users THEN DO:
        CREATE gn-users.
        ASSIGN
            gn-users.CodCia = s-codcia
            gn-users.Create-Date = DATETIME(TODAY, MTIME)
            gn-users.User-Id = DICTDB._user._userid
            gn-users.User-Name = DICTDB._user._user-name.
    END.
    ASSIGN
        gn-users.CodPer = FILL-IN-CodPer.

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
  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN
          FILL-IN-CodPer FILL-IN-Comentario FILL-IN-NomPer.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-statement':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN
        PF-G004.Aplic-Id = PF-G003.Aplic-Id.
  RUN Graba-Usuario.

  ASSIGN PF-G004.Seguridad = SELECT-grp-user:LIST-ITEMS IN FRAME {&FRAME-NAME}.

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
       FILL-IN-CodPer:SENSITIVE = NO.
       FILL-IN-Comentario:SENSITIVE = NO.
       Btn-anade:SENSITIVE = NO.
       Btn-remove:SENSITIVE = NO.
       SELECT-grp-aplic:SENSITIVE = NO.
       SELECT-grp-user:SENSITIVE = NO.
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
  DO WITH FRAME {&FRAME-NAME}:
      RUN Carga-Usuario.
      RUN Carga-Grupos.
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
      FILL-IN-CodPer:SENSITIVE = YES.
      FILL-IN-Comentario:SENSITIVE = YES.
      Btn-anade:SENSITIVE = YES.
      Btn-remove:SENSITIVE = YES.
      SELECT-grp-aplic:SENSITIVE = YES.
      SELECT-grp-user:SENSITIVE = YES.
      RUN GET-ATTRIBUTE('ADM-NEW-RECORD').
      IF RETURN-VALUE = 'NO' THEN PF-G004.User-Id:SENSITIVE = NO.
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
  {src/adm/template/snd-list.i "PF-G004"}
  {src/adm/template/snd-list.i "PF-G003"}

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
    IF INPUT PF-G004.User-Id = "" THEN DO:
        MESSAGE "No se permiten usuarios en blanco" VIEW-AS ALERT-BOX ERROR.
        APPLY "ENTRY" TO PF-G004.User-Id.
        RETURN 'ADM-ERROR'.
    END.
    RUN GET-ATTRIBUTE('ADM-NEW-RECORD').
    IF RETURN-VALUE = 'YES' THEN DO:
        IF CAN-FIND(FIRST PF-G004 WHERE PF-G004.Aplic-Id = PF-G003.Aplic-Id
                    AND PF-G004.User-Id = INPUT PF-G004.User-Id) 
            THEN DO:
            MESSAGE "Usuario ya registrado para esta aplicación"
                VIEW-AS ALERT-BOX ERROR.
            APPLY "ENTRY" TO PF-G004.User-Id.
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
RETURN "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

