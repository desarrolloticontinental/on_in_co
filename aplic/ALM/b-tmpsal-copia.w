&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          integral         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File:  

  Description: from BROWSER.W - Basic SmartBrowser Object Template

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

/* Local Variable Definitions --- */
DEFINE SHARED VAR S-USER-ID AS CHAR. 
DEFINE SHARED VAR S-CODCIA  AS INTEGER. 
DEFINE SHARED VAR S-CODALM  AS CHAR. 
DEFINE VARIABLE F-Equivale AS DECIMAL NO-UNDO.
DEFINE VARIABLE S-Unidad   AS CHAR NO-UNDO.
DEFINE VARIABLE S-UndStk   AS CHAR NO-UNDO.
DEFINE VARIABLE S-CODART   AS CHAR NO-UNDO.
DEFINE SHARED TEMP-TABLE ITEM LIKE almdmov.
DEFINE BUFFER DMOV       FOR  ITEM.
DEFINE VARIABLE S-UndBas   AS CHAR NO-UNDO.

DEFINE VAR stkdes LIKE Almmmate.StkAct.
DEFINE VAR SW AS LOGICAL.

DEFINE BUFFER B-Almmmate FOR Almmmate.
DEFINE SHARED VARIABLE C-CODALM  AS CHAR.
DEFINE SHARED VARIABLE ORDTRB    AS CHAR.

DEF VAR s-local-new-record AS CHAR INIT 'NO'.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br_table

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ITEM Almmmate Almmmatg

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table ITEM.codmat Almmmate.DesMat Almmmatg.DesMar ITEM.CodUnd Almmmate.Stkact stkdes ITEM.CanDes   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table ITEM.codmat /*   ITEM.CodUnd*/ ~
ITEM.CanDes   
&Scoped-define ENABLED-TABLES-IN-QUERY-br_table ITEM
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br_table ITEM
&Scoped-define SELF-NAME br_table
&Scoped-define QUERY-STRING-br_table FOR EACH ITEM WHERE ~{&KEY-PHRASE} NO-LOCK, ~
             FIRST Almmmate OF ITEM NO-LOCK, ~
             FIRST Almmmatg OF Almmmate NO-LOCK
&Scoped-define OPEN-QUERY-br_table OPEN QUERY {&SELF-NAME} FOR EACH ITEM WHERE ~{&KEY-PHRASE} NO-LOCK, ~
             FIRST Almmmate OF ITEM NO-LOCK, ~
             FIRST Almmmatg OF Almmmate NO-LOCK.
&Scoped-define TABLES-IN-QUERY-br_table ITEM Almmmate Almmmatg
&Scoped-define FIRST-TABLE-IN-QUERY-br_table ITEM
&Scoped-define SECOND-TABLE-IN-QUERY-br_table Almmmate
&Scoped-define THIRD-TABLE-IN-QUERY-br_table Almmmatg


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br_table 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" B-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Advanced Query Options" B-table-Win _INLINE
/* Actions: ? adm/support/advqedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
</KEY-OBJECT>
<SORTBY-OPTIONS>
</SORTBY-OPTIONS> 
<SORTBY-RUN-CODE>
************************
* Set attributes related to SORTBY-OPTIONS */
RUN set-attribute-list (
    'SortBy-Options = ""':U).
/************************
</SORTBY-RUN-CODE> 
<FILTER-ATTRIBUTES>
</FILTER-ATTRIBUTES> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      ITEM, 
      Almmmate, 
      Almmmatg SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _FREEFORM
  QUERY br_table NO-LOCK DISPLAY
      ITEM.codmat COLUMN-LABEL "Codigo!Articulo" FORMAT "x(14)" WIDTH 6.43
      Almmmate.DesMat FORMAT "X(47)" COLUMN-LABEL "D E S C R I P C I O N"
      Almmmatg.DesMar FORMAT "X(15)" COLUMN-LABEL "MARCA"
      ITEM.CodUnd FORMAT 'x(7)' COLUMN-LABEL "Unidad" WIDTH 6.43
      Almmmate.Stkact   FORMAT '(ZZ,ZZZ,ZZ9.999)'
      stkdes            COLUMN-LABEL "Stock!Pedido" FORMAT "(Z,ZZZ,ZZ9.99)"
      ITEM.CanDes       FORMAT "(Z,ZZZ,ZZ9.999)"
  ENABLE
      ITEM.codmat
/*      ITEM.CodUnd*/
      ITEM.CanDes
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 87 BY 12.5
         FONT 4.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br_table AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 4.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
   Allow: Basic,Browse
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
  CREATE WINDOW B-table-Win ASSIGN
         HEIGHT             = 12.69
         WIDTH              = 88.72.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}
{src/adm-vm/method/vmviewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit                                              */
/* BROWSE-TAB br_table 1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       br_table:NUM-LOCKED-COLUMNS IN FRAME F-Main     = 2.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _START_FREEFORM

OPEN QUERY {&SELF-NAME} FOR EACH ITEM WHERE ~{&KEY-PHRASE} NO-LOCK,
      FIRST Almmmate OF ITEM NO-LOCK,
      FIRST Almmmatg OF Almmmate NO-LOCK.
     _END_FREEFORM
     _Options          = "NO-LOCK KEY-PHRASE"
     _Query            is NOT OPENED
*/  /* BROWSE br_table */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME br_table
&Scoped-define SELF-NAME br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-ENTRY OF br_table IN FRAME F-Main
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}
  /* Display de Datos Adicionales */
  /*
  DO WITH FRAME {&FRAME-NAME}:
     ITEM.CodUnd:READ-ONLY = YES.
  END. 
  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-LEAVE OF br_table IN FRAME F-Main
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
   {src/adm/template/brsleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON VALUE-CHANGED OF br_table IN FRAME F-Main
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  DEFINICION DE TRIGGER  *************************** */

ON "RETURN":U OF ITEM.CodMat, /*ITEM.CodUnd,*/ ITEM.CanDes
DO:
   APPLY "TAB":U.
   RETURN NO-APPLY.
END.

ON "MOUSE-SELECT-DBLCLICK":U OF ITEM.Codmat
DO:
    input-var-1 = S-CODALM.
    input-var-2 = ITEM.Codmat:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}.
    FIND Almtfami WHERE Almtfami.codcia = S-CODCIA AND
         Almtfami.codfam = input-var-2  NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Almtfami THEN input-var-2 = ''.
    RUN lkup\c-artalm.r ('Articulos por Almacen').
    IF output-var-1 <> ? THEN
       ITEM.Codmat:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = output-var-2.
END.

ON "LEAVE":U OF ITEM.CodMat 
DO:
    IF s-local-new-record = 'NO' THEN RETURN.
    DEF VAR s-Ok AS LOG.

   IF ITEM.CodMat:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = "" THEN RETURN.

   DEF VAR pCodMat AS CHAR NO-UNDO.
   pCodMat = SELF:SCREEN-VALUE.
   RUN vta2/p-codigo-producto (INPUT-OUTPUT pCodMat, YES).
   IF pCodMat = '' THEN RETURN NO-APPLY.
   SELF:SCREEN-VALUE = pCodMat.

   S-CODART = STRING(INTEGER(ITEM.CodMat:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}),"999999").
   ITEM.CodMat:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = S-CODART.
   FIND Almmmate WHERE Almmmate.CodCia = S-CODCIA 
                  AND  Almmmate.CodAlm = S-CODALM 
                  AND  Almmmate.CodMat = S-CODART 
                 NO-LOCK NO-ERROR.
   IF NOT AVAILABLE Almmmate THEN DO:
      MESSAGE "Material no asignado a este Almacen" VIEW-AS ALERT-BOX ERROR.
      APPLY "ENTRY" TO ITEM.CodMat.
      RETURN NO-APPLY.   
   END.

  /**** Busca el stock del almacen despacho  ****/
  stkdes = 0.
  SW = FALSE.
  IF S-CODALM = "11" OR S-CODALM = "83" THEN DO:
    IF S-CODALM = "11" AND C-CODALM = "83" THEN SW = TRUE. 
    ELSE 
    IF S-CODALM = "83" AND C-CODALM <> "11" THEN SW = TRUE.
  END.

  IF SW THEN DO:
      FIND B-Almmmate WHERE B-Almmmate.CodCia = S-CODCIA 
                       AND  B-Almmmate.codmat = SELF:SCREEN-VALUE 
                       AND  B-Almmmate.CodAlm = C-CODALM 
                      NO-LOCK NO-ERROR.
      IF AVAILABLE B-Almmmate THEN stkdes = B-Almmmate.StkAct.
  END.
   
   DISPLAY Almmmate.StkAct @ Almmmate.StkAct WITH BROWSE {&BROWSE-NAME}.
   FIND Almmmatg WHERE Almmmatg.CodCia = S-CODCIA 
                  AND  Almmmatg.CodMat = S-CODART 
                 NO-LOCK NO-ERROR.
   IF AVAILABLE Almmmatg THEN DO:

      /* Ic - 26Set2018, correo sr juan ponte/harold segura, no permitir articulos sin pesos ni volumenes */
      FIND FIRST almtfami OF almmmatg NO-LOCK NO-ERROR.
      IF AVAILABLE almtfami AND almtfami.swcomercial = YES THEN DO:
          IF almmmatg.pesmat <= 0 OR almmmatg.libre_d02 <= 0  THEN DO:
              MESSAGE "Articulo NO tiene Peso y/o Volumen" VIEW-AS ALERT-BOX ERROR.
              APPLY "ENTRY" TO ITEM.CodMat.
              RETURN NO-APPLY.
          END.
      END.

      IF Almmmatg.TpoArt <> "A" THEN DO:
         MESSAGE "Articulo no Activo" VIEW-AS ALERT-BOX ERROR.
         APPLY "ENTRY" TO ITEM.CodMat.
         RETURN NO-APPLY.
      END.
   
      ASSIGN S-UndBas   = Almmmatg.UndBas.
      ASSIGN S-UndStk   = Almmmatg.UndStk.
      DISPLAY Almmmatg.DesMat @ Almmmate.DesMat 
              Almmmatg.DesMar @ Almmmatg.DesMar
              WITH BROWSE {&BROWSE-NAME}.
      DISPLAY Almmmatg.UndStk @ ITEM.CodUnd WITH BROWSE {&BROWSE-NAME}.
      F-Equivale = 1.
   END.      
   ELSE DO:
        MESSAGE "Articulo no registrado"
                VIEW-AS ALERT-BOX ERROR.
        APPLY "ENTRY" TO ITEM.CodMat.
        RETURN NO-APPLY.
   END.

   FIND Almtconv WHERE Almtconv.CodUnid = Almmmatg.UndBas 
                  AND  Almtconv.Codalter = Almmmatg.UndStk
                 NO-LOCK NO-ERROR.
   IF NOT AVAILABLE Almtconv THEN DO:
      MESSAGE "Codigo de unidad no existe" VIEW-AS ALERT-BOX ERROR.
      RETURN NO-APPLY.
   END.
   F-Equivale = Almtconv.Equival /* / Almmmatg.FacEqu*/.
   /*RUN lkup/c-uniofi ('Unidades de Venta', almmmate.codmat, OUTPUT s-Ok).*/
   RUN gn/c-uniofi ('Unidades de Venta', almmmate.codmat, OUTPUT s-Ok).
   IF s-Ok = NO THEN DO:
    SELF:SCREEN-VALUE = ''.
    RETURN NO-APPLY.
   END.
END.

ON "LEAVE":U OF ITEM.CodUnd
DO:
   IF SELF:SCREEN-VALUE = "" THEN RETURN.
   FIND Almmmatg WHERE Almmmatg.CodCia = S-CODCIA 
                  AND  Almmmatg.codmat = S-CODART 
                  NO-LOCK NO-ERROR.
   FIND Almtconv WHERE Almtconv.CodUnid = Almmmatg.UndBas 
                  AND  Almtconv.Codalter = SELF:SCREEN-VALUE
                 NO-LOCK NO-ERROR.
   IF NOT AVAILABLE Almtconv THEN DO:
      MESSAGE "Codigo de unidad no existe" VIEW-AS ALERT-BOX ERROR.
      RETURN NO-APPLY.
   END.
   F-Equivale = Almtconv.Equival /* / Almmmatg.FacEqu*/.
END.

ON "LEAVE":U OF ITEM.CanDes
DO:
    ASSIGN
        s-CodArt = ITEM.CodMat:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}.
    FIND Almmmatg WHERE Almmmatg.CodCia = S-CODCIA 
        AND Almmmatg.codmat = S-CODART NO-LOCK NO-ERROR.
    ASSIGN 
        S-UndBas   = Almmmatg.UndBas
        S-UndStk   = ITEM.CodUnd:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}.
    FIND Almtconv WHERE Almtconv.CodUnid = Almmmatg.UndBas 
        AND Almtconv.Codalter = S-UNDSTK NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Almtconv THEN DO:
        MESSAGE "Codigo de unidad no existe" VIEW-AS ALERT-BOX ERROR.
/*         RETURN NO-APPLY.*/
    END.
    ASSIGN
        F-Equivale = Almtconv.Equival.
/*     IF (Almtconv.CodUnid = "Und" OR Almtconv.CodUnid = "UNI" )                                                                          */
/*             AND Almtconv.Equival = 1 THEN DO:                                                                                           */
/*        IF DECIMAL(ITEM.CanDes:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) <> INT(ITEM.CanDes:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) THEN DO: */
/*           MESSAGE " Venta en Unidades, Cantidad " SKIP                                                                                  */
/*                   " no debe Tener Decimales. " VIEW-AS ALERT-BOX ERROR.                                                                 */
/*           RETURN NO-APPLY.                                                                                                              */
/*        END.                                                                                                                             */
/*     END.                                                                                                                                */
END.



/* ***************************  Main Block  *************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
0&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available B-table-Win  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI B-table-Win  _DEFAULT-DISABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-add-record B-table-Win 
PROCEDURE local-add-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  
 
  /* Code placed here will execute PRIOR to standard behavior. */
  /*
  IF ORDTRB <> "" THEN DO:
    MESSAGE "NO AUTORIZADO PARA ADICIONAR ITEMS ....."  VIEW-AS ALERT-BOX ERROR.
    RETURN "ADM-ERROR".
  END.
  */
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .
   
  /* Code placed here will execute AFTER standard behavior.    */
  s-local-new-record = 'YES'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-statement B-table-Win 
PROCEDURE local-assign-statement :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-statement':U ) .
  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN ITEM.Factor = F-Equivale
         ITEM.CodCia = S-CODCIA 
         ITEM.CODALM = S-CODALM
         ITEM.CodUnd = ITEM.CodUnd:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-busca B-table-Win 
PROCEDURE local-busca :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE OK-WAIT-STATE AS LOGICAL NO-UNDO.
  ASSIGN  input-var-1 = ""
          input-var-2 = ""
          input-var-3 = ""
          output-var-1 = ?
          OK-WAIT-STATE = SESSION:SET-WAIT-STATE("GENERAL").

  DO ON ERROR UNDO, LEAVE ON STOP UNDO, LEAVE ON ENDKEY UNDO, LEAVE:
    /*RUN PL/C-XXX.W("").*/
    IF OUTPUT-VAR-1 <> ? THEN DO:
         FIND {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} WHERE
              ROWID({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}) = OUTPUT-VAR-1
              NO-LOCK NO-ERROR.
         IF AVAIL {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} THEN DO:
            REPOSITION {&BROWSE-NAME}  TO ROWID OUTPUT-VAR-1.
         END.
    END.
  END.
  OK-WAIT-STATE = SESSION:SET-WAIT-STATE("").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-cancel-record B-table-Win 
PROCEDURE local-cancel-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  s-local-new-record = 'NO'.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable-fields B-table-Win 
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
  IF RETURN-VALUE = 'YES' 
  THEN DO:
      ITEM.codmat:READ-ONLY IN BROWSE {&browse-name} = NO.
      /*APPLY 'ENTRY':U TO ITEM.codmat IN BROWSE {&browse-name}.*/
  END.
  ELSE DO:
      ITEM.codmat:READ-ONLY IN BROWSE {&browse-name} = YES.
      APPLY 'ENTRY':U TO ITEM.candes IN BROWSE {&browse-name}.
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record B-table-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE procesa-parametros B-table-Win 
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
        WHEN "" THEN.
    END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE recoge-parametros B-table-Win 
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
        WHEN "CodMat" THEN ASSIGN input-var-1 = S-CodAlm.
        WHEN "CodUnd" THEN ASSIGN input-var-1 = S-UndBas.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records B-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "ITEM"}
  {src/adm/template/snd-list.i "Almmmate"}
  {src/adm/template/snd-list.i "Almmmatg"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed B-table-Win 
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
      {src/adm/template/bstates.i}
  END CASE.
  
  IF p-state = 'update-begin':U THEN DO WITH FRAME {&FRAME-NAME}:
     
  END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valida B-table-Win 
PROCEDURE valida :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:  EN CASO DE ERROR RETORNAR : RETURN "ADM-ERROR"
------------------------------------------------------------------------------*/
    DEF VAR s-Ok AS LOG.
    DEF VAR s-StkDis AS DEC.

IF ITEM.CodMat:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = "" THEN DO:
   MESSAGE "Codigo de articulo en blanco" VIEW-AS ALERT-BOX ERROR.
   APPLY "ENTRY" TO ITEM.CodMat.
   RETURN "ADM-ERROR".   
END.
IF DECIMAL(ITEM.CanDes:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) <= 0 THEN DO:
   MESSAGE "Cantidad debe ser mayor a cero" VIEW-AS ALERT-BOX ERROR.
   APPLY "ENTRY" TO ITEM.CanDes.
   RETURN "ADM-ERROR".   
END.
IF ITEM.CodUnd:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = "" THEN DO:
   MESSAGE "Codigo de Unidad no puede ser blanco" VIEW-AS ALERT-BOX ERROR.
   APPLY "ENTRY" TO ITEM.CodUnd.
   RETURN "ADM-ERROR".
END.
FIND DMOV WHERE DMOV.Codcia  = S-CODCIA AND
     DMOV.CodAlm = S-CODALM AND
     DMOV.CodMat = ITEM.CodMat:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
     NO-LOCK NO-ERROR.
IF AVAILABLE DMOV AND ROWID(DMOV) <> ROWID(ITEM) THEN DO:
   MESSAGE "Codigo ya esta registrado" VIEW-AS ALERT-BOX ERROR.
   APPLY "ENTRY" TO ITEM.CodMat.
   RETURN "ADM-ERROR".   
END. 
FIND Almmmate WHERE Almmmate.CodCia = S-CodCia AND
     Almmmate.CodAlm = S-CodAlm AND
     Almmmate.CodMat = ITEM.CodMat:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
     NO-LOCK NO-ERROR.
IF NOT AVAILABLE Almmmate OR 
   (Almmmate.StkAct - DECIMAL(ITEM.CanDes:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) ) < 0 THEN DO:
   IF NOT AVAILABLE Almmmate THEN 
      MESSAGE "Articulo no esta asignado al almacen " S-CodAlm VIEW-AS ALERT-BOX ERROR.
   ELSE MESSAGE "No hay Stock en almacen " S-CodAlm VIEW-AS ALERT-BOX ERROR.
   APPLY "ENTRY" TO ITEM.CanDes.
   RETURN "ADM-ERROR".   
END. 

    /* RHC 29.04.04 NUEVA RUTINA PARA VERIFICAR EL STOCK DISPONIBLE */
/*     RUN vta/stkdispo ( s-codcia,                                                                   */
/*                         s-codalm,                                                                  */
/*                         ITEM.CodMat:SCREEN-VALUE IN BROWSE {&BROWSE-NAME},                         */
/*                         DECIMAL(ITEM.CanDes:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) * f-Equivale,   */
/*                         OUTPUT s-Ok,                                                               */
/*                         OUTPUT s-StkDis).                                                          */
/*     IF s-Ok = NO THEN DO:                                                                          */
/*         MESSAGE 'OJO: Stock Disponible' s-StkDis ITEM.CodUnd:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} */
/*             VIEW-AS ALERT-BOX WARNING.                                                             */
/*     END.                                                                                           */

    /* RHC 11.01.10 nueva rutina */
    DEF VAR pComprometido AS DEC.
    RUN gn/stock-comprometido (Almmmate.codmat, Almmmate.codalm, OUTPUT pComprometido).
    /* CONSISTENCIA NORMAL */
    IF DECIMAL(ITEM.CanDes:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) *  f-Equivale > (Almmmate.stkact - pComprometido)
        THEN DO:
        MESSAGE 'NO se puede sacar mas de' (Almmmate.stkact - pComprometido)
            VIEW-AS ALERT-BOX ERROR.
        APPLY "ENTRY" TO ITEM.CanDes.
        RETURN "ADM-ERROR".
    END.

RETURN "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Valida-UpDate B-table-Win 
PROCEDURE Valida-UpDate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/* MESSAGE 'Acceso denegado' SKIP              */
/*     'Elimine la linea y vuelva a digitarla' */
/*     VIEW-AS ALERT-BOX ERROR.                */
/* RETURN 'ADM-ERROR'.                         */

ASSIGN F-Equivale = ITEM.Factor.
FIND Almmmatg WHERE Almmmatg.CodCia = S-CODCIA AND
     Almmmatg.codmat = ITEM.codmat NO-LOCK NO-ERROR.
ASSIGN S-UndBas = Almmmatg.UndBas.
s-local-new-record = 'NO'.

RETURN "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

