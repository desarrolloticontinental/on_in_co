                            */
&Scoped-define EXTERNAL-TABLES T-cb-cmov
&Scoped-define FIRST-EXTERNAL-TABLE T-cb-cmov


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR T-cb-cmov.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS T-cb-cmov.Notast T-cb-cmov.Codope 
&Scoped-define FIELD-PAIRS~
 ~{&FP1}Notast ~{&FP2}Notast ~{&FP3}~
 ~{&FP1}Codope ~{&FP2}Codope ~{&FP3}
&Scoped-define ENABLED-TABLES T-cb-cmov
&Scoped-define FIRST-ENABLED-TABLE T-cb-cmov
&Scoped-Define ENABLED-OBJECTS RECT-22 
&Scoped-Define DISPLAYED-FIELDS T-cb-cmov.Nroast T-cb-cmov.Fchast ~
T-cb-cmov.Notast T-cb-cmov.GloAst T-cb-cmov.Codope T-cb-cmov.Tpocmb ~
T-cb-cmov.CodDiv T-cb-cmov.Usuario T-cb-cmov.C-Fcaja T-cb-cmov.Codmon ~
T-cb-cmov.Totitm 
&Scoped-Define DISPLAYED-OBJECTS F-Situacion 

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
DEFINE VARIABLE F-Situacion AS CHARACTER FORMAT "X(256)":U 
     LABEL "Situacion" 
     VIEW-AS FILL-IN 
     SIZE 13.57 BY .81
     BGCOLOR 1 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-22
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 83.86 BY 4.19.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     T-cb-cmov.Nroast AT ROW 1.12 COL 5.57 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.14 BY .81
     T-cb-cmov.Fchast AT ROW 1.92 COL 5.57 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9.86 BY .81
     T-cb-cmov.Notast AT ROW 2.77 COL 5.57 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 52 BY .81
     T-cb-cmov.GloAst AT ROW 3.65 COL 7.57 NO-LABEL
          VIEW-AS EDITOR SCROLLBAR-VERTICAL
          SIZE 52.29 BY 1.38
     T-cb-cmov.Codope AT ROW 1.15 COL 27.43 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.72 BY .81
     T-cb-cmov.Tpocmb AT ROW 1.92 COL 31.43 COLON-ALIGNED FORMAT "ZZ,ZZ9.99"
          VIEW-AS FILL-IN 
          SIZE 7.86 BY .81
     F-Situacion AT ROW 1.08 COL 49.43 COLON-ALIGNED
     T-cb-cmov.CodDiv AT ROW 1.88 COL 49.43 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.14 BY .81
     T-cb-cmov.Usuario AT ROW 1.04 COL 71.43 COLON-ALIGNED
          LABEL "Usuario"
          VIEW-AS FILL-IN 
          SIZE 10 BY .81
     T-cb-cmov.C-Fcaja AT ROW 1.88 COL 71.43 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.43 BY .81
     T-cb-cmov.Codmon AT ROW 2.88 COL 73.57 NO-LABEL
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Soles", 1,
"Dolares", 2
          SIZE 8.43 BY 1.15
     T-cb-cmov.Totitm AT ROW 4.08 COL 71.57 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 3.43 BY .81
     RECT-22 AT ROW 1 COL 1
     "Moneda:" VIEW-AS TEXT
          SIZE 6.29 BY .42 AT ROW 3 COL 66.72
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 4.

 

/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: integral.T-cb-cmov
   Allow: Basic,DB-Fields
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY COMPILE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT."
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW V-table-Win ASSIGN
         HEIGHT             = 4.19
         WIDTH              = 83.86.
                                                                        */
&ANALYZE-RESUME


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit Default                                      */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN T-cb-cmov.C-Fcaja IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN T-cb-cmov.CodDiv IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET T-cb-cmov.Codmon IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F-Situacion IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN T-cb-cmov.Fchast IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR EDITOR T-cb-cmov.GloAst IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN T-cb-cmov.Nroast IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN T-cb-cmov.Totitm IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN T-cb-cmov.Tpocmb IN FRAME F-Main
   NO-ENABLE EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN T-cb-cmov.Usuario IN FRAME F-Main
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

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm-vm/method/vmviewer.i}
{src/adm/method/viewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         
  
  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available V-table-Win _ADM-ROW-AVAILABLE
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
  {src/adm/template/row-list.i "T-cb-cmov"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "T-cb-cmov"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI V-table-Win _DEFAULT-DISABLE
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imprimir V-table-Win 
PROCEDURE Imprimir :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER F-CUENTAS AS Integer.
    /* 1 todas las cuentas
       2 eliminar las automaticas
     */
    DEF VAR X-IMPRESO AS CHAR FORMAT "X(20)".
    X-IMPRESO = STRING(TIME,"HH:MM AM") + "-" + STRING(TODAY,"99/99/99").
    DEFINE var X-PAG AS CHAR FORMAT "999". 
    DEFINE VARIABLE x-filtro   AS CHAR.   
    DEFINE VARIABLE x-glodoc   AS CHARACTER FORMAT "X(40)".
    DEFINE VARIABLE x-debe     AS DECIMAL FORMAT "(ZZZ,ZZZ,ZZ9.99)".
    DEFINE VARIABLE x-haber    AS DECIMAL FORMAT "(ZZZ,ZZZ,ZZ9.99)".
    DEFINE VARIABLE x-fecha    AS DATE FORMAT "99/99/99" INITIAL TODAY.
    DEFINE VARIABLE x-codmon   AS INTEGER INITIAL 1.
    DEFINE VARIABLE x-con-reg  AS INTEGER.
    DEFINE VARIABLE x-nom-ope  AS CHARACTER FORMAT "x(40)".
    DEFINE VARIABLE pinta-mes  AS CHARACTER FORMAT "X(40)".
    DEFINE VARIABLE x-nomope   AS CHARACTER FORMAT "X(40)".
    RUN bin/_mes.p ( INPUT s-NroMes , 1,  OUTPUT pinta-mes ).
    pinta-mes = pinta-mes + " DE " + STRING( s-periodo , "9999" ).
    x-codmon = integral.t-cb-cmov.codmon.
    x-fecha  = integral.t-cb-cmov.Fchast.
  
    FIND cb-oper WHERE cb-oper.codcia = cb-codcia AND
         cb-oper.codope = t-cb-cmov.codope NO-LOCK NO-ERROR.
    x-nomope = ''.
    IF AVAILABLE cb-oper THEN 
       x-nomope = cb-oper.Nomope.

    DEFINE FRAME f-cab
        t-cb-dmov.coddiv LABEL "Divisi�n"
        t-cb-dmov.codcta LABEL "Cuenta"
        t-cb-dmov.clfaux LABEL "Clf!Aux"
        t-cb-dmov.codaux       LABEL "Auxiliar"
        t-cb-dmov.cco          LABEL "C.Cos"
        t-cb-dmov.nroref LABEL "Referencia"
        t-cb-dmov.coddoc LABEL "Cod"
        t-cb-dmov.nrodoc LABEL "Nro!Documento"
        t-cb-dmov.fchdoc LABEL "Fecha!Doc"
        x-glodoc       LABEL "Detalle"
        x-debe         LABEL "Cargos"
        x-haber        LABEL "Abonos"
        WITH WIDTH 150 NO-BOX STREAM-IO DOWN.
    OUTPUT STREAM report TO PRINTER PAGED PAGE-SIZE 66.    
    PUT STREAM report CONTROL "~033@~0335~033F~033P~033x~001~033E~033C" CHR(66) .
    PUT STREAM report empresas.nomcia SKIP.
    PUT STREAM report x-nomope. 
    PUT STREAM report "ASIENTO No: " AT 60 
                      integral.t-cb-cmov.codope
                      "-"
                      integral.t-cb-cmov.nroast 
                      SKIP(1).
    PUT STREAM report pinta-mes SKIP(1).
    PUT STREAM report CONTROL CHR(27) CHR(70) CHR(27) CHR(120) 0.
    PUT STREAM report "FECHA     : " t-cb-cmov.fchast SKIP.
    IF integral.t-cb-cmov.codmon = 1 THEN
        PUT STREAM report "MONEDA    :         S/. " SKIP.
    ELSE
        PUT STREAM report "MONEDA    :         US$ " SKIP.
    PUT STREAM report "T.CAMBIO  : " integral.t-cb-cmov.tpocmb SKIP.    
    PUT STREAM report "CONCEPTO  : " integral.t-cb-cmov.notast SKIP.
    
    PUT STREAM report CONTROL CHR(27) CHR(77) CHR(15).

    PUT STREAM report FILL("_",150) FORMAT "X(150)" SKIP.

    FOR EACH t-cb-dmov NO-LOCK WHERE t-cb-dmov.codcia = s-codcia
                    AND t-cb-dmov.periodo = s-periodo
                    AND t-cb-dmov.nromes  = s-NroMes
                    AND t-cb-dmov.codope  = t-cb-cmov.codope
                    AND t-cb-dmov.nroast  = t-cb-cmov.nroast
        BREAK BY (t-cb-dmov.nroast)
              BY (t-cb-dmov.Nroitm) ON ERROR UNDO, LEAVE:
        IF F-CUENTAS = 2 AND t-cb-dmov.tpoitm = "A" THEN NEXT.
        x-glodoc = glodoc.
        IF x-glodoc = "" THEN DO:
            CASE t-cb-dmov.clfaux:
            WHEN "@CL" THEN DO:
                FIND gn-clie WHERE gn-clie.codcli = t-cb-dmov.codaux
                    AND gn-clie.CodCia = cb-codcia NO-LOCK NO-ERROR.
                IF AVAILABLE gn-clie THEN x-glodoc = gn-clie.nomcli.
            END.
            WHEN "@PV" THEN DO:
                FIND gn-prov WHERE gn-prov.codpro = t-cb-dmov.codaux
                    AND gn-prov.CodCia = cb-codcia NO-LOCK NO-ERROR.
                IF AVAILABLE gn-prov THEN x-glodoc = gn-prov.nompro.
            END.
            WHEN "@CT" THEN DO:
                find cb-ctas WHERE cb-ctas.codcta = t-cb-dmov.codaux
                    AND cb-ctas.CodCia = cb-codcia NO-LOCK NO-ERROR.                      
               IF AVAILABLE cb-ctas THEN x-glodoc = cb-ctas.nomcta.
            END.
            OTHERWISE DO:
                FIND cb-auxi WHERE cb-auxi.clfaux = t-cb-dmov.clfaux
                    AND cb-auxi.codaux = t-cb-dmov.codaux
                    AND cb-auxi.CodCia = cb-codcia NO-LOCK NO-ERROR.                      
                IF AVAILABLE cb-auxi THEN x-glodoc = cb-auxi.nomaux.
            END.
            END CASE.
        END.
        IF x-glodoc = "" THEN DO:
            IF AVAILABLE t-cb-cmov THEN x-glodoc = t-cb-cmov.notast.
        END.
        CASE x-codmon:
            WHEN 2 THEN DO:
                SUBSTR(x-glodoc,( 35 - LENGTH(STRING(ImpMn2)) ),31) = "(US$" + STRING(ImpMn2) + ")".
            END.
        END CASE.
        IF t-cb-dmov.tpomov THEN DO:
            x-debe  = 0.
            x-haber = ImpMn1.
        END.
        ELSE DO:
            x-debe  = ImpMn1.
            x-haber = 0.
        END.
        IF NOT (x-haber = 0 AND x-debe = 0) AND x-debe <> ? AND x-haber <> ?
        THEN DO:
            ACCUMULATE x-debe  (SUB-TOTAL BY t-cb-dmov.nroast).
            ACCUMULATE x-haber (SUB-TOTAL BY t-cb-dmov.nroast).
            IF LINE-COUNTER(report)  + 5 > PAGE-SIZE(report)
            THEN DO :
                      X-PAG =  STRING(PAGE-NUMBER(report),"999").
                      UNDERLINE STREAM report 
                              t-cb-dmov.coddiv
                              t-cb-dmov.codcta
                              x-glodoc
                              x-debe 
                              x-haber
                       WITH FRAME f-cab.    
                       DOWN STREAM report with frame f-cab.
                       DISPLAY STREAM report 
                              "PAG."              @ t-cb-dmov.coddiv
                              X-PAG               @ t-cb-dmov.codcta
                              "    .....Van.... " @ x-glodoc
                              ACCUM SUB-TOTAL BY (t-cb-dmov.nroast) x-debe  @ x-debe
                              ACCUM SUB-TOTAL BY (t-cb-dmov.nroast) x-haber @ x-haber
                       WITH FRAME f-cab.    
                       X-PAG =  STRING(PAGE-NUMBER(report) + 1 ,"999").  
                       DOWN STREAM report with frame f-cab.
                       PAGE stream report.
                       DISPLAY STREAM report 
                              "PAG."              @ t-cb-dmov.coddiv
                              X-PAG               @ t-cb-dmov.codcta
                              "    .....Vienen.... " @ x-glodoc
                              ACCUM SUB-TOTAL BY (t-cb-dmov.nroast) x-debe  @ x-debe
                              ACCUM SUB-TOTAL BY (t-cb-dmov.nroast) x-haber @ x-haber
                       WITH FRAME f-cab.    
                       DOWN STREAM report with frame f-cab.
                       UNDERLINE STREAM report 
                              t-cb-dmov.coddiv
                              t-cb-dmov.codcta
                              x-glodoc
                              x-debe 
                              x-haber
                       WITH FRAME f-cab.    
                       DOWN STREAM report with frame f-cab.
                     
                       
                 END.     
            DISPLAY STREAM report t-cb-dmov.coddiv
                                  t-cb-dmov.codcta
                                  t-cb-dmov.clfaux
                                  t-cb-dmov.codaux
                                  t-cb-dmov.cco
                                  t-cb-dmov.nroref
                                  t-cb-dmov.coddoc
                                  t-cb-dmov.nrodoc
                                  t-cb-dmov.fchdoc
                                  x-glodoc
                                  x-debe   WHEN (x-debe  <> 0)
                                  x-haber  WHEN (x-haber <> 0) 
                            WITH FRAME f-cab.

        END.
        IF LAST-OF (t-cb-dmov.nroast)
        THEN DO:
            x-glodoc = "                    TOTALES :".
            IF LINE-COUNTER(report)  + 3 > PAGE-SIZE(report)
            THEN PAGE stream report.
            DOWN STREAM report 1 WITH FRAME f-cab.
            UNDERLINE STREAM report 
                              x-glodoc
                              x-debe 
                              x-haber
                       WITH FRAME f-cab.    
            DOWN STREAM report with frame f-cab.
            DISPLAY STREAM report x-glodoc
                    ACCUM SUB-TOTAL BY (t-cb-dmov.nroast) x-debe  @ x-debe
                    ACCUM SUB-TOTAL BY (t-cb-dmov.nroast) x-haber @ x-haber
                WITH FRAME f-cab.
                


        END.
    END.
    DO WHILE LINE-COUNTER(report) < PAGE-SIZE(report) - 10 :
        PUT STREAM report "" skip.
    END.
    PUT STREAM report "               -----------------       -----------------       -----------------       ----------------- " SKIP.
    PUT STREAM report "                  PREPARADO                  Vo. Bo.            ADMINISTRACION             CONTADOR           Impreso:".
    PUT STREAM report x-impreso SKIP.
    
    OUTPUT STREAM report CLOSE.

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

  /* Dispatch standard ADM method.                             */
  /* RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) . */

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record V-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  RETURN 'ADM-ERROR'.
  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  /* RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .*/

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
  DO WITH FRAME {&FRAME-NAME}:
     IF AVAILABLE t-cb-cmov THEN
        IF t-cb-cmov.flgtra THEN
           DISPLAY 'CENTRALIZADO' @ F-Situacion.
        ELSE
           DISPLAY ' PENDIENTE  ' @ F-Situacion.
  END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-imprime V-table-Win 
PROCEDURE local-imprime :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'imprime':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN Imprimir(1).
  
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records V-table-Win _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "T-cb-cmov"}

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
  Purpose:     Validacion de datos
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME} :
   /* IF CAMPO:SCREEN-VALUE = "" THEN DO:
         MESSAGE "Campo no debe ser blanco"
         VIEW-AS ALERT-BOX ERROR.
         APPLY "ENTRY" TO CAMPO.
         RETURN "ADM-ERROR".   
   
      END.
   */

END.

RETURN "OK".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valida-update V-table-Win 
PROCEDURE valida-update :
/*------------------------------------------------------------------------------
  Purpose:     Rutina de validacion en caso de modificacion
  Parameters:  Regresar "ADM-ERROR" si no se quiere modificar
  Notes:       
------------------------------------------------------------------------------*/

IF NOT AVAILABLE t-cb-cmov THEN RETURN 'ADM-ERROR'.

IF t-cb-cmov.flgtra THEN RETURN 'ADM-ERROR'.

RETURN "OK".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     	��V�ʸ:    x 4              !                                �     ,  �!              R�  ,�HC      `7  )  e  �  �e  �  �f  p	 �i  �
 �m  �  �n  �  `o  @ �p  �  tq  , �r  �  ts  �  8t  �  �t  � �v  �  Dw  �  x  ( 0y   Dz  X �{   �|  8 �}   �~  x t�  @ ��  �  T�   p�     p�  �!  �  " �  �# ��  �$  �  |% ��  �& ��  �' l�  �( �  |) ��  * ��  �+  �  <, \�  �- ��  �. ��  � / ��   0 ��  � 1 ��  � 2 l�  � 3 X�  h4 ��  � 5 ��  � 6 ��  87 ��  8 ��  � 9 p�  � : �  �; ��  <  �  �=         ��  �  ? 0�  "iSO8859-1                        � & �     �                T           � 7        � � ��           �                                          PROGRESS                         ,      �                                              ��    
          INTEGRAL                         PROGRESS                         `  �     C          �t�8       E              �  �              �t�8       E           ��       ��       ��       ��       ��               4 �              �t�8      S]                �    0
   
                t     h                                            �      
�  �               �t�8       E  !           �  �               �t�8       E  !           H  �               �t�8       E  !                                                       |   �               �t�8       E  2             # H�   H           �t�8      Hl5                > ��        > �� ('       ��                           k       ����                  �P                 #           h   TH 8 \  t             X      �̃  O ��    e�      O ��    R�      O ��    ��    t        � / ] �    �             3 ��    �         3 ��$ ` � � �     4 ��\         �           ��             � �        y�   � �  	�                     3 ��p   O � ��  ��x   � @        �      � @        �        � ߱  �$ � 0���        �                � ߱  �$ � t���        �o �  
  �  �             �  ��(�G<�D T h          L    ,  ��            � � <      ���  O ��    e�      O ��    R�      O ��    ��    l/ � d                 3 ��|  � � �  ��                ����                      �            x              g                   8      ��            � � (      (��  O ��    e�      O ��    R�      O ��    ��      � � �  ��                ����                      �            D              g           l   "�          �               � ߱  �$ � ����        g U�       ] �              �    H8  ��            UYX      ���  O ��    e�      O ��    R�      O ��    ��    @             H@        8       � ߱    $ Vh���          ��                  ��    k           ����                                   �              g         �g [(        ]4�             �    |l  ��            [a�     ��W  O ��    e�      O ��    R�      O ��    ��    � \��    4 ��\  O \��  ��     ]�    4 ���                  ��             ]`       �W   ]�\/ ^,   4            3 ���H @        3 ���   T        3 ���  _}    ��                  ��    k           ����                      4            h              g         Dg c�       ]�    ]4             d    D4  ��            cgT     ��W  O ��    e�      O ��    R�      O ��    ��       dp�    4 ��        �          ��             df       ,�W   dx   e��    4 ��8  ed l  ��                  ��    k           ����                      �            �              g         �g wT       ]!�             �    ��  ��            wy�     ���  O ��    e�      O ��    R�      O ��    ��    � x��    4 ��x  O x��  ��     x�8	8  4 ���        p	          ��             xx       $��   x 	�   
  	     	   �   
            � ߱  �	$ x@	���        
/ x�	   �	            3 ���	 �	        3 ��$�	 �	        3 ��4   �	�	      3 ��@  $ x�	���                          � ߱  < x
\
    4 ��L        �
          ��             xx       ���   x$
�@        t     �@        �       � ߱  �
$ xd
���          p x��
,x  �
�
 ��  �               � ߱    $ x�
���           �               � ߱    $ x�
���          O x��  ��     xHl�  4 ��,h@        X       � ߱    $ xP���        �@        �     �@        �     @        �     4@        $     d@        T       � ߱    $ x����                @          ��             xx        ��   x    xL�    4 ��x�@        �     @               � ߱    $ xT���          ��                  ��    k           ����                      `            �              g         adm-busca                     �                     �	 adm-imprime @                �                     � _busca-lookup   L|p    (    	   @                  <4 _corre-program  ��         �
   �                  �^ ,$ ����        ,
   
            � ߱  `/	�D   LX
          3 ��<
   X        3 ��d
8 �l�    4 ��x
        �          ��             ��       p3�   �t�/��               3 ���
$ �����        �
   
              � ߱    /�   $�
          3 ���
   0        3 ���
�$ �L���         @        �
       � ߱  adm-apply-entry �h                �                     � adm-destroy x�                �                     � adm-disable ��                                    � adm-edit-attribute-list �                 �                     � adm-enable  8h                �                     
 adm-exit    t�                �                     - adm-hide    ��                �                     6 adm-initialize  �                �                     ? adm-show-errors ,\        	 D   \                  XS adm-UIB-mode    l�                �                     c adm-view    ��                �                     p dispatch    �p       
 �                       � � get-attribute   $Tp        �    �                   � � get-attribute-list  d�p           0                  ,� new-state   ��p        �    �                   � �	 notify  �p        �                      � set-attribute-list  Lp        �    �                   � � set-position    `�p        8   P                  L adm-display-fields  ��                                    ' adm-open-query  �                x                     : adm-row-changed $T                �                     f reposition-query    d�p        �     �                   � � P ���    4 ��d  /�   �          3 ��x          3 ���4 ,        3 ���H @        3 ���   T        3 ���adm-add-record  �\         4!   T                  L adm-assign-record   l�            "    �                    X adm-assign-statement    ��            #    T                    v adm-cancel-record   �(            $    \                    � adm-copy-record <l         <%   T                  P	 adm-create-record   |�         �&   �                  �J	 adm-current-changed ��            '    �                    �	 adm-delete-record   4         \(   t                  p
 adm-disable-fields  Hx            )    @                    X
 adm-enable-fields   ��            *    �                    �
 adm-end-update  �          X+   p                  ln adm-reset-record    @            ,                        � adm-update-record   T�            -    x                    � check-modified  ��p    D T.   p                  l9 get-rowid   �p        � /   �                   � P	 set-editors Dp        �0   �                  �q use-check-modified-all  P�p        � 1   �                   � � use-create-on-add   ��p        � 2   �                   � � use-initial-lock    �p       ! � 3   �                   � �   / h   p            3 ��6   |        3 ��06adm-row-available    �        "  4   @                  < disable_UI  ��            5    �                     &
 local-assign-record �            6    �                     4 local-delete-record H            7    �                    Q local-update-record \�            8    �                     o procesa-parametros  ��            9    �                     � recoge-parametros   �            :    �                     � send-records    (Xp       $ �;   �                  �� state-changed   h�p       % �<   �                  �� valida  ��            =    L                    � �   �   �  ���    �   �   � �NO-LOCK   ���  ���������    �    d8 ��# l8 ��# |8 �� �8 ��         8 ��   8 ��     %     set-attribute-list %4 * $   Keys-Accepted = "",     Keys-Supplied = ""  �     }    �� } F %               �         I%               �         �%              � %              %              %              %         %          � e  
�         �G%              %               %     _corre-program  %      ENTRY   
"  	 
   %      ENTRY   
"  	 
   
"  
 
#��     �  �A� l 
"  
 
���      �   %               
"  
 
���      ,   %                    S    �     }     � y %               %                   �     }     � � %     bin/_inslook.r  �     }    �"     � �     �     }     � � 
"  
 
"�    �      ,   %              � � 
"  
 
        S    �     }     � y %               %                   �     }     � � 
�     }    �G
�     }    � %     _busca-lookup   �     }    �"     "         "   "�� e  
"  
 
���      h   %               
"  
 
���      �   %               
"  	 
   �      �  6@� � � � � � � � � � %               
"   
"�    �          � � 
"  
 
���      L   %              
"  
 
&��      |  �     }    
"  
 
��      �       �     }    �     }    �
"  
 
   �      �  ��     }    �
"  
 
VT�         %               
"  
 
~��      H   %              (     $     
�     }    �
"  
 
�W    �     }    �G� � 
"  
 
��      �   %               
"  
 
��      �   %               %      notify  � � %      notify  � � "      "      &    &    &    &        %              %              *    "      "      � 2 "      &    &    &    &        %              %              *    "      "      � e  � e  �    }    �� T "      � � %     bin/_calc.r     �  %              
"  	 
�  �      �  B�  � � %     bin/_calenda.r      �  %              
"  	 
�  �      D	  B�  � \ %     recoge-parametros T"      "          "   "�%              
"  	 
�  �      �	  B"    �	%     procesa-parametros �    }    �� e  
�         �G%     get-attribute   
"    
   %      TYPE        �  �  
 %      adm/objects/broker.p �W
"   
   %     set-broker-owner QT
"    
   
�    �     }    �G �    �     �     �     �     �     h L    X     H     8     (              �  �  �   � " � ' �   � ' ((       
"   
�
%   
           � 0       
"   
'��   � 1H � z+ �   � � (T  �   ,       �     }    �G� 0      �     }    �G%              � �  T 4    D      4   � � T   %              �     }    �G� � � � T   %              �     }    �G� � %     broker-apply-entry 
"    
   
�    %     broker-destroy  
"    
   
�    %     dispatch �W%     disable-fields %     set-attribute-list % 
    ENABLED=no %               %      adm/support/viewerd.w W
�    %               % 	    enable_UI W
�    %     set-attribute-list %     ENABLED=yes %               %      notify  %      exit    %               %     broker-hide 
"    
   
�    %     broker-initialize T
"    
   
�        %              %                   " 	     %                  " 	     �     }    ��    }    �" 	     %               %     broker-UIB-mode 
"    
   
�    %     broker-view 
"    
   
�    %     broker-dispatch 
"    
   
�    " 
         �  � �	 % 	    ADM-ERROR T%      broker-get-attribute �W
"    
   
�    "      �  %$     broker-get-attribute-list W
"    
   
�    o%   o           "     %               %     broker-new-state QT
"    
   
�    "      %               %     broker-notify   
"    
   
�    "          �  � �	 % 	    ADM-ERROR T%               %$     broker-set-attribute-list W
"    
   
�    "      %               � 
"   
QT
"   
���      �  "    �
"   
���      �  "    �%               *    "      "      %     check-modified  
�    %      clear   %               %               � 
"   
QT%     dispatch �W
�    %     display-fields %      notify  %     row-available W%               %     set-attribute-list %      REPOSITION-PENDING = NO %               %          abl %      modify-list-attribute W
"    
   
�    %      REMOVE  %     SUPPORTED-LINKS %     TABLEIO-TARGET  %     check-modified  
�    %      check   �    %              %              (   *    %               %              %     set-attribute-list %     ADM-NEW-RECORD=yes %     dispatch �W%     enable-fields W    "   "�%              %     request-attribute T
"    
   
�    %     GROUP-ASSIGN-SOURCE %     ENABLED-TABLES U    S    �   �  %               %              %               X    ( (       "   #�%                   "      %                  o �    � � m � � %      DICTDB  �    %      adm/objects/get-init.p %      cb-tabl "      %      DICTDB  "      %     set-editors %      CLEAR       "   "�%                   o �    � � "      &        )    &    "      "      % 	    ADM-ERROR T% 	    ADM-ERROR T%               %     dispatch �W%     create-record W    �  � �	 % 	    ADM-ERROR T"      "      %              %      notify  %(     add-record, GROUP-ASSIGN-TARGET % 	    new-state W%      update  %     dispatch �W
�    %     apply-entry %               %              "           "   �W "   QT%     dispatch �W%     create-record W    �  � �	 % 	    ADM-ERROR T( (       "   ��%                  "      %              %     dispatch �W%     current-changed     �  � �	 % 	    ADM-ERROR T%     dispatch �W%     current-changed     �  � �	 % 	    ADM-ERROR T%     dispatch �W%     assign-statement �W    �  � �	 % 	    ADM-ERROR T%      notify  %, !    assign-record,GROUP-ASSIGN-TARGET W    �  � �	 % 	    ADM-ERROR T"      %     dispatch �W%     display-fields %               �     }    �%     dispatch �W%     show-errors % 	    ADM-ERROR T%               %     check-modified  
�    %      clear   "