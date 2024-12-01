&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r11 GUI
&ANALYZE-RESUME
/* Connected Databases 
          integral         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
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

{bin/s-global.i}
{pln/s-global.i}

DEFINE NEW GLOBAL SHARED VARIABLE VAL-VAR AS DECIMAL EXTENT 20 FORMAT "ZZZ,ZZ9.99".

DEFINE VARIABLE CMB-seccion AS CHARACTER NO-UNDO.
DEFINE VARIABLE stat-reg    AS LOGICAL NO-UNDO.
DEFINE VARIABLE i           AS INTEGER NO-UNDO.
DEFINE VARIABLE x-ImpTot    AS DECIMAL NO-UNDO.

DEFINE STREAM strm-concep. /* STREAM para el reporte */

DEFINE TEMP-TABLE tmp-aporte
    FIELD t-codafp AS INTEGER
    FIELD t-codPer AS CHARACTER
    FIELD t-NomPer AS CHARACTER
    FIELD t-afp0   AS DECIMAL
    FIELD t-afp1   AS DECIMAL
    FIELD t-afp2   AS DECIMAL
    FIELD t-afp3   AS DECIMAL
    FIELD t-afp4   AS DECIMAL
    INDEX i-codper IS PRIMARY t-codafp t-CodPer ASCENDING.

DEFINE BUFFER b-PL-SEM FOR PL-SEM.

DEFINE BUTTON Btn_OK IMAGE-UP FILE "img/afps" LABEL "OK" 
    SIZE 6.43 BY 1.65 BGCOLOR 8.

DEFINE VARIABLE FILL-IN-afp AS CHARACTER FORMAT "X(256)":U 
    LABEL "AFP" VIEW-AS FILL-IN SIZE 28 BY .81
    BGCOLOR 15 FGCOLOR 0 NO-UNDO.

DEFINE VARIABLE FILL-IN-Codigo AS CHARACTER FORMAT "X(256)":U 
    LABEL "Aporte de personal" VIEW-AS FILL-IN 
    SIZE 6.72 BY .81 BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE FILL-IN-Des AS CHARACTER FORMAT "X(256)":U 
    VIEW-AS FILL-IN SIZE 21 BY .81 NO-UNDO.

DEFINE RECTANGLE RECT-20 EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL SIZE 44.43 BY 3.08.

DEFINE FRAME F-msg
    Btn_OK AT ROW 1.23 COL 3
    FILL-IN-afp AT ROW 2.08 COL 14 COLON-ALIGNED
    FILL-IN-Codigo AT ROW 2.92 COL 14 COLON-ALIGNED
    FILL-IN-Des AT ROW 2.92 COL 21 COLON-ALIGNED NO-LABEL
    RECT-20 AT ROW 1 COL 1
    "Espere un momento por favor ..." VIEW-AS TEXT
    SIZE 22.57 BY .62 AT ROW 1.31 COL 11.86
    SPACE(10.99) SKIP(2.14)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
    SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
    FONT 4 TITLE "Procesando..." CENTERED.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES integral.PL-PLAN integral.PL-CALC
&Scoped-define FIRST-EXTERNAL-TABLE integral.PL-PLAN


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR integral.PL-PLAN, integral.PL-CALC.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-19 FILL-IN-NRO-MES Btn-UP Btn-DOWN ~
FILL-IN-Copias Btn-UP-2 Btn-DOWN-2 T-Todos B-aceptar 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-NRO-MES FILL-IN-Copias T-Todos 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON B-aceptar 
     IMAGE-UP FILE "img/b-ok":U
     LABEL "&Aceptar" 
     SIZE 10.72 BY 1.54.

DEFINE BUTTON Btn-DOWN 
     IMAGE-UP FILE "img/btn-down":U
     LABEL "" 
     SIZE 3 BY .69.

DEFINE BUTTON Btn-DOWN-2 
     IMAGE-UP FILE "img/btn-down":U
     LABEL "" 
     SIZE 3 BY .69.

DEFINE BUTTON Btn-UP 
     IMAGE-UP FILE "img/btn-up":U
     LABEL "" 
     SIZE 3 BY .69.

DEFINE BUTTON Btn-UP-2 
     IMAGE-UP FILE "img/btn-up":U
     LABEL "" 
     SIZE 3 BY .69.

DEFINE VARIABLE FILL-IN-Copias AS INTEGER FORMAT "Z9":U INITIAL 0 
     LABEL "Copias" 
     VIEW-AS FILL-IN 
     SIZE 3.72 BY .81
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE FILL-IN-NRO-MES AS INTEGER FORMAT "Z9":U INITIAL 0 
     LABEL "Mes" 
     VIEW-AS FILL-IN 
     SIZE 3.72 BY .81
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE RECTANGLE RECT-19
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 68.72 BY 2.19.

DEFINE VARIABLE T-Todos AS LOGICAL INITIAL no 
     LABEL "Todos los c�lculos" 
     VIEW-AS TOGGLE-BOX
     SIZE 15.86 BY .77 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     FILL-IN-NRO-MES AT ROW 1.62 COL 3.72 COLON-ALIGNED
     Btn-UP AT ROW 1.42 COL 9.43
     Btn-DOWN AT ROW 2.04 COL 9.43
     FILL-IN-Copias AT ROW 1.62 COL 17 COLON-ALIGNED
     Btn-UP-2 AT ROW 1.46 COL 23.14
     Btn-DOWN-2 AT ROW 2.08 COL 23.14
     T-Todos AT ROW 1.65 COL 28.86
     B-aceptar AT ROW 1.27 COL 46.29
     RECT-19 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 4.

 

/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: integral.PL-PLAN,integral.PL-CALC
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
         HEIGHT             = 2.19
         WIDTH              = 68.72.
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

{src/adm/method/viewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME B-aceptar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-aceptar V-table-Win
ON CHOOSE OF B-aceptar IN FRAME F-Main /* Aceptar */
DO:
    ASSIGN FILL-IN-NRO-MES FILL-IN-Copias T-Todos.
    RUN imp_aportes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-DOWN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-DOWN V-table-Win
ON CHOOSE OF Btn-DOWN IN FRAME F-Main
DO:
  IF INPUT FRAME F-Main FILL-IN-NRO-MES - 1 >= 1 THEN DO:
    DISPLAY INPUT FILL-IN-NRO-MES - 1 @ FILL-IN-NRO-MES WITH FRAME F-Main.
    ASSIGN FILL-IN-NRO-MES.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-DOWN-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-DOWN-2 V-table-Win
ON CHOOSE OF Btn-DOWN-2 IN FRAME F-Main
DO:
  IF INPUT FRAME F-Main FILL-IN-Copias - 1 >= 1 THEN DO:
    DISPLAY INPUT FILL-IN-Copias - 1 @ FILL-IN-Copias WITH FRAME F-Main.
    ASSIGN FILL-IN-Copias.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-UP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-UP V-table-Win
ON CHOOSE OF Btn-UP IN FRAME F-Main
DO:
  IF INPUT FRAME F-Main FILL-IN-NRO-MES + 1 <= 53 THEN DO:
    DISPLAY INPUT FILL-IN-NRO-MES + 1 @ FILL-IN-NRO-MES WITH FRAME F-Main.
    ASSIGN FILL-IN-NRO-MES.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-UP-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-UP-2 V-table-Win
ON CHOOSE OF Btn-UP-2 IN FRAME F-Main
DO:
  IF INPUT FRAME F-Main FILL-IN-Copias + 1 <= 99 THEN DO:
    DISPLAY INPUT FILL-IN-Copias + 1 @ FILL-IN-Copias WITH FRAME F-Main.
    ASSIGN FILL-IN-Copias.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-NRO-MES
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-NRO-MES V-table-Win
ON LEAVE OF FILL-IN-NRO-MES IN FRAME F-Main /* Mes */
DO:
    IF INPUT FILL-IN-NRO-MES > 12 OR INPUT FILL-IN-NRO-MES = 0 THEN DO:
        BELL.
        MESSAGE "Rango de mes es de 1 a 12"
            VIEW-AS ALERT-BOX ERROR.
        APPLY "ENTRY" TO FILL-IN-NRO-MES.
        RETURN NO-APPLY.
    END.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE acumula_aportes V-table-Win 
PROCEDURE acumula_aportes :
DEFINE INPUT PARAMETER x-NroSem  AS INTEGER.
DEFINE INPUT PARAMETER x-CodCal  AS INTEGER.
DEFINE INPUT PARAMETER x-Periodo AS INTEGER.

ASSIGN FILL-IN-Des = "Semana " + STRING(x-NroSem,"Z9") + ". C�lculo " + STRING(x-CodCal,"9").

DISPLAY FILL-IN-Des WITH FRAME F-msg.

FOR EACH PL-FLG-SEM WHERE
    PL-FLG-SEM.CodCia  = s-CodCia AND
    PL-FLG-SEM.Periodo = x-Periodo AND
    PL-FLG-SEM.NroSem  = x-NroSem AND
    PL-FLG-SEM.CodPln  = PL-PLAN.CodPln:
    FIND PL-AFPS WHERE PL-AFPS.codafp = PL-FLG-SEM.CodAfp NO-LOCK NO-ERROR.
    IF NOT AVAILABLE PL-AFPS THEN NEXT.

    DISPLAY
        PL-FLG-SEM.CodPer @ FILL-IN-Codigo
        PL-AFPS.DesAfp @ FILL-IN-afp
        WITH FRAME F-msg.

    ASSIGN x-ImpTot = 0.
    RUN pln/p-calc-s.r(
        s-codcia,
        x-Periodo,
        x-NroSem,
        PL-PLAN.codpln,
        x-codcal,
        PL-FLG-SEM.CodPer,
        "^500;^503;^505;^506;^504"
        ).

    ASSIGN x-Imptot = VAL-VAR[2] + VAL-VAR[3] + VAL-VAR[4] + VAL-VAR[5].

    IF x-Imptot <> 0 THEN DO:
        FIND FIRST tmp-aporte WHERE t-CodPer = PL-FLG-SEM.CodPer NO-ERROR.
        IF NOT AVAILABLE tmp-aporte THEN DO:
            CREATE tmp-aporte.
            ASSIGN
                t-codafp = PL-FLG-SEM.CodAfp
                t-codPer = PL-FLG-SEM.CodPer.
            FIND PL-PERS WHERE PL-PERS.CodPer = PL-FLG-SEM.CodPer NO-LOCK NO-ERROR.
            IF AVAILABLE PL-PERS THEN
                ASSIGN t-NomPer = PL-PERS.PatPer + " " + PL-PERS.MatPer + ", " + PL-PERS.NomPer.
        END.
        ASSIGN
            t-afp0     = t-afp0 + VAL-VAR[1]
            t-afp1     = t-afp1 + VAL-VAR[2]
            t-afp2     = t-afp2 + VAL-VAR[3]
            t-afp3     = t-afp3 + VAL-VAR[4]
            t-afp4     = t-afp4 + VAL-VAR[5]
            VAL-VAR[1] = 0
            VAL-VAR[2] = 0
            VAL-VAR[3] = 0
            VAL-VAR[4] = 0
            VAL-VAR[5] = 0.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
  {src/adm/template/row-list.i "integral.PL-PLAN"}
  {src/adm/template/row-list.i "integral.PL-CALC"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "integral.PL-PLAN"}
  {src/adm/template/row-find.i "integral.PL-CALC"}

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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE imp_aportes V-table-Win 
PROCEDURE imp_aportes :
/*------------------------------------------------------------------------------
    Impresi�n de conceptos.
------------------------------------------------------------------------------*/

FIND PF-CIAS WHERE PF-CIAS.CodCia = s-CodCia NO-LOCK.

DEFINE VARIABLE x-Titulo  AS CHARACTER FORMAT "x(17)".
DEFINE VARIABLE x-MesAnt  AS INTEGER.
DEFINE VARIABLE x-Periodo AS INTEGER.

DEFINE FRAME F-aportaciones
    t-CodPer COLUMN-LABEL "C�digo" FORMAT "x(6)"
    t-NomPer COLUMN-LABEL "Apellidos y nombres" FORMAT "x(50)"
    t-afp0   COLUMN-LABEL "Monto    !Asegurable " FORMAT "Z,ZZZ,ZZ9.99"
    t-afp1   COLUMN-LABEL "Aporte   !Obligatorio " FORMAT "Z,ZZZ,ZZ9.99"
    t-afp2   COLUMN-LABEL "Comisi�n  !Fija    " FORMAT "Z,ZZZ,ZZ9.99"
    t-afp3   COLUMN-LABEL "Comisi�n  !Porcentual " FORMAT "Z,ZZZ,ZZ9.99"
    t-afp4   COLUMN-LABEL "Seguro de  !Invalidez  " FORMAT "Z,ZZZ,ZZ9.99"
    x-Imptot COLUMN-LABEL "Total    " FORMAT "Z,ZZZ,ZZ9.99"
    HEADER
    integral.PF-CIAS.NomCia x-titulo AT 62 "Fecha :" TO 130 TODAY TO 140
    integral.PF-CIAS.DirCia "Aportaciones de OBREROS a AFPS" AT 55 SKIP(1)
    integral.PL-AFPS.codafp integral.PL-AFPS.desafp SKIP(2)
    WITH DOWN NO-BOX STREAM-IO WIDTH 140.

RUN bin/_mes.r( FILL-IN-NRO-MES, 1 , OUTPUT x-titulo ).

ASSIGN x-Titulo = x-Titulo + " - " + STRING(s-Periodo, "9,999").

IF FILL-IN-NRO-MES = 1 THEN
    ASSIGN
        x-MesAnt = 12
        x-Periodo = s-Periodo - 1.
ELSE 
    ASSIGN
        x-MesAnt = FILL-IN-NRO-MES - 1
        x-Periodo = s-Periodo.

IF T-Todos = TRUE THEN DO:
    FOR EACH PL-SEM WHERE
        PL-SEM.Periodo = S-Periodo AND
        PL-SEM.NroMes  = FILL-IN-NRO-MES NO-LOCK:
        RUN acumula_aportes( PL-SEM.NroSem, 1, PL-SEM.Periodo ). /* SEMANAL */   
        RUN acumula_aportes( PL-SEM.NroSem, 4, PL-SEM.Periodo ). /* GRATIFICACIONES */
    END.
    FOR EACH PL-SEM WHERE
        PL-SEM.PERIODO = x-Periodo AND
        PL-SEM.NROMES  = x-MesAnt NO-LOCK:          
        RUN acumula_aportes( PL-SEM.NroSem, 3, x-periodo ). /* VACACIONES */
    END.
END.
ELSE DO:
    IF integral.PL-CALC.CodCal = 3 THEN
        FOR EACH PL-SEM WHERE
            PL-SEM.PERIODO = x-Periodo AND
            PL-SEM.NROMES  = x-MesAnt NO-LOCK:          
            RUN acumula_aportes( PL-SEM.NroSem, integral.PL-CALC.CodCal, PL-SEM.Periodo ).
        END.
    ELSE
        FOR EACH PL-SEM WHERE
            PL-SEM.Periodo = S-Periodo AND
            PL-SEM.NroMes  = FILL-IN-NRO-MES NO-LOCK:
            RUN acumula_aportes( PL-SEM.NroSem, integral.PL-CALC.CodCal, PL-SEM.Periodo ).
        END.
END.

ASSIGN FRAME F-msg:TITLE = "Imprimiendo...".

DO i = 1 TO FILL-IN-Copias:

    DISPLAY
        "Imprimiendo Copia # " + STRING(i,"Z9") @
        FILL-IN-Des WITH FRAME F-msg.

    OUTPUT STREAM strm-concep TO PRINTER PAGED PAGE-SIZE 66.

    PUT STREAM strm-concep CONTROL "~033@~0335~033F~033P~033x~001~033E~033C" CHR(66).
    PUT STREAM strm-concep CONTROL "~033x" NULL "~017~033P".

    FOR EACH tmp-aporte NO-LOCK
        BREAK BY t-CodAfp
        BY t-CodPer:
        IF FIRST-OF(t-CodAfp) THEN DO:
            FIND PL-AFPS WHERE PL-AFPS.codafp = t-CodAfp NO-LOCK NO-ERROR.
            IF AVAILABLE PL-AFPS THEN
                DISPLAY PL-AFPS.desafp @ FILL-IN-afp WITH FRAME F-msg.
        END.

        DISPLAY t-CodPer @ FILL-IN-Codigo WITH FRAME F-msg.

        ASSIGN x-Imptot = t-afp1 + t-afp2 + t-afp3 + t-afp4.

        DISPLAY STREAM strm-concep
            t-CodPer
            t-NomPer
            t-afp0
            t-afp1
            t-afp2
            t-afp3
            t-afp4
            x-ImpTot
            WITH FRAME F-aportaciones.

        ACCUMULATE t-afp0 (SUB-TOTAL BY t-codafp).
        ACCUMULATE t-afp1 (SUB-TOTAL BY t-codafp).
        ACCUMULATE t-afp2 (SUB-TOTAL BY t-codafp).
        ACCUMULATE t-afp3 (SUB-TOTAL BY t-codafp).
        ACCUMULATE t-afp4 (SUB-TOTAL BY t-codafp).
        ACCUMULATE x-Imptot (SUB-TOTAL BY t-codafp).

        IF LAST-OF(t-CodAfp) THEN DO:
            UNDERLINE STREAM strm-concep
                t-afp0
                t-afp1
                t-afp2
                t-afp3
                t-afp4
                x-ImpTot
                WITH FRAME F-aportaciones.
            DISPLAY STREAM strm-concep
                "TOTALES : " @ t-NomPer
                ACCUM SUB-TOTAL BY (t-codafp) t-afp0 @ t-afp0
                ACCUM SUB-TOTAL BY (t-codafp) t-afp1 @ t-afp1
                ACCUM SUB-TOTAL BY (t-codafp) t-afp2 @ t-afp2
                ACCUM SUB-TOTAL BY (t-codafp) t-afp3 @ t-afp3
                ACCUM SUB-TOTAL BY (t-codafp) t-afp4 @ t-afp4
                ACCUM SUB-TOTAL BY (t-codafp) x-ImpTot @ x-ImpTot
                WITH FRAME F-aportaciones.
            PAGE STREAM strm-concep.
        END.
    END.
    
    HIDE FRAME F-msg.

    OUTPUT STREAM strm-concep CLOSE.

    FOR EACH tmp-aporte:
        DELETE tmp-aporte.
    END.

END.

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

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

    ASSIGN
        FILL-IN-NRO-MES     = s-NroMes
        FILL-IN-Copias  = 1
        T-Todos             = TRUE.

    DISPLAY
        FILL-IN-NRO-MES
        FILL-IN-Copias
        T-Todos
        WITH FRAME F-Main.

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
  {src/adm/template/snd-list.i "integral.PL-PLAN"}
  {src/adm/template/snd-list.i "integral.PL-CALC"}

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

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
      {src/adm/template/vstates.i}
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


