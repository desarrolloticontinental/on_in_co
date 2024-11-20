&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Method-Library 
/*--------------------------------------------------------------------------
    Library     : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE NEW SHARED VARIABLE input-var-1 AS CHARACTER.
DEFINE NEW SHARED VARIABLE input-var-2 AS CHARACTER.
DEFINE NEW SHARED VARIABLE input-var-3 AS CHARACTER.
DEFINE NEW SHARED VARIABLE output-var-1 AS ROWID.
DEFINE NEW SHARED VARIABLE output-var-2 AS CHARACTER.
DEFINE NEW SHARED VARIABLE output-var-3 AS CHARACTER.

DEFINE VARIABLE HANDLE-CAMPO  AS WIDGET-HANDLE.
DEFINE VARIABLE BUTTON-LOOKUP AS WIDGET-HANDLE.
DEFINE VARIABLE PARIENTE      AS WIDGET-HANDLE.
DEFINE VARIABLE load-imagen   AS LOGICAL.
DEFINE VARIABLE program_name  AS CHARACTER.
DEFINE VARIABLE program_call  AS CHARACTER.
DEFINE VARIABLE titulo-look   AS CHARACTER.

ASSIGN program_call = PROGRAM-NAME(1).

CREATE BUTTON BUTTON-LOOKUP
    ASSIGN
        ROW = 3
        COLUMN = 5
        WIDTH = 2.14
        HEIGHT = .81
        LABEL = ""
        FRAME = FRAME {&FRAME-NAME}:HANDLE
        SENSITIVE = TRUE
        VISIBLE = FALSE
    TRIGGERS:
        ON CHOOSE DO:
            RUN _corre-program.
            APPLY "ENTRY":U TO HANDLE-CAMPO.
        END.
        ON LEAVE DO:
            APPLY "ENTRY":U TO HANDLE-CAMPO.
        END.
    END TRIGGERS.

ASSIGN load-imagen = BUTTON-LOOKUP:LOAD-IMAGE("img/b-lookup":U).

&GLOBAL-DEFINE ENTRY_FIELD~
    IF LOOKUP(SELF:TYPE, "FRAME,WINDOW") > 0 THEN RETURN.~
    IF SELF:TYPE = "FILL-IN" THEN DO:~
        ASSIGN~
            HANDLE-CAMPO = SELF:HANDLE~
            PARIENTE     = SELF:PARENT.~
        RUN _busca-lookup(INPUT SELF:NAME, program_call, OUTPUT program_name).~
        IF program_name = "" THEN DO:~
            ASSIGN~
                BUTTON-LOOKUP:VISIBLE   = NO~
                BUTTON-LOOKUP:SENSITIVE = NO.~
            CASE HANDLE-CAMPO:DATA-TYPE:~
                WHEN "INTEGER" OR WHEN "DECIMAL" THEN~
                    ASSIGN program_name = "corre_calculadora".~
                WHEN "DATE" THEN~
                    ASSIGN program_name = "corre_calendario".~
                OTHERWISE RETURN.~
            END CASE.~
        END.~
        IF PARIENTE:TYPE = "BROWSE" THEN~
            ASSIGN BUTTON-LOOKUP:SENSITIVE = YES.~
        ELSE~
            ASSIGN~
                BUTTON-LOOKUP:ROW       = SELF:ROW~
                BUTTON-LOOKUP:COL       = SELF:COL + SELF:WIDTH~
                BUTTON-LOOKUP:HEIGHT    = SELF:HEIGHT~
                BUTTON-LOOKUP:VISIBLE   = NO~
                BUTTON-LOOKUP:SENSITIVE = YES.~
    END.~
    ELSE DO:~
        IF NOT LAST-EVENT:widget-enter = BUTTON-LOOKUP~
            OR LAST-EVENT:EVENT-TYPE   = "KEYPRESS" THEN~
            ASSIGN~
                BUTTON-LOOKUP:VISIBLE   = NO~
                BUTTON-LOOKUP:SENSITIVE = NO.~
    END.~

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Method-Library
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Method-Library ASSIGN
         HEIGHT             = 1.12
         WIDTH              = 46.86.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Method-Library 
/* ************************* Included-Libraries *********************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Method-Library 


/* ***************************  Main Block  *************************** */
ON LEAVE OF FRAME {&FRAME-NAME} DO:
    ASSIGN
        BUTTON-LOOKUP:VISIBLE   = NO
        BUTTON-LOOKUP:SENSITIVE = NO.
END.

ON "ALT-F8":U OF FRAME {&FRAME-NAME} ANYWHERE DO:
    IF LOOKUP(SELF:TYPE, "FRAME,WINDOW") > 0 THEN RETURN.
    IF SELF:TYPE = "FILL-IN" THEN DO:
        RUN bin/_inslook.r(SELF:NAME, program_call).
        APPLY "ENTRY" TO SELF.
    END.
END.

ON "MOUSE-SELECT-DBLCLICK", F8 OF FRAME {&FRAME-NAME} ANYWHERE DO:
    IF SELF:TYPE = "FILL-IN" THEN DO:
      IF BUTTON-LOOKUP:SENSITIVE = TRUE THEN APPLY "CHOOSE" TO BUTTON-LOOKUP.
    END.
END.

/*ON CTRL-F OF FRAME {&FRAME-NAME} ANYWHERE DO:
 *     IF LOOKUP(SELF:TYPE, "FRAME,WINDOW") > 0 THEN RETURN.
 *     STATUS INPUT "Field : " + SELF:NAME.
 *     /*
 *     WAIT-FOR WINDOW-CLOSE OF CURRENT-WINDOW FOCUS SELF:NAME.
 *     STATUS INPUT "".
 *     */
 * END.
 * 
 * ON "F2":U OF FRAME {&FRAME-NAME} ANYWHERE DO:
 *   RUN Procesa-Handle IN lh_Handle ('browse1').
 *   RETURN NO-APPLY.
 * END.*/

ON ENTRY OF FRAME {&FRAME-NAME} ANYWHERE DO:
    {&ENTRY_FIELD}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-adm-busca) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-busca Method-Library 
PROCEDURE adm-busca :
RUN notify('qbusca').
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-adm-imprime) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-imprime Method-Library 
PROCEDURE adm-imprime :
RUN notify('qimprime').
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_busca-lookup) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _busca-lookup Method-Library 
PROCEDURE _busca-lookup :
DEFINE INPUT  PARAMETER campo_name   AS CHARACTER.
DEFINE INPUT  PARAMETER program_call AS CHARACTER.
DEFINE OUTPUT PARAMETER program_name AS CHARACTER.

    FIND PF-G005 WHERE
        PF-G005.campo = campo_name AND
        PF-G005.programa_call = program_call NO-LOCK NO-ERROR.
    IF AVAILABLE PF-G005 THEN DO:
        ASSIGN
            program_name = PF-G005.programa_lookup
            titulo-look  = PF-G005.descripcion.
    END.
    ELSE DO:
        FIND PF-G005 WHERE
            PF-G005.campo = campo_name AND
            PF-G005.programa_call = "*" NO-LOCK NO-ERROR.
        IF AVAILABLE PF-G005 THEN
            ASSIGN
                program_name = PF-G005.programa_lookup
                titulo-look  = PF-G005.descripcion.
        ELSE
            ASSIGN
                program_name = ""
                titulo-look  = "".
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_corre-program) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _corre-program Method-Library 
PROCEDURE _corre-program :
DEFINE VARIABLE OK-SET-WAIT-STATE AS LOGICAL NO-UNDO.
OK-SET-WAIT-STATE = SESSION:SET-WAIT-STATE("GENERAL").
DO ON ERROR UNDO, LEAVE
    ON STOP UNDO, LEAVE
    ON END-KEY UNDO, LEAVE:
    CASE program_name:
        WHEN "corre_calculadora" THEN DO:
            RUN bin/_calc.r.
            IF RETURN-VALUE <> ? THEN
                HANDLE-CAMPO:SCREEN-VALUE = RETURN-VALUE.
        END.
        WHEN "corre_calendario" THEN DO:
            RUN bin/_calenda.r.
            IF RETURN-VALUE <> ? THEN
                HANDLE-CAMPO:SCREEN-VALUE = RETURN-VALUE.
        END.
        WHEN "?" THEN.
        OTHERWISE DO:
            RUN recoge-parametros.
            RUN VALUE(program_name)(titulo-look).
            IF output-var-2 <> ? THEN
                HANDLE-CAMPO:SCREEN-VALUE = output-var-2.
            RUN procesa-parametros.
        END.
    END CASE.
END.
OK-SET-WAIT-STATE = SESSION:SET-WAIT-STATE("").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

