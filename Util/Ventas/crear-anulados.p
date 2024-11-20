DEF BUFFER b-cdocu FOR ccbcdocu.
DEF BUFFER x-cdocu FOR ccbcdocu.
DEF VAR s-coddoc AS CHAR INIT 'BOL'.

DEFINE VAR x-nrodesde AS INT64.
DEFINE VAR x-nrohasta AS INT64.

DEFINE VAR x-nrodoc AS CHAR.
DEFINE VAR x-nrodocref AS INT64.
DEFINE VAR x-nroref AS CHAR.

DEFINE VAR x-fechaemi AS DATE.

s-coddoc = 'FAC'.
/* Documentos a Generar como ANULADOS */
x-nrodesde = 77500002019.
x-nrohasta = 77500002019.

/* Documento como base de referencia para crear los otros documentos */
x-nrodocref = 77500002022.

SESSION:SET-WAIT-STATE('GENERAL').

/* REFERENCIA */
x-nroref = STRING(x-nrodocref, '99999999999').

/* Documentos para tomar de Referencia */
FIND ccbcdocu WHERE codcia = 1
    AND coddoc = s-coddoc
    AND nrodoc = x-nroref NO-LOCK NO-ERROR.
IF AVAILABLE ccbcdocu THEN DO:
    x-fechaemi = ccbcdocu.fchdoc.
    DEF VAR k AS INT64.
    DO k = x-nrodesde TO x-nrohasta:
        x-nrodoc = STRING(k, '99999999999').
        FIND FIRST x-cdocu WHERE x-cdocu.codcia = 1 AND 
                                x-cdocu.coddoc = s-coddoc AND 
                                x-cdocu.nrodoc = x-nrodoc NO-LOCK NO-ERROR.
        IF NOT AVAILABLE x-cdocu THEN DO:
            CREATE b-cdocu.
            BUFFER-COPY ccbcdocu TO b-cdocu
                ASSIGN
                b-cdocu.fchdoc = x-fechaemi    
                b-cdocu.flgest = 'A'
                b-cdocu.nrodoc = x-nrodoc .  /*STRING(k, '99999999999').*/
        END.
        ELSE DO:
            MESSAGE "Documento ya existe :" + x-nrodoc.
        END.
    END.  
END.
ELSE DO:
    MESSAGE "Documento de Referencia NO Existe".
END.          

MESSAGE "Proceso completado".

SESSION:SET-WAIT-STATE('').
