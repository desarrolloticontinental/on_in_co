/* Generar un asiento en base a otro */
DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR cb-codcia AS INT INIT 000 NO-UNDO.

DEF VAR s-codope AS CHAR INIT '076'.
DEF VAR s-periodo AS INT INIT 2015.
DEF VAR s-nromes AS INT INIT 05.
DEF VAR s-mesfin AS INT INIT 4.
DEF VAR s-nroast AS CHAR.

DEFINE VAR p-nroast  AS CHAR NO-UNDO.
DEFINE VAR x-nroast  AS INTE NO-UNDO.

DEF BUFFER B-CMOV FOR cb-cmov.
DEF BUFFER B-DMOV FOR cb-dmov.

REPEAT:    
    PROMPT-FOR 'Periodo:' s-periodo SKIP
        'Mes:' s-nromes SKIP
        'Operación:' s-codope SKIP
        'Asiento:' s-nroast SKIP
        'Mes destino:' s-mesfin 
        WITH NO-LABELS.
    ASSIGN s-periodo s-nromes s-codope s-nroast s-mesfin.
    FIND B-CMOV WHERE B-CMOV.codcia = s-codcia
        AND B-CMOV.periodo =  s-periodo
        AND B-CMOV.nromes = s-nromes
        AND B-CMOV.codope = s-codope
        AND B-CMOV.nroast = s-nroast
        NO-ERROR.
    IF NOT AVAILABLE B-CMOV THEN DO:      
        MESSAGE "Asiento NO registrado." .      
        UNDO, RETRY.    
    END.
    RUN cbd/cbdnast.p(cb-codcia,
                      s-codcia, 
                      s-periodo, 
                      s-mesfin, 
                      s-codope, 
                      OUTPUT x-nroast). 
    p-nroast = STRING(x-nroast, '999999').
    FIND cb-cmov WHERE cb-cmov.codcia  = s-codcia 
        AND cb-cmov.PERIODO = s-periodo 
        AND cb-cmov.NROMES  = s-mesfin
        AND cb-cmov.CODOPE  = s-codope 
        AND cb-cmov.NROAST  = p-nroast NO-ERROR.
    IF NOT AVAILABLE cb-cmov THEN DO:
        CREATE cb-cmov.
        BUFFER-COPY B-CMOV TO cb-cmov
            ASSIGN
            cb-cmov.NROMES  = s-mesfin
            cb-cmov.NROAST  = p-nroast. 
        FOR EACH B-DMOV OF B-CMOV NO-LOCK:
            CREATE cb-dmov.
            BUFFER-COPY B-DMOV TO cb-dmov
                ASSIGN
                cb-dmov.nromes = cb-cmov.nromes
                cb-dmov.nroast = cb-cmov.nroast.
        END.
        MESSAGE 'asiento generado:' p-nroast.
    END.
END.
