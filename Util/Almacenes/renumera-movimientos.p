/* RENUMERACION DE MOVIMIENTOS */
DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR s-codalm AS CHAR NO-UNDO.
DEF VAR s-nroser AS INT NO-UNDO.
DEF VAR s-nrodoc AS INT NO-UNDO.
DEF VAR s-tipmov AS CHAR NO-UNDO.
DEF VAR s-codmov AS INT NO-UNDO.
DEF VAR s-newmov AS INT NO-UNDO.

DEF TEMP-TABLE t-cmov LIKE almcmov.
DEF TEMP-TABLE t-dmov LIKE almdmov.

REPEAT:
    EMPTY TEMP-TABLE t-cmov.
    EMPTY TEMP-TABLE t-dmov.

    PROMPT-FOR
    'Almacen:' s-codalm FORMAT 'x(3)' SKIP
    'Tipo de mov:' s-tipmov FORMAT 'x' SKIP
    'Cod. de mov:' s-codmov FORMAT '99' SKIP
    'New cod. mov:' s-newmov FORMAT '99' SKIP
    'Nro de Serie' s-nroser FORMAT '999' SKIP
    'Nro de Doc.' s-nrodoc FORMAT '9999999' SKIP
        WITH NO-LABELS.
    ASSIGN s-codalm s-tipmov s-codmov s-newmov s-nroser s-nrodoc.
    FIND almcmov WHERE almcmov.codcia = s-codcia
        AND almcmov.codalm = s-codalm
        AND almcmov.tipmov = s-tipmov
        AND almcmov.codmov = s-newmov
        AND almcmov.nroser = s-nroser
        AND almcmov.nrodoc = s-nrodoc
        NO-LOCK NO-ERROR.
    IF AVAILABLE almcmov THEN DO:
        MESSAGE 'EXISTE el movimiento' s-newmov
            VIEW-AS ALERT-BOX ERROR.
        RETRY.
    END.
    FIND almcmov WHERE almcmov.codcia = s-codcia
        AND almcmov.codalm = s-codalm
        AND almcmov.tipmov = s-tipmov
        AND almcmov.codmov = s-codmov
        AND almcmov.nroser = s-nroser
        AND almcmov.nrodoc = s-nrodoc
        NO-ERROR.
    IF NOT AVAILABLE almcmov THEN DO:
        MESSAGE 'NO existe el movimiento' s-codmov
            VIEW-AS ALERT-BOX ERROR.
        RETRY.
    END.
    /* cargamos temporales */
    CREATE t-cmov.
    BUFFER-COPY almcmov 
        TO t-cmov
        ASSIGN t-cmov.codmov = s-newmov.
    FOR EACH almdmov OF almcmov:
        CREATE t-dmov.
        BUFFER-COPY almdmov
            TO t-dmov
            ASSIGN t-dmov.codmov = s-newmov.
        DELETE almdmov.
    END.
    DELETE almcmov.
    FOR EACH t-cmov.
        CREATE almcmov.
        BUFFER-COPY t-cmov TO almcmov.
    END.
    FOR EACH t-dmov:
        CREATE almdmov.
        BUFFER-COPY t-dmov TO almdmov.
    END.
END.


