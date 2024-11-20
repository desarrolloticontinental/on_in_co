/* OJO: ANTES DE PROCESAR PROCEDER A "LIMPIAR" EL faccorre.d */

DEF TEMP-TABLE t-corre LIKE faccorre.

INPUT FROM d:\faccorre.d.
REPEAT :
    CREATE t-corre.
    IMPORT t-corre NO-ERROR.
    IF ERROR-STATUS:ERROR THEN UNDO.
END.
INPUT CLOSE.

DISABLE TRIGGERS FOR LOAD OF faccorre.

FOR EACH t-corre NO-LOCK WHERE t-corre.codcia = 1:
    FIND faccorre WHERE faccorre.codcia = t-corre.codcia AND
        faccorre.coddiv = t-corre.coddiv AND
        faccorre.coddoc = t-corre.coddoc AND
        faccorre.nroser = t-corre.nroser
        EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE faccorre THEN CREATE faccorre.
    BUFFER-COPY t-corre TO faccorre NO-ERROR.
    IF ERROR-STATUS:ERROR THEN UNDO.
END.
