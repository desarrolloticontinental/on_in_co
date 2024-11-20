DEF TEMP-TABLE t-clie NO-UNDO LIKE gn-clie.
DEF VAR Inicio AS INT64 NO-UNDO.

Inicio = ETIME(YES).

INPUT FROM d:\gn-clie.d.
REPEAT:
    CREATE t-clie.
    IMPORT t-clie.
END.
INPUT CLOSE.

DISABLE TRIGGERS FOR LOAD OF gn-clie.

FOR EACH t-clie NO-LOCK WHERE t-clie.codcia = 0:
    FIND FIRST gn-clie OF t-clie EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE gn-clie THEN CREATE gn-clie.
    BUFFER-COPY t-clie TO gn-clie.
END.

DISPLAY (ETIME / 1000) 'segundos' WITH STREAM-IO NO-BOX.

