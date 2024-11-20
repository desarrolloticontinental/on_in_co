/* pruebas performnace */
DEF TEMP-TABLE t-clie NO-UNDO LIKE gn-clie.

FOR each gn-clie WHERE codcia = 0 NO-LOCK:
    CREATE t-clie.
    BUFFER-COPY gn-clie TO t-clie.
END.

DEF VAR A AS INT64 NO-UNDO.
DEF VAR X AS INTE NO-UNDO.

X = 0.
a = ETIME(YES).
FOR EACH t-clie NO-LOCK:
    X = X + 1.
END.

MESSAGE ETIME X.
