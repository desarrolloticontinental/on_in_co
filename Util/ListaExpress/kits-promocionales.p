DEF TEMP-TABLE kidsku
    FIELD codkit AS CHAR
    FIELD codsku AS CHAR
    FIELD cantidad AS DEC.

DEF TEMP-TABLE t-ckits LIKE almckits.
DEF TEMP-TABLE t-dkits LIKE almdkits.

DEF VAR x-linea AS CHAR.
INPUT FROM d:\tmp\kitvssku.prn.
REPEAT :
    IMPORT UNFORMATTED x-linea.
    IF x-linea = "" THEN LEAVE.
    CREATE kidsku.
    ASSIGN
        kidsku.codkit = SUBSTRING(x-linea,1,10)
        kidsku.codsku = SUBSTRING(x-linea,11,10)
        kidsku.cantidad = DEC(SUBSTRING(x-linea,21)).
END.
INPUT CLOSE.
DEF BUFFER b-matg FOR almmmatg.
FOR EACH kidsku,
    FIRST almmmatg WHERE almmmatg.codcia = 1
    AND almmmatg.fching = TODAY
    AND almmmatg.libre_c10 = kidsku.codkit NO-LOCK,
    FIRST b-matg WHERE b-matg.codcia = 1
    AND b-matg.fching = TODAY
    AND b-matg.libre_c10 = kidsku.codsku NO-LOCK
    BREAK BY kidsku.codkit:
    DISPLAY kidsku.codkit kidsku.codsku. PAUSE 0.
    IF FIRST-OF(kidsku.codkit) THEN DO:
        CREATE t-ckits.
        ASSIGN
            t-ckits.codcia = 001
            t-ckits.codmat = almmmatg.codmat.
    END.
    CREATE t-dkits.
    BUFFER-COPY t-ckits TO t-dkits
        ASSIGN
        t-dkits.codmat2 = b-matg.codmat
        t-dkits.cantidad = kidsku.cantidad.
END.
FOR EACH t-ckits:
    CREATE almckits.
    BUFFER-COPY t-ckits TO almckits NO-ERROR.
    IF ERROR-STATUS:ERROR THEN UNDO, NEXT.
END.
FOR EACH t-dkits:
    CREATE almdkits.
    BUFFER-COPY t-dkits TO almdkits NO-ERROR.
    IF ERROR-STATUS:ERROR THEN UNDO, NEXT.
END.
