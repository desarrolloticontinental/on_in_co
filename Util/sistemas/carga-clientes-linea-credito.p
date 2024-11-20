
DEF TEMP-TABLE t-clieL NO-UNDO LIKE gn-clieL.
DEF VAR Inicio AS INT64 NO-UNDO.

Inicio = ETIME(YES).

INPUT FROM d:\gn-clieL.d.
REPEAT:
    CREATE t-clieL.
    IMPORT t-clieL.
END.
INPUT CLOSE.

DISABLE TRIGGERS FOR LOAD OF gn-clieL.

FOR EACH t-clieL NO-LOCK WHERE t-clieL.codcia = 0:

    /*FIND FIRST gn-clieL OF t-clieL EXCLUSIVE-LOCK NO-ERROR.*/

    FIND FIRST gn-clieL WHERE gn-clieL.codcia = t-clieL.codcia AND
                                gn-clieL.codcli = t-clieL.codcli AND
                                gn-clieL.FchFin = t-clieL.FchFin EXCLUSIVE-LOCK NO-ERROR.

    IF NOT AVAILABLE gn-clieL THEN CREATE gn-clieL.
    BUFFER-COPY t-clieL TO gn-clieL.

END.

DISPLAY (ETIME / 1000) 'segundos' WITH STREAM-IO NO-BOX.

