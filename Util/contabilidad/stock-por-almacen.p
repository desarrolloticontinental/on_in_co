DEF VAR x-codalm AS CHAR.
DEF VAR x-fecha AS DATE.
DEF VAR p-fecha AS DATE.

DEF STREAM salida.

OUTPUT STREAM salida TO c:\tmp\stock-por-almacen.txt.
REPEAT :
    PROMPT-FOR x-codalm x-fecha.
    ASSIGN x-codalm x-fecha.
    p-fecha = x-fecha.
    FIND LAST almstkal WHERE codcia = 1
        AND codalm = x-codalm
        AND fecha <= x-fecha
        NO-LOCK NO-ERROR.
    IF AVAILABLE almstkal THEN DO:
        DISPLAY STREAM salida
        almstkal.codalm
        p-fecha
        almstkal.stkact
        WITH NO-BOX NO-UNDERLINE.
        PAUSE 0.
    END.
END.
OUTPUT STREAM salida CLOSE.

