DEF TEMP-TABLE t-cmov LIKE almcmov.
DEF TEMP-TABLE t-dmov LIKE almdmov.

FOR EACH almacen NO-LOCK WHERE codcia = 1
    AND codalm <> '11t':
    FOR EACH almdmov NO-LOCK WHERE almdmov.codcia = 1
        AND almdmov.codalm = almacen.codalm
        AND almdmov.tipmov = 's'
        AND almdmov.codmov = 03
        AND almdmov.almori = '11t'
        AND almdmov.fchdoc >= 12/01/2010:
        CREATE t-dmov.
        BUFFER-COPY almdmov TO t-dmov.
    END.
END.

FOR EACH almacen NO-LOCK WHERE codcia = 1
    AND codalm = '11t':
    FOR EACH almdmov NO-LOCK WHERE almdmov.codcia = 1
        AND almdmov.codalm = almacen.codalm
        AND almdmov.tipmov = 'i'
        AND almdmov.codmov = 03
        AND almdmov.fchdoc >= 12/01/2010:
        CREATE t-dmov.
        BUFFER-COPY almdmov TO t-dmov.
    END.
END.

FOR EACH almacen NO-LOCK WHERE codcia = 1
    AND codalm = '11t':
    FOR EACH almdmov NO-LOCK WHERE almdmov.codcia = 1
        AND almdmov.codalm = almacen.codalm
        AND almdmov.tipmov = 's'
        AND almdmov.codmov = 03
        AND almdmov.fchdoc >= 12/01/2010:
        CREATE t-dmov.
        BUFFER-COPY almdmov TO t-dmov.
    END.
END.

FOR EACH almacen NO-LOCK WHERE codcia = 1
    AND codalm <> '11t':
    FOR EACH almdmov NO-LOCK WHERE almdmov.codcia = 1
        AND almdmov.codalm = almacen.codalm
        AND almdmov.tipmov = 'i'
        AND almdmov.codmov = 03
        AND almdmov.almori = '11t'
        AND almdmov.fchdoc >= 12/01/2010:
        CREATE t-dmov.
        BUFFER-COPY almdmov TO t-dmov.
    END.
END.

OUTPUT TO c:\tmp\moves.txt.
FOR EACH t-dmov NO-LOCK:
    DISPLAY
        t-dmov.tipmov '|'
        t-dmov.codmov '|'
        t-dmov.codalm '|'
        t-dmov.almori '|'
        t-dmov.fchdoc '|'
        t-dmov.codmat '|'
        t-dmov.candes
        WITH STREAM-IO NO-BOX NO-LABELS WIDTH 320.
END.
