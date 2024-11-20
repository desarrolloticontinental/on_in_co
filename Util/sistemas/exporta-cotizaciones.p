DEF TEMP-TABLE t-cpedi LIKE faccpedi.
DEF TEMP-TABLE t-dpedi LIKE facdpedi.

FOR EACH faccpedi NO-LOCK WHERE codcia = 1
    AND coddoc = 'cot'
    AND fchped >= DATE(04,01,2019)
    AND flgest <> 'a':
    CREATE t-cpedi.
    BUFFER-COPY faccpedi TO t-cpedi.
    ASSIGN
        t-cpedi.flgest = 'P'.
    FOR EACH facdpedi OF faccpedi NO-LOCK:
        CREATE t-dpedi.
        BUFFER-COPY facdpedi TO t-dpedi.
        ASSIGN
            t-dpedi.canate = 0.
    END.
END.

OUTPUT TO d:\faccpedi.d.
FOR EACH t-cpedi NO-LOCK:
    EXPORT t-cpedi.
END.
OUTPUT TO d:\facdpedi.d.
FOR EACH t-dpedi NO-LOCK:
    EXPORT t-dpedi.
END.
OUTPUT CLOSE.

