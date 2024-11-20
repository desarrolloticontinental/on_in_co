DEF TEMP-TABLE t-cpedi LIKE faccpedi.
DEF TEMP-TABLE t-dpedi LIKE facdpedi.

DISABLE TRIGGERS FOR LOAD OF faccpedi.
DISABLE TRIGGERS FOR LOAD OF facdpedi.


INPUT FROM d:\faccpedi.d.
REPEAT :
    CREATE t-cpedi.
    IMPORT t-cpedi.
END.
INPUT CLOSE.
INPUT FROM d:\facdpedi.d.
REPEAT:
    CREATE t-dpedi.
    IMPORT t-dpedi.
END.
INPUT CLOSE.


FOR EACH t-cpedi, FIRST faccpedi WHERE faccpedi.codcia = t-cpedi.codcia
    AND faccpedi.coddiv = t-cpedi.coddiv
    AND faccpedi.coddoc = t-cpedi.coddoc
    AND faccpedi.nroped = t-cpedi.nroped EXCLUSIVE-LOCK:
    BUFFER-COPY t-cpedi 
        EXCEPT t-cpedi.codcia t-cpedi.coddiv t-cpedi.coddoc t-cpedi.nroped
        TO faccpedi.
END.

FOR EACH t-dpedi, FIRST facdpedi WHERE facdpedi.codcia = t-dpedi.codcia
    AND facdpedi.coddoc = t-dpedi.coddoc 
    AND facdpedi.nroped = t-dpedi.nroped
    AND facdpedi.codmat = t-dpedi.codmat EXCLUSIVE-LOCK:
    BUFFER-COPY t-dpedi 
        EXCEPT t-dpedi.codcia t-dpedi.coddoc t-dpedi.nroped t-dpedi.codmat
        TO facdpedi.
END.
