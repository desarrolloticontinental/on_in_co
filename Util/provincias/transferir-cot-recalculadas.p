DEF TEMP-TABLE t-cab LIKE faccpedi.
DEF TEMP-TABLE t-det LIKE facdpedi.

INPUT FROM c:\tmp\faccpedi.d.
REPEAT :
    CREATE t-cab.
    IMPORT t-cab.
END.
INPUT CLOSE.
INPUT FROM c:\tmp\facdpedi.d.
REPEAT :
    CREATE t-det.
    IMPORT t-det.
END.
INPUT CLOSE.

DISABLE TRIGGERS FOR LOAD OF faccpedi.
DISABLE TRIGGERS FOR LOAD OF facdpedi.

FOR EACH t-cab WHERE t-cab.nroped <> '', FIRST faccpedi OF t-cab.
    FOR EACH facdpedi OF faccpedi:
        DELETE facdpedi.
    END.
    FOR EACH t-det OF t-cab.
        CREATE facdpedi.
        BUFFER-COPY t-det TO facdpedi.
    END.
    BUFFER-COPY t-cab TO faccpedi.
    DISPLAY t-cab.nroped t-cab.imptot faccpedi.nroped faccpedi.imptot
        WITH STREAM-IO NO-BOX.
    PAUSE 0.
END.
