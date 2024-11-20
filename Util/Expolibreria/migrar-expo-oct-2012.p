DISABLE TRIGGERS FOR LOAD OF faccpedi.
DISABLE TRIGGERS FOR LOAD OF facdpedi.

DEF TEMP-TABLE c-doc LIKE faccpedi.
DEF TEMP-TABLE d-doc LIKE facdpedi.

INPUT FROM C:\tmp\ExpoOct2012\faccpedi.d.
REPEAT:
    CREATE c-doc.
    IMPORT c-doc.
END.
INPUT CLOSE.

INPUT FROM C:\tmp\ExpoOct2012\facdpedi.d.
REPEAT:
    CREATE d-doc.
    IMPORT d-doc.
END.
INPUT CLOSE.

FOR EACH c-doc WHERE codcia = 0:
    DELETE c-doc.
END.

FOR EACH c-doc WHERE LOOKUP(c-doc.codven, '018,261,262,899,932,933') > 0:
    FOR EACH d-doc OF c-doc:
        ASSIGN d-doc.coddiv = "10018".
    END.
    ASSIGN c-doc.coddiv = '10018'.
END.
FOR EACH c-doc:
    CREATE faccpedi.
    BUFFER-COPY c-doc TO faccpedi.
    FOR EACH d-doc OF c-doc:
        CREATE facdpedi.
        BUFFER-COPY d-doc TO facdpedi.
    END.
END.
