DEF BUFFER b-cpedi FOR faccpedi.
DEF BUFFER b-dpedi FOR facdpedi.

FOR EACH faccpedi NO-LOCK WHERE codcia = 1
    AND coddiv = '00015'
    AND fchped >= 10/01/2011
    AND coddoc = 'cot'
    AND LOOKUP(codven, '015,017,173,900,901,902') > 0:
    CREATE b-cpedi.
    BUFFER-COPY faccpedi TO b-cpedi
        ASSIGN
        b-cpedi.coddiv = '00018'
        b-cpedi.nroped = '018' + SUBSTRING(faccpedi.nroped,4)
        b-cpedi.usuario = 'VTA-18'.
    FOR EACH facdpedi OF faccpedi:
        CREATE b-dpedi.
        BUFFER-COPY facdpedi TO b-dpedi
            ASSIGN
            b-dpedi.coddiv = b-cpedi.coddiv
            b-dpedi.nroped = b-cpedi.nroped.
    END.
END.
