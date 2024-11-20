DISABLE TRIGGERS FOR LOAD OF almcmov.
DISABLE TRIGGERS FOR LOAD OF almdmov.

DEF BUFFER b-cmov FOR almcmov.
DEF BUFFER b-dmov FOR almdmov.

DEF VAR iNroSer AS INTE INIT 251.
DEF VAR iNroDoc AS INTE INIT 3501.

FOR EACH almcmov EXCLUSIVE-LOCK WHERE almcmov.codcia = 001
    AND almcmov.codalm = '508'
    AND almcmov.tipmov = 'S'
    AND almcmov.codmov = 03
    AND almcmov.nroser = 752
    AND almcmov.nrodoc >= 6001
    AND almcmov.nrodoc <= 6570:
    DISPLAY almcmov.nrodoc. PAUSE 0.
    CREATE b-cmov.
    BUFFER-COPY almcmov TO b-cmov
        ASSIGN
        b-cmov.nroser = iNroSer
        b-cmov.nrodoc = iNroDoc.
    FOR EACH almdmov OF almcmov EXCLUSIVE-LOCK:
        CREATE b-dmov.
        BUFFER-COPY almdmov TO b-dmov
            ASSIGN
            b-dmov.nroser = iNroSer
            b-dmov.nrodoc = iNroDoc.
        DELETE almdmov.
    END.
    ASSIGN
        almcmov.flgest = 'A'
        Almcmov.FchAnu = TODAY.
    iNroDoc = iNroDoc + 1.
END.

