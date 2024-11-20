DISABLE TRIGGERS FOR LOAD OF almdmov.
DISABLE TRIGGERS FOR LOAD OF ccbddocu.

DEF VAR x-codmat AS CHAR NO-UNDO.
DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR x-fchdoc AS DATE NO-UNDO.

DO:
    x-codmat = '005206'.
    FIND almmmatg WHERE almmmatg.codcia = s-codcia
        AND almmmatg.codmat = x-codmat
        EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
    IF NOT AVAILABLE almmmatg THEN LEAVE.
    DISPLAY 'INICIO:' DATETIME(TODAY,MTIME). PAUSE 0.
    ASSIGN
        almmmatg.undbas = 'PAQ500'
        almmmatg.undcmp = 'PAQ500'
        almmmatg.undstk = 'PAQ500'
        almmmatg.CHR__01 = 'PAQ500'
        almmmatg.undalt[1] = 'PAQ500'
        Almmmatg.FacEqu = 1.
    x-fchdoc = ?.
    FOR EACH almdmov WHERE codcia = s-codcia AND codmat = x-codmat,
        FIRST Almmmatg OF almdmov NO-LOCK:
        FIND Almtconv WHERE Almtconv.CodUnid  = Almmmatg.UndBas
            AND Almtconv.Codalter = almdmov.codund
            NO-LOCK NO-ERROR.
        IF AVAILABLE almtconv THEN almdmov.factor = Almtconv.Equival.
        IF x-fchdoc = ? THEN x-fchdoc = almdmov.fchdoc.
        ELSE x-fchdoc = MINIMUM(x-fchdoc,almdmov.fchdoc).
    END.
    DISPLAY DATETIME(TODAY,MTIME). PAUSE 0.
    FOR EACH gn-divi WHERE gn-divi.codcia = s-codcia,
        EACH ccbddocu WHERE ccbddocu.codcia = s-codcia
        AND ccbddocu.codmat = x-codmat,
        FIRST almmmatg OF ccbddocu NO-LOCK:
        FIND Almtconv WHERE Almtconv.CodUnid  = Almmmatg.UndBas
            AND Almtconv.Codalter = ccbddocu.undvta
            NO-LOCK NO-ERROR.
        IF AVAILABLE almtconv THEN ccbddocu.factor = Almtconv.Equival.
    END.
    DISPLAY DATETIME(TODAY,MTIME). PAUSE 0.
    FOR EACH facdpedi WHERE facdpedi.codcia = s-codcia 
        AND facdpedi.codmat = x-codmat,
        FIRST Almmmatg OF facdpedi NO-LOCK:
        FIND Almtconv WHERE Almtconv.CodUnid  = Almmmatg.UndBas
            AND Almtconv.Codalter = facdpedi.undvta
            NO-LOCK NO-ERROR.
        IF AVAILABLE almtconv THEN facdpedi.factor = Almtconv.Equival.
    END.
    DISPLAY DATETIME(TODAY,MTIME). PAUSE 0.
    RUN alm/calc-costo-promedio (x-codmat, x-fchdoc).
    DISPLAY x-codmat x-fchdoc DATETIME(TODAY,MTIME).
END.


