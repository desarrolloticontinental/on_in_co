DEF BUFFER b-cpedi FOR faccpedi.
DEF BUFFER b-dpedi FOR facdpedi.

FOR EACH faccpedi NO-LOCK WHERE faccpedi.codcia = 001
    AND faccpedi.coddiv = '00015'
    AND faccpedi.coddoc = 'COT'
    AND faccpedi.fchped = TODAY                      
    AND faccpedi.flgest <> 'a':
    FIND FIRST b-cpedi WHERE b-cpedi.codcia = 001
        AND b-cpedi.coddiv = '00015'
        AND b-cpedi.coddoc = 'PPX'
        AND b-cpedi.codcli = faccpedi.codcli
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE b-cpedi THEN NEXT.
    FOR EACH facdpedi OF faccpedi WHERE INDEX(facdpedi.libre_c03,'*') = 0,
        FIRST b-dpedi OF b-cpedi NO-LOCK WHERE b-dpedi.codmat = facdpedi.codmat
        AND b-dpedi.libre_c03 <> facdpedi.libre_c03:
        DISPLAY faccpedi.coddoc faccpedi.nroped
            facdpedi.codmat facdpedi.libre_c03 FORMAT 'x(25)' b-dpedi.libre_c03 FORMAT 'x(25)'
            WITH STREAM-IO NO-BOX.
        PAUSE 0.
        facdpedi.libre_c03 = TRIM(b-dpedi.libre_c03) + '|*'.
    END.

END.
