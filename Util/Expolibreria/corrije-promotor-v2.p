DEF BUFFER b-dpedi FOR facdpedi.
DEF BUFFER b-cpedi FOR faccpedi.
FOR EACH faccpedi NO-LOCK WHERE faccpedi.codcia = 1
    AND faccpedi.coddiv = '00015'
    AND faccpedi.coddoc = 'cot'
    AND faccpedi.fchped >= TODAY - 1:
    FIND FIRST facdpedi OF faccpedi WHERE TRUE <> (facdpedi.libre_c03 > '')
        AND CAN-FIND(FIRST almmmatg OF facdpedi 
                     WHERE almmmatg.codpr1 = '51135890' NO-LOCK)
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE facdpedi THEN NEXT.
    FIND FIRST b-cpedi WHERE b-cpedi.codcia = 001
        AND b-cpedi.coddiv = '00015'
        AND b-cpedi.coddoc = 'PPX'
        AND b-cpedi.codcli = faccpedi.codcli
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE b-cpedi THEN NEXT.
    FIND FIRST b-dpedi OF b-cpedi WHERE b-dpedi.libre_c03 > '' 
        AND NUM-ENTRIES(b-dpedi.libre_c03,'|') >= 3
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE b-dpedi THEN NEXT.
    DISPLAY facdpedi.codmat facdpedi.libre_c03 b-dpedi.libre_c03
        WITH 1 COL STREAM-IO.
    PAUSE 0.
    FOR EACH facdpedi OF faccpedi WHERE TRUE <> (facdpedi.libre_c03 > ''),
        FIRST almmmatg OF facdpedi WHERE almmmatg.codpr1 = '51135890':
        facdpedi.libre_c03 = TRIM(b-dpedi.libre_c03) + '|*'.
    END.

END.
