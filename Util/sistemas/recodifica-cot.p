DEF NEW SHARED VAR s-codcia AS INT.
DEF NEW SHARED VAR s-coddiv AS CHAR.

DISABLE TRIGGERS FOR LOAD OF facdpedi.

DEF VAR s-codori AS CHAR.
DEF VAR s-codfin AS CHAR.

ASSIGN
    s-codcia = 001
    s-coddiv = '00015,10060,10067,10065,10011,10031,10032,10038,10039'
    s-codori = '033235'
    s-codfin = '093636'.

DEF VAR x-archivo AS CHAR.
x-archivo = 'd:\det' + s-codori + '.d'.
OUTPUT TO VALUE(x-archivo).
FOR EACH faccpedi EXCLUSIVE-LOCK WHERE faccpedi.codcia = s-codcia AND
    LOOKUP(faccpedi.coddiv, s-coddiv) > 0 AND
    faccpedi.coddoc = 'cot' AND
    faccpedi.fchped >= DATE(09,20,2019) AND
    faccpedi.flgest = 'P':
    FIND FIRST facdpedi OF faccpedi WHERE facdpedi.canate <> 0 NO-LOCK NO-ERROR.
    IF AVAILABLE facdpedi THEN NEXT.
    FOR EACH facdpedi OF faccpedi EXCLUSIVE-LOCK WHERE facdpedi.codmat = s-codori:
        EXPORT facdpedi.
        ASSIGN facdpedi.codmat = s-codfin.
        LEAVE.
    END.
END.
OUTPUT CLOSE.

