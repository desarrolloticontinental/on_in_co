DISABLE TRIGGERS FOR LOAD OF facdpedi.

DEF TEMP-TABLE detalle
    FIELD codold AS CHAR
    FIELD codnew AS CHAR
    INDEX llave01 AS PRIMARY codold.
DEF VAR x-linea AS CHAR NO-UNDO.

INPUT FROM d:\tmp\cambio.prn.
REPEAT:
    IMPORT UNFORMATTED x-linea .
    IF x-linea = '' THEN LEAVE.
    CREATE detalle.
    detalle.codold = SUBSTRING(x-linea,1,6).
    detalle.codnew = SUBSTRING(x-linea,11,6).
END.
INPUT CLOSE.

FOR EACH faccpedi NO-LOCK WHERE codcia = 1
    AND coddoc = 'cot'
    AND LOOKUP(coddiv, '00018,10067') > 0
    AND flgest = 'P'
    AND fchped >= DATE(07,01,2017)
    AND LOOKUP(libre_c01, '00018,00015,20067') > 0:
    DISPLAY coddiv coddoc nroped fchped flgest libre_c01 
        WITH STREAM-IO NO-BOX WIDTH 320.
    PAUSE 0.
    FOR EACH detalle NO-LOCK:
        FIND facdpedi OF faccpedi WHERE facdpedi.codmat = detalle.codold
            EXCLUSIVE-LOCK NO-ERROR.
        IF AVAILABLE facdpedi THEN facdpedi.codmat = detalle.codnew.
    END.
END.
