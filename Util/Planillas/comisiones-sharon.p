DEF VAR x-impsol AS DEC.
DEF VAR x-impdol AS DEC.
DEF VAR x-cansol AS DEC.
DEF VAR x-candol AS DEC.
DEF VAR x-adesol AS DEC.
DEF VAR x-adedol AS DEC.

OUTPUT TO c:\tmp\sharon.txt.
FOR EACH ccbcdocu NO-LOCK WHERE codcia = 001
    AND fchdoc >= 12/15/08 AND fchdoc <= 05/15/09
    AND flgest <> 'a'
    AND coddiv = '00000'
    AND LOOKUP(coddoc, 'fac,bol,n/c') > 0
    AND LOOKUP(codven, '015,901,173,902,042,260,308') > 0:
    ASSIGN
        x-impsol = 0
        x-impdol = 0
        x-cansol = 0
        x-candol = 0
        x-adesol = 0
        x-adedol = 0.
    FIND FIRST ccbddocu OF ccbcdocu WHERE implin < 0 NO-LOCK NO-ERROR.
    IF AVAILABLE ccbddocu THEN DO:
        IF ccbcdocu.codmon = 1 THEN x-adesol = ABSOLUTE(implin).
            ELSE x-adedol = ABSOLUTE(implin).
    END.
    IF codmon = 1 THEN x-impsol = imptot.
    ELSE x-impdol = imptot.
    IF ccbcdocu.coddoc <> 'N/C' AND flgest = 'C' 
        THEN IF codmon = 1 THEN x-cansol = imptot.
            ELSE x-candol = imptot.
    ASSIGN
        x-impsol = x-impsol + x-adesol
        x-impdol = x-impdol + x-adedol
        x-cansol = x-cansol + x-adesol
        x-candol = x-candol + x-adedol.
    DISPLAY
        codven
        ccbcdocu.coddoc
        ccbcdocu.nrodoc
        ccbcdocu.fchdoc
        ccbcdocu.codcli
        nomcli
        fmapgo
        x-impsol    COLUMN-LABEL 'Importe S/.'
        x-impdol    COLUMN-LABEL 'Importe US$'
        x-cansol    COLUMN-LABEL 'Cancelado S/.'
        x-candol    COLUMN-LABEL 'Cancelado US$'
        WITH STREAM-IO NO-BOX WIDTH 200.
END.
OUTPUT CLOSE.

