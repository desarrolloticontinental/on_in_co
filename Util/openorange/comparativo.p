DEF VAR x-linea AS CHAR.
DEF TEMP-TABLE t-matg LIKE produccion.almmmatg.

INPUT FROM c:\tmp\listaopen.prn.
REPEAT :
    IMPORT UNFORMATTED x-linea.
    IF x-linea = '' THEN LEAVE.
    CREATE t-matg.
    ASSIGN 
        t-matg.codcia = 001
        t-matg.codmat = SUBSTRING(x-linea,1,6)
        t-matg.preofi = DECIMAL(SUBSTRING(x-linea,21,20))
        t-matg.CHR__01 = SUBSTRING(x-linea,41)
        NO-ERROR.
END.
INPUT CLOSE.

FOR EACH t-matg:
    FIND produccion.almmmatg OF t-matg NO-LOCK NO-ERROR.
    IF AVAILABLE produccion.almmmatg THEN
        ASSIGN
        t-matg.prevta[1] = produccion.almmmatg.ctotot
        t-matg.prevta[2] = produccion.almmmatg.preofi.
    FIND desarrollo.almmmatg OF t-matg NO-LOCK NO-ERROR.
    IF AVAILABLE desarrollo.almmmatg THEN
        ASSIGN
        t-matg.prevta[3] = desarrollo.almmmatg.ctotot
        t-matg.prevta[4] = desarrollo.almmmatg.preofi.
END.
OUTPUT TO c:\tmp\comparativo.txt.
PUT UNFORMATTED
    'ARTCODE|VATPRICE|CURRENCY|CTOPROD|PREOFIPROD|CTODESA|PREOFIDESA'
    SKIP.
FOR EACH t-matg:
    PUT UNFORMATTED
        t-matg.codmat '|'
        t-matg.preofi '|'
        t-matg.CHR__01 '|'
        t-matg.prevta[1] '|'
        t-matg.prevta[2] '|'
        t-matg.prevta[3] '|'
        t-matg.prevta[4]
        SKIP.
END.
OUTPUT CLOSE.

