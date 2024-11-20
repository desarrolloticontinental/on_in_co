DEF VAR i AS INT INIT 1.
DEF VAR X AS CHAR.

OUTPUT TO c:\tmp\cabeceraempleados.txt.
FOR EACH pl-pers NO-LOCK WHERE (codcia = 000 OR codcia = 001)
    AND pl-pers.codper <> '':
    CASE PL-PERS.TpoDocId:
        WHEN '01' THEN X = 'DNI'.
        WHEN '04' THEN X = 'CE'.
        WHEN '07' THEN X = 'PAS'.
    END CASE.
    PUT UNFORMATTED
        '10'
        STRING(i, '999999')
        STRING(X, 'x(3)')
        STRING(pl-pers.nrodocid, 'x(12)')
        FILL(' ', 3)
        STRING(pl-pers.patper, 'x(25)')
        STRING(pl-pers.matper, 'x(25)')
        STRING(pl-pers.nomper, 'x(25)')
        FILL(' ', 60)
        FILL(' ', 92)
        SKIP.
    i = i + 1.
END.
OUTPUT CLOSE.

