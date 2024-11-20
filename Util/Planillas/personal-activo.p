OUTPUT TO c:\tmp\personal-conti.txt.
PUT 'CODIGO|APE PAT|APE MAT|NOMBRES|SECCION|CARGO|CCO' SKIP.
FOR EACH pl-flg-mes NO-LOCK WHERE codcia = 001
    AND periodo = 2012
    AND nromes = 07,
    FIRST pl-pers NO-LOCK WHERE pl-pers.codper = pl-flg-mes.codper:
    PUT UNFORMATTED
        pl-pers.codper '|'
        pl-pers.patper '|'
        pl-pers.matper '|'
        pl-pers.nomper '|'
        pl-flg-mes.seccion '|'
        pl-flg-mes.cargos '|'
        pl-flg-mes.cco
        SKIP.
END.
OUTPUT CLOSE.

