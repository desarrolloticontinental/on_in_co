DEF VAR i AS INT INIT 1.
DEF VAR x-Moneda AS CHAR INIT "S/".

OUTPUT TO c:\tmp\cuentas-abonos-empleados.txt.
FOR EACH pl-flg-mes NO-LOCK WHERE pl-flg-mes.codcia = 001
    AND pl-flg-mes.periodo = YEAR(TODAY)
    AND pl-flg-mes.nromes = MONTH(TODAY)
    AND pl-flg-mes.codpln = 01
    AND pl-flg-mes.cts BEGINS 'BANCO DE CREDITO'
    AND pl-flg-mes.nrodpt <> '',
    FIRST pl-pers NO-LOCK WHERE pl-pers.codper = pl-flg-mes.codper:
    PUT UNFORMATTED
        '20'
        STRING(i, '999999')
        STRING("A", 'x')
        STRING(REPLACE(NroDpt, '-', ''), 'x(20)')
        STRING("RUC", 'x(3)')
        STRING("20100038146", 'x(12)')
        STRING(x-Moneda, 'x(2)')
        FILL(' ', 207)
        SKIP.
    i = i + 1.
END.
OUTPUT CLOSE.

