DEF VAR s-codcia AS INT INIT 001.
DEF VAR i AS INT INIT 1.
DEF VAR x-Moneda AS CHAR INIT "S/".
DEF VAR s-Periodo AS INT INIT 2012.
DEF VAR s-NroMes  AS INT INIT 03.
DEF VAR x-Secuencia AS INT INIT 1.
DEF VAR x-DocEmp AS CHAR.
DEF VAR x-Empleados AS INT.
DEF VAR x-Cuentas AS INT.
DEF TEMP-TABLE t-flg-mes LIKE pl-flg-mes.

FOR EACH pl-flg-mes NO-LOCK WHERE pl-flg-mes.codcia = s-codcia
    AND pl-flg-mes.periodo = s-Periodo
    AND pl-flg-mes.nromes = s-NroMes
    AND pl-flg-mes.codpln = 01
    AND ( ( pl-flg-mes.cnpago BEGINS 'BANCO DE CREDITO'
            AND pl-flg-mes.nrodpt <> '' )
          OR ( pl-flg-mes.cts BEGINS 'BANCO DE CREDITO' 
               AND pl-flg-mes.nrodpt-cts <> '' ) ):
    CREATE t-flg-mes.
    BUFFER-COPY pl-flg-mes TO t-flg-mes.
    t-flg-mes.nrodpt = REPLACE(pl-flg-mes.NroDpt, '-', '').
    t-flg-mes.nrodpt-cts = REPLACE(pl-flg-mes.NroDpt-Cts, '-', '').
END.

OUTPUT TO c:\tmp\cuentas-bcp-empleados-mar.txt.
FOR EACH t-flg-mes NO-LOCK WHERE 
    ( ( t-flg-mes.cnpago BEGINS 'BANCO DE CREDITO'
        AND LENGTH(TRIM(t-flg-mes.nrodpt)) = 14 )
      OR ( t-flg-mes.cts BEGINS 'BANCO DE CREDITO' 
           AND LENGTH(TRIM(t-flg-mes.nrodpt-cts)) = 14 ) ),
    FIRST pl-pers NO-LOCK WHERE pl-pers.codper = t-flg-mes.codper
    BREAK BY t-flg-mes.codcia BY t-flg-mes.codper:
    IF FIRST-OF(t-flg-mes.codper) THEN DO:
        x-Empleados = x-Empleados + 1.
        /* Cabecera */
        CASE PL-PERS.TpoDocId:
            WHEN '01' THEN x-DocEmp = 'DNI'.
            WHEN '04' THEN x-DocEmp = 'CE'.
            WHEN '07' THEN x-DocEmp = 'PAS'.
        END CASE.
        PUT UNFORMATTED
            '10'
            STRING(x-Secuencia, '999999')
            STRING(x-DocEmp, 'x(3)')
            STRING(pl-pers.nrodocid, 'x(12)')
            FILL(' ', 3)
            STRING(pl-pers.patper, 'x(25)')
            STRING(pl-pers.matper, 'x(25)')
            STRING(pl-pers.nomper, 'x(25)')
            FILL(' ', 60)
            FILL(' ', 92)
            SKIP.
        x-Secuencia = x-Secuencia + 1.
    END.
    /* Detalle */
    /* Cuenta de Ahorros */
    IF t-flg-mes.cnpago BEGINS 'BANCO DE CREDITO'
        AND t-flg-mes.nrodpt <> '' THEN DO:
        x-Cuentas = x-Cuentas + 1.
        PUT UNFORMATTED
            '20'
            STRING(x-Secuencia, '999999')
            STRING("A", 'x')
            STRING(t-flg-mes.NroDpt, 'x(20)')
            STRING("RUC", 'x(3)')
            STRING("20100038146", 'x(12)')
            STRING("S/", 'x(2)')
            FILL(' ', 207)
            SKIP.
        x-Secuencia = x-Secuencia + 1.
    END.
    /* Cuenta de CTS */
    IF t-flg-mes.cts BEGINS 'BANCO DE CREDITO'
        AND t-flg-mes.nrodpt-cts <> '' THEN DO:
        x-Cuentas = x-Cuentas + 1.
        IF INDEX(t-flg-mes.nrodpt-cts, 'DOLARES') > 0 THEN x-Moneda = "US".
        IF INDEX(t-flg-mes.nrodpt-cts, 'SOLES') > 0 THEN x-Moneda = "S/".
        PUT UNFORMATTED
            '20'
            STRING(x-Secuencia, '999999')
            STRING("T", 'x')
            STRING(t-flg-mes.NroDpt-Cts, 'x(20)')
            STRING("RUC", 'x(3)')
            STRING("20100038146", 'x(12)')
            STRING(x-Moneda, 'x(2)')
            FILL(' ', 207)
            SKIP.
        x-Secuencia = x-Secuencia + 1.
    END.
    IF LAST-OF(t-flg-mes.codcia) THEN DO:
        /* Filler del archivo */
        PUT UNFORMATTED
            '99'
            STRING(x-Secuencia, '999999')
            STRING(x-Empleados, '999999')
            STRING(x-Cuentas, '999999')
            FILL(' ', 233)
            SKIP.
    END.
END.
OUTPUT CLOSE.
