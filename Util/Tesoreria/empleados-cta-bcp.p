DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR s-periodo AS INT INIT 2013 NO-UNDO.
DEF VAR s-nromes AS INT INIT 02 NO-UNDO.

DEF VAR i AS INT INIT 1.
DEF VAR j AS INT INIT 1.
DEF VAR k AS INT INIT 0.
DEF VAR X AS CHAR.

OUTPUT TO c:\tmp\cuentasempleados.txt.
ASSIGN 
    i = 1
    j = 1
    k = 0.
FOR EACH pl-flg-mes NO-LOCK WHERE codcia = s-codcia
    AND periodo = s-periodo
    AND nromes = s-nromes
    AND cnpago = 'BANCO DE CREDITO',
    FIRST pl-pers OF pl-flg-mes NO-LOCK:
    /* Revisamos las cuentas de sueldo y cts */
    IF cnpago BEGINS 'BANCO DE CREDITO'
        OR cts BEGINS 'BANCO DE CREDITO' THEN DO:
        j = 1.
        CASE PL-PERS.TpoDocId:
            WHEN '01' THEN X = 'DNI'.
            WHEN '04' THEN X = 'CE'.
            WHEN '07' THEN X = 'PAS'.
        END CASE.
        PUT UNFORMATTED
            "10"
            STRING(i, '999999')
            STRING(X, 'x(3)')
            STRING( PL-PERS.NroDocId, 'x(12)')
            FILL(" ", 3)
            STRING(REPLACE(pl-pers.patper, "Ñ", "N"), 'x(25)')
            STRING(REPLACE(pl-pers.matper, "Ñ", "N"), 'x(25)')
            STRING(REPLACE(pl-pers.nomper, "Ñ", "N"), 'x(25)')
            FILL(" ", 60)
            FILL(" ", 92)
            SKIP.
        /* Cuenta Sueldo */
        IF cnpago BEGINS 'BANCO DE CREDITO' 
            AND pl-flg-mes.nrodpt <> ""
            THEN DO:
            PUT UNFORMATTED
                "20"
                STRING(j, "999999")
                "A"
                STRING(REPLACE(pl-flg-mes.nrodpt, "-", ""), 'x(20)')
                "RUC"
                "20100038146 "
                FILL(" ",209)
                SKIP.
            j = j + 1.
            k = k + 1.
        END.
        IF cts BEGINS 'BANCO DE CREDITO' 
            AND pl-flg-mes.nrodpt-cts <> ""
            THEN DO:
            PUT UNFORMATTED
                "20"
                STRING(j, "999999")
                "T"
                STRING(REPLACE(pl-flg-mes.nrodpt-cts, "-", ""), 'x(20)')
                "RUC"
                "20100038146 "
                FILL(" ",209)
                SKIP.
            j = j + 1.
            k = k + 1.
        END.
        i = i + 1.
    END.
END.
PUT UNFORMATTED
    "99"
    STRING(i, '999999')
    STRING(i - 1, '999999')
    STRING(k, '999999')
    FILL(" ", 233)
    SKIP.


OUTPUT CLOSE.

