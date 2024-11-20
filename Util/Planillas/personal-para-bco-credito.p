DEF VAR x-item AS INT.
DEF STREAM CABECERA.
DEF STREAM CUENTA.
DEF STREAM FILLER.
x-item = 1.
OUTPUT STREAM CABECERA TO c:\tmp\cabecera.txt.
OUTPUT STREAM CUENTA TO c:\tmp\cuenta.txt.
OUTPUT STREAM FILLER TO c:\tmp\filler.txt.

FOR EACH pl-flg-mes NO-LOCK WHERE codcia = 1
    AND periodo = 2011
    AND nromes = 03
    AND codpln = 01
    AND cnpago = 'BANCO DE CREDITO',
    FIRST pl-pers no-lock WHERE pl-pers.codper = pl-flg-mes.codper:
    PUT STREAM CABECERA
        UNFORMATTED
        '10'
        STRING(x-item, '999999')
        'DNI'
        STRING (pl-pers.nrodocid,'99999999') 
        fill(' ', 4)
        FILL(' ', 3)
        STRING(pl-pers.patper, 'x(25)')
        STRING(pl-pers.matper, 'x(25)')
        STRING(pl-pers.nomper, 'x(25)')
        FILL(' ', 60)
        FILL(' ', 92)
        SKIP.
    PUT STREAM CABECERA
        UNFORMATTED
        '20'
        STRING(x-item, '999999')
        'A'
        STRING(ENTRY(1, pl-flg-mes.nrodpt, '-'), 'x(3)')
        STRING(ENTRY(2, pl-flg-mes.nrodpt, '-'), 'x(8)')
        STRING(ENTRY(3, pl-flg-mes.nrodpt, '-'), 'x(1)')
        STRING(ENTRY(4, pl-flg-mes.nrodpt, '-'), 'x(2)')
        FILL(' ', 6)
        'RUC'
        '20100038146 '
        'S/'
        FILL(' ', 207)
        SKIP.
    x-item = x-item + 1.
END.
PUT STREAM CABECERA
    UNFORMATTED
    '99'
    STRING(x-item, '999999')
    STRING(x-item - 1, '999999')
    STRING(x-item - 1, '999999')
    FILL(' ', 233)
    SKIP.
