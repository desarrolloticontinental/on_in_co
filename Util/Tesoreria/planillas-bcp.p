DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR s-periodo AS INT INIT 2013 NO-UNDO.
DEF VAR s-nromes AS INT INIT 02 NO-UNDO.
DEF VAR k AS INT NO-UNDO.
DEF VAR j AS DEC NO-UNDO.
DEF VAR m AS DEC NO-UNDO.

ASSIGN
    k = 0
    j = 0
    m = 1417588056.
FOR EACH pl-flg-mes NO-LOCK WHERE codcia = s-codcia
    AND periodo = s-periodo
    AND nromes = s-nromes
    AND cnpago = 'BANCO DE CREDITO',
    FIRST pl-mov-mes OF pl-flg-mes WHERE PL-MOV-MES.CodMov = 403
    AND PL-MOV-MES.codcal = 001:
    k = k + 1.
    j = j + valcal-mes.
    m = m + DECIMAL( SUBSTRING( REPLACE(pl-flg-mes.nrodpt, "-", ""), 4 ) ).
END.


OUTPUT TO c:\tmp\prueba.txt.
PUT UNFORMATTED
    "1" 
    STRING(k, '999999')
    STRING(YEAR(TODAY), '9999') STRING(MONTH(TODAY), '99') STRING(DAY(TODAY))
    "G"
    "C"
    "0001"
    STRING("2851417588056", 'x(20)')
    STRING(j, '99999999999999.99')
    STRING("Pago de haberes", 'x(40)')
    STRING(m, '999999999999999')
    SKIP.
FOR EACH pl-flg-mes NO-LOCK WHERE codcia = s-codcia
    AND periodo = s-periodo
    AND nromes = s-nromes
    AND cnpago = 'BANCO DE CREDITO',
    FIRST pl-pers OF pl-flg-mes NO-LOCK,
    FIRST pl-mov-mes OF pl-flg-mes WHERE PL-MOV-MES.CodMov = 403
    AND PL-MOV-MES.codcal = 001:
    PUT UNFORMATTED
        "2"
        "A"
        STRING(REPLACE(pl-flg-mes.nrodpt, "-", ""), 'x(20)')
        "1"
        STRING( PL-PERS.NroDocId, 'x(12)')
        FILL(" ", 3)
        STRING(TRIM(pl-pers.patper) + ' '  + TRIM(pl-pers.matper) + ', '+
               pl-pers.nomper, 'x(75)')
        FILL(" ", 40)
        FILL(" ", 20)
        "0001"
        STRING(pl-mov-mes.valcal-mes, '99999999999999.99')
        "N"
        SKIP.
END.
OUTPUT CLOSE.

