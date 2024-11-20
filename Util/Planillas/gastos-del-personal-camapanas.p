DEF VAR x-Campana AS CHAR.
OUTPUT TO c:\tmp\gastos.txt.
PUT 'CAMPAÑA|AÑO|MES|PERSONAL|SECCION|CONCEPTO REMUNERATIVO|IMPORTE' SKIP.
FOR EACH pl-mov-mes NO-LOCK WHERE codcia = 001
    AND periodo >= 2009
    AND periodo <= 2012
    AND codcal = 001
    AND ( CodMov = 101 OR
          CodMov = 103 OR
          CodMov = 106 OR
          CodMov = 107 OR
          CodMov = 108 OR
          CodMov = 118 OR
          CodMov = 125 OR
          CodMov = 126 OR
          CodMov = 127 OR
          CodMov = 131 OR
          CodMov = 134 OR
          CodMov = 136 OR
          CodMov = 138 OR
          CodMov = 139 OR
          CodMov = 801 OR
          CodMov = 802 OR
          CodMov = 803 OR
          CodMov = 130 OR
          CodMov = 146 OR
          CodMov = 209 OR
          CodMov = 212 OR
          CodMov = 431 OR
          CodMov = 139 OR
          CodMov = 431 OR
          CodMov = 611 ),
    FIRST pl-flg-mes OF pl-mov-mes NO-LOCK,
    FIRST pl-pers NO-LOCK WHERE pl-pers.codper = pl-mov-mes.codper,
    FIRST pl-conc OF pl-mov-mes NO-LOCK:
    x-Campana = ''.
    IF pl-mov-mes.periodo = 2009 AND pl-mov-mes.nromes = 12 THEN x-Campana = "CAMPAÑA 2010".
    IF pl-mov-mes.periodo = 2010 AND pl-mov-mes.nromes <= 03 THEN x-Campana = "CAMPAÑA 2010".
    IF pl-mov-mes.periodo = 2010 AND pl-mov-mes.nromes = 12 THEN x-Campana = "CAMPAÑA 2011".
    IF pl-mov-mes.periodo = 2011 AND pl-mov-mes.nromes <= 03 THEN x-Campana = "CAMPAÑA 2011".
    IF pl-mov-mes.periodo = 2011 AND pl-mov-mes.nromes = 12 THEN x-Campana = "CAMPAÑA 2012".
    IF pl-mov-mes.periodo = 2012 AND pl-mov-mes.nromes <= 03 THEN x-Campana = "CAMPAÑA 2012".
    IF x-Campana = '' THEN NEXT.

    PUT UNFORMATTED
        x-Campana '|'
        pl-mov-mes.periodo '|'
        pl-mov-mes.nromes '|'
        pl-pers.codper ' ' 
        TRIM(pl-pers.patper) ' ' 
        TRIM(pl-pers.matper) ', ' 
        pl-pers.nomper '|'
        pl-flg-mes.seccion '|'
        pl-mov-mes.codmov ' ' pl-conc.DesMov '|'
        pl-mov-mes.valcal
        SKIP.
END.
OUTPUT CLOSE.

