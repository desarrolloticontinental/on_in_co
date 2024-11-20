DEF VAR s-codcia AS INT INIT 001.
DEF VAR s-periodo AS INT INIT 2010.
DEF VAR s-nromes AS INT INIT 03.
DEF VAR s-valcal-mes AS INT INIT 1.     /* Término de contrato */

FOR EACH pl-flg-mes WHERE codcia = s-codcia
    AND periodo = s-periodo
    AND nromes = s-nromes
    AND vcontr <> ?
    AND YEAR(vcontr) = s-periodo
    AND MONTH(vcontr) = s-nromes:
    FIND FIRST pl-mov-mes OF pl-flg-mes
        WHERE codmov = 017
        AND  PL-MOV-MES.codcal = 000
        EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE pl-mov-mes THEN DO:
        CREATE pl-mov-mes.
        BUFFER-COPY pl-flg-mes TO pl-mov-mes
            ASSIGN
            PL-MOV-MES.codcal = 000
            PL-MOV-MES.CodMov = 017
            PL-MOV-MES.codpln = 01.
    END.
    ASSIGN
        PL-MOV-MES.valcal-mes = s-valcal-mes
        pl-flg-mes.situacion = '13'.
END.
