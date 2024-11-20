DEF BUFFER b-mes FOR pl-mov-mes.

DEF VAR x-senati AS DEC DECIMALS 4 FORMAT '>>.9999'.
DEF VAR x-remuneracion AS DEC.
DEF VAR s-codcia AS INT INIT 001.
DEF VAR s-periodo AS INT INIT 2009.
DEF VAR s-nromes AS INT INIT 08.

FIND pl-var-mes WHERE periodo = s-periodo
    AND nromes = s-nromes
    NO-LOCK.
x-senati = valvar-mes[14]. 

OUTPUT TO c:\tmp\senati.txt.
FOR EACH pl-mov-mes NO-LOCK WHERE codcia = s-codcia
    AND periodo = s-periodo
    AND nromes = s-nromes
    AND codcal = 001
    AND codmov = 306
    AND valcal-mes > 0,
    FIRST pl-flg-mes OF pl-mov-mes NO-LOCK,
    FIRST pl-pers WHERE pl-pers.codper = pl-mov-mes.codper:
    FIND b-mes WHERE b-mes.codcia = 1
        AND b-mes.periodo = s-periodo
        AND b-mes.nromes = s-nromes
        AND b-mes.codper = pl-mov-mes.codper
        AND b-mes.codcal = 001
        AND b-mes.codmov = 018 NO-LOCK.
    x-remuneracion = pl-mov-mes.valcal-mes.
    IF b-mes.valcal-mes = 2 THEN x-remuneracion = x-remuneracion / 0.5.
    x-remuneracion = x-remuneracion * 100 / x-senati.
    DISPLAY
        pl-pers.codper
        pl-pers.patper
        pl-pers.matper
        pl-pers.nomper
        pl-flg-mes.cargos
        pl-flg-mes.seccion
        x-senati * (IF b-mes.valcal-mes = 1 THEN 1 ELSE 0.5) FORMAT '>>.9999' COLUMN-LABEL 'Tasa'
        x-remuneracion COLUMN-LABEL 'Imponible'
        pl-mov-mes.valcal-mes
        WITH STREAM-IO NO-BOX WIDTH 320.
        
END.
OUTPUT CLOSE.

