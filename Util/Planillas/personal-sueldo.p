OUTPUT TO c:\tmp\personal.txt.
FOR EACH pl-flg-mes NO-LOCK WHERE codcia = 1
    AND periodo = 2010
    AND nromes = 04,
    FIRST pl-pers OF pl-flg-mes NO-LOCK,
    FIRST pl-mov-mes OF pl-flg-mes NO-LOCK WHERE codmov = 101
    AND codcal = 0:
    DISPLAY
        pl-pers.codper
        pl-pers.patper
        pl-pers.matper
        pl-pers.nomper
        PL-FLG-MES.fecing
        PL-PERS.fecnac
        pl-mov-mes.valcal-mes
        WITH STREAM-IO NO-BOX WIDTH 320.



END.

