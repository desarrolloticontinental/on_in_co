OUTPUT TO c:\tmp\personal.txt.
FOR EACH pl-flg-mes NO-LOCK WHERE codcia = 1
    AND ( (periodo = 2009 AND nromes = 12) OR 
          (periodo = 2010) ),
    FIRST pl-pers OF pl-flg-mes NO-LOCK
    BREAK BY pl-flg-mes.codper:
    IF LAST-OF(pl-flg-mes.codper) THEN
    DISPLAY
        PL-PERS.codper 
        '|'
        PL-PERS.patper 
        '|'
        PL-PERS.matper 
        '|'
        PL-PERS.nomper 
        '|'
        PL-PERS.telefo
        '|'
        pl-flg-mes.periodo
        '|'
        pl-flg-mes.nromes
        '|'
        pl-flg-mes.fecing
        '|'
        pl-flg-mes.vcontr
        '|'
        pl-flg-mes.seccion FORMAT 'x(60)'
        WITH STREAM-IO NO-BOX NO-LABELS WIDTH 320.
END.
OUTPUT CLOSE.
