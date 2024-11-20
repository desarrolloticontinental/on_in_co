OUTPUT TO c:\tmp\promediovacacional.txt.
FOR EACH pl-flg-mes NO-LOCK WHERE codcia = 001
    AND periodo = 2012
    AND nromes = 04
    AND PL-FLG-MES.vcontr = ?,
    FIRST pl-pers NO-LOCK WHERE pl-pers.codper = pl-flg-mes.codper:
    FOR EACH pl-mov-mes NO-LOCK WHERE pl-mov-mes.codcia = 001
        AND pl-mov-mes.periodo >= 2011
        AND pl-mov-mes.codper = pl-flg-mes.codper
        AND ( (pl-mov-mes.periodo = 2011 AND pl-mov-mes.nromes >= 10)
              OR (pl-mov-mes.periodo = 2012 AND pl-mov-mes.nromes <= 03) )
        AND pl-mov-mes.codcal = 01
        AND (pl-mov-mes.codmov = 209
             OR pl-mov-mes.codmov = 125
             OR pl-mov-mes.codmov = 126
             OR pl-mov-mes.codmov = 127):
        PUT UNFORMATTED
            pl-flg-mes.codper ' '
            trim(pl-pers.patper) + ' '
            trim(pl-pers.matper) + ', ' +
            pl-pers.nomper '|'
            pl-mov-mes.periodo '|'
            pl-mov-mes.nromes '|'
            pl-mov-mes.codmov '|'
            pl-mov-mes.valcal-mes '|'
            PL-FLG-MES.fecing
            SKIP.
    END.
END.
