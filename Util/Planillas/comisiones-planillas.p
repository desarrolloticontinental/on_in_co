OUTPUT TO c:\tmp\comisiones.txt.
PUT UNFORMATTED
    'PERIODO|MES|PERSONAL|COMISION'
    SKIP.
FOR EACH pl-mov-mes NO-LOCK WHERE codcia = 001
    AND periodo = 2015
    AND nromes >= 01
    AND PL-MOV-MES.CodMov = 209
    AND PL-MOV-MES.codcal = 001,
    FIRST pl-pers NO-LOCK WHERE pl-pers.codper = pl-mov-mes.codper:
    PUT UNFORMATTED
        pl-mov-mes.periodo '|'
        pl-mov-mes.nromes '|'
        pl-mov-mes.codper + ' ' +
        trim(PL-PERS.patper) + ' ' + 
        trim(PL-PERS.matper) + ', ' + 
        PL-PERS.nomper '|'
        PL-MOV-MES.valcal-mes
        SKIP.
END.
