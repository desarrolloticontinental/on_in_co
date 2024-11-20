
DEFINE TEMP-TABLE t-pers
    FIELDS t-codcia LIKE pl-mov-mes.codcia INITIAL 1
    FIELDS t-periodo LIKE pl-mov-mes.periodo
    FIELDS t-nromes LIKE pl-mov-mes.nromes
    FIELDS t-codpln LIKE pl-mov-mes.codpln INITIAL 1
    FIELDS t-codcal LIKE pl-mov-mes.codcal INITIAL 0
    FIELDS t-codper LIKE pl-mov-mes.codper
    FIELDS t-codmov LIKE pl-mov-mes.codmov
    FIELDS t-valcal-mes LIKE pl-mov-mes.valcal-mes
    INDEX t-codper IS PRIMARY t-codper.

INPUT FROM d:\tmp\pl-mov-mes.csv.

REPEAT:
    CREATE t-pers.
    IMPORT DELIMITER ","
        t-periodo
        t-nromes
        t-codper
        t-codmov
        t-valcal-mes.
END.

FOR EACH t-pers :
    IF t-codper = "" OR
        t-valcal-mes <= 0 THEN NEXT.
    FIND PL-MOV-MES WHERE
        CODCIA = t-codcia AND
        PERIODO = t-periodo AND
        NROMES = t-nromes AND
        CODPLN = t-codpln AND
        CODCAL = t-codcal AND
        codper = t-codper AND
        CODMOV = t-codmov NO-ERROR.
    IF NOT AVAILABLE PL-MOV-MES THEN DO:
        CREATE PL-MOV-MES.
        ASSIGN
            CODCIA = t-codcia
            PERIODO = t-periodo
            NROMES = t-nromes
            CODPLN = t-codpln
            CODCAL = t-codcal
            codper = t-codper
            CODMOV = t-codmov.
    END.
    ASSIGN VALCAL-MES = t-valcal-mes.
END.

MESSAGE
    "Proceso terminado"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

