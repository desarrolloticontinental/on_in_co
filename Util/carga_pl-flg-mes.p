
/*
SESSION:DATE-FORMAT = "mdy".
*/

DEFINE TEMP-TABLE t-pers
    FIELDS t-codper LIKE PL-PERS.codper
    FIELDS t-fecnac LIKE pl-pers.fecnac
    FIELDS t-cargo AS CHARACTER
    FIELDS t-canal AS CHARACTER
    FIELDS t-nro AS CHARACTER
    INDEX t-codper IS PRIMARY t-codper.

INPUT FROM d:\tmp\pl-flg-mes.csv.

REPEAT:
    CREATE t-pers.
    IMPORT DELIMITER ","
        t-codper
        t-fecnac
        t-cargo
        t-canal
        t-nro.
END.

FOR EACH t-pers :
    IF t-codper = "" THEN NEXT.
    DISPLAY t-pers.
    FIND pl-FLG-MES WHERE
        CODCIA = 1 AND
        PERIODO = 2010 AND
        CODPLN = 1 AND
        NROMES = 1 AND
        codper = t-codper NO-ERROR.
    IF NOT AVAILABLE pl-FLG-MES THEN DO:
        CREATE pl-FLG-MES.
        ASSIGN
            CODCIA = 1 
            PERIODO = 2010
            CODPLN = 1
            NROMES = 1 
            codper = t-codper.
    END.
    ASSIGN
        CNPAGO  = t-CANAL
        NRODPT  = t-NRO
        CARGOS  = t-CARGO
        FECING  = T-FECNAC.
    
END.
