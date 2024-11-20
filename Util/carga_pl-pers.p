
/*
SESSION:DATE-FORMAT = "mdy".
*/

DEFINE TEMP-TABLE t-pers
    FIELDS t-codper LIKE PL-PERS.codper
    FIELDS t-tpodocid LIKE PL-PERS.TpoDocId INITIAL "01"
    FIELDS t-nrodocid LIKE PL-PERS.nrodocid
    FIELDS t-patper LIKE PL-PERS.patper
    FIELDS t-matper LIKE PL-PERS.matper
    FIELDS t-nomper LIKE PL-PERS.nomper
    FIELDS t-fecnac LIKE pl-pers.fecnac
    FIELDS t-sexper LIKE pl-pers.sexper
    FIELDS t-codnac LIKE pl-pers.codnac
    FIELDS t-telefo LIKE pl-pers.telefo
    FIELDS t-lmilit LIKE pl-pers.lmilit
    FIELDS t-codprov AS CHARACTER
    FIELDS t-coddist AS CHARACTER
    INDEX t-codper IS PRIMARY t-codper.

INPUT FROM d:\tmp\pl-pers.csv.

REPEAT:
    CREATE t-pers.
    IMPORT DELIMITER ","
        t-codper
        t-nrodocid
        t-patper
        t-matper
        t-nomper
        t-fecnac
        t-sexper
        t-codnac
        t-telefo
        t-lmilit
        t-coddist
        
        t-codprov.
END.

FOR EACH t-pers :
    IF t-codper = "" THEN NEXT.
    DISPLAY t-pers.
    FIND pl-pers WHERE
        codper = t-codper NO-ERROR.
    IF NOT AVAILABLE pl-pers THEN DO:
        CREATE pl-pers.
        ASSIGN
            codper = t-codper.
    END.
    ASSIGN
        tpodocid  = t-tpodocid 
        nrodocid  = t-nrodocid 
        patper    = t-patper   
        matper    = t-matper   
        nomper    = t-nomper   
        fecnac    = t-fecnac   
        sexper    = t-sexper   
        codnac    = t-codnac   
        telefo    = t-telefo   
        lmilit    = t-lmilit
        ubigeo    = "15" + t-codprov + t-coddist
        codcia    = 1.
END.
