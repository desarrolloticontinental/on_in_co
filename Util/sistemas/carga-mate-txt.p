
DEF TEMP-TABLE t-mate
    FIELD codcia LIKE almmmate.codcia
    FIELD codalm LIKE almmmate.codalm
    FIELD stkmin LIKE almmmate.stkmin
    FIELD stkmax LIKE almmmate.stkmax
    FIELD vctmn1 LIKE almmmate.vctmn1
    FIELD vctmn2 LIKE almmmate.vctmn2
    FIELD codmat LIKE almmmate.codmat
    FIELD stockmax LIKE almmmate.stockmax
    FIELD stockseg LIKE almmmate.stockseg
    .

FOR EACH almmmatg NO-LOCK WHERE codcia = 1 AND codfam = '001',
    EACH almmmate NO-LOCK WHERE almmmate.codcia = 1 AND codalm = '03'
    AND almmmate.codmat = almmmatg.codmat:
    CREATE t-mate.
    BUFFER-COPY almmmate TO t-mate.
END.

OUTPUT TO d:\almmmate.txt.
FOR EACH t-mate NO-LOCK:
    EXPORT DELIMITER ',' t-mate.
END.
OUTPUT CLOSE.
