DEFINE VAR a AS int64 NO-UNDO.

a = ETIME(YES).

DEF TEMP-TABLE t-matg
    FIELD codcia AS INTE
    FIELD codmat AS CHAR.

DEFINE VAR cCodCia AS INTE NO-UNDO.
DEFINE VAR cCodMat AS CHAR NO-UNDO.


DECLARE cMatg CURSOR FOR
    SELECT codcia, codmat
    FROM almmmatg
    WHERE almmmatg.codcia = 1 AND almmmatg.codfam = '010'.

OPEN cMatg.
REPEAT:
    FETCH cMatg INTO cCodCia, cCodMat.
    CREATE t-Matg.
    t-Matg.codcia = cCodCia.
    t-matg.codmat = cCodMat.
END.
CLOSE cMatg.
/*
FOR EACH t-matg.
    DISPLAY t-matg.codcia t-matg.codmat.
END.

*/
message ETIME.
