DEF VAR x-linea AS CHAR NO-UNDO.
DEF VAR pComprometido AS DEC NO-UNDO.

DEF TEMP-TABLE detalle
    FIELD codmat LIKE almmmatg.codmat
    FIELD candes LIKE ccbddocu.candes
    FIELD stock11 AS DEC
    FIELD stock21 AS DEC
    FIELD stock21f AS DEC.

INPUT FROM c:\tmp\cissac-a-conti.txt.
REPEAT:
    IMPORT UNFORMATTED x-linea.
    IF x-linea = '' THEN LEAVE.
    CREATE detalle.
    ASSIGN
        detalle.codmat = SUBSTRING(x-linea,1,6)
        detalle.candes = DECIMAL(SUBSTRING(x-linea,7)).
    FOR EACH almmmate NO-LOCK WHERE codcia = 001
        AND codmat = SUBSTRING(x-linea,1,6)
        AND LOOKUP(codalm, '11,21,21f') > 0:
        RUN vta2/stock-comprometido (almmmate.codmat,
                                     almmmate.codalm,
                                     OUTPUT pComprometido).
        CASE almmmate.codalm:
            WHEN '11'  THEN detalle.stock11  = (almmmate.stkact - pComprometido).
            WHEN '21'  THEN detalle.stock21  = (almmmate.stkact - pComprometido).
            WHEN '21f' THEN detalle.stock21f = (almmmate.stkact - pComprometido).
        END CASE.
    END.
END.
INPUT CLOSE.

OUTPUT TO c:\tmp\conti-a-cissac.txt.
PUT UNFORMATTED
    'CODIGO|VENDIDO|STOCK11|STOSCK21|STOCK21F'
    SKIP.
FOR EACH detalle NO-LOCK BY detalle.codmat:
    PUT UNFORMATTED
        detalle.codmat '|'
        detalle.candes '|'
        detalle.stock11 '|'
        detalle.stock21 '|'
        detalle.stock21f
        SKIP.
END.
OUTPUT CLOSE.
