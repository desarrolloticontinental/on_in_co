DEF VAR x-linea AS CHAR.
DEF VAR x-stkact60 AS DEC.
DEF VAR x-stkact60e AS DEC.
DEF VAR x-comprometido AS DEC.

INPUT FROM d:\tmp\comprar.prn.
OUTPUT TO d:\tmp\quecomprar.txt.
PUT UNFORMATTED
    'PROVEEDOR|NOMBRE|RUC|ARTICULO|DESCRIPCION|CANTIDAD|UNIDAD|STOCK|MASTER|INNER'
    SKIP.
REPEAT :
    IMPORT UNFORMATTED x-linea.
    IF x-linea = '' THEN LEAVE.
    FIND almmmatg WHERE codcia = 1
        AND codmat = SUBSTRING(x-linea,1,6)
        NO-LOCK.
    FIND gn-prov WHERE gn-prov.codcia = 000
        AND gn-prov.codpro = almmmatg.codpr1
        NO-LOCK.
    x-stkact60 = 0.
    FIND almmmate WHERE almmmate.codcia = 001
        AND almmmate.codalm = '60'
        AND almmmate.codmat = almmmatg.codmat
        NO-LOCK NO-ERROR.
    IF AVAILABLE almmmate THEN DO:
        x-stkact60 = almmmate.stkact.
        RUN vta2/Stock-Comprometido (almmmatg.CodMat, almmmate.codalm, OUTPUT x-Comprometido).
        x-stkact60 = x-stkact60 - x-comprometido.
        IF x-stkact60 <= 0 THEN x-stkact60 = 0.
    END.
    x-stkact60e = 0.
    FIND almmmate WHERE almmmate.codcia = 001
        AND almmmate.codalm = '60e'
        AND almmmate.codmat = almmmatg.codmat
        NO-LOCK NO-ERROR.
    IF AVAILABLE almmmate THEN DO:
        x-stkact60e = almmmate.stkact.
        RUN vta2/Stock-Comprometido (almmmatg.CodMat, almmmate.codalm, OUTPUT x-Comprometido).
        x-stkact60e = x-stkact60e - x-comprometido.
        IF x-stkact60e <= 0 THEN x-stkact60e = 0.
    END.
    PUT UNFORMATTED
        gn-prov.codpro '|'
        gn-prov.nompro '|'
        gn-prov.Ruc '|'
        almmmatg.codmat '|'
        almmmatg.desmat '|'
        DECIMAL(SUBSTRING(x-linea,7)) '|'
        almmmatg.undstk '|'
        x-stkact60 + x-stkact60e '|'
        almmmatg.canemp '|'
        almmmatg.stkrep
        SKIP.
END.
INPUT CLOSE.

