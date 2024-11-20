DEF VAR x-linea AS CHAR.
DEF VAR x-codmat AS CHAR FORMAT 'x(6)'.
DEF VAR x-unidad AS CHAR.

DEF TEMP-TABLE kitvssku
    FIELD kit AS CHAR 
    FIELD sku AS CHAR.

DEF TEMP-TABLE sku
    FIELD sku AS CHAR
    FIELD desmat AS CHAR.

INPUT FROM d:\tmp\kitvssku.prn.
REPEAT:
    IMPORT UNFORMATTED x-linea.
    IF x-linea = '' THEN LEAVE.
    CREATE kitvssku.
    ASSIGN
        kitvssku.kit = SUBSTRING(x-linea,1,10)
        kitvssku.sku = SUBSTRING(x-linea,11).
END.
INPUT CLOSE.

INPUT FROM d:\tmp\sku.prn.
REPEAT:
    IMPORT UNFORMATTED x-linea.
    IF x-linea = '' THEN LEAVE.
    CREATE sku.
    ASSIGN
        sku.sku = SUBSTRING(x-linea,1,10)
        sku.desmat = SUBSTRING(x-linea,11).
END.
INPUT CLOSE.

OUTPUT TO d:\tmp\newskus.txt.
PUT UNFORMATTED
    'DESCRIPCION|MARCA|LINEA|SUBLINEA|SUBSUBSLINEA|PROPIOS/TERCEROS|AFECTO|MAYORISTA/MINORISTA|'
    'UNIDAD BASICA|UNIDAD COMNPRAS|UNIDAD STOCK|UNIDAD A|UNIDAD B|UNIDAD C|UNIDAD OFICINA|'
    'UNIDAD AL POR MENOR|MINIMO VTA MAY|MINIMO VTA MIN|EMP MASTER|EMP INNER|'
    'PROVEEDOR A|PROVEEDOR B|PESO|EAN 13|DIGESA|VCTO DIGESA|'
    'MIN VTA EXPO|EMP EXPO|KIT'
    SKIP.
FOR EACH sku, FIRST kitvssku WHERE kitvssku.sku = sku.sku, 
    FIRST almmmatg NO-LOCK WHERE almmmatg.codcia = 1 AND almmmatg.codmat = SUBSTRING(kitvssku.kit,2):
    PUT UNFORMATTED
        sku.desmat '|'
        almmmatg.codmar '|'
        almmmatg.codfam '|'
        almmmatg.subfam '|'
        Almmmatg.CodSSFam '|'
        almmmatg.CHR__02 '|'
        almmmatg.aftigv '|'
        almmmatg.tpomrg '|'
        almmmatg.undbas '|'
        almmmatg.undcmp '|'
        almmmatg.undstk '|'
        '|'
        '|'
        '|'
        almmmatg.undstk '|'
        almmmatg.undstk '|'
        '0|'
        '0|'
        '0|'
        '0|'
        almmmatg.codpr1 '|'
        almmmatg.codpr2 '|'
        '0|'
        '|'
        '|'
        '|'
        '0|'
        '0|'
        sku.sku
        SKIP.
END.


OUTPUT CLOSE.
