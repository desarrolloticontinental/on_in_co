DEF VAR x-linea AS CHAR.
DEF VAR x-codmat AS CHAR FORMAT 'x(6)'.
DEF VAR x-unidad AS CHAR.
INPUT FROM d:\tmp\newkits.prn.
OUTPUT TO d:\tmp\newkits.txt.
PUT UNFORMATTED
    'DESCRIPCION|MARCA|LINEA|SUBLINEA|SUBSUBSLINEA|PROPIOS/TERCEROS|AFECTO|MAYORISTA/MINORISTA|'
    'UNIDAD BASICA|UNIDAD COMNPRAS|UNIDAD STOCK|UNIDAD A|UNIDAD B|UNIDAD C|UNIDAD OFICINA|'
    'UNIDAD AL POR MENOR|MINIMO VTA MAY|MINIMO VTA MIN|EMP MASTER|EMP INNER|'
    'PROVEEDOR A|PROVEEDOR B|PESO|EAN 13|DIGESA|VCTO DIGESA|'
    'MIN VTA EXPO|EMP EXPO|KIT'
    SKIP.
REPEAT :
    IMPORT UNFORMATTED x-linea.
    IF x-linea = '' THEN LEAVE.
    x-codmat = SUBSTRING(x-linea,2,6).
    x-unidad = 'CJA' + TRIM(SUBSTRING(x-linea,11)).
    FIND almmmatg WHERE codcia = 1 AND codmat = x-codmat NO-LOCK NO-ERROR.
    PUT UNFORMATTED
        'KIT ' + almmmatg.desmat '|'
        almmmatg.codmar '|'
        almmmatg.codfam '|'
        almmmatg.subfam '|'
        Almmmatg.CodSSFam '|'
        almmmatg.CHR__02 '|'
        almmmatg.aftigv '|'
        almmmatg.tpomrg '|'
        x-unidad '|'
        x-unidad '|'
        x-unidad '|'
        '|'
        '|'
        '|'
        x-unidad '|'
        x-unidad '|'
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
        SUBSTRING(x-linea,1,7)
        SKIP.
END.
INPUT CLOSE.
OUTPUT CLOSE.

