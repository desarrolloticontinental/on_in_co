DEF VAR x-linea AS CHAR.
DEF VAR x-ruc   AS CHAR.
DEF VAR x-nombre AS CHAR.
DEF VAR x-codigo AS CHAR.

INPUT FROM d:\codigos.prn.
OUTPUT TO d:\resultados.txt.
REPEAT:
    IMPORT UNFORMATTED x-linea.
    IF x-linea = '' THEN LEAVE.
    ASSIGN
        x-ruc = SUBSTRING(x-linea,1,20)
        x-nombre = SUBSTRING(x-linea,21,40)
        x-codigo = SUBSTRING(x-linea,81).
    /* 1ro por ruc */
    FOR EACH gn-prov NO-LOCK WHERE gn-prov.codcia = 0
        AND gn-prov.ruc = x-ruc:
        DISPLAY 
            'Análisis por RUC'  FORMAT 'x(20)'
            gn-prov.codpro  FORMAT 'x(15)'  COLUMN-LABEL 'Cod Progress'
            x-ruc           FORMAT 'x(15)'  COLUMN-LABEL 'Cod Open'
            gn-prov.ruc     FORMAT 'x(15)'  COLUMN-LABEL 'Ruc Progress'
            gn-prov.nompro  FORMAT 'x(40)'  COLUMN-LABEL 'Progress'
            x-nombre        FORMAT 'x(40)'  COLUMN-LABEL 'Open'
            WITH STREAM-IO NO-BOX WIDTH 320.
    END.
    /* 2do por codigo */
    FOR EACH gn-prov NO-LOCK WHERE gn-prov.codcia = 0
        AND gn-prov.codpro = x-codigo:
        DISPLAY 
            'Análisis por codigo'  FORMAT 'x(20)'
            gn-prov.codpro  FORMAT 'x(15)'  COLUMN-LABEL 'Cod Progress'
            x-ruc           FORMAT 'x(15)'  COLUMN-LABEL 'Cod Open'
            gn-prov.ruc     FORMAT 'x(15)'  COLUMN-LABEL 'Ruc Progress'
            gn-prov.nompro  FORMAT 'x(40)'  COLUMN-LABEL 'Progress'
            x-nombre        FORMAT 'x(40)'  COLUMN-LABEL 'Open'
            WITH STREAM-IO NO-BOX WIDTH 320.
    END.
    /* 3ro por nombre */
    FOR EACH gn-prov NO-LOCK WHERE gn-prov.codcia = 0
        AND INDEX(gn-prov.nompro, x-nombre) > 0:
        DISPLAY 
            'Análisis por codigo'  FORMAT 'x(20)'
            gn-prov.codpro  FORMAT 'x(15)'  COLUMN-LABEL 'Cod Progress'
            x-ruc           FORMAT 'x(15)'  COLUMN-LABEL 'Cod Open'
            gn-prov.ruc     FORMAT 'x(15)'  COLUMN-LABEL 'Ruc Progress'
            gn-prov.nompro  FORMAT 'x(40)'  COLUMN-LABEL 'Progress'
            x-nombre        FORMAT 'x(40)'  COLUMN-LABEL 'Open'
            WITH STREAM-IO NO-BOX WIDTH 320.
    END.
END.
INPUT CLOSE.
OUTPUT CLOSE.

