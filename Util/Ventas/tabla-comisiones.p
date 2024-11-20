DEF BUFFER detalle FOR factabla.

OUTPUT TO c:\tmp\comision.txt.
PUT UNFORMATTED
    'LINEA|DESCRIPCION|DIVISION|DESCRIPCION|% CAMPAÑA|% NO CAMPAÑA'
    SKIP.
FOR EACH factabla NO-LOCK WHERE codcia = 1 AND tabla = 'CV',
    EACH almtfami NO-LOCK WHERE almtfami.codcia = 1
    AND almtfami.codfam = factabla.codigo:
    FOR EACH detalle NO-LOCK WHERE detalle.codcia = 1
        AND detalle.tabla = factabla.tabla
        AND detalle.codigo BEGINS factabla.codigo,
        FIRST gn-divi NO-LOCK WHERE gn-divi.codcia = 1
        AND gn-divi.coddiv = detalle.nombre:
        PUT UNFORMATTED
            factabla.codigo '|'
            almtfami.desfam '|'
            gn-divi.coddiv '|'
            gn-divi.desdiv '|'
            detalle.valor[1] '|'
            detalle.valor[2]
            SKIP.
    END.
END.
