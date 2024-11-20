DEF BUFFER b-tabla FOR factabla.
OUTPUT TO c:\tmp\comisiones.txt.
PUT UNFORMATTED
    'Linea|Descripcion|Division|Descripcion|% Comi Campaña|% Comi no campaña'
    SKIP.
FOR EACH factabla NO-LOCK WHERE codcia = 001
    AND tabla = 'CV'
    AND nombre = '',
    FIRST almtfami NO-LOCK WHERE almtfami.codcia = 001
    AND almtfami.codfam = factabla.codigo,
    EACH b-tabla NO-LOCK WHERE b-tabla.codcia = 001
    AND b-tabla.tabla = 'CV'
    AND b-tabla.codigo BEGINS factabla.codigo
    AND b-tabla.nombre <> '',
    FIRST gn-divi NO-LOCK WHERE gn-divi.codcia = 001
    AND gn-divi.coddiv = b-tabla.nombre:
    PUT UNFORMATTED
            factabla.codigo '|'
            almtfami.desfam '|'
            b-tabla.nombre '|'
            gn-divi.desdiv '|'
            b-tabla.valor[1] '|'
            b-tabla.valor[2]
            SKIP.
END.
