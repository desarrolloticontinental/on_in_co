DEF VAR x-nompro LIKE gn-prov.nompro.
DEF VAR k AS INT NO-UNDO.
OUTPUT TO d:\tmp\barras.txt.
PUT UNFORMATTED
    'ARTICULO|DESCRIPCION|UNIDAD|PESO|VOLUMEN|LINEA|SUBLINEA|EAN|EQUIVALENCIA'    SKIP.
FOR EACH INTEGRAL.almmmatg WHERE almmmatg.CodCia = 1
    AND almmmatg.TpoArt <> "D" NO-LOCK,
    EACH Almmmat1 OF Almmmatg NO-LOCK:
    PUT UNFORMATTED
        Almmmatg.codmat '|'
        Almmmatg.desmat '|'
        Almmmatg.undbas '|'
        Almmmatg.pesmat '|'
        Almmmatg.libre_d02 '|'
        Almmmatg.codfam '|'
        Almmmatg.subfam '|'
        Almmmatg.codbrr '|'
        SKIP.
    DO k = 1 TO 6:
        IF Almmmat1.barras[1] <> '' THEN DO:
            PUT UNFORMATTED
                Almmmatg.codmat '|'
                Almmmatg.desmat '|'
                Almmmatg.undbas '|'
                Almmmatg.pesmat '|'
                Almmmatg.libre_d02 '|'
                Almmmatg.codfam '|'
                Almmmatg.subfam '|'
                Almmmat1.barras[k] '|'
                Almmmat1.equival[k]
                SKIP.
        END.
    END.
END.
OUTPUT CLOSE.
