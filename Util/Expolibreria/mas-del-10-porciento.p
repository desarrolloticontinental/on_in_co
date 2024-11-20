DEF TEMP-TABLE detalle LIKE facdpedi.

FOR EACH faccpedi NO-LOCK WHERE codcia = 1
    AND coddiv = '10015'
    AND coddoc = 'cot'
    AND flgest <> 'A':
    FOR EACH facdpedi OF faccpedi NO-LOCK:
        IF facdpedi.implin > (faccpedi.imptot * 0.10) THEN DO:
            CREATE detalle.
            BUFFER-COPY facdpedi TO detalle.
        END.
    END.
END.

OUTPUT TO c:\tmp\revisar.txt.
PUT UNFORMATTED 
    'DOC|NUMERO|CLIENTE|NOMBRE|PRODUCTO|DESCRIPCION|UNIDAD|CANTIDAD|UNITARIO|TOTAL.PRODUCTO|TOTAL.COTIZACION'
    SKIP.
FOR EACH detalle, FIRST faccpedi OF detalle NO-LOCK, FIRST almmmatg OF detalle NO-LOCK:
    PUT UNFORMATTED
        faccpedi.coddoc '|'
        faccpedi.nroped '|'
        faccpedi.codcli '|'
        faccpedi.nomcli '|'
        detalle.codmat '|'
        almmmatg.desmat '|'
        detalle.undvta '|'
        detalle.canped '|'
        detalle.preuni '|'
        detalle.implin '|'
        faccpedi.imptot
        SKIP.
END.
OUTPUT CLOSE.

