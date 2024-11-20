OUTPUT TO d:\tmp\pedidos.txt.
PUT UNFORMATTED 
    'DIVISION|DOC|NUMERO|FECHA|CLIENTE|NOMBRE|REF|NUMERO|ORDEN COMPRA|FORMA PAGO|DIV DESTINO|ALMACEN|'
    'IMPORTE|MONEDA|TIPO CAMBIO|GLOSA|ESTADO|ARTICULO|CANTIDAD|UNIDAD|IMPORTE'
    SKIP.
FOR EACH faccpedi NO-LOCK WHERE codcia = 1 
    and coddoc = 'ped' 
    and fchped >= 01/01/2016 
    and fchped <= 09/30/2016 
    and flgest <> 'A',
    EACH facdpedi OF faccpedi NO-LOCK:
    PUT UNFORMATTED
        faccpedi.coddiv '|'
        faccpedi.coddoc '|'
        faccpedi.nroped '|'
        faccpedi.fchped '|'
        faccpedi.codcli '|'
        faccpedi.nomcli '|'
        faccpedi.codref '|'
        faccpedi.nroref '|'
        faccpedi.ordcmp '|'
        faccpedi.fmapgo '|'
        faccpedi.divdes '|'
        faccpedi.codalm '|'
        faccpedi.imptot '|'
        faccpedi.codmon '|'
        faccpedi.tpocmb '|'
        faccpedi.glosa '|'
        faccpedi.flgest '|'
        facdpedi.codmat '|'
        facdpedi.canped '|'
        facdpedi.undvta '|'
        facdpedi.implin
        SKIP
        .
END.
OUTPUT CLOSE.
OUTPUT TO d:\tmp\salidas.txt.
PUT UNFORMATTED 
    'ALMACEN|SERIE|NUMERO|FECHA|ARTICULO|CANTIDAD|FACTOR|UNIDAD'
    SKIP.
FOR EACH almcmov NO-LOCK WHERE codcia = 1 
    and tipmov = 's' 
    and codmov = 02 
    and fchdoc >= 01/01/2016 
    and fchdoc <= 09/30/2016 
    and flgest <> 'a',
    EACH almdmov OF almcmov NO-LOCK:
    PUT UNFORMATTED
        almcmov.codalm '|'
        almcmov.nroser '|'
        almcmov.nrodoc '|'
        almcmov.fchdoc '|'
        almdmov.codmat '|'
        almdmov.candes '|'
        almdmov.factor '|'
        almdmov.codund 
        SKIP
        .
END.
OUTPUT CLOSE.
