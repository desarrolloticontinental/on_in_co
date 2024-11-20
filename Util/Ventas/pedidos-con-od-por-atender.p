DEF BUFFER ordenes FOR faccpedi.
OUTPUT TO d:\tmp\poratender.txt.
PUT UNFORMATTED
    'ORIGEN|DESTINO|EMISION|ENTREGAR|PEDIDO|CLIENTE|NOMBRE|IMPORTE|'+
    'ARTICULO|DESCRIPCION|PEDIDO|ANTENDIDO|' +
    'ORDEN|EMISION'
    SKIP.
FOR EACH faccpedi NO-LOCK WHERE codcia = 1
    AND coddoc = 'PED'
    AND flgest = 'C'
    AND fchped >= 07/01/2015,
    FIRST ordenes NO-LOCK WHERE ordenes.codcia = 1
    AND ordenes.coddiv = faccpedi.coddiv
    AND ordenes.coddoc = 'O/D'
    AND ordenes.codref = faccpedi.coddoc
    AND ordenes.nroref = faccpedi.nroped
    AND ordenes.flgest = 'P'
    AND ordenes.flgsit = 'T',
    EACH facdpedi OF ordenes NO-LOCK,
    FIRST almmmatg OF facdpedi NO-LOCK:
    PUT UNFORMATTED
        faccpedi.coddiv '|'
        faccpedi.divdes '|'
        faccpedi.fchped '|'
        faccpedi.fchent '|'
        faccpedi.nroped '|'
        faccpedi.codcli '|'
        faccpedi.nomcli '|'
        faccpedi.imptot '|'
        facdpedi.codmat '|'
        almmmatg.desmat '|'
        facdpedi.canped '|'
        facdpedi.canate '|'
        ordenes.coddoc  ' ' ordenes.nroped  '|'
        ordenes.fchped  '|'
        SKIP.
END.
