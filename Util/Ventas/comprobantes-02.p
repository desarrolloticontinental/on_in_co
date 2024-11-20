
DEF BUFFER b-cpedi FOR faccpedi.

OUTPUT TO c:\tmp\comprobantes.txt.

PUT UNFORMATTED
    'DIVISION|ORIGEN|CODIGO|NUMERO|EMISION|FMAPGO|PEDIDO|FCHPEDIDO|COTIZACION|FCHCOTIZACION|LISTA|CLIENTE|NOMBRE|MONEDA|' +
    'ARTICULO|DESCRIPCION|LINEA|CANTIDAD|UNIDAD|PREUNI|IMPORTE'
    SKIP.
FOR EACH ccbcdocu NO-LOCK WHERE codcia = 1
    AND coddoc = 'FAC'
    AND divori = '10060'
    AND flgest <> "A",
    FIRST faccpedi NO-LOCK WHERE faccpedi.codcia = ccbcdocu.codcia
    AND faccpedi.coddoc = ccbcdocu.codped
    AND faccpedi.nroped = ccbcdocu.nroped,
    FIRST b-cpedi NO-LOCK WHERE b-cpedi.codcia = faccpedi.codcia
    AND b-cpedi.coddoc = faccpedi.codref
    AND b-cpedi.nroped = faccpedi.nroref,
    FIRST ccbddocu OF ccbcdocu NO-LOCK, FIRST Almmmatg OF Ccbddocu NO-LOCK WHERE Almmmatg.codfam = '011':
    PUT UNFORMATTED
        ccbcdocu.coddiv '|'
        ccbcdocu.divori '|'
        ccbcdocu.coddoc '|'
        ccbcdocu.nrodoc '|'
        ccbcdocu.fchdoc '|'
        ccbcdocu.fmapgo '|'
        ccbcdocu.codpe ccbcdocu.nroped '|'
        faccpedi.fchped '|'
        b-cpedi.coddoc b-cpedi.nroped '|'
        b-cpedi.fchped '|'
        b-cpedi.libre_c01 '|'
        ccbcdocu.codcli '|'
        ccbcdocu.nomcli '|'
        ccbcdocu.codmon '|'
        ccbddocu.codmat '|'
        desmat '|'
        codfam '|'
        candes '|'
        undvta '|'
        preuni '|'
        implin
        SKIP.
END.
OUTPUT CLOSE.

