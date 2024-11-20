/* COTIZACIONES AL DETALLE  */
DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR s-coddoc AS CHAR INIT 'COT' NO-UNDO.
DEF VAR FechaD AS DATE NO-UNDO.
DEF VAR FechaH AS DATE NO-UNDO.

ASSIGN
    FechaD = 05/01/2014
    FechaH = 05/31/2014.
OUTPUT TO c:\tmp\cotizaciones-mayo.txt.
PUT UNFORMATTED
    'DIVISION|DOC|NUMERO|CLIENTE|NOMBRE|CONDICION|DESCRIPCION|EMISION|~
    MONEDA|ARTICULO|DESCRIPCION|CANTIDAD|UNIDAD|FACTOR|UNITARIO|IMPORTE'
    SKIP.
FOR EACH faccpedi NO-LOCK WHERE codcia = s-codcia
    AND coddoc = s-coddoc
    AND fchped >= FechaD
    AND fchped <= FechaH
    AND flgest <> 'A',
    FIRST gn-ConVt WHERE gn-ConVt.Codig = faccpedi.fmapgo,
    EACH facdpedi OF faccpedi NO-LOCK,
    FIRST almmmatg OF facdpedi NO-LOCK:
    PUT UNFORMATTED
        faccpedi.coddiv '|'
        faccpedi.coddoc '|'
        faccpedi.nroped '|'
        faccpedi.codcli '|'
        faccpedi.nomcli '|'
        faccpedi.fmapgo '|'
        gn-convt.nombr '|'
        faccpedi.fchped '|'
        faccpedi.codmon '|'
        facdpedi.codmat '|'
        almmmatg.desmat '|'
        facdpedi.canped '|'
        facdpedi.undvta '|'
        facdpedi.factor '|'
        facdpedi.preuni '|'
        facdpedi.implin '|'
        SKIP.
END.
OUTPUT CLOSE.

