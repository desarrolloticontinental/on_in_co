DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR s-coddiv AS CHAR INIT '00015' NO-UNDO.
DEF BUFFER PEDIDO FOR faccpedi.
DEF BUFFER COTIZACION FOR faccpedi. 
DEF VAR pEstado AS CHAR NO-UNDO.

OUTPUT TO c:\tmp\coti-pedi.txt.
PUT UNFORMATTED
    'DIVISION|LISTA|DOC|NUMERO|EMISION|ENTREGAR|VENCIMIENTO|CLIENTE|NOMCLI|'
    'IMPORTE|ESTADO|DOC|NUMERO|EMISION|ENTREGA|VENCIMIENTO|IMPORTE|ESTADO'
    SKIP.
FOR EACH COTIZACION NO-LOCK WHERE codcia = s-codcia
    AND coddiv = s-coddiv
    AND coddoc = 'COT'
    AND fchped >= 09/01/2014
    AND flgest <> 'A',
    EACH pedido NO-LOCK WHERE pedido.codcia = s-codcia
    AND pedido.coddoc = 'PED'
    AND pedido.codref = COTIZACION.coddoc
    AND pedido.nroref = COTIZACION.nroped
    AND pedido.fchped >= COTIZACION.fchped
    AND pedido.flgest <> 'A':
    PUT UNFORMATTED
        COTIZACION.coddiv '|'
        COTIZACION.Libre_c01 '|'
        COTIZACION.coddoc '|'
        COTIZACION.nroped '|'
        COTIZACION.FchEnt '|'
        COTIZACION.fchven '|'
        COTIZACION.fchped '|'
        COTIZACION.CodCli '|'
        COTIZACION.NomCli '|'
        COTIZACION.ImpTot '|'.
    RUN vta2/p-COTIZACION-flgest (COTIZACION.flgest, COTIZACION.coddoc, OUTPUT pEstado).
    PUT UNFORMATTED
        pEstado '|'
        pedido.coddoc '|'
        pedido.nroped '|'
        pedido.FchEnt '|'
        pedido.fchven '|'
        pedido.fchped '|'
        pedido.imptot '|'.
    RUN vta2/p-COTIZACION-flgest (pedido.flgest, pedido.coddoc, OUTPUT pEstado).
    PUT UNFORMATTED
        pEstado
        SKIP.
END.
OUTPUT CLOSE.

