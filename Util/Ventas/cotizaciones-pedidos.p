DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR s-coddiv AS CHAR INIT '00024' NO-UNDO.
DEF BUFFER PEDIDO FOR faccpedi.
DEF VAR pEstado AS CHAR NO-UNDO.

OUTPUT TO c:\tmp\coti-pedi.txt.
PUT UNFORMATTED
    'DIVISION|LISTA|DOC|NUMERO|EMISION|ENTREGAR|VENCIMIENTO|CLIENTE|NOMCLI|'
    'IMPORTE|ESTADO|DOC|NUMERO|EMISION|ENTREGA|VENCIMIENTO|IMPORTE|ESTADO'
    SKIP.
FOR EACH faccpedi NO-LOCK WHERE codcia = s-codcia
    AND coddiv = s-coddiv
    AND coddoc = 'COT'
    AND fchped >= 11/01/2014
    AND flgest <> 'A',
    EACH pedido NO-LOCK WHERE pedido.codcia = s-codcia
    AND pedido.coddoc = 'PED'
    AND pedido.codref = faccpedi.coddoc
    AND pedido.nroref = faccpedi.nroped
    AND pedido.fchped >= faccpedi.fchped
    AND pedido.flgest <> 'A':
    PUT UNFORMATTED
        faccpedi.coddiv '|'
        FacCPedi.Libre_c01 '|'
        faccpedi.coddoc '|'
        faccpedi.nroped '|'
        FacCPedi.FchEnt '|'
        FacCPedi.fchven '|'
        faccpedi.fchped '|'
        FacCPedi.CodCli '|'
        FacCPedi.NomCli '|'
        FacCPedi.ImpTot '|'.
    RUN vta2/p-faccpedi-flgest (faccpedi.flgest, faccpedi.coddoc, OUTPUT pEstado).
    PUT UNFORMATTED
        pEstado '|'
        pedido.coddoc '|'
        pedido.nroped '|'
        pedido.FchEnt '|'
        pedido.fchven '|'
        pedido.fchped '|'
        pedido.imptot '|'.
    RUN vta2/p-faccpedi-flgest (pedido.flgest, pedido.coddoc, OUTPUT pEstado).
    PUT UNFORMATTED
        pEstado
        SKIP.
END.
OUTPUT CLOSE.

