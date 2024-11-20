/* Transferencias pendientes de recepcionar */
DEF VAR s-codcia AS INT INIT 001.
DEF VAR x-costo AS DEC NO-UNDO.
DEF VAR x-estado AS CHAR.
DEF TEMP-TABLE detalle
    FIELD coddiv AS CHAR
    FIELD codped AS CHAR
    FIELD nroped AS CHAR
    FIELD fchped AS DATE
    FIELD estped AS CHAR
    FIELD codalm AS CHAR
    FIELD almdes AS CHAR
    FIELD nroser AS INT
    FIELD nrodoc AS INT
    FIELD fchdoc AS DATE
    FIELD sitdoc AS CHAR
    FIELD codmat AS CHAR
    FIELD desmat AS CHAR
    FIELD codund AS CHAR
    FIELD codfam AS CHAR
    FIELD subfam AS CHAR
    FIELD candes AS DEC
    FIELD peso AS DEC
    FIELD costo AS DEC
    .

FOR EACH faccpedi NO-LOCK WHERE faccpedi.codcia = s-codcia
    AND faccpedi.coddoc = 'OTR'
    AND faccpedi.fchped >= 01/01/2015,
    EACH facdpedi OF faccpedi NO-LOCK,
    FIRST Almmmatg OF facdpedi NO-LOCK:
    CREATE detalle.
    ASSIGN
        detalle.coddiv = faccpedi.coddiv
        detalle.codped = faccpedi.coddoc
        detalle.nroped = faccpedi.nroped
        detalle.fchped = faccpedi.fchped
        detalle.codmat = facdpedi.codmat
        detalle.desmat = almmmatg.desmat
        detalle.codund = facdpedi.undvta
        detalle.codfam = almmmatg.codfam
        detalle.subfam = almmmatg.subfam
        detalle.candes = facdpedi.canped
        detalle.peso   = (facdpedi.canped * Almmmatg.PesMat)
        detalle.costo  = facdpedi.canped * Almmmatg.CtoTot * 
        (IF almmmatg.monvta = 2 THEN almmmatg.tpocmb ELSE 1).
    RUN vta2/p-faccpedi-flgest (faccpedi.flgest, faccpedi.coddoc, OUTPUT x-estado).
    ASSIGN detalle.estped = x-estado.
END.
FOR EACH detalle,
    EACH almcmov NO-LOCK WHERE almcmov.codcia = s-codcia
    AND almcmov.codref = detalle.codped
    AND almcmov.nroref = detalle.nroped
    AND almcmov.tipmov = 's'
    AND almcmov.codmov = 03
    AND almcmov.flgest <> 'A':
    ASSIGN
        detalle.codalm = almcmov.codalm
        detalle.almdes = almcmov.almdes
        detalle.nroser = almcmov.nroser
        detalle.nrodoc = almcmov.nrodoc
        detalle.fchdoc = almcmov.fchdoc
        detalle.sitdoc = (IF almcmov.flgsit = 'R' THEN 'RECEPCIONADO' ELSE 'EN TRANSITO').
        .
END.
OUTPUT TO c:\tmp\otr-trf.txt.
PUT UNFORMATTED
    'DIVISION|DOC|NUMERO|FECHA|ESTADO|'
    'ALM SALIDA|ALM DESTINO|SERIE|NUMERO|FECHA|SITUACION|'
    'ARTICULO|DESCRIPCION|UNIDAD|LINEA|SUBLINEA|CANTIDAD|PESO|COSTO'
    SKIP.
FOR EACH detalle:
    PUT UNFORMATTED
        detalle.coddiv '|'
        detalle.codped '|'
        detalle.nroped '|'
        detalle.fchped '|'
        detalle.estped '|'
        detalle.codalm '|'
        detalle.almdes '|'
        detalle.nroser '|'
        detalle.nrodoc '|'
        detalle.fchdoc '|'
        detalle.sitdoc '|'
        detalle.codmat '|'
        detalle.desmat '|'
        detalle.codund '|'
        detalle.codfam '|'
        detalle.subfam '|'
        detalle.candes '|'
        detalle.peso '|'
        detalle.costo
        SKIP.
END.
OUTPUT CLOSE.

