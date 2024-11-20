/* Transferencias pendientes de recepcionar */
DEF VAR s-codcia AS INT INIT 001.
DEF VAR x-costo AS DEC NO-UNDO.

OUTPUT TO c:\tmp\transferencias.txt.
PUT UNFORMATTED
    'Alm Origen|Alm Destino|Referencia|Fecha|Serie|Numero|Estado|Situacion|Observaciones|'
    'Articulo|Descripcion|Linea|Sublinea|Cantidad|Unidad|Peso|Costo S/.|Usuario Almacen|'
    'Inner|Master|Min Origen|Min Destino|Usuarioo OTR'
    SKIP.
FOR EACH almacen NO-LOCK WHERE almacen.codcia = s-codcia,
    EACH almcmov NO-LOCK WHERE almcmov.codcia = s-codcia
    AND almcmov.codalm = almacen.codalm
    AND almcmov.tipmov = 'S'
    AND almcmov.codmov = 03
    AND almcmov.flgest <> "A"
    AND almcmov.flgsit = "R"    /* Recepcionado */
    AND almcmov.fchdoc >= 11/01/2014
    AND almcmov.fchdoc <= 03/31/2015,
    EACH almdmov OF almcmov NO-LOCK,
    FIRST almmmatg OF almdmov NO-LOCK:
    x-costo = almdmov.candes * Almmmatg.CtoTot * 
        (IF almmmatg.monvta = 2 THEN almmmatg.tpocmb ELSE 1).
    PUT UNFORMATTED
        almcmov.codalm '|'
        almcmov.almdes '|'
        almcmov.codref almcmov.nroref '|'
        almcmov.fchdoc '|'
        almcmov.nroser '|'
        almcmov.nrodoc '|'
        (IF almcmov.flgest = 'A' THEN 'ANULADA' ELSE '') '|'
        (IF almcmov.flgsit = 'R' THEN 'RECEPCIONADO' ELSE 'EN TRANSITO') '|'
        almcmov.observ '|'
        almdmov.codmat '|'
        almmmatg.desmat '|'
        almmmatg.codfam '|'
        almmmatg.subfam '|'
        almdmov.candes '|'
        almdmov.codund '|'
        (almdmov.candes * Almmmatg.PesMat) '|'
        x-costo '|'
        almcmov.usuario '|'
        almmmatg.stkrep '|'
        almmmatg.canemp '|'.
    /* Minimo Origen */
    FIND FIRST almmmate OF almmmatg WHERE almmmate.codalm = almcmov.codalm NO-LOCK NO-ERROR.
    IF AVAILABLE almmmate THEN PUT UNFORMATTED almmmate.stkmax '|'.
    ELSE PUT UNFORMATTED '|'.
    FIND FIRST almmmate OF almmmatg WHERE almmmate.codalm = almcmov.almdes NO-LOCK NO-ERROR.
    IF AVAILABLE almmmate THEN PUT UNFORMATTED almmmate.stkmax '|'.
    ELSE PUT UNFORMATTED '|'.
    /* Usuario OTR */
    IF almcmov.codref = 'OTR' THEN DO:
        FIND faccpedi WHERE faccpedi.codcia = s-codcia
            AND faccpedi.coddoc = almcmov.codref
            AND faccpedi.nroped = almcmov.nroref
            NO-LOCK NO-ERROR.
        IF AVAILABLE faccpedi THEN DO:
            FIND almcrepo WHERE almcrepo.CodCia = s-codcia
                AND almcrepo.almped = almcmov.codalm
                AND almcrepo.nroser = INT(SUBSTRING(faccpedi.nroref,1,3))
                AND almcrepo.NroDoc = INT(SUBSTRING(faccpedi.nroref,4))
                NO-LOCK NO-ERROR.
            IF AVAILABLE almcrepo THEN PUT UNFORMATTED almcrepo.Usuario '|'.
            ELSE PUT '|'.
        END.
        ELSE PUT '|'.
    END.
    PUT '|'.
    PUT SKIP.
END.
OUTPUT CLOSE.
