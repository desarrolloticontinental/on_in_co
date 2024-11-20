DEF BUFFER pedido FOR faccpedi.
DEF VAR x-estado AS CHAR.
DEF STREAM reporte.
DEF VAR x-hr-nro LIKE di-rutac.nrodoc.
DEF VAR x-hr-cod LIKE di-rutac.coddoc.
DEF VAR x-hr-fch LIKE di-rutac.fchdoc.

OUTPUT STREAM reporte TO c:\tmp\div00024.txt.
PUT STREAM reporte UNFORMATTED 
    'DOC|NUMERO|FECHA|DOC|NUMERO|FECHA|ESTADO|DOC|NUMERO|FECHA'
    'ARTICULO|DESCRIPCION|UNIDAD|CANTIDAD|IMPORTE|'
    SKIP.
FOR EACH faccpedi NO-LOCK WHERE codcia = 1
    AND coddiv = '00024'
    AND coddoc = 'cot'
    AND fchped >= 11/01/2014
    AND flgest <> 'A',
    EACH pedido NO-LOCK WHERE pedido.codcia = 1
    AND pedido.coddiv = faccpedi.coddiv
    AND pedido.coddoc = 'ped'
    AND pedido.codref = faccpedi.coddoc
    AND pedido.nroref = faccpedi.nroped
    AND pedido.flgest <> 'A':
    DISPLAY faccpedi.fchped faccpedi.coddoc faccpedi.nroped.
    PAUSE 0.
    RUN vta2/p-faccpedi-flgest (pedido.flgest, pedido.coddoc, OUTPUT x-estado).
    FIND LAST ccbcdocu WHERE ccbcdocu.codcia = 1
        AND ccbcdocu.coddoc = 'g/r'
        AND ccbcdocu.codped = pedido.coddoc
        AND ccbcdocu.nroped = pedido.nroped
        AND ccbcdocu.flgest <> 'A'
        NO-LOCK NO-ERROR.
    ASSIGN
        x-hr-cod = ?
        x-hr-nro = ?
        x-hr-fch = ?.
    IF AVAILABLE ccbcdocu THEN DO:
        FIND LAST di-rutad WHERE DI-RutaD.CodCia = 1
            AND DI-RutaD.CodDoc = 'H/R'
            AND DI-RutaD.CodRef = ccbcdocu.coddoc
            AND DI-RutaD.NroRef = ccbcdocu.nrodoc
            AND CAN-FIND(LAST di-rutac OF di-rutad WHERE di-rutac.flgest <> 'A' NO-LOCK)
            NO-LOCK NO-ERROR.
        IF AVAILABLE di-rutad THEN DO:
            FIND di-rutac OF di-rutad NO-LOCK NO-ERROR.
            ASSIGN
                x-hr-cod = di-rutac.coddoc
                x-hr-nro = di-rutac.nrodoc
                x-hr-fch = di-rutac.fchdoc.
        END.
    END.
    FOR EACH facdpedi OF pedido NO-LOCK, FIRST almmmatg OF facdpedi NO-LOCK:
        PUT STREAM reporte UNFORMATTED
            faccpedi.coddoc '|'
            faccpedi.nroped '|'
            faccpedi.fchped '|'
            pedido.coddoc '|'
            pedido.nroped '|'
            pedido.fchped '|'
            x-estado '|'
            x-hr-cod '|'
            x-hr-nro '|'
            x-hr-fch '|'
            facdpedi.codmat '|'
            almmmatg.desmat '|'
            facdpedi.undvta '|'
            facdpedi.canped '|'
            facdpedi.implin
            SKIP.
    END.
END.
OUTPUT CLOSE.

