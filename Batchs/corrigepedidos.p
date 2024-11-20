DISABLE TRIGGERS FOR LOAD OF faccpedi.
DISABLE TRIGGERS FOR LOAD OF facdpedi.

/* Proceso que verifica los pedidos versus lo facturado */
DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR x-fchped AS DATE NO-UNDO.

DEF BUFFER pedido FOR faccpedi.
DEF BUFFER detalle FOR facdpedi.
    DEF BUFFER ccredi FOR ccbcdocu.
    DEF BUFFER dcredi FOR ccbddocu.

DEF TEMP-TABLE t-detalle LIKE facdpedi
    FIELD prowid AS ROWID.
DEF TEMP-TABLE t-pedido LIKE faccpedi
    FIELD prowid AS ROWID.

ASSIGN
    x-fchped = TODAY - (3 * 30).
ASSIGN
    x-fchped = x-fchped - DAY(x-fchped) + 1.
/* CARGAMOS LOS PEDIDOS CON ATENCIONES EN CERO */
FOR EACH faccpedi NO-LOCK WHERE faccpedi.codcia = s-codcia
    AND faccpedi.coddiv = '00015'
    AND faccpedi.coddoc = 'PED'
    AND faccpedi.fchped >= 09/01/2014
    AND faccpedi.fchped <= TODAY - 1
    AND LOOKUP(faccpedi.flgest, "G,X,W,WX,WL,P,C,S,E") > 0:
    CREATE t-pedido.
    BUFFER-COPY faccpedi TO t-pedido ASSIGN t-pedido.prowid = ROWID(faccpedi).
    FOR EACH facdpedi OF faccpedi NO-LOCK:
        CREATE t-detalle.
        BUFFER-COPY facdpedi TO t-detalle 
            ASSIGN t-detalle.prowid = ROWID(facdpedi) t-detalle.canate = 0.
    END.
END.
/* CARGAMOS EN LOS PEDIDOS LAS CANTIDADES FACTURADAS */
FOR EACH t-pedido, EACH t-detalle OF t-pedido:
    FOR EACH ccbcdocu NO-LOCK WHERE ccbcdocu.codcia = s-codcia
        AND ccbcdocu.fchdoc >= t-pedido.fchped
        AND ccbcdocu.codped = t-pedido.coddoc
        AND ccbcdocu.nroped = t-pedido.nroped
        AND LOOKUP(ccbcdocu.coddoc, 'FAC,BOL') > 0
        AND ccbcdocu.flgest <> 'A',
        EACH ccbddocu OF ccbcdocu NO-LOCK WHERE ccbddocu.codmat = t-detalle.codmat:
        ASSIGN t-detalle.canate = t-detalle.canate + ccbddocu.candes.
    END.
END.
/* Buscamos O/D para PED aún no facturados */
FOR EACH t-pedido, FIRST faccpedi NO-LOCK WHERE faccpedi.codcia = s-codcia
    AND faccpedi.coddoc = 'O/D'
    AND faccpedi.codref = t-pedido.coddoc
    AND faccpedi.nroref = t-pedido.nroped
    AND faccpedi.fchped >= t-pedido.fchped
    AND faccpedi.flgest = 'P',
    EACH t-detalle OF t-pedido WHERE t-detalle.canate = 0,
    FIRST facdpedi OF faccpedi NO-LOCK WHERE facdpedi.codmat = t-detalle.codmat:
    ASSIGN t-detalle.canate = t-detalle.canate + facdpedi.canped.
END.

/* ACTUALIZAMOS LA CANTIDAD ATENDIDA EN LAS COTIZACIONES */

/* BUSCAMOS LAS DIFERENCIAS */
FOR EACH t-pedido,
    EACH t-detalle OF t-pedido,
    FIRST facdpedi NO-LOCK WHERE ROWID(facdpedi) = t-detalle.prowid:
    IF t-detalle.canate <> facdpedi.canate THEN DO:
        DISPLAY t-pedido.flgest t-pedido.coddoc t-pedido.nroped t-detalle.codmat t-detalle.canped 
            t-detalle.canate facdpedi.canate
            WITH STREAM-IO NO-BOX WIDTH 320.
        PAUSE 0.
        facdpedi.canate = t-detalle.canate.
    END.
END.
