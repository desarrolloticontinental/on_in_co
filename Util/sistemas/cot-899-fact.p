DEF BUFFER PEDIDO FOR Faccpedi.

DEFINE TEMP-TABLE t-cotizacion NO-UNDO
    FIELD coddoc LIKE faccpedi.coddoc
    FIELD nroped LIKE faccpedi.nroped
    FIELD codcli LIKE faccpedi.codcli
    FIELD nomcli LIKE faccpedi.nomcli
    FIELD fchped LIKE faccpedi.fchped
    FIELD codven LIKE faccpedi.codven
    FIELD imptot LIKE faccpedi.imptot
    FIELD codfac AS CHAR LABEL 'Comprobante'
    FIELD nrofac AS CHAR LABEL 'Nro Comprobante'
    .

FOR EACH faccpedi NO-LOCK WHERE codcia = 1
    AND coddoc = 'cot'
    AND fmapgo = '899'
    AND fchped >= 01/01/2018
    AND flgest <> 'A',
    EACH PEDIDO NO-LOCK WHERE PEDIDO.codcia = 1
    AND PEDIDO.coddoc = 'PED'
    AND PEDIDO.codref = faccpedi.coddoc 
    AND PEDIDO.nroref = faccpedi.nroped
    AND PEDIDO.flgest <> 'A',
    FIRST ccbcdocu NO-LOCK WHERE ccbcdocu.codcia = 1
    AND ccbcdocu.codped = PEDIDO.coddoc
    AND ccbcdocu.nroped = PEDIDO.nroped 
    AND LOOKUP(ccbcdocu.coddoc, 'FAC,BOL') > 0
    AND ccbcdocu.flgest <> 'A':
    CREATE t-cotizacion.
    BUFFER-COPY faccpedi TO t-cotizacion
        ASSIGN
        t-cotizacion.codfac = ccbcdocu.coddoc
        t-cotizacion.nrofac = ccbcdocu.nrodoc.
END.
OUTPUT TO d:\cot899.txt.
FOR EACH t-cotizacion NO-LOCK:
    DISPLAY t-cotizacion WITH STREAM-IO NO-BOX WIDTH 320.
END.
OUTPUT CLOSE.

