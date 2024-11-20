DEF TEMP-TABLE detalle
    FIELD coddiv AS CHAR
    FIELD periodo AS INT
    FIELD nromes AS INT
    FIELD coddoc AS CHAR
    FIELD nrodoc AS CHAR
    FIELD cantidad AS INT.

DEF TEMP-TABLE pedido
    FIELD coddiv AS CHAR
    FIELD periodo AS INT
    FIELD nromes AS INT
    FIELD codped AS CHAR
    FIELD nroped AS CHAR
    INDEX llave AS PRIMARY coddiv codped nroped.


FOR EACH ccbcdocu NO-LOCK WHERE codcia = 1
    AND LOOKUP(coddoc, 'FAC,BOL,N/C,N/D,TCK') > 0
    AND fchdoc >= 10/01/2013
    AND fchdoc <= 03/31/2014:
    FIND detalle WHERE detalle.coddiv = ccbcdocu.coddiv
        AND detalle.periodo = YEAR(ccbcdocu.fchdoc)
        AND detalle.nromes = MONTH(ccbcdocu.fchdoc)
        AND detalle.coddoc = ccbcdocu.coddoc
        NO-ERROR.
    IF NOT AVAILABLE detalle THEN CREATE detalle.
    ASSIGN
        detalle.coddiv = ccbcdocu.coddiv
        detalle.periodo = YEAR(ccbcdocu.fchdoc)
        detalle.nromes = MONTH(ccbcdocu.fchdoc)
        detalle.coddoc = ccbcdocu.coddoc
        detalle.cantidad = detalle.cantidad + 1.
END.

DEF VAR cSerie AS CHAR NO-UNDO.
cSerie      = '914,915'.

&SCOPED-DEFINE CONDICION ( ~
            CcbCMvto.CodCia = 001 AND ~
            CcbCMvto.CodDoc = "PER" AND ~
            LOOKUP(SUBSTRING(CcbCMvto.NroDoc,1,3), cSerie) > 0 AND ~
            (CcbCMvto.fchdoc >= 10/01/2013 AND CcbCMvto.fchdoc <= 03/31/2014) )

FOR EACH CcbCMvto WHERE {&CONDICION} NO-LOCK:
    FIND detalle WHERE detalle.coddiv = ccbcmvto.coddiv
        AND detalle.periodo = YEAR(ccbcmvto.fchdoc)
        AND detalle.nromes = MONTH(ccbcmvto.fchdoc)
        AND detalle.coddoc = ccbcmvto.coddoc
        NO-ERROR.
    IF NOT AVAILABLE detalle THEN CREATE detalle.
    ASSIGN
        detalle.coddiv = ccbcmvto.coddiv
        detalle.periodo = YEAR(ccbcmvto.fchdoc)
        detalle.nromes = MONTH(ccbcmvto.fchdoc)
        detalle.coddoc = ccbcmvto.coddoc
        detalle.cantidad = detalle.cantidad + 1.
END.
OUTPUT TO c:\tmp\cuenta-comprobantes.txt.
PUT UNFORMATTED 'DIVISION|PERIODO|MES|COMPROBANTE|CANTIDAD' SKIP.
FOR EACH detalle.
    PUT UNFORMATTE
        detalle.coddiv '|'
        detalle.periodo '|'
        detalle.nromes '|'
        detalle.coddoc '|'
        detalle.cantidad
        SKIP.
END.
OUTPUT CLOSE.


/* Cuenta Pedidos */
EMPTY TEMP-TABLE detalle.
FOR EACH ccbcdocu NO-LOCK WHERE codcia = 1
    AND LOOKUP(coddoc, 'FAC,BOL,TCK') > 0
    AND LOOKUP(tpofac, 'A,S,M') = 0
    AND fchdoc >= 10/01/2013
    AND fchdoc <= 03/31/2014:
    FIND pedido WHERE pedido.coddiv = ccbcdocu.coddiv
        AND pedido.periodo = YEAR(ccbcdocu.fchdoc)
        AND pedido.nromes = MONTH(ccbcdocu.fchdoc)
        AND pedido.codped = ccbcdocu.codped
        AND pedido.nroped = ccbcdocu.nroped
        NO-ERROR.
    IF NOT AVAILABLE pedido THEN CREATE pedido.
    ASSIGN
        pedido.coddiv = ccbcdocu.coddiv
        pedido.periodo = YEAR(ccbcdocu.fchdoc)
        pedido.nromes = MONTH(ccbcdocu.fchdoc)
        pedido.codped = ccbcdocu.codped
        pedido.nroped = ccbcdocu.nroped.
END.
FOR EACH pedido:
    FIND detalle WHERE detalle.coddiv = pedido.coddiv
        AND detalle.periodo = pedido.periodo
        AND detalle.nromes = pedido.nromes
        AND detalle.coddoc = pedido.codped
        NO-ERROR.
    IF NOT AVAILABLE detalle THEN CREATE detalle.
    ASSIGN
        detalle.coddiv = pedido.coddiv
        detalle.periodo = pedido.periodo
        detalle.nromes = pedido.nromes
        detalle.coddoc = pedido.codped
        detalle.cantidad = detalle.cantidad + 1.
END.
OUTPUT TO c:\tmp\cuenta-pedidos.txt.
PUT UNFORMATTED 'DIVISION|PERIODO|MES|COMPROBANTE|CANTIDAD' SKIP.
FOR EACH detalle.
    PUT UNFORMATTE
        detalle.coddiv '|'
        detalle.periodo '|'
        detalle.nromes '|'
        detalle.coddoc '|'
        detalle.cantidad
        SKIP.
END.
OUTPUT CLOSE.
