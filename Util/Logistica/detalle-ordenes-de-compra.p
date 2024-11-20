DEF TEMP-TABLE detalle LIKE lg-cocmp.

DEF VAR x-TpoCmbCmp AS DEC.
DEF VAR x-TpoCmbVta AS DEC.

FOR EACH lg-cocmp NO-LOCK WHERE codcia = 1
    /*AND LOOKUP(flgsit, 'P,T,C') > 0*/
    AND fchdoc >= 01/01/2008
    AND fchdoc <= 01/31/2010:
    FIND FIRST Almcmov WHERE Almcmov.codcia = 1
        AND Almcmov.codalm = lg-cocmp.codalm
        AND Almcmov.tipmov = 'I'
        AND Almcmov.codmov = 02
        AND Almcmov.codpro = lg-cocmp.codpro
        AND INTEGER (Almcmov.nrorf1) = lg-cocmp.nrodoc
        AND Almcmov.flgest <> 'A'
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Almcmov THEN NEXT.
    FIND LAST Gn-Tcmb WHERE Gn-Tcmb.Fecha <= lg-cocmp.FchDoc
        USE-INDEX Cmb01 NO-LOCK NO-ERROR.
    IF NOT AVAIL Gn-Tcmb THEN 
        FIND FIRST Gn-Tcmb WHERE Gn-Tcmb.Fecha >= lg-cocmp.FchDoc
            USE-INDEX Cmb01 NO-LOCK NO-ERROR.
    IF AVAIL Gn-Tcmb THEN 
        ASSIGN
            x-TpoCmbCmp = Gn-Tcmb.Compra
            x-TpoCmbVta = Gn-Tcmb.Venta.
    FIND detalle OF lg-cocmp EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE detalle THEN DO:
        CREATE detalle.
        BUFFER-COPY lg-cocmp TO detalle.
    END.
    IF lg-cocmp.codmon = 1 
    THEN ASSIGN
            detalle.impbrt = lg-cocmp.impbrt
            detalle.impnet = lg-cocmp.impnet
            detalle.impigv = lg-cocmp.impigv
            detalle.imptot = lg-cocmp.imptot.
    ELSE ASSIGN
            detalle.impbrt = lg-cocmp.impbrt * x-TpoCmbVta
            detalle.impnet = lg-cocmp.impnet * x-TpoCmbVta
            detalle.impigv = lg-cocmp.impigv * x-TpoCmbVta
            detalle.imptot = lg-cocmp.imptot * x-TpoCmbVta.
END.

OUTPUT TO c:\tmp\detalle-compras-conti.txt.
FOR EACH detalle NO-LOCK:
    DISPLAY
        detalle.fchdoc
        detalle.nrodoc
        detalle.codpro
        detalle.nompro
        detalle.impbrt
        detalle.impnet
        detalle.impigv
        detalle.imptot
        WITH STREAM-IO NO-BOX WIDTH 320.
END.
OUTPUT CLOSE.

