DEF TEMP-TABLE detalle LIKE lg-cocmp.

DEF VAR x-TpoCmbCmp AS DEC.
DEF VAR x-TpoCmbVta AS DEC.
DEF VAR X-ImpTot AS DEC.
DEF VAR x-nrodoc LIKE lg-cocmp.nrodoc.

FOR EACH lg-cocmp NO-LOCK WHERE lg-cocmp.codcia = 1
    AND lg-cocmp.flgsit = 'P'
    AND lg-cocmp.codpro = '10005035'
    AND lg-cocmp.fchdoc >= 01/01/2008
    AND lg-cocmp.fchdoc <= 01/31/2010:
    x-ImpTot = 0.
    x-nrodoc = lg-cocmp.nrodoc.
    FOR EACH lg-docmp NO-LOCK WHERE lg-docmp.codcia = 1
        AND lg-docmp.nrodoc = x-nrodoc:
        x-ImpTot = ( LG-DOCmp.CanPedi - LG-DOCmp.CanAten ) * ( LG-DOCmp.ImpTot / LG-DOCmp.CanPedi  ).
    END.
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
            detalle.imptot = x-ImpTot.
    ELSE ASSIGN
            detalle.imptot = x-ImpTot * x-TpoCmbVta.
END.

OUTPUT TO c:\tmp\detalle-compras-conti-faber.txt.
FOR EACH detalle NO-LOCK:
    DISPLAY
        detalle.fchdoc
        detalle.nrodoc
        detalle.codpro
        detalle.nompro
        detalle.imptot    COLUMN-LABEL 'Saldo'
        WITH STREAM-IO NO-BOX WIDTH 320.
END.
OUTPUT CLOSE.

