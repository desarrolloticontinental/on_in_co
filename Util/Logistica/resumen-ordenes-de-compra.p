DEF TEMP-TABLE detalle 
    FIELD periodo AS INT
    FIELD nromes AS INT
    FIELD codpro LIKE lg-cocmp.codpro
    FIELD nompro LIKE lg-cocmp.nompro
    FIELD codmon AS INT
    FIELD impbrt LIKE lg-cocmp.impbrt EXTENT 2
    FIELD impnet LIKE lg-cocmp.impnet EXTENT 2
    FIELD impigv LIKE lg-cocmp.impigv EXTENT 2
    FIELD imptot LIKE lg-cocmp.imptot EXTENT 2
    INDEX llave01 AS PRIMARY periodo nromes codpro.

DEF VAR x-codpro LIKE lg-cocmp.codpro.
DEF VAR x-nompro LIKE lg-cocmp.nompro.
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
    IF lg-cocmp.codpro = '51135890' 
    THEN ASSIGN
            x-codpro = lg-cocmp.codpro
            x-nompro = lg-cocmp.nompro.
    ELSE ASSIGN
            x-codpro = '11111111111'
            x-nompro = 'VARIOS'.
    FIND detalle WHERE detalle.periodo = YEAR(lg-cocmp.fchdoc)
        AND detalle.nromes = MONTH(lg-cocmp.fchdoc)
        AND detalle.codpro = x-codpro
        EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE detalle THEN DO:
        CREATE detalle.
        ASSIGN
            detalle.periodo = YEAR(lg-cocmp.fchdoc)
            detalle.nromes = MONTH(lg-cocmp.fchdoc)
            detalle.codpro = x-codpro
            detalle.nompro = x-nompro.
    END.
    IF lg-cocmp.codmon = 1 
    THEN ASSIGN
            detalle.impbrt[1] = detalle.impbrt[1] + lg-cocmp.impbrt
            detalle.impnet[1] = detalle.impnet[1] + lg-cocmp.impnet
            detalle.impigv[1] = detalle.impigv[1] + lg-cocmp.impigv
            detalle.imptot[1] = detalle.imptot[1] + lg-cocmp.imptot
            detalle.impbrt[2] = detalle.impbrt[2] + lg-cocmp.impbrt / x-TpoCmbCmp
            detalle.impnet[2] = detalle.impnet[2] + lg-cocmp.impnet / x-TpoCmbCmp
            detalle.impigv[2] = detalle.impigv[2] + lg-cocmp.impigv / x-TpoCmbCmp
            detalle.imptot[2] = detalle.imptot[2] + lg-cocmp.imptot / x-TpoCmbCmp.
    ELSE ASSIGN
            detalle.impbrt[1] = detalle.impbrt[1] + lg-cocmp.impbrt * x-TpoCmbVta
            detalle.impnet[1] = detalle.impnet[1] + lg-cocmp.impnet * x-TpoCmbVta
            detalle.impigv[1] = detalle.impigv[1] + lg-cocmp.impigv * x-TpoCmbVta
            detalle.imptot[1] = detalle.imptot[1] + lg-cocmp.imptot * x-TpoCmbVta
            detalle.impbrt[2] = detalle.impbrt[2] + lg-cocmp.impbrt
            detalle.impnet[2] = detalle.impnet[2] + lg-cocmp.impnet
            detalle.impigv[2] = detalle.impigv[2] + lg-cocmp.impigv
            detalle.imptot[2] = detalle.imptot[2] + lg-cocmp.imptot.
END.

OUTPUT TO c:\tmp\resumen-compras-conti.txt.
FOR EACH detalle NO-LOCK:
    DISPLAY
        detalle.periodo
        detalle.nromes
        detalle.codpro
        detalle.nompro
        detalle.impbrt[1]
        detalle.impnet[1]
        detalle.impigv[1]
        detalle.imptot[1]
        WITH STREAM-IO NO-BOX WIDTH 320.
END.
OUTPUT CLOSE.

