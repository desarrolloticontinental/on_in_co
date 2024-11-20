DEF TEMP-TABLE detalle 
    FIELD periodo AS INT
    FIELD nromes AS INT
    FIELD codcli LIKE ccbcdocu.codcli
    FIELD nomcli LIKE ccbcdocu.nomcli
    FIELD codmon AS INT
    FIELD impbrt LIKE ccbcdocu.impbrt EXTENT 2
    FIELD impvta LIKE ccbcdocu.impvta EXTENT 2
    FIELD impigv LIKE ccbcdocu.impigv EXTENT 2
    FIELD imptot LIKE ccbcdocu.imptot EXTENT 2
    INDEX llave01 AS PRIMARY periodo nromes codcli.

DEF VAR x-codcli LIKE ccbcdocu.codcli.
DEF VAR x-nomcli LIKE ccbcdocu.nomcli.
DEF VAR x-factor AS INT.
DEF VAR x-TpoCmbCmp AS DEC.
DEF VAR x-TpoCmbVta AS DEC.

FOR EACH ccbcdocu NO-LOCK WHERE codcia = 1
    AND flgest <> 'A'
    AND LOOKUP(coddoc, 'FAC,BOL,TCK,N/D,N/C') > 0
    AND fchdoc >= 01/01/2008
    AND fchdoc <= 01/31/2010:
    FIND LAST Gn-Tcmb WHERE Gn-Tcmb.Fecha <= CcbCdocu.FchDoc
        USE-INDEX Cmb01 NO-LOCK NO-ERROR.
    IF NOT AVAIL Gn-Tcmb THEN 
        FIND FIRST Gn-Tcmb WHERE Gn-Tcmb.Fecha >= CcbCdocu.FchDoc
            USE-INDEX Cmb01 NO-LOCK NO-ERROR.
    IF AVAIL Gn-Tcmb THEN 
        ASSIGN
            x-TpoCmbCmp = Gn-Tcmb.Compra
            x-TpoCmbVta = Gn-Tcmb.Venta.

    IF ccbcdocu.codcli = '20511358907' 
    THEN ASSIGN
            x-codcli = ccbcdocu.codcli
            x-nomcli = ccbcdocu.nomcli.
    ELSE ASSIGN
            x-codcli = '11111111111'
            x-nomcli = 'VARIOS'.
    x-factor = 1.
    IF ccbcdocu.coddoc = 'N/C' THEN x-factor = -1.
    FIND detalle WHERE detalle.periodo = YEAR(ccbcdocu.fchdoc)
        AND detalle.nromes = MONTH(ccbcdocu.fchdoc)
        AND detalle.codcli = x-codcli
        EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE detalle THEN DO:
        CREATE detalle.
        ASSIGN
            detalle.periodo = YEAR(ccbcdocu.fchdoc)
            detalle.nromes = MONTH(ccbcdocu.fchdoc)
            detalle.codcli = x-codcli
            detalle.nomcli = x-nomcli.
    END.
    IF ccbcdocu.codmon = 1 
    THEN ASSIGN
            detalle.impbrt[1] = detalle.impbrt[1] + ccbcdocu.impbrt * x-factor
            detalle.impvta[1] = detalle.impvta[1] + ccbcdocu.impvta * x-factor
            detalle.impigv[1] = detalle.impigv[1] + ccbcdocu.impigv * x-factor
            detalle.imptot[1] = detalle.imptot[1] + ccbcdocu.imptot * x-factor
            detalle.impbrt[2] = detalle.impbrt[2] + ccbcdocu.impbrt * x-factor / x-TpoCmbCmp
            detalle.impvta[2] = detalle.impvta[2] + ccbcdocu.impvta * x-factor / x-TpoCmbCmp
            detalle.impigv[2] = detalle.impigv[2] + ccbcdocu.impigv * x-factor / x-TpoCmbCmp
            detalle.imptot[2] = detalle.imptot[2] + ccbcdocu.imptot * x-factor / x-TpoCmbCmp.
    ELSE ASSIGN
            detalle.impbrt[1] = detalle.impbrt[1] + ccbcdocu.impbrt * x-factor * x-TpoCmbVta
            detalle.impvta[1] = detalle.impvta[1] + ccbcdocu.impvta * x-factor * x-TpoCmbVta
            detalle.impigv[1] = detalle.impigv[1] + ccbcdocu.impigv * x-factor * x-TpoCmbVta
            detalle.imptot[1] = detalle.imptot[1] + ccbcdocu.imptot * x-factor * x-TpoCmbVta
            detalle.impbrt[2] = detalle.impbrt[2] + ccbcdocu.impbrt * x-factor
            detalle.impvta[2] = detalle.impvta[2] + ccbcdocu.impvta * x-factor
            detalle.impigv[2] = detalle.impigv[2] + ccbcdocu.impigv * x-factor
            detalle.imptot[2] = detalle.imptot[2] + ccbcdocu.imptot * x-factor.
END.

OUTPUT TO c:\tmp\ventas-conti.txt.
FOR EACH detalle NO-LOCK:
    DISPLAY
        detalle.periodo
        detalle.nromes
        detalle.codcli
        detalle.nomcli
        detalle.impbrt[1]
        detalle.impvta[1]
        detalle.impigv[1]
        detalle.imptot[1]
        WITH STREAM-IO NO-BOX WIDTH 320.
END.
OUTPUT CLOSE.

