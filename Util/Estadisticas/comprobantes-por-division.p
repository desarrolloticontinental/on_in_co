DEF VAR x-TpoCmbCmp AS DECI INIT 1.
DEF VAR x-TpoCmbVta AS DECI INIT 1.
DEF VAR x-factor AS INT.

DEF TEMP-TABLE detalle
    FIELD coddoc LIKE ccbcdocu.coddoc
    FIELD coddiv LIKE ccbcdocu.coddiv
    FIELD divori LIKE ccbcdocu.divori
    FIELD periodo AS INT
    FIELD nromes AS INT
    FIELD imptot LIKE ccbcdocu.imptot.

FOR EACH gn-divi NO-LOCK WHERE codcia = 001:
    FOR EACH ccbcdocu NO-LOCK WHERE ccbcdocu.codcia = 001
        AND ccbcdocu.coddiv = gn-divi.coddiv
        AND LOOKUP(ccbcdocu.coddoc, 'fac,bol,tck,n/c') > 0
        AND ccbcdocu.fchdoc >= 01/01/2012
        AND ccbcdocu.fchdoc <= 03/31/2012
        AND flgest <> 'A':
        DISPLAY ccbcdocu.coddiv ccbcdocu.coddoc ccbcdocu.fchdoc.
        PAUSE 0.
        IF ccbcdocu.coddoc = "N/C" THEN x-Factor = -1. ELSE x-Factor = 1.
        FIND LAST Gn-Tcmb WHERE Gn-Tcmb.Fecha <= CcbCdocu.FchDoc NO-LOCK NO-ERROR.
        IF NOT AVAIL Gn-Tcmb THEN FIND FIRST Gn-Tcmb WHERE Gn-Tcmb.Fecha >= CcbCdocu.FchDoc NO-LOCK NO-ERROR.
        IF AVAIL Gn-Tcmb THEN 
            ASSIGN
                x-TpoCmbCmp = Gn-Tcmb.Compra
                x-TpoCmbVta = Gn-Tcmb.Venta.
        FIND detalle WHERE detalle.coddoc = ccbcdocu.coddoc
            AND detalle.coddiv = ccbcdocu.coddiv
            AND detalle.divori = ccbcdocu.divori
            AND detalle.periodo = YEAR(ccbcdocu.fchdoc)
            AND detalle.nromes = MONTH(ccbcdocu.fchdoc)
            NO-ERROR.
        IF NOT AVAILABLE detalle THEN CREATE detalle.
        ASSIGN
            detalle.coddoc = ccbcdocu.coddoc
            detalle.coddiv = ccbcdocu.coddiv
            detalle.divori = ccbcdocu.divori
            detalle.periodo = YEAR(ccbcdocu.fchdoc)
            detalle.nromes = MONTH(ccbcdocu.fchdoc)
            detalle.imptot = detalle.imptot + x-Factor *
            (IF ccbcdocu.codmon = 1 THEN ccbcdocu.imptot ELSE ccbcdocu.imptot * x-TpoCmbVta).
    END.
END.

OUTPUT TO c:\tmp\ventas.txt.
PUT UNFORMATTED
    'DIVISION|ORIGEN|PERIODO|MES|IMPORTE'
    SKIP.
FOR EACH detalle:
    PUT UNFORMATTED
        detalle.coddiv '|'
        detalle.divori '|'
        detalle.periodo '|'
        detalle.nromes '|'
        detalle.imptot
        SKIP.
END.
OUTPUT CLOSE.

