DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR s-codcli AS CHAR INIT '10071569484' NO-UNDO.

DEF TEMP-TABLE detalle
    FIELD codcia AS INT
    FIELD coddoc AS CHAR
    FIELD nrodoc AS CHAR
    FIELD fchdoc AS DATE
    FIELD codmon AS INT
    FIELD tpocmb AS DEC
    FIELD codref AS CHAR
    FIELD nroref AS CHAR
    FIELD imptot AS DEC.

FOR EACH ccbdcaja NO-LOCK WHERE codcia = s-codcia
    AND fchdoc >= 01/01/2014
    AND LOOKUP(codref, 'FAC,BOL,TCK,LET,N/C,N/D,CHQ') > 0,
    FIRST ccbcdocu NO-LOCK WHERE ccbcdocu.codcia = s-codcia
    AND ccbcdocu.coddoc = ccbdcaja.codref
    AND ccbcdocu.nrodoc = ccbdcaja.nroref
    AND ccbcdocu.codcli = '10071569484':
    CREATE detalle.
    ASSIGN
        detalle.codcia = s-codcia
        detalle.coddoc = ccbdcaja.coddoc
        detalle.nrodoc = ccbdcaja.nrodoc
        detalle.fchdoc = ccbdcaja.fchdoc
        detalle.codmon = ccbdcaja.codmon
        detalle.tpocmb = ccbdcaja.tpocmb
        detalle.codref = ccbdcaja.codref
        detalle.nroref = ccbdcaja.nroref
        detalle.imptot = ccbdcaja.imptot.
END.
FOR EACH ccbdmov NO-LOCK WHERE ccbdmov.codcia = s-codcia
    AND ccbdmov.codcli = s-codcli
    AND ccbdmov.fchdoc >= 01/01/2014
    AND LOOKUP(ccbdmov.codref, "I/C,E/C,CJE") > 0:
    CREATE detalle.
    ASSIGN
        detalle.codcia = s-codcia
        detalle.coddoc = ccbdmov.codref
        detalle.nrodoc = ccbdmov.nroref
        detalle.fchdoc = ccbdmov.fchdoc
        detalle.codmon = ccbdmov.codmon
        detalle.tpocmb = ccbdmov.tpocmb
        detalle.codref = ccbdmov.coddoc
        detalle.nroref = ccbdmov.nrodoc
        detalle.imptot = ccbdmov.imptot.
END.
FOR EACH ccbdmov NO-LOCK WHERE ccbdmov.codcia = s-codcia
    AND ccbdmov.codcli = s-codcli
    AND ccbdmov.fchdoc >= 01/01/2014
    AND ccbdmov.coddoc = "A/C":
    CREATE detalle.
    ASSIGN
        detalle.codcia = s-codcia
        detalle.coddoc = ccbdmov.coddoc
        detalle.nrodoc = ccbdmov.nrodoc
        detalle.fchdoc = ccbdmov.fchdoc
        detalle.codmon = ccbdmov.codmon
        detalle.tpocmb = ccbdmov.tpocmb
        detalle.codref = ccbdmov.codref
        detalle.nroref = ccbdmov.nroref
        detalle.imptot = ccbdmov.imptot.
END.
OUTPUT TO c:\tmp\abonos.txt.
PUT UNFORMATTED 'DOC|NUMERO|FECHA|MON|TC|REF|NUMERO|IMPORTE'    
    SKIP.
FOR EACH detalle.
    PUT UNFORMATTED
        detalle.coddoc '|'
        detalle.nrodoc '|'
        detalle.fchdoc '|'
        detalle.codmon '|'
        detalle.tpocmb '|'
        detalle.codref '|'
        detalle.nroref '|'
        detalle.imptot 
        SKIP.
END.

