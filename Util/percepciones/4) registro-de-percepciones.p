DEF VAR FechaD AS DATE NO-UNDO.
DEF VAR FechaH AS DATE NO-UNDO.

ASSIGN
    FechaD = 12/01/2014
    FechaH = 12/31/2014.

OUTPUT TO c:\tmp\percepciones.txt.
PUT UNFORMATTED
    'Division|Correlativo Per|Fecha I/C|Nro I/C|Tipo|Cliente|Nombre|Comprobante|Numero Comprb|Emision|'
    'Forma Pago|Precio Venta|% Percepcion|Imp Percepcion|Total Cobrado'
    SKIP.
FOR EACH ccbcmvto NO-LOCK WHERE codcia = 1
    AND coddoc = 'per'
    AND flgest <> 'a'
    AND fchdoc >= FechaD
    AND fchdoc <= FechaH,
    EACH ccbdmvto NO-LOCK WHERE ccbdmvto.codcia = 1
    AND ccbdmvto.coddoc = ccbcmvto.coddoc
    AND ccbdmvto.nrodoc = ccbcmvto.nrodoc
    AND ccbdmvto.coddiv = ccbcmvto.coddiv,
    FIRST ccbcdocu NO-LOCK WHERE ccbcdocu.codcia = 1
    AND ccbcdocu.coddoc = ccbdmvto.codref
    AND ccbcdocu.nrodoc = ccbdmvto.nroref:
    PUT UNFORMATTED
        ccbcmvto.coddiv '|'
        ccbcmvto.nrodoc '|'
        ccbcmvto.fchdoc '|'
        ccbcmvto.libre_chr[2] '|'
        ccbcmvto.libre_chr[3] '|'
        ccbcmvto.codcli '|'
        ccbcdocu.nomcli '|'
        ccbdmvto.codref '|'
        ccbdmvto.nroref '|'
        ccbcdocu.fchdoc '|'
        ccbcdocu.fmapgo '|'
        ccbdmvto.impdoc '|'
        ccbdmvto.impint '|'
        ccbdmvto.imptot '|'
        (ccbdmvto.imptot + ccbdmvto.impdoc)
        SKIP.
END.
OUTPUT CLOSE.

