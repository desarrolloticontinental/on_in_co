/* 1ro todos los documentos del año 2012 */
DEF VAR x-estado AS CHAR NO-UNDO.
OUTPUT TO c:\tmp\comprobantes.txt.
PUT UNFORMATTED
    'DESTINO|ORIGEN|CODIGO|NUMERO|EMISION|VENCIMIENTO|CLIENTE|NOMBRE|MONEDA|IMPORTE TOTAL|SALDO|ESTADO'
    SKIP.
FOR EACH ccbcdocu NO-LOCK WHERE codcia = 1
    AND LOOKUP(coddoc, 'FAC,BOL,TCK,N/C,N/D') > 0
    AND fchdoc >= 01/01/2012
    AND fchdoc <= 12/31/2012:
    CASE flgest:
        WHEN "P" THEN x-estado = "PENDIENTE".
        WHEN "C" THEN x-estado = "CANCELADO".
        WHEN "A" THEN x-estado = "ANULADO".
        OTHERWISE x-estado = flgest.
    END CASE.
    PUT UNFORMATTED
        coddiv '|'
        divori '|'
        coddoc '|'
        nrodoc '|'
        fchdoc '|'
        fchvto '|'
        codcli '|'
        nomcli '|'
        codmon '|'
        imptot '|'
        sdoact '|'
        x-estado
        SKIP.
END.
/* 2do todos los documentos pendientes antes del año 2012 */
FOR EACH ccbcdocu NO-LOCK WHERE codcia = 1
    AND LOOKUP(coddoc, 'FAC,BOL,TCK,N/C,N/D') > 0
    AND fchdoc < 01/01/2012
    AND flgest = "P":
    CASE flgest:
        WHEN "P" THEN x-estado = "PENDIENTE".
        WHEN "C" THEN x-estado = "CANCELADO".
        WHEN "A" THEN x-estado = "ANULADO".
        OTHERWISE x-estado = flgest.
    END CASE.
    PUT UNFORMATTED
        coddiv '|'
        divori '|'
        coddoc '|'
        nrodoc '|'
        fchdoc '|'
        fchvto '|'
        codcli '|'
        nomcli '|'
        codmon '|'
        imptot '|'
        sdoact '|'
        x-estado
        SKIP.
END.
OUTPUT CLOSE.

