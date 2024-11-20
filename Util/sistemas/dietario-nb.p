OUTPUT TO d:\dietario.txt.
SELECT 
    ccbcmvto.nrodoc     COLUMN-LABEL 'Numero',
    ccbcmvto.fchdoc     COLUMN-LABEL 'Fecha de Registro',
    ccbcmvto.codcta + ' '  + cb-ctas.nomcta COLUMN-LABEL 'Banco' FORMAT 'x(60)',
    ccbcmvto.fchcbd     COLUMN-LABEL 'Fecha de N/B',
    SUM(ccbdmvto.imptot) FORMAT '->>>,>>>,>>9.99' COLUMN-LABEL 'Total'
    FROM ccbcmvto, ccbdmvto, cb-ctas
    WHERE ccbcmvto.codcia = 1
    AND ccbcmvto.coddoc = 'N/B'
    AND ccbcmvto.flgest <> 'A'
    AND ccbcmvto.fchdoc >= 01/01/2018
    AND ccbdmvto.codcia = ccbcmvto.codcia 
    AND ccbdmvto.coddoc = ccbcmvto.coddoc
    AND ccbdmvto.nrodoc = ccbcmvto.nrodoc
    AND cb-ctas.codcia = 0
    AND cb-ctas.codcta = ccbcmvto.codcta
    GROUP BY ccbdmvto.codcia, ccbdmvto.coddoc, ccbdmvto.nrodoc
    WITH STREAM-IO NO-BOX WIDTH 320
    .
PAUSE 0.
OUTPUT CLOSE.
