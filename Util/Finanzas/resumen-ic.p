DEF temp-table Detalle LIKE ccbcierr
    FIELD coddiv AS CHAR FORMAT 'x(5)'
    INDEX llave01 AS PRIMARY codcia coddiv usuario fchcie horcie.

DEF VAR k AS INT.

FOR EACH ccbccaja NO-LOCK USE-INDEX llave08 WHERE codcia = 1
    AND flgcie = 'C'
    AND fchcie >= 01/01/2010
    AND fchcie <= 11/30/2011:
    IF ccbccaja.flgest = 'a' THEN NEXT.
    IF ccbccaja.coddoc <> 'i/c' THEN NEXT.
    FIND detalle WHERE detalle.codcia = ccbccaja.codcia
        AND detalle.coddiv = ccbccaja.coddiv
        AND detalle.usuario = ccbccaja.usuario
        AND detalle.fchcie = ccbccaja.fchcie
        AND detalle.horcie = ccbccaja.horcie
        NO-ERROR.
    IF NOT AVAILABLE detalle THEN CREATE detalle.
    ASSIGN
        detalle.codcia = ccbccaja.codcia
        detalle.coddiv = ccbccaja.coddiv
        detalle.usuario = ccbccaja.usuario
        detalle.fchcie = ccbccaja.fchcie
        detalle.horcie = ccbccaja.horcie.
    DO k = 1 TO 10:
        detalle.impnac[k] = detalle.impnac[k] + ccbccaja.impnac[k].
        detalle.impusa[k] = detalle.impusa[k] + ccbccaja.impusa[k].
    END.
    detalle.impnac[1] = detalle.impnac[1] - ccbccaja.vuenac.
    detalle.impusa[1] = detalle.impusa[1] - ccbccaja.vueusa.
END.
OUTPUT TO c:\tmp\resumen-ic.txt.
FOR EACH detalle:
    DISPLAY
        detalle.coddiv
        detalle.fchcie
        detalle.horcie
        detalle.usuario
        detalle.impnac[1] LABEL 'Efe S/.'
        detalle.impusa[1] LABEL 'Efe US$'
        detalle.impnac[2] LABEL 'Chq S/.'
        detalle.impusa[2] LABEL 'Chq US$'
        detalle.impnac[3] LABEL 'Chq Dif S/.'
        detalle.impusa[3] LABEL 'Chq Dif US$'
        detalle.impnac[4] LABEL 'Tarj S/.'
        detalle.impusa[4] LABEL 'Tarj US$'
        detalle.impnac[5] LABEL 'Bol Dep S/.'
        detalle.impusa[5] LABEL 'Bol Dep US$'
        detalle.impnac[6] LABEL 'N/C S/.'
        detalle.impusa[6] LABEL 'N/C US$'
        detalle.impnac[7] LABEL 'Anticp S/.'
        detalle.impusa[7] LABEL 'Anticip US$'
        detalle.impnac[8] LABEL 'Comis S/.'
        detalle.impusa[8] LABEL 'Comis US$'
        detalle.impnac[9] LABEL 'Retenc S/.'
        detalle.impusa[9] LABEL 'Retenc US$'
        detalle.impnac[10] LABEL 'Vales S/.'
        detalle.impusa[10] LABEL 'Vales US$'
        WITH STREAM-IO NO-BOX WIDTH 320 NO-UNDERLINE.

END.
OUTPUT CLOSE.

