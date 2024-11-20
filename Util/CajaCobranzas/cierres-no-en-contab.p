DEF VAR s-codcia AS INT INIT 001 NO-UNDO.

OUTPUT TO c:\tmp\faltan.txt.
PUT UNFORMATTED "DIVISION|CIERRE" SKIP.
FOR EACH gn-divi NO-LOCK WHERE gn-divi.codcia = s-codcia,
    EACH ccbccaja NO-LOCK WHERE ccbccaja.codcia = s-codcia
        AND ccbccaja.coddiv = gn-divi.coddiv
        AND LOOKUP(ccbccaja.coddoc, 'E/C,I/C') > 0
        AND ccbccaja.fchcie >= 01/01/2013
        AND ccbccaja.fchcie <= 12/31/2013
        AND ccbccaja.flgcie = 'C'
        BREAK BY ccbccaja.coddiv BY ccbccaja.fchcie:
        IF first-of(ccbccaja.coddiv) OR FIRST-OF(ccbccaja.fchcie) THEN DO:
            FIND FIRST cb-control WHERE cb-control.codcia = s-codcia
                AND cb-control.tipo = "@CJ"
                AND cb-control.coddiv = ccbccaja.coddiv
                AND cb-control.fchpro = ccbccaja.fchcie
                AND cb-control.tipmov = CcbCCaja.Usuario + CcbCCaja.HorCie
                NO-LOCK NO-ERROR.
            IF NOT AVAILABLE cb-control THEN
                PUT UNFORMATTED ccbccaja.coddiv '|' ccbccaja.fchcie SKIP.
        END.
END.
OUTPUT CLOSE.

