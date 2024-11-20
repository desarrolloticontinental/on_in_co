DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR s-coddoc AS CHAR INIT 'A/C' NO-UNDO.
DEF VAR s-nrodoc AS CHAR INIT '015000265' NO-UNDO.
DEF VAR s-codref AS CHAR INIT 'FAC' NO-UNDO.
DEF VAR s-nroref AS CHAR INIT '011005100' NO-UNDO.

DEF BUFFER b-cdocu FOR ccbcdocu.

FIND ccbcdocu WHERE codcia = s-codcia
    AND coddoc = s-coddoc
    AND nrodoc = s-nrodoc.
DISPLAY ccbcdocu.imptot ccbcdocu.sdoact ccbcdocu.flgest.
/* Actualizamos el saldo de la FAC afectada */
FOR EACH ccbdcaja WHERE ccbdcaja.codcia = s-codcia
    AND ccbdcaja.coddoc = ccbcdocu.coddoc
    AND ccbdcaja.nrodoc = ccbcdocu.nrodoc
    AND ccbdcaja.codref = s-codref
    AND ccbdcaja.nroref = s-nroref:
    FIND b-cdocu WHERE b-cdocu.codcia = s-codcia
        AND b-cdocu.coddoc = ccbdcaja.codref
        AND b-cdocu.nrodoc = ccbdcaja.nroref.
    ASSIGN
        b-cdocu.flgest = "P"
        b-cdocu.sdoact = b-cdocu.sdoact + ccbdcaja.imptot.
    DELETE ccbdcaja.
END.
/* Actualizamos el saldo del A/C */
FOR EACH ccbdmov WHERE ccbdmov.codcia = s-codcia
    AND ccbdmov.coddoc = ccbcdocu.coddoc
    AND ccbdmov.nrodoc = ccbcdocu.nrodoc
    AND ccbdmov.codref = s-codref
    AND ccbdmov.nroref = s-nroref:
    ASSIGN
        ccbcdocu.flgest = "P"
        ccbcdocu.sdoact = ccbcdocu.sdoact + ccbdmov.imptot.
    DELETE ccbdmov.
END.
