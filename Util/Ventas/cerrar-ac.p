DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR s-coddoc AS CHAR INIT 'A/C' NO-UNDO.
DEF VAR s-nrodoc AS CHAR INIT '015000266' NO-UNDO.

FIND ccbcdocu WHERE codcia = s-codcia
    AND coddoc = s-coddoc
    AND nrodoc = s-nrodoc.
DISPLAY 
    ccbcdocu.nomcli 
    ccbcdocu.imptot 
    ccbcdocu.sdoact COLUMN-LABEL 'SALDO'
    ccbcdocu.flgest 
    WITH 1 COL.
MESSAGE 'continuamos' VIEW-AS ALERT-BOX QUESTION
    BUTTONS YES-NO UPDATE rpta AS LOG.
IF rpta = NO THEN RETURN.

/* Actualizamos el saldo del A/C */
CREATE ccbdmov.
ASSIGN
    ccbdmov.codcia = s-codcia
    ccbdmov.fchdoc = TODAY
    ccbdmov.coddoc = ccbcdocu.coddoc
    ccbdmov.nrodoc = ccbcdocu.nrodoc
    ccbdmov.codref = ccbcdocu.coddoc
    ccbdmov.nroref = ccbcdocu.nrodoc
    ccbdmov.codmon = ccbcdocu.codmon
    ccbdmov.imptot = ccbcdocu.sdoact.
ASSIGN
    ccbcdocu.flgest = "C"
    ccbcdocu.fchcan = TODAY
    ccbcdocu.sdoact = ccbcdocu.sdoact - ccbdmov.imptot.
DISPLAY ccbcdocu.imptot ccbcdocu.sdoact ccbcdocu.flgest.
