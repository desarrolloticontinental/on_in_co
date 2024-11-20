/* Cierre de N/C */
DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR s-coddoc AS CHAR INIT 'N/C' NO-UNDO.
DEF VAR s-nrodoc AS CHAR INIT '25200010112'.

FIND ccbcdocu WHERE codcia = s-codcia 
    AND coddoc = s-coddoc
    AND nrodoc = s-nrodoc
    AND flgest = "P"
    EXCLUSIVE-LOCK.
CREATE ccbdmov.
ASSIGN
    CCBDMOV.CodCia = s-codcia
    CCBDMOV.CodCli = ccbcdocu.codcli
    CCBDMOV.CodDiv = ccbcdocu.coddiv
    CCBDMOV.CodDoc = ccbcdocu.coddoc
    CCBDMOV.CodMon = ccbcdocu.codmon
    CCBDMOV.CodRef = ccbcdocu.coddoc
    CCBDMOV.FchDoc = TODAY 
    CCBDMOV.FchMov = TODAY
    CCBDMOV.HraMov = STRING(TIME,'HH:MM:SS')
    CCBDMOV.ImpTot = ccbcdocu.sdoact
    CCBDMOV.NroDoc = ccbcdocu.nrodoc
    CCBDMOV.NroRef = ccbcdocu.nrodoc
    CCBDMOV.TpoCmb = ccbcdocu.tpocmb
    CCBDMOV.usuario = 'ADMIN'.
ASSIGN
    ccbcdocu.sdoact = 0
    ccbcdocu.flgest = 'C'
    ccbcdocu.fchcan = TODAY.
