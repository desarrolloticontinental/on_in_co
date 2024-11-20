DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR s-coddiv AS CHAR INIT '00000' NO-UNDO.
DEF VAR s-coddoc AS CHAR INIT 'B/D' NO-UNDO.

FIND ccbcmvto WHERE ccbcmvto.codcia = s-codcia
    AND ccbcmvto.coddiv = s-coddiv
    AND ccbcmvto.coddoc = s-coddoc
    AND ccbcmvto.nrodoc = '000000462'.
FOR EACH ccbdmvto NO-LOCK WHERE CcbDMvto.CodCia = ccbcmvto.codcia
    AND CcbDMvto.CodDiv = ccbcmvto.coddiv
    AND CcbDMvto.CodDoc = ccbcmvto.coddoc
    AND CcbDMvto.NroDoc = ccbcmvto.nrodoc,
    FIRST ccbcdocu WHERE CcbCDocu.CodCia = CcbDMvto.CodCia
    AND CcbCDocu.CodDoc = CcbDMvto.CodRef
    AND CcbCDocu.NroDoc = CcbDMvto.NroRef:
    DISPLAY ccbcdocu.coddoc ccbcdocu.nrodoc ccbcdocu.flgsit ccbcdocu.flgubi
        ccbcdocu.flgsita ccbcdocu.flgubia
        WITH STREAM-IO NO-BOX.
    PAUSE 0.
    ASSIGN
        ccbcdocu.flgubi = ccbcdocu.flgubia
        ccbcdocu.flgsit = ccbcdocu.flgsita.
END.
CcbCMvto.FlgEst = 'A'.


