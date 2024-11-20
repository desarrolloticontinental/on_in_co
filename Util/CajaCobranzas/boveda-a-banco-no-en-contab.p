DEF VAR s-codcia AS INT INIT 001 NO-UNDO.

OUTPUT TO c:\tmp\faltanbov.txt.
PUT UNFORMATTED "DIVISION|EMISION" SKIP.
FOR EACH gn-divi NO-LOCK WHERE gn-divi.codcia = s-codcia,
    EACH CcbDMvto WHERE CcbDMvto.CodCia = s-codcia 
    AND CcbDMvto.FlgEst = 'C' 
    AND CcbDMvto.TpoRef = 'BOV'
    AND CcbDMvto.FchEmi >= 01/01/2013
    AND CcbDMvto.FchEmi <= 12/31/2013
    AND CcbDMvto.CodDiv = gn-divi.coddiv
    AND CcbDMvto.Coddoc = "E/C"
    BREAK BY ccbdmvto.coddiv BY ccbdmvto.fchemi:
        IF first-of(ccbdmvto.coddiv) OR FIRST-OF(ccbdmvto.fchemi) THEN DO:
            FIND FIRST cb-control WHERE cb-control.codcia = s-codcia
                AND cb-control.tipo = "@BOV"
                AND cb-control.coddiv = ccbdmvto.coddiv
                AND cb-control.fchpro = ccbdmvto.fchemi
                NO-LOCK NO-ERROR.
            IF NOT AVAILABLE cb-control THEN
                PUT UNFORMATTED ccbdmvto.coddiv '|' ccbdmvto.fchemi SKIP.
        END.
END.
OUTPUT CLOSE.

