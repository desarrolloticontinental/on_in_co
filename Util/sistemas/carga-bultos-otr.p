DEF VAR x-linea AS CHAR.

INPUT FROM d:\otr.prn.
REPEAT :
    IMPORT UNFORMATTED x-linea.
    IF x-linea = '' THEN LEAVE.
    FIND faccpedi WHERE codcia = 1
        AND coddoc = 'otr' AND
        nroped = SUBSTRING(x-linea,1,9)
        .
    CREATE ccbcbult.
    ASSIGN
        CcbCBult.CodCia = 1
        CcbCBult.CodDiv = '00000'
        CcbCBult.CodDoc = faccpedi.coddoc 
        CcbCBult.Bultos = 1
        CcbCBult.FchDoc = TODAY
        CcbCBult.NroDoc = faccpedi.nroped
        CcbCBult.usuario = 'SYSTEM'
        CcbCBult.CodCli = faccpedi.codcli.
    ASSIGN
        faccpedi.flgsit = "C".
END.
INPUT CLOSE.

