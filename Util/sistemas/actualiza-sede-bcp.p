
DEF NEW SHARED VAR cl-codcia AS INTE INIT 000.
DEF NEW SHARED VAR s-codcia AS INTE INIT 001.


DEF VAR x-nroped AS CHAR.

INPUT FROM d:\bcp.prn.
REPEAT:
    IMPORT UNFORMATTED x-nroped.
    IF TRUE <> (x-nroped > '') THEN LEAVE.
    FIND faccpedi WHERE codcia = s-codcia AND
        coddoc = "O/D" AND 
        nroped = x-nroped
        EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE faccpedi THEN DO:
        DISPLAY faccpedi.nroped.
        PAUSE 0.
        ASSIGN
            faccpedi.sede = "1224"
            faccpedi.ubigeo[1] = "1224".
        FIND gn-clied WHERE gn-clied.codcia = cl-codcia
            AND gn-clied.codcli = Faccpedi.codcli
            AND gn-clied.sede = Faccpedi.sede NO-LOCK NO-ERROR.
        IF AVAILABLE gn-clied AND Gn-ClieD.Codpos > '' THEN FacCPedi.CodPos = Gn-ClieD.Codpos.
        RELEASE faccpedi.
    END.
END.
INPUT CLOSE.

