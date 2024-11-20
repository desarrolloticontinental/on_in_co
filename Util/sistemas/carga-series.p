DEF VAR x-linea AS CHAR NO-UNDO.
DEF VAR x-codmat AS CHAR NO-UNDO.
DEF VAR x-serie AS CHAR NO-UNDO.
DEF VAR x-codalm AS CHAR INIT '506' NO-UNDO.


INPUT FROM d:\articulos.prn.
REPEAT :
    IMPORT UNFORMATTED x-linea.
    IF x-linea = '' THEN LEAVE.
    ASSIGN
        x-codmat = SUBSTRING(x-linea,1,6)
        x-serie = SUBSTRING(x-linea,11).
    FIND FIRST fifommatg WHERE fifommatg.codcia = 1
        AND fifommatg.codmat = x-codmat
        AND fifommatg.serialnumber = x-serie
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE fifommatg THEN DO:
        CREATE fifommatg.
        ASSIGN
            fifommatg.CodCia = 001
            fifommatg.CodMat = x-codmat
            fifommatg.SerialNumber = x-serie.
    END.
END.
INPUT CLOSE.


INPUT FROM d:\articulos.prn.
REPEAT :
    IMPORT UNFORMATTED x-linea.
    IF x-linea = '' THEN LEAVE.
    ASSIGN
        x-codmat = SUBSTRING(x-linea,1,6)
        x-serie = SUBSTRING(x-linea,11).
    FIND FIRST fifommate WHERE fifommate.codcia = 1
        AND fifommate.codmat = x-codmat
        AND fifommate.serialnumber = x-serie
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE fifommate THEN DO:
        CREATE fifommate.
        ASSIGN
            fifommate.CodCia = 001
            fifommate.codalm = x-codalm
            fifommate.CodMat = x-codmat
            fifommate.SerialNumber = x-serie
            fifommate.stkact = 1.
    END.
END.
INPUT CLOSE.

