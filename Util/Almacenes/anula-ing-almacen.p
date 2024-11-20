DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR s-codalm AS CHAR INIT '63' NO-UNDO.
DEF VAR s-tipmov AS CHAR INIT 'I' NO-UNDO.
DEF VAR s-codmov AS INT INIT 09 NO-UNDO.
DEF VAR s-nroser AS INT INIT 288 NO-UNDO.
DEF VAR s-nrodoc AS INT INIT 700000023 NO-UNDO.

FIND almcmov WHERE almcmov.codcia = s-codcia
    AND almcmov.codalm = s-codalm
    AND almcmov.tipmov = s-tipmov
    AND almcmov.codmov = s-codmov
    AND almcmov.nroser = s-nroser
    AND almcmov.nrodoc = s-nrodoc.
DISPLAY codalm nroser nrodoc fchdoc flgest.
FOR EACH almdmov OF almcmov:
    DISPLAY almdmov.codmat almdmov.candes.
END.

