DEF VAR x-linea AS CHAR NO-UNDO.

DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR x-codmat AS CHAR NO-UNDO.
DEF VAR x-codfam AS CHAR NO-UNDO.
DEF VAR x-subfam AS CHAR NO-UNDO.
DEF VAR x-desmat AS CHAR FORMAT 'x(60)' NO-UNDO.

DEFINE VARIABLE dFactor AS DECIMAL NO-UNDO.

INPUT FROM c:\tmp\codigos.prn.
OUTPUT TO c:\tmp\diferencias.txt.
REPEAT :
    IMPORT UNFORMATTED x-linea.
    IF x-linea = '' THEN LEAVE.
    ASSIGN
        x-codfam = SUBSTRING(x-linea,1,3)
        x-subfam = SUBSTRING(x-linea,31,3)
        x-codmat = SUBSTRING(x-linea,61,6)
        x-desmat = SUBSTRING(x-linea,71).
    FIND almmmatg WHERE codcia = s-codcia AND codmat = x-codmat NO-ERROR.
    IF ERROR-STATUS:ERROR THEN NEXT.
    RUN _fuzzycmp (desmat,
                   x-desmat,
                   2,
                   OUTPUT dFactor).
    PUT UNFORMATTED
        codmat '|'
        desmat '|'
        x-desmat '|'
        dfactor '|'
        x-codfam '|'
        x-subfam
        SKIP.

END.
INPUT CLOSE.
OUTPUT CLOSE.


