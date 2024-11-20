DEF VAR x-linea AS CHAR FORMAT 'x(50)'.
DEF VAR x-codmat AS CHAR FORMAT 'x(6)'.
DEF VAR x-stkact AS DEC.

FOR EACH almmmatg WHERE codcia = 1:
    tpoart = 'D'.
END.

INPUT FROM c:\tmp\productos.prn.
REPEAT:
    IMPORT UNFORMATTED x-linea.
    ASSIGN
        x-codmat = SUBSTRING(x-linea,1,6)
        x-stkact = DECIMAL(SUBSTRING(x-linea,7,10)).
    FIND almmmatg WHERE codcia = 1
        AND codmat = x-codmat
        NO-ERROR.
    IF AVAILABLE almmmatg THEN DO:
        almmmatg.tpoart = 'A'.
        almmmatg.almacenes = '40'.
        FIND almmmate WHERE almmmate.codcia = 1
            AND almmmate.codmat = x-codmat
            AND almmmate.codalm = '40'
            NO-ERROR.
        IF NOT AVAILABLE almmmate THEN CREATE almmmate.
        ASSIGN
            almmmate.codcia = 1
            almmmate.codalm = '40'
            almmmate.codmat = x-codmat
            almmmate.stkact = x-stkact.
    END.
    ELSE DISPLAY x-codmat.
END.
INPUT CLOSE.
