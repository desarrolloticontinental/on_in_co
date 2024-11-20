DEF VAR x-linea AS CHAR.

DEF VAR x-codalm AS CHAR.
DEF VAR x-codmat AS CHAR.
DEF VAR x-codubi AS CHAR.

INPUT FROM d:\actualizar.prn.
REPEAT :
    IMPORT UNFORMATTED x-linea.
    IF x-linea = '' THEN LEAVE.
    x-codalm = SUBSTRING(x-linea,1,10).
    x-codmat = SUBSTRING(x-linea,11,10).
    x-codubi = SUBSTRING(x-linea,21,10).
        
    FIND almmmate WHERE codcia = 1
        AND codalm = x-codalm
        AND codmat = x-codmat.
    DISPLAY codalm codmat Almmmate.CodUbi x-codubi. PAUSE 0.
    Almmmate.CodUbi = x-codubi.
        
END.
