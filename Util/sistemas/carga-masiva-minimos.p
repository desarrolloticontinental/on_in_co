DEF VAR x-linea AS CHAR NO-UNDO.

INPUT FROM d:\tmp\retail.prn.
REPEAT :
    IMPORT UNFORMATTED x-linea.
    IF x-linea = '' THEN LEAVE.
    FIND almmmatg WHERE codcia = 1 AND codmat = TRIM(SUBSTRING(x-linea,1,10))
        EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE almmmatg THEN NEXT.
    DISPLAY codmat. PAUSE 0.
    ASSIGN 
        DEC__03 = DEC(SUBSTRING(x-linea,11,10)) 
        pesobruto = DEC(SUBSTRING(x-linea,21,10)) 
        paquete = DEC(SUBSTRING(x-linea,21,10)).
END.
INPUT CLOSE.
