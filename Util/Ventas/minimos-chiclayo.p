DEF VAR x-linea AS CHAR.
DEF VAR x-codmat AS CHAR.
DEF VAR x-pesobruto AS DEC.
DEF VAR x-paquete AS DEC.


INPUT FROM c:\tmp\chiclayo.prn.
REPEAT :
    IMPORT UNFORMATTED x-linea.
    IF x-linea = '' THEN LEAVE.
    ASSIGN
        x-codmat = STRING(INTEGER(SUBSTRING(x-linea,1,10)), '999999')
        x-pesobruto = DEC(SUBSTRING(x-linea,11,10))
        x-paquete = DEC(SUBSTRING(x-linea,21)).
    FIND almmmatg WHERE codcia = 1
        AND codmat = x-codmat
        NO-ERROR.
    IF AVAILABLE almmmatg THEN DO:
        DISPLAY codmat pesobruto x-pesobruto.
        PAUSE 0.
        ASSIGN
            pesobruto = x-pesobruto 
            paquete = x-paquete.
    END.
END.
