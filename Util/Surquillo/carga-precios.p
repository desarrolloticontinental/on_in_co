DEF VAR x-linea AS CHAR FORMAT 'x(100)' NO-UNDO.
DEF VAR x-codmat AS CHAR FORMAT 'x(6)' NO-UNDO.
DEF VAR x-preofi AS DEC DECIMALS 4 FORMAT '>>>,>>9.9999'.


INPUT FROM c:\tmp\precios.prn.
REPEAT :
    IMPORT UNFORMATTED x-linea.
    IF x-linea <> '' THEN DO:
        ASSIGN
            x-codmat = SUBSTRING(x-linea,1,6)
            x-preofi = DECIMAL(SUBSTRING(x-linea,7)).
        FIND vtalistamin WHERE codcia = 1
            AND coddiv = '00023'
            AND codmat = x-codmat 
            NO-ERROR.
        IF NOT AVAILABLE vtalistamin THEN DO:
            DISPLAY x-codmat.
            NEXT.
        END.
        IF vtalistamin.monvta = 2 THEN x-preofi = x-preofi / vtalistamin.tpocmb.
        VtaListaMin.PreOfi = x-preofi.
    END.
END.
