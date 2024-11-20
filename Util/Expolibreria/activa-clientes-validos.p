DEF VAR x-codcli AS CHAR FORMAT 'x(11)'.
DEF VAR x-linea AS CHAR FORMAT 'x(30)'.

FOR EACH gn-clie WHERE codcia = 0:
    flgsit = 'I'.
END.

INPUT FROM c:\tmp\clientes.prn.
REPEAT:
    IMPORT UNFORMATTED x-linea.
    ASSIGN
        x-codcli = SUBSTRING(x-linea,1,11).
    FIND gn-clie WHERE codcia = 0
        AND codcli = x-codcli
        NO-ERROR.
    IF AVAILABLE gn-clie THEN DO:
        gn-clie.flgsit = 'A'.
        gn-clie.cndvta = '001'.
    END.
END.
INPUT CLOSE.
