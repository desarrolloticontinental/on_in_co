/* 15/06/2023 Carla Tenazoa */
DEF VAR x-linea AS CHAR.

INPUT FROM d:\clientes.prn.
REPEAT:
    IMPORT UNFORMATTED x-linea.
    IF x-linea BEGINS 'CODIGO' THEN NEXT.
    IF TRUE <> (x-linea > '') THEN LEAVE.
    FIND gn-clie WHERE codcia = 0 AND codcli = SUBSTRING(x-linea,1,20)
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE gn-clie THEN NEXT.
    FIND CURRENT gn-clie EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
    IF AVAILABLE gn-clie THEN DO:
        canal = SUBSTRING(x-linea,21,10).
        gircli = SUBSTRING(x-linea,31,10).
        clfcom = SUBSTRING(x-linea,41,10).
    END.
END.
INPUT CLOSE.
