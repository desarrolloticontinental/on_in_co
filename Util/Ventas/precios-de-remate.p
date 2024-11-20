DEF VAR x-linea AS CHAR NO-UNDO.
DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR s-tabla AS CHAR INIT 'REMATES' NO-UNDO.
DEF VAR s-codalm AS CHAR INIT '24' NO-UNDO.
DEF VAR s-user-id AS CHAR INIT 'RSALAS' NO-UNDO.
DEF VAR x-preuni AS DEC DECIMALS 4 NO-UNDO.

INPUT FROM c:\tmp\remates.prn.
REPEAT:
    IMPORT UNFORMATTED x-linea.
    IF x-linea = '' THEN LEAVE.
    FIND almmmatg WHERE almmmatg.codcia = s-codcia
        AND almmmatg.codmat = SUBSTRING(x-linea,1,6)
        NO-ERROR.
    IF NOT AVAILABLE almmmatg THEN NEXT.
    FIND almmmate WHERE almmmate.codcia = s-codcia
        AND almmmate.codalm = s-codalm
        AND almmmate.codmat = SUBSTRING(x-linea,1,6)
        NO-ERROR.
    IF NOT AVAILABLE almmmate THEN CREATE almmmate.
    ASSIGN
        almmmate.codcia = s-codcia
        almmmate.codalm = s-codalm
        almmmate.codmat = SUBSTRING(x-linea,1,6).
    IF almmmatg.monvta = 1 THEN x-preuni = DECIMAL(SUBSTRING(x-linea,7)).
    ELSE x-preuni = DECIMAL(SUBSTRING(x-linea,7)) / almmmatg.tpocmb.
    ASSIGN 
        almmmate.libre_d01 = x-preuni.
    FIND Vtatabla WHERE Vtatabla.codcia = s-codcia
        AND Vtatabla.tabla = s-tabla
        AND Vtatabla.llave_c1 = almmmate.codmat
        NO-ERROR.
    IF NOT AVAILABLE Vtatabla THEN DO:
        CREATE Vtatabla.
        ASSIGN
            Vtatabla.codcia = s-codcia
            Vtatabla.tabla = s-tabla
            Vtatabla.llave_c1 = almmmate.codmat.
    END.
    ASSIGN
        Vtatabla.valor[1] = x-preuni
        Vtatabla.Libre_c01 = STRING(DATETIME(TODAY, MTIME), '99/99/9999 HH:MM')
        Vtatabla.Libre_c02 = s-user-id.
END.
