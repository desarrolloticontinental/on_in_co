DEF VAR x-linea AS CHAR NO-UNDO.
DEF VAR s-coddiv AS CHAR INIT '20060'.

DISABLE TRIGGERS FOR LOAD OF expasist.
DISABLE TRIGGERS FOR LOAD OF expturno.

DELETE FROM expasist WHERE codcia = 1 AND coddiv = s-coddiv.
DELETE FROM expturno WHERE codcia = 1 AND coddiv = s-coddiv.

INPUT FROM d:\tmp\preventa.prn.
REPEAT :
    IMPORT UNFORMATTED x-linea.        
    IF x-linea = '' THEN LEAVE.
    CREATE expasist.
    ASSIGN
        codcia = 001
        coddiv = s-coddiv
        codcli = SUBSTRING(x-linea,1,11)
        nomcli = trim(SUBSTRING(x-linea,12))
        fecpro = DATE(01,08,2018)
        horapro = "08:00"
        estado[1] = ""
        estado[2] = "L".
END.
INPUT CLOSE.

