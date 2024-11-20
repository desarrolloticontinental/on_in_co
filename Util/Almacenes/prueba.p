DEF VAR x-divisiones AS CHAR NO-UNDO.
DEF VAR x-coddiv AS CHAR NO-UNDO.
DEF VAR x-codmat AS CHAR NO-UNDO.
DEF VAR x-preuni AS DEC DECIMALS 4 NO-UNDO.
DEF VAR x-linea AS CHAR FORMAT 'x(100)' NO-UNDO.
DEF VAR s-codcia AS INT INIT 001 NO-UNDO.

DISABLE TRIGGERS FOR LOAD OF almmmatg.
DEF TEMP-TABLE t-matg LIKE almmmatg
    FIELD preuni AS DEC.

DEF VAR i AS INT NO-UNDO.
DEF VAR j AS INT NO-UNDO.
DEF VAR l-ok AS LOG NO-UNDO.

x-divisiones = '00001,00002,00003,00014,00011'.

INPUT FROM c:\tmp\promociones.prn.
REPEAT:
    IMPORT UNFORMATTED x-linea.
    ASSIGN
        x-codmat = SUBSTRING(x-linea,1,6)
        x-preuni = DECIMAL(SUBSTRING(x-linea,7)).
    CREATE t-matg.
    ASSIGN
        t-matg.codcia = s-codcia
        t-matg.codmat = x-codmat
        t-matg.preuni = x-preuni.
END.
INPUT CLOSE.

FOR EACH t-matg , FIRST almmmatg OF t-matg NO-LOCK:
    DO  i = 1 TO NUM-ENTRIES(x-divisiones):
        x-coddiv = ENTRY(i, x-divisiones).
        ASSIGN
            t-matg.prevta[1]   = almmmatg.prevta[1]
            t-matg.promdivi[i] = x-coddiv
            t-matg.promfchd[i] = 10/06/2010
            t-matg.promfchh[i] = 10/30/2010
            t-matg.promdto[i] = ROUND( ( 1 - ( t-matg.preuni / Almmmatg.Prevta[1] ) ) * 100 , 4 ).
    END.
END.

FOR EACH t-matg , FIRST almmmatg OF t-matg:
    DO j = 1 TO 10:
        ASSIGN
            almmmatg.promdivi[j] = ""
            almmmatg.promfchd[j] = ?
            almmmatg.promfchh[j] = ?
            almmmatg.promdto[j] = 0.
    END.
    DO j = 1 TO 10:
        ASSIGN
            almmmatg.promdivi[j] = t-matg.promdivi[j]
            almmmatg.promfchd[j] = t-matg.promfchd[j]
            almmmatg.promfchh[j] = t-matg.promfchh[j]
            almmmatg.promdto[j] = t-matg.promdto[j].
    END.
END.
