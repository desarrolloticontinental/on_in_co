DEF NEW SHARED VAR s-codcia AS INTE INIT 001.

DEF VAR x-fecha AS DATE NO-UNDO.

x-fecha = DATE(12,31,2018).
DISABLE TRIGGERS FOR LOAD OF faccpedi.
DISABLE TRIGGERS FOR LOAD OF facdpedi.

/*DEF VAR x-Documentos AS CHAR INIT 'P/M,PED,COT,O/D,C/M,O/M,OTR,ODC,OTC,PSV,PET,PPV,PPX,PNC,PPD' NO-UNDO.*/
DEF VAR x-Documentos AS CHAR INIT 'P/M,PED,COT' NO-UNDO.
DEF VAR x-CodDoc AS CHAR NO-UNDO.
DEF VAR k AS INTE NO-UNDO.

OUTPUT TO /u/backup/IN/ON_IN_CO/log/borrado250222.txt.
PUT NOW ' INICIO' SKIP.
DO k = NUM-ENTRIES(x-Documentos) TO 1 BY -1:
    x-CodDoc = ENTRY(k, x-Documentos).
    FOR EACH gn-divi NO-LOCK WHERE gn-divi.codcia = s-codcia:
        RUN borra-pedidos.
        PUT NOW ' ' gn-divi.coddiv ' ' x-coddoc SKIP.
    END.
END.

DISABLE TRIGGERS FOR LOAD OF almcrepo.
DISABLE TRIGGERS FOR LOAD OF almdrepo.

DEF VAR x-Movimientos AS CHAR INIT 'A,M,INC,RAN' NO-UNDO.
DEF VAR x-TipMov AS CHAR NO-UNDO.

DO k = NUM-ENTRIES(x-Movimientos) TO 1 BY -1:
    x-TipMov = ENTRY(k, x-Movimientos).
    RUN borra-ran.
    PUT NOW ' ' x-tipmov SKIP.
END.
PUT NOW ' FIN' SKIP.
OUTPUT CLOSE.
QUIT.

PROCEDURE borra-pedidos:
/* ******************** */
    FOR EACH faccpedi EXCLUSIVE-LOCK WHERE faccpedi.codcia = s-codcia
        AND faccpedi.coddiv = gn-divi.coddiv
        AND faccpedi.coddoc = x-CodDoc
        AND faccpedi.fchped <= x-fecha:
        FOR EACH facdpedi OF faccpedi EXCLUSIVE-LOCK:
            DELETE facdpedi.
        END.
        DELETE faccpedi.
    END.

END PROCEDURE.

PROCEDURE borra-ran:
/* ***************** */
    FOR EACH almcrepo EXCLUSIVE-LOCK WHERE almcrepo.codcia = s-codcia
        AND almcrepo.tipmov = x-TipMov
        AND almcrepo.fchdoc <= x-fecha:
        FOR EACH almdrepo OF almcrepo EXCLUSIVE-LOCK:
            DELETE almdrepo.
        END.
        DELETE almcrepo.
    END.

END PROCEDURE.
