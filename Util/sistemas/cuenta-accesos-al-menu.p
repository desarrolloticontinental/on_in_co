DEF TEMP-TABLE t-tabla LIKE logtabla.


FOR EACH logtabla NO-LOCK WHERE codcia = 1 and evento = 'RUN-PROGRAM' 
    and dia >= 01/01/2022
    AND dia <= DATE(03,20,2023):
    FIND FIRST t-tabla WHERE t-tabla.valorllave = logtabla.valorllave
        EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE t-tabla THEN DO:
        CREATE t-tabla.
        ASSIGN
            t-tabla.valorllave = logtabla.valorllave
            t-tabla.numid = 0.
    END.
    t-tabla.numid = t-tabla.numid + 1.
END.


OUTPUT TO d:\resumen.txt.
FOR EACH t-tabla:
    PUT UNFORMATTED
        t-tabla.valorllave '|'
        t-tabla.numid
        SKIP.
END.
