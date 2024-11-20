DEF VAR i AS INT.
DEF VAR k AS INT.
DEF VAR f-divisiones AS CHAR INIT '00023,00027,00501,00502'.

DEF VAR x-linea AS CHAR FORMAT 'x(100)'.

INPUT FROM z:\ofertas.prn.
REPEAT :
    IMPORT UNFORMATTED x-linea.
    IF x-linea <> '' THEN DO:
        FIND vtalistamingn WHERE codcia = 1
            AND codmat = SUBSTRING(x-linea,1,6)
            NO-ERROR.
        IF AVAILABLE vtalistamingn THEN DO:
            /* limpiamos ofertas */
            DO I = 1 TO 10:
                ASSIGN
                    promdivi[i] = ''
                    promdto[i] = 0
                    promfchd[i] = ?
                    promfchh[i] = ?.
            END.
            /* cargamos para las divisiones */
            DO k = 1 TO NUM-ENTRIES(f-divisiones):
                ASSIGN
                    promdivi[k] = ENTRY(k, f-divisiones)
                    promdto[k] = 25
                    promfchd[k] = TODAY
                    promfchh[k] = 09/30/2012.
            END.
        END.
    END.
END.
INPUT CLOSE.
