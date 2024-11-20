/* NO borra los grabados en otras divisiones */

DEF VAR i AS INT.
DEF VAR k AS INT.
DEF VAR x-ok AS LOG.
DEF VAR f-divisiones AS CHAR INIT '00503'.

DEF VAR x-linea AS CHAR.

INPUT FROM c:\tmp\utilex.prn.
REPEAT :
    IMPORT UNFORMATTED x-linea.
    IF x-linea = '' THEN LEAVE.
    FIND vtalistamingn WHERE codcia = 1
        AND codmat = SUBSTRING(x-linea,1,6)
        NO-ERROR.
    IF AVAILABLE vtalistamingn THEN DO:
        DISPLAY SUBSTRING(x-linea,1,6).
        PAUSE 0.
        /* cargamos para las divisiones */
        DO k = 1 TO NUM-ENTRIES(f-divisiones):
            x-ok = NO.
            DO I = 1 TO 10:
                IF promdivi[i] = ENTRY(k, f-divisiones) THEN DO:
                    ASSIGN
                        /*promdivi[i] = ENTRY(k, f-divisiones)*/
                        promdto[i]  = DECIMAL(SUBSTRING(x-linea,7))
                        promfchd[i] = TODAY
                        promfchh[i] = 08/31/2013.
                    x-ok = YES.
                    LEAVE.
                END.
            END.
            IF x-ok = NO THEN DO:
                DO I = 1 TO 10:
                    IF promdivi[i] = '' THEN DO:
                        ASSIGN
                            promdivi[i] = ENTRY(k, f-divisiones)
                            promdto[i]  = DECIMAL(SUBSTRING(x-linea,7))
                            promfchd[i] = TODAY
                            promfchh[i] = 08/31/2013.
                        x-ok = YES.
                        LEAVE.
                    END.
                END.
            END.
        END.
    END.
END.
INPUT CLOSE.
