DEF VAR x-linea AS CHAR NO-UNDO.
DEF VAR I AS INT NO-UNDO.
DEF VAR x-preprom LIKE vtalistamingn.preofi NO-UNDO.
DEF VAR x-prenor LIKE vtalistamingn.preofi NO-UNDO.

/* 1ro limpiamos */
RUN Promocionales.

PROCEDURE Promocionales:
/* ******************** */
DEF VAR i AS INT.
DEF VAR k AS INT.
DEF VAR f-divisiones AS CHAR INIT '00023,00027,00501,00502,00503'.

INPUT FROM c:\tmp\cuadernos.prn.
REPEAT :
    IMPORT UNFORMATTED x-linea.
    IF x-linea = '' THEN LEAVE.
    FIND vtalistamingn WHERE codcia = 1 AND codmat = SUBSTRING(x-linea,1,6).
    DISPLAY codmat DECIMAL(SUBSTRING(x-linea,11,10)) DECIMAL(SUBSTRING(x-linea,21)).
    PAUSE 0.
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
            promdto[k]  = DECIMAL(SUBSTRING(x-linea,11,10))
            promfchd[k] = TODAY + 1
            promfchh[k] = 03/10/2013.
        IF promdivi[k] = '00501' THEN promdto[k]  = DECIMAL(SUBSTRING(x-linea,21)).
    END.
END.
INPUT CLOSE.

END PROCEDURE.

