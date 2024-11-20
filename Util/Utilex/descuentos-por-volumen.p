DEF VAR i AS INT.
DEF VAR k AS INT.

/* 1ro limpiamos */
FOR EACH VtaListaMinGn WHERE CodCia = 001:
    DO I = 1 TO 10:
        ASSIGN
            DtoVolR[i] = 0
            DtoVolD[i] = 0.
    END.
END.

DEF VAR x-linea AS CHAR NO-UNDO.

INPUT FROM z:\dcto-por-volumen.prn.
REPEAT :
    IMPORT UNFORMATTED x-linea.
    IF x-linea = '' THEN LEAVE.
    FIND VtaListaMinGn WHERE codcia = 001
        AND codmat = SUBSTRING(x-linea,1,6)
        NO-ERROR.
    IF NOT AVAILABLE vtalistamingn THEN NEXT.
    /* buscamos un espacio libre */
    CICLO:
    DO k = 1 TO 10:
        IF DtoVolR[k] = 0 THEN DO:
            ASSIGN
            DtoVolR[k] = DECIMAL(SUBSTRING(x-linea, 7, 10))  /* Cantidad */
            DtoVolD[k] = DECIMAL(SUBSTRING(x-linea, 17, 10))  /* % Dcto   */
            FchAct = TODAY.
            LEAVE CICLO.
        END.
    END.
END.

INPUT CLOSE.
