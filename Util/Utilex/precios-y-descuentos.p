DEF VAR x-linea AS CHAR NO-UNDO.
DEF VAR I AS INT NO-UNDO.

/* 1ro limpiamos */
/* FOR EACH VtaListaMinGn WHERE CodCia = 001: */
/*     DO I = 1 TO 10:                        */
/*         ASSIGN                             */
/*             promdivi[i] = ''               */
/*             promdto[i] = 0                 */
/*             promfchd[i] = ?                */
/*             promfchh[i] = ?.               */
/*     END.                                   */
/*     DO I = 1 TO 10:                        */
/*         ASSIGN                             */
/*             DtoVolR[i] = 0                 */
/*             DtoVolD[i] = 0.                */
/*     END.                                   */
/* END.                                       */

RUN Precios.
RUN Promocionales.
RUN Volumen.


PROCEDURE Precios:
/* ************** */

INPUT FROM c:\tmp\precios.prn.
REPEAT:
    IMPORT UNFORMATTED x-linea.
    IF x-linea = '' THEN LEAVE.
    FIND vtalistamingn WHERE codcia = 1
        AND codmat = SUBSTRING(x-linea,1,6).
    ASSIGN
        monvta = 1
        preofi = DECIMAL(SUBSTRING(x-linea,11)).
END.

END PROCEDURE.

PROCEDURE Promocionales:
/* ******************** */

DEF VAR i AS INT.
DEF VAR k AS INT.
DEF VAR f-divisiones AS CHAR INIT '00023,00027,00501,00502,00503'.

INPUT FROM c:\tmp\promociones.prn.
REPEAT :
    IMPORT UNFORMATTED x-linea.
    IF x-linea = '' THEN LEAVE.
    FIND vtalistamingn WHERE codcia = 1
        AND codmat = SUBSTRING(x-linea,1,6).
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
            promdto[k]  = DECIMAL(SUBSTRING(x-linea,11))
            promfchd[k] = 01/01/2013
            promfchh[k] = 03/03/2013.
    END.
END.
INPUT CLOSE.

END PROCEDURE.

PROCEDURE Volumen:
/* ************** */

DEF VAR i AS INT.
DEF VAR k AS INT.

INPUT FROM c:\tmp\volumen.prn.
REPEAT :
    IMPORT UNFORMATTED x-linea.
    IF x-linea = '' THEN LEAVE.
    FIND VtaListaMinGn WHERE codcia = 001 AND codmat = SUBSTRING(x-linea,1,6).
    /* limpiamos ofertas */
    DO I = 1 TO 10:
        ASSIGN
            DtoVolR[i] = 0
            DtoVolD[i] = 0.
    END.
    /* buscamos un espacio libre */
    CICLO:
    DO k = 1 TO 10:
        IF DtoVolR[k] = 0 THEN DO:
            ASSIGN
            DtoVolR[k] = DECIMAL(SUBSTRING(x-linea, 21, 10))  /* Cantidad */
            DtoVolD[k] = DECIMAL(SUBSTRING(x-linea, 11, 10))  /* % Dcto   */
            FchAct = TODAY.
            LEAVE CICLO.
        END.
    END.
END.

END PROCEDURE.
