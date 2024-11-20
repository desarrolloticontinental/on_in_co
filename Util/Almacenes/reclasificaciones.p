DEFINE NEW SHARED VARIABLE S-TPOMOV AS CHAR INIT 'S'.   /* Salidas */
DEFINE NEW SHARED VARIABLE C-CODMOV AS CHAR INIT '14'.  /* Reclasificación */

DEF TEMP-TABLE ITEM LIKE Almdmov.
DEF BUFFER B-MATG FOR Almmmatg.
    

FOR EACH almcmov NO-LOCK WHERE codcia = 001
    AND tipmov = s-tpomov
    AND codmov = 14
    AND flgest <> "A":
    FOR EACH Almdmov OF Almcmov NO-LOCK:
        CREATE ITEM.
        BUFFER-COPY Almdmov TO ITEM.
    END.

END.

OUTPUT TO c:\tmp\reclasificacion.txt.
FOR EACH ITEM, EACH INTEGRAL.Almmmatg OF ITEM NO-LOCK,
    EACH B-MATG WHERE B-MATG.CodCia = ITEM.CodCia
    AND B-MATG.codmat = ITEM.CodAnt NO-LOCK:
    PUT UNFORMATTED
        ITEM.codmat '|'
        ITEM.codant '|'
        SKIP.
END.
