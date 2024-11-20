DEF VAR X AS INT.
DEF VAR x-nombre AS CHAR FORMAT 'x(25)' LABEL 'Nombre'.
DEF VAR x-grupo AS CHAR FORMAT 'x(25)' LABEL 'Grupo'.
DEF TEMP-TABLE detalle
    FIELD tipo LIKE pf-g002.tipo FORMAT 'x(10)'
    FIELD etiqueta LIKE pf-g002.etiqueta FORMAT 'x(50)'
    FIELD grupo LIKE x-grupo
    FIELD USER-ID LIKE pf-g004.USER-ID
    FIELD nombre LIKE x-nombre.


DEF FRAME f-frame
    detalle.Tipo 
    detalle.Etiqueta 
    detalle.grupo
    detalle.USER-ID
    detalle.NOMBRE
    WITH STREAM-IO NO-BOX NO-LABELS NO-UNDERLINE DOWN WIDTH 320.

FOR EACH  pf-g002 NO-LOCK WHERE aplic-id = 'pln':
    CREATE detalle.
    ASSIGN
        detalle.tipo = pf-g002.tipo
        detalle.etiqueta = pf-g002.etiqueta.
    DO X = 1 TO NUM-ENTRIES(PF-G002.Seguridad-Grupos):
        x-grupo = ENTRY(X, pf-g002.seguridad-grupos).
        FOR EACH pf-g004 NO-LOCK WHERE pf-g004.aplic-id = pf-g002.aplic-id
            AND LOOKUP(x-grupo, pf-g004.seguridad) > 0:
            x-nombre = ''.
            FIND _user WHERE _USER._userid = pf-g004.USER-ID NO-LOCK NO-ERROR.
            IF AVAILABLE _USER THEN X-NOMBRE = _USER._USER-NAME.
            IF X = 1 
            THEN ASSIGN
                    detalle.grupo = x-grupo
                    detalle.USER-ID = pf-g004.USER-ID
                    detalle.nombre = x-nombre.
            ELSE DO:
                CREATE detalle.
                ASSIGN
                    detalle.tipo = pf-g002.tipo
                    detalle.etiqueta = pf-g002.etiqueta
                    detalle.grupo = x-grupo
                    detalle.USER-ID = pf-g004.USER-ID
                    detalle.nombre = x-nombre.
            END.
        END.
    END.
END.


OUTPUT TO c:\tmp\accesos-planilla.txt.
FOR EACH detalle:
    DISPLAY
        detalle.Tipo 
        detalle.Etiqueta 
        detalle.grupo
        detalle.USER-ID
        detalle.NOMBRE
        WITH FRAME f-frame.

END.
OUTPUT CLOSE.




