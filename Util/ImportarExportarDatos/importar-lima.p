/* IMPORTAR ARCHIVOS DE LIMA  */

DEF VAR s-codcia AS INT INIT 001 NO-UNDO.

/* PRECIOS */
DEF TEMP-TABLE t-matg LIKE almmmatg.
DISABLE TRIGGERS FOR LOAD OF almmmatg.

INPUT FROM C:\tmp\almmmatg.d.
REPEAT:
    CREATE t-matg.
    IMPORT t-matg NO-ERROR.
END.
INPUT CLOSE.
FOR EACH t-matg NO-LOCK WHERE codcia = s-codcia AND codmat <> "":
    FIND FIRST almmmatg OF t-matg NO-ERROR.
    IF NOT AVAILABLE almmmatg THEN DO:
        IF LOCKED(almmmatg) THEN NEXT.
        CREATE almmmatg.
    END.
    DISPLAY t-matg.codmat.
    PAUSE 0.
    BUFFER-COPY t-matg TO almmmatg.
END.

DEF TEMP-TABLE t-tabla LIKE vtatabla.
DISABLE TRIGGERS FOR LOAD OF vtatabla.

INPUT FROM c:\tmp\vtatabla.d.
REPEAT:
    CREATE t-tabla.
    IMPORT t-tabla.
END.
FOR EACH t-tabla NO-LOCK WHERE t-tabla.codcia = s-codcia AND t-tabla.tabla <> "":
    FIND FIRST vtatabla WHERE vtatabla.codcia = t-tabla.codcia
        AND vtatabla.tabla = t-tabla.tabla
        AND vtatabla.llave_c1 = t-tabla.llave_c1
        AND vtatabla.llave_c2 = t-tabla.llave_c2
        AND vtatabla.llave_c3 = t-tabla.llave_c3
        NO-ERROR.
    IF NOT AVAILABLE vtatabla THEN DO:
        IF LOCKED(vtatabla) THEN NEXT.
        CREATE vtatabla.
    END.
    DISPLAY t-tabla.tabla t-tabla.llave_c1.
    PAUSE 0.
    BUFFER-COPY t-tabla TO vtatabla.
END.
