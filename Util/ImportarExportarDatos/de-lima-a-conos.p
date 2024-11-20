/* GENERACION DE ARCHIVOS DE ENVIO A CONOS */
DEF VAR s-codcia AS INT INIT 001 NO-UNDO.

/* PRECIOS */
OUTPUT TO c:\tmp\almmmatg.d.
FOR EACH almmmatg NO-LOCK WHERE codcia = s-codcia:
    EXPORT almmmatg.
END.
OUTPUT CLOSE.
/* TABLAS */
OUTPUT TO c:\tmp\vtatabla.d.
FOR EACH vtatabla NO-LOCK WHERE codcia = s-codcia:
    EXPORT vtatabla.
END.
OUTPUT CLOSE.

