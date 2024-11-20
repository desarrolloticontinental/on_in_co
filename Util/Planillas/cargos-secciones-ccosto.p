OUTPUT TO c:\tmp\cargos.txt.
FOR EACH pl-carg NO-LOCK.
    PUT UNFORMATTED cargos SKIP.
END.
OUTPUT CLOSE.

OUTPUT TO c:\tmp\secciones.txt.
FOR EACH pl-secc NO-LOCK:
    PUT UNFORMATTED seccion SKIP.
END.
OUTPUT CLOSE.

OUTPUT TO c:\tmp\ccosto.txt.
FOR EACH cb-auxi WHERE cb-auxi.CodCia = 0
    AND cb-auxi.CLFAUX = "CCO":
    PUT UNFORMATTED
        cb-auxi.codaux '|'
        cb-auxi.nomaux
        SKIP.
END.

