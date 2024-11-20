DEFINE VAR hProc AS HANDLE NO-UNDO.

RUN clf\clasificacion-articulos-proceso.r PERSISTENT SET hProc.

DEFINE VAR cRetVal AS CHAR.
DEFINE VAR dtDesde AS DATETIME.
DEFINE VAR dtHasta AS DATETIME.

dtDesde = NOW.

/*
    1 : Campa�a
    2 : No campa�a
*/

/* Procedimientos */
RUN calcular_clasificacion IN hProc (4,1,NO,YES, OUTPUT cRetVal).

DELETE PROCEDURE hProc.

dtHasta = NOW.

MESSAGE cRetVal SKIP
        "Desde " dtDesde SKIP
        "Hasta " dthasta.
