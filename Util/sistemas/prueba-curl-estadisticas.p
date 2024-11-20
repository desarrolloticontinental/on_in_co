DEF VAR x-CodFchI AS DATE NO-UNDO.
DEF VAR x-CodFchF AS DATE NO-UNDO.
DEF VAR x-Archivo AS CHAR NO-UNDO.

ASSIGN
    x-CodFchF = TODAY - 1
    x-CodFchI = DATE(01, 01, YEAR(TODAY)).      /* Por defecto */

/* Fecha de Cierre */
/* un meses atrás */
x-CodFchI = ADD-INTERVAL(TODAY, -5, "days").


DEF TEMP-TABLE t-ventas_cabecera NO-UNDO LIKE ventas_cabecera
    FIELD CodRef AS CHAR FORMAT 'x(8)'
    FIELD NroRef AS CHAR FORMAT 'x(15)'.

DEF TEMP-TABLE t-ventas_detalle NO-UNDO LIKE ventas_detalle
    FIELD CodRef AS CHAR FORMAT 'x(8)'
    FIELD NroRef AS CHAR FORMAT 'x(15)'.

EMPTY TEMP-TABLE t-Ventas_Cabecera.
EMPTY TEMP-TABLE t-Ventas_Detalle.

FOR EACH Ventas_Cabecera NO-LOCK WHERE Ventas_Cabecera.DateKey >= x-CodFchI AND
    Ventas_Cabecera.DateKey <= x-CodFchF:
    CREATE t-Ventas_Cabecera.
    BUFFER-COPY Ventas_Cabecera TO t-Ventas_Cabecera.
END.

FOR EACH Ventas_Detalle NO-LOCK WHERE Ventas_Detalle.DateKey >= x-CodFchI AND
    Ventas_Detalle.DateKey <= x-CodFchF:
    CREATE t-Ventas_Detalle.
    BUFFER-COPY Ventas_Detalle TO t-Ventas_Detalle.
END.

FOR EACH t-Ventas_Cabecera NO-LOCK,
    EACH t-Ventas_Detalle OF t-Ventas_Cabecera NO-LOCK:
    ASSIGN
        t-Ventas_Detalle.CodRef = t-Ventas_Cabecera.CodRef
        t-Ventas_Detalle.NroRef = t-Ventas_Cabecera.NroRef.
END.

x-Archivo = "d:\" + "ventas_cabecera.txt".

OUTPUT TO VALUE(x-archivo) KEEP-MESSAGES.
FOR EACH t-Ventas_Cabecera NO-LOCK:
    EXPORT DELIMITER ";" t-ventas_cabecera 
        EXCEPT t-Ventas_Cabecera.ImpNacSIGV 
        t-Ventas_Cabecera.ImpNacCIGV 
        t-Ventas_Cabecera.ImpExtSIGV 
        t-Ventas_Cabecera.ImpExtCIGV.
END.
OUTPUT CLOSE.

x-Archivo = "d:\ventas_detalle.txt".

OUTPUT TO VALUE(x-archivo) KEEP-MESSAGES.
FOR EACH t-Ventas_Detalle NO-LOCK:
    EXPORT delimiter ";" t-Ventas_Detalle.
END.
OUTPUT CLOSE.
