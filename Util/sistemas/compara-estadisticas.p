DEF TEMP-TABLE t-cabecera LIKE ventas_cabecera.
DEF TEMP-TABLE t-detalle  LIKE ventas_detalle.

INPUT FROM d:\ventas_cabecera.txt.
REPEAT :
    CREATE t-cabecera.
    IMPORT DELIMITER ";" t-cabecera.
END.

INPUT FROM d:\ventas_detalle.txt.
REPEAT :
    CREATE t-detalle.
    IMPORT DELIMITER ";" t-detalle.
END.

FOR EACH t-cabecera, FIRST ventas_cabecera OF t-cabecera NO-LOCK,
    EACH t-detalle OF t-cabecera,
    FIRST ventas_detalle OF ventas_cabecera NO-LOCK
    WHERE ventas_detalle.codmat = t-detalle.codmat
    AND ABS(Ventas_Detalle.ImpNacsIGV - t-Detalle.ImpNacsIGV) > 0.1:
    DISPLAY 
        Ventas_Cabecera.CodDoc 
        Ventas_Cabecera.NroDoc 
        t-detalle.codmat
        Ventas_Detalle.ImpNacCIGV 
        t-Detalle.ImpNacCIGV 
        Ventas_Detalle.ImpNacSIGV
        t-Detalle.ImpNacSIGV
        WITH STREAM-IO NO-BOX WIDTH 320
        .
END.
