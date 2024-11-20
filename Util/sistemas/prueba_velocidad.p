DEF VAR a AS INT64 NO-UNDO.
a=ETIME(YES).
FOR EACH ventas_detalle NO-LOCK WHERE
    ventas_detalle.datekey >= DATE(01,01,2023)
    AND ventas_detalle.datekey <= DATE(03,31,2023):
END.
MESSAGE ETIME.

