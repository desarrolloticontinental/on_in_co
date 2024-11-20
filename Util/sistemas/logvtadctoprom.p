DEF TEMP-TABLE t-puente LIKE vtadctoprom.
DEF TEMP-TABLE t-dctoprom LIKE vtadctoprom
    FIELD event LIKE replog.event.

FOR EACH replog NO-LOCK WHERE flgdb20 = YES AND DATE(logdate) = TODAY
    AND tablename = 'vtadctoprom'
    AND keyvalue BEGINS '001|101794':
    CREATE t-dctoprom.
    CREATE t-puente.
    RAW-TRANSFER replog.datarecord TO t-puente.
    BUFFER-COPY t-puente TO t-dctoprom
        ASSIGN t-dctoprom.event = replog.event.
END.

OUTPUT TO d:\prueba.txt.
FOR EACH t-dctoprom NO-LOCK:
    DISPLAY t-dctoprom.event 
        coddiv 
        fchini 
        fchfin 
        descuento 
        precio
        usrcreacion 
        fchcreacion 
        horacreacion
        WITH STREAM-IO NO-BOX WIDTH 320.
END.
OUTPUT CLOSE.

