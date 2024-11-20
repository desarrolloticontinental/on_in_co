/* Carga ventas historicas por lista de precios */
DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR s-tabla AS CHAR INIT 'EXPOVTAHIST' NO-UNDO.

DEF BUFFER PEDIDO FOR Faccpedi.
DEF BUFFER COTIZACION FOR Faccpedi.
DEF BUFFER CDOCU FOR Ccbcdocu.

/* Borramos histórico */
DEF VAR x-FchDoc-1 AS DATE NO-UNDO.
DEF VAR x-FchDoc-2 AS DATE NO-UNDO.

ASSIGN
    x-FchDoc-1 = DATE(01,01,2018)
    x-FchDoc-2 = DATE(12,31,2018).
FOR EACH VtaTabla EXCLUSIVE-LOCK WHERE VtaTabla.CodCia = s-codcia AND 
    VtaTabla.Tabla  = s-tabla AND
    (VtaTabla.Rango_fecha[1] >= x-FchDoc-1 AND 
     VtaTabla.Rango_fecha[1] <= x-FchDoc-2):
    DELETE VtaTabla.
END.
DEF VAR k AS INT NO-UNDO.
/* Cargamos histórico */
FOR EACH gn-divi NO-LOCK WHERE gn-divi.codcia = s-codcia AND
    GN-DIVI.CanalVenta = "FER",
    EACH ccbcdocu NO-LOCK WHERE ccbcdocu.codcia = s-codcia
    AND ccbcdocu.divori = gn-divi.coddiv 
    AND ccbcdocu.fchdoc >= x-FchDoc-1
    AND ccbcdocu.fchdoc <= x-FchDoc-2
    AND LOOKUP(ccbcdocu.coddoc, 'FAC,BOL,N/C') > 0:
    IF ccbcdocu.tpofac = "S" THEN NEXT.
    IF ccbcdocu.tpofac = "A" THEN NEXT.
    IF ccbcdocu.flgest = "A" THEN NEXT.
    IF ccbcdocu.coddoc = "N/C" AND ccbcdocu.cndcre <> 'D' THEN NEXT.
    k = k + 1.
    IF k > 1000 AND (k MODULO 1000 = 0) THEN DO:
        DISPLAY ccbcdocu.coddiv ccbcdocu.coddoc ccbcdocu.nrodoc ccbcdocu.fchdoc
            WITH STREAM-IO NO-BOX.
        PAUSE 0.
    END.
    CASE TRUE:
        WHEN ccbcdocu.coddoc = "N/C" THEN DO:
            /* Buscamos referencia */
            FIND CDOCU WHERE CDOCU.codcia = s-codcia AND
                CDOCU.coddoc = ccbcdocu.codref AND
                CDOCU.nrodoc = ccbcdocu.nroref
                NO-LOCK NO-ERROR.
            IF NOT AVAILABLE CDOCU THEN NEXT.
            FIND PEDIDO WHERE PEDIDO.codcia = s-codcia AND
                PEDIDO.coddoc = CDOCU.codped AND
                PEDIDO.nroped = CDOCU.nroped
                NO-LOCK NO-ERROR.
            IF NOT AVAILABLE PEDIDO THEN NEXT.
            FIND COTIZACION WHERE COTIZACION.codcia = s-codcia AND
                COTIZACION.coddoc = PEDIDO.codref AND 
                COTIZACION.nroped = PEDIDO.nroref
                NO-LOCK NO-ERROR.
            IF NOT AVAILABLE COTIZACION THEN NEXT.
            FOR EACH ccbddocu OF ccbcdocu NO-LOCK:
                FIND FIRST VtaTabla WHERE VtaTabla.CodCia = s-codcia AND
                    VtaTabla.Tabla  = s-tabla AND
                    VtaTabla.Llave_c1 = COTIZACION.Libre_c01 AND
                    VtaTabla.Llave_c2 = ccbcdocu.codcli AND
                    VtaTabla.LLave_c3 = ccbddocu.codmat AND
                    VtaTabla.Rango_fecha[1] = ccbcdocu.fchdoc
                    EXCLUSIVE-LOCK NO-ERROR.
                IF NOT AVAILABLE VtaTabla THEN CREATE VtaTabla.
                ASSIGN
                    VtaTabla.CodCia = s-codcia
                    VtaTabla.Tabla  = s-tabla
                    VtaTabla.Llave_c1 = COTIZACION.Libre_c01
                    VtaTabla.Llave_c2 = ccbcdocu.codcli
                    VtaTabla.LLave_c3 = ccbddocu.codmat
                    VtaTabla.Rango_fecha[1] = ccbcdocu.fchdoc.
                ASSIGN
                    VtaTabla.Valor[1] = VtaTabla.Valor[1] + (-1 * (ccbddocu.candes * ccbddocu.factor))
                    VtaTabla.Valor[2] = VtaTabla.Valor[2] + (-1 * (ccbddocu.implin - ccbddocu.impdto2)).
            END.
        END.
        OTHERWISE DO:
            FIND PEDIDO WHERE PEDIDO.codcia = s-codcia AND
                PEDIDO.coddoc = ccbcdocu.codped AND
                PEDIDO.nroped = ccbcdocu.nroped
                NO-LOCK NO-ERROR.
            IF NOT AVAILABLE PEDIDO THEN NEXT.
            FIND COTIZACION WHERE COTIZACION.codcia = s-codcia AND
                COTIZACION.coddoc = PEDIDO.codref AND 
                COTIZACION.nroped = PEDIDO.nroref
                NO-LOCK NO-ERROR.
            IF NOT AVAILABLE COTIZACION THEN NEXT.
            FOR EACH ccbddocu OF ccbcdocu NO-LOCK:
                FIND FIRST VtaTabla WHERE VtaTabla.CodCia = s-codcia AND
                    VtaTabla.Tabla  = s-tabla AND
                    VtaTabla.Llave_c1 = COTIZACION.Libre_c01 AND
                    VtaTabla.Llave_c2 = ccbcdocu.codcli AND
                    VtaTabla.LLave_c3 = ccbddocu.codmat AND
                    VtaTabla.Rango_fecha[1] = ccbcdocu.fchdoc
                    EXCLUSIVE-LOCK NO-ERROR.
                IF NOT AVAILABLE VtaTabla THEN CREATE VtaTabla.
                ASSIGN
                    VtaTabla.CodCia = s-codcia
                    VtaTabla.Tabla  = s-tabla
                    VtaTabla.Llave_c1 = COTIZACION.Libre_c01
                    VtaTabla.Llave_c2 = ccbcdocu.codcli
                    VtaTabla.LLave_c3 = ccbddocu.codmat
                    VtaTabla.Rango_fecha[1] = ccbcdocu.fchdoc.
                ASSIGN
                    VtaTabla.Valor[1] = VtaTabla.Valor[1] + (ccbddocu.candes * ccbddocu.factor)
                    VtaTabla.Valor[2] = VtaTabla.Valor[2] + (ccbddocu.implin - ccbddocu.impdto2).
            END.
        END.
    END CASE.
END.
