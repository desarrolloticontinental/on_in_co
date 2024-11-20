DEF VAR x-CostoExtCIGV AS DEC.
DEF VAR x-CostoExtSIGV AS DEC.
DEF VAR x-CostoNacCIGV AS DEC.
DEF VAR x-CostoNacSIGV AS DEC.
DEF VAR x-TpoCmbCmp AS DEC.
DEF VAR x-TpoCmbVta AS DEC.

DEF VAR k AS INTE NO-UNDO.

FOR EACH ventas_detalle EXCLUSIVE-LOCK WHERE 
    Ventas_Detalle.DateKey >= DATE(01,01,2020) AND
    Ventas_Detalle.DateKey <= DATE(12,31,2020) AND
    LOOKUP(Ventas_Detalle.coddoc, 'FAC,BOL') > 0,
    FIRST ccbddocu NO-LOCK WHERE ccbddocu.codcia = 001 AND
    ccbddocu.coddoc = ventas_detalle.coddoc AND
    ccbddocu.nrodoc = ventas_detalle.nrodoc AND
    ccbddocu.codmat = ventas_detalle.codmat,
    FIRST almmmatg OF ccbddocu NO-LOCK,
    FIRST ccbcdocu OF ccbddocu NO-LOCK:
    k = k + 1.
    IF k >= 10000 AND (k MODULO 10000) = 0 THEN DO:
        DISPLAY ventas_detalle.datekey ventas_detalle.codmat. 
        PAUSE 0.
    END.
/*     FIND LAST Gn-Tcmb WHERE Gn-Tcmb.Fecha <= CcbCdocu.FchDoc NO-LOCK NO-ERROR.      */
/*     IF NOT AVAIL Gn-Tcmb THEN                                                       */
/*         FIND FIRST Gn-Tcmb WHERE Gn-Tcmb.Fecha >= CcbCdocu.FchDoc NO-LOCK NO-ERROR. */
/*     IF AVAIL Gn-Tcmb THEN                                                           */
/*         ASSIGN                                                                      */
/*             x-TpoCmbCmp = Gn-Tcmb.Compra                                            */
/*             x-TpoCmbVta = Gn-Tcmb.Venta.                                            */
    IF ccbcdocu.codmon = 1 THEN DO:
        x-CostoNacCIGV = ccbddocu.impcto.
        x-CostoNacSIGV = x-CostoNacCIGV.
        IF ccbddocu.aftigv = YES THEN x-CostoNacSIGV = ccbddocu.impcto / (1 + ccbcdocu.porigv / 100).
        x-CostoExtCIGV = x-CostoNacCIGV / Almmmatg.TpoCmb.
        x-CostoExtSIGV = x-CostoNacSIGV / Almmmatg.TpoCmb.
/*         x-CostoExtCIGV = x-CostoNacCIGV / x-TpoCmbCmp. */
/*         x-CostoExtSIGV = x-CostoNacSIGV / x-TpoCmbCmp. */
    END.
    ELSE DO:
        x-CostoExtCIGV = ccbddocu.impcto.
        x-CostoExtSIGV = x-CostoExtCIGV .
        IF ccbddocu.aftigv = YES THEN x-CostoExtSIGV = ccbddocu.impcto / (1 + ccbcdocu.porigv / 100).
        x-CostoNacCIGV = x-CostoExtCIGV * Almmmatg.tpocmb.
        x-CostoNacSIGV = x-CostoExtSIGV * Almmmatg.tpocmb.
/*         x-CostoNacCIGV = x-CostoExtCIGV * x-TpoCmbVta. */
/*         x-CostoNacSIGV = x-CostoExtSIGV * x-TpoCmbVta. */
    END.
    ASSIGN
        Ventas_Detalle.CostoNacCIGV = x-CostoNacCIGV 
        Ventas_Detalle.CostoNacSIGV = x-CostoNacSIGV 
        Ventas_Detalle.CostoExtCIGV = x-CostoExtCIGV 
        Ventas_Detalle.CostoExtSIGV = x-CostoExtSIGV 
        .
END.
