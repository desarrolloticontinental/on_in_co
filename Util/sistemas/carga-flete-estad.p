DEF VAR x-TpoCmbCmp AS DECI INIT 1 NO-UNDO.
DEF VAR x-TpoCmbVta AS DECI INIT 1 NO-UNDO.
DEF VAR x-ImpFlete AS DEC NO-UNDO.
DEF VAR x-PorIgv AS DEC DECIMALS 4 NO-UNDO.
DEF VAR pCodDiv AS CHAR NO-UNDO.
DEF VAR x-CodVen AS CHAR NO-UNDO.

FOR EACH ccbcdocu NO-LOCK WHERE ccbcdocu.codcia = 1
        AND LOOKUP(ccbcdocu.coddoc, 'FAC,BOL') > 0
        AND fchdoc >= 01/01/2016 AND fchdoc < TODAY
        AND flgest <> 'A':
    x-CodVen = Ccbcdocu.codven.

    pCodDiv = IF (Ccbcdocu.DivOri > '') THEN Ccbcdocu.DivOri ELSE Ccbcdocu.CodDiv.
    IF Ccbcdocu.codcli = '20511358907' THEN pCodDiv = '00022'.  /* STANDFORD */
    IF pCodDiv <> '00099' AND x-codven = '998' THEN pCodDiv = '00099'.   /* Exportaciones */
    IF pCodDiv <> '00098' AND x-codven = '157' THEN pCodDiv = '00098'.   /* Refiles */
    /* FACTURAS ANTICIPOS Y/O SERVICIOS SE VAN A OTRA DIVISION */
    IF LOOKUP(CcbCDocu.CodDoc,"TCK,FAC,BOL") > 0 AND LOOKUP(CcbCdocu.TpoFac, 'A,S') > 0 THEN pCodDiv = "99999".

    FIND DimDivision WHERE DimDivision.CodDiv = pCodDiv NO-LOCK NO-ERROR.
    IF NOT AVAILABLE DimDivision OR DimDivision.CanalVenta = 'MIN' THEN NEXT.

    FIND FIRST Ventas_Cabecera WHERE Ventas_Cabecera.CodDiv = pCodDiv AND
        Ventas_Cabecera.CodDoc = Ccbcdocu.coddoc AND
        Ventas_Cabecera.NroDoc = Ccbcdocu.nrodoc NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Ventas_Cabecera THEN NEXT.

    DISPLAY ccbcdocu.coddiv ccbcdocu.fchdoc ccbcdocu.coddoc ccbcdocu.nrodoc WITH STREAM-IO NO-BOX.
    PAUSE 0.

    x-PorIgv    = Ccbcdocu.porigv.
    FIND LAST Gn-Tcmb WHERE Gn-Tcmb.Fecha <= CcbCdocu.FchDoc NO-LOCK NO-ERROR.
    IF NOT AVAIL Gn-Tcmb THEN 
        FIND FIRST Gn-Tcmb WHERE Gn-Tcmb.Fecha >= CcbCdocu.FchDoc NO-LOCK NO-ERROR.
    IF AVAIL Gn-Tcmb THEN 
        ASSIGN
            x-TpoCmbCmp = Gn-Tcmb.Compra
            x-TpoCmbVta = Gn-Tcmb.Venta.
    FOR EACH Ventas_Detalle OF Ventas_Cabecera EXCLUSIVE-LOCK,
        FIRST Ccbddocu OF Ccbcdocu NO-LOCK WHERE Ccbddocu.codmat = Ventas_Detalle.CodMat:
        x-ImpFlete = Ccbddocu.CanDes * Ccbddocu.ImpDcto_Adelanto[4].
        IF Ccbcdocu.CodMon = 1 THEN 
            ASSIGN
                Ventas_Detalle.FleteExtCIGV   = x-ImpFlete / x-TpoCmbCmp
                Ventas_Detalle.FleteExtSIGV   = Ventas_Detalle.FleteExtCIGV / ( 1 + ( x-PorIgv / 100) )
                Ventas_Detalle.FleteNacCIGV   = x-ImpFlete 
                Ventas_Detalle.FleteNacSIGV   = Ventas_Detalle.FleteNacCIGV / ( 1 + ( x-PorIgv / 100) ).
        IF Ccbcdocu.CodMon = 2 THEN 
            ASSIGN
                Ventas_Detalle.FleteExtCIGV   = x-ImpFlete  
                Ventas_Detalle.FleteExtSIGV   = Ventas_Detalle.FleteExtCIGV / ( 1 + ( x-PorIgv / 100) )
                Ventas_Detalle.FleteNacCIGV   = x-ImpFlete * x-TpoCmbVta
                Ventas_Detalle.FleteNacSIGV   = Ventas_Detalle.FleteNacCIGV / ( 1 + ( x-PorIgv / 100) ).
        FIND FIRST Ventas WHERE Ventas.DateKey = Ventas_Cabecera.DateKey
            AND Ventas.coddiv = Ventas_Cabecera.coddiv
            AND Ventas.divdes = Ventas_Cabecera.divdes
            AND Ventas.codcli = Ventas_Cabecera.codcli
            AND Ventas.codven = Ventas_Cabecera.codven
            AND Ventas.CodMat = Ventas_Detalle.codmat
            AND Ventas.Tipo   = Ventas_Cabecera.tipo
            AND Ventas.Delivery = Ventas_Cabecera.Delivery
            AND Ventas.ListaBase = Ventas_Cabecera.ListaBase
            EXCLUSIVE-LOCK NO-ERROR.
        IF AVAILABLE Ventas THEN DO:
            ASSIGN
                Ventas.FleteExtCIGV = Ventas.Fleteextcigv + Ventas_Detalle.Fleteextcigv
                Ventas.FleteExtSIGV = Ventas.Fleteextsigv + Ventas_Detalle.Fleteextsigv
                Ventas.FleteNacCIGV = Ventas.Fletenaccigv + Ventas_Detalle.Fletenaccigv
                Ventas.FleteNacSIGV = Ventas.Fletenacsigv + Ventas_Detalle.Fletenacsigv.
        END.
    END.
END.

