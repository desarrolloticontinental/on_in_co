DEF VAR x-articulo-ICBPer AS CHAR INIT '099268'.
DEFINE VARIABLE F-PREBAS LIKE Almmmatg.PreBas NO-UNDO.
DEFINE VARIABLE F-DSCTOS LIKE Almmmatg.PorMax NO-UNDO.
DEFINE VARIABLE F-PREVTA AS DECI NO-UNDO.
DEFINE VARIABLE Y-DSCTOS AS DECI NO-UNDO.
DEFINE VARIABLE Z-DSCTOS AS DECI NO-UNDO.
DEFINE VARIABLE X-TIPDTO AS CHAR NO-UNDO.
DEFINE VARIABLE f-FleteUnitario AS DEC NO-UNDO.
DEF VAR i-nItem AS INT NO-UNDO.
DEF VAR x-StkAct AS DEC NO-UNDO.
DEF VAR s-StkComprometido AS DEC NO-UNDO.
DEF VAR s-StkDis AS DEC NO-UNDO.
DEF VAR s-UndVta AS CHAR.
DEF VAR f-Factor AS DEC.
DEF VAR s-NroDec AS INT INIT 4.

DEF NEW SHARED VAR s-coddiv AS CHAR INIT '00015'.
DEF BUFFER COTIZACION FOR faccpedi.

DEF VAR pCodDiv  AS CHAR NO-UNDO.   /* Lista de Precios */


FOR EACH faccpedi WHERE codcia = 1 AND coddiv = '00015'
    AND coddoc = 'o/d' AND fmapgo = '899'
    AND fchped >= 01/01/2020
    AND flgest <> 'C'
    AND CAN-FIND(FIRST facdpedi OF faccpedi WHERE facdpedi.preuni = 0):
    DISPLAY faccpedi.coddoc faccpedi.nroped.
    ASSIGN
        pCodDiv = s-CodDiv.
    IF LOOKUP(Faccpedi.CodDoc, 'PED,P/M') > 0 THEN DO:
        FIND COTIZACION WHERE COTIZACION.codcia = Faccpedi.codcia AND
            COTIZACION.coddiv = Faccpedi.coddiv AND
            COTIZACION.coddoc = Faccpedi.codref AND
            COTIZACION.nroped = Faccpedi.nroref NO-LOCK NO-ERROR.
        IF AVAILABLE COTIZACION THEN DO:
            pCodDiv = COTIZACION.Lista_de_Precios.
            IF TRUE <> (pCodDiv > '') THEN pCodDiv = COTIZACION.Libre_c01.
            IF TRUE <> (pCodDiv > '') THEN pCodDiv = COTIZACION.CodDiv.
        END.
    END.

    FOR EACH facdpedi OF faccpedi WHERE facdpedi.preuni = 0,
        FIRST Almmmatg OF facdpedi NO-LOCK:
        /* Buscamos el precio unitario referencial */
        RUN vtagn/PrecioVentaMayorCredito.p (
            Faccpedi.TpoPed,        /*"N",        */
            pCodDiv,                /*"00001",    */
            Faccpedi.CodCli,
            Faccpedi.CodMon,
            INPUT-OUTPUT s-UndVta,
            OUTPUT f-Factor,
            Almmmatg.codmat,
            Faccpedi.FmaPgo,
            Facdpedi.CanPed,
            s-NroDec,
            OUTPUT f-PreBas,
            OUTPUT f-PreVta,
            OUTPUT f-Dsctos,
            OUTPUT y-Dsctos,
            OUTPUT z-Dsctos,
            OUTPUT x-TipDto,
            "",     /* ClfCli: lo ingresamos solo si se quiere forzar la clasificacion */
            OUTPUT f-FleteUnitario,
            "",
            NO
            ).
        ASSIGN
            facdpedi.prebas = f-prebas
            facdpedi.preuni = f-prevta.
        ASSIGN
            Facdpedi.ImpLin = Facdpedi.CanPed * Facdpedi.PreUni * 
            ( 1 - Facdpedi.Por_Dsctos[1] / 100 ) *
            ( 1 - Facdpedi.Por_Dsctos[2] / 100 ) *
            ( 1 - Facdpedi.Por_Dsctos[3] / 100 ).
        IF Facdpedi.Por_Dsctos[1] = 0 AND Facdpedi.Por_Dsctos[2] = 0 AND Facdpedi.Por_Dsctos[3] = 0 
            THEN Facdpedi.ImpDto = 0.
        ELSE Facdpedi.ImpDto = Facdpedi.CanPed * Facdpedi.PreUni - Facdpedi.ImpLin.
        IF Facdpedi.Libre_d02 > 0 THEN DO:
            /* El flete afecta el monto final */
            IF Facdpedi.ImpDto = 0 THEN DO:       /* NO tiene ningun descuento */
                ASSIGN
                    Facdpedi.PreUni = ROUND(Facdpedi.PreUni + Facdpedi.Libre_d02, s-NroDec)  /* Incrementamos el PreUni */
                    Facdpedi.ImpLin = Facdpedi.CanPed * Facdpedi.PreUni.
                END.
            ELSE DO:      /* CON descuento promocional o volumen */
                ASSIGN
                    Facdpedi.ImpLin = Facdpedi.ImpLin + (Facdpedi.CanPed * Facdpedi.Libre_d02)
                    Facdpedi.PreUni = ROUND( (Facdpedi.ImpLin + Facdpedi.ImpDto) / Facdpedi.CanPed, s-NroDec).
            END.
        END.
        /* ***************************************************************** */
        ASSIGN
            Facdpedi.ImpLin = ROUND(Facdpedi.ImpLin, 2)
            Facdpedi.ImpDto = ROUND(Facdpedi.ImpDto, 2).
        IF Facdpedi.AftIsc 
            THEN Facdpedi.ImpIsc = ROUND(Facdpedi.PreBas * Facdpedi.CanPed * (Almmmatg.PorIsc / 100),4).
        IF Facdpedi.AftIgv 
            THEN Facdpedi.ImpIgv = Facdpedi.ImpLin - ROUND( Facdpedi.ImpLin  / ( 1 + (Faccpedi.PorIgv / 100) ), 4 ).
    END.
    DEFINE VARIABLE F-IGV AS DECIMAL NO-UNDO.
    DEFINE VARIABLE F-ISC AS DECIMAL NO-UNDO.
    DEFINE VARIABLE x-Dto2xExonerados AS DEC NO-UNDO.
    DEFINE VARIABLE x-Dto2xAfectosIgv AS DEC NO-UNDO.
      
    ASSIGN
        FacCPedi.ImpDto = 0
        FacCPedi.ImpDto2 = 0
        FacCPedi.ImpIgv = 0
        FacCPedi.ImpIsc = 0
        FacCPedi.ImpTot = 0
        FacCPedi.ImpExo = 0
        FacCPedi.Importe[3] = 0
        F-IGV = 0
        F-ISC = 0
        x-Dto2xExonerados = 0
        x-Dto2xAfectosIgv = 0.
    /* RHC 15/08/19 pedido por G.P.: Impuesto a la bolsa plástica */
    ASSIGN
        Faccpedi.AcuBon[10] = 0.
    
    /* VENTAS INAFECTAS A IGV */
    IF FacCPedi.FlgIgv = NO THEN DO:
        FacCPedi.PorIgv = 0.00.
        FOR EACH FacDPedi OF FacCPedi:
            ASSIGN
                FacDPedi.AftIgv = NO
                FacDPedi.ImpIgv = 0.00.
        END.
    END.
    
    FOR EACH FacDPedi OF FacCPedi NO-LOCK, FIRST Almmmatg OF FacDPedi NO-LOCK:
        F-Igv = F-Igv + FacDPedi.ImpIgv.
        F-Isc = F-Isc + FacDPedi.ImpIsc.
    
        FacCPedi.ImpTot = FacCPedi.ImpTot + FacDPedi.ImpLin.
        FacCPedi.ImpDto2 = FacCPedi.ImpDto2 + FacDPedi.ImpDto2.
    
        /*IF NOT FacDPedi.AftIgv THEN FacCPedi.ImpExo = FacCPedi.ImpExo + FacDPedi.ImpLin.*/
    
        IF FacDPedi.AftIgv = YES
        THEN FacCPedi.ImpDto = FacCPedi.ImpDto + ROUND(FacDPedi.ImpDto / (1 + FacCPedi.PorIgv / 100), 2).
        ELSE FacCPedi.ImpDto = FacCPedi.ImpDto + FacDPedi.ImpDto.
    
        /* BOLSAS PLASTICAS */
        /* S/0.10 por cada bolsa */
        /*IF LOOKUP(Facdpedi.CodMat, '098878,098879,098880,098881,098882,098883') > 0 THEN*/
        /*
        IF Almmmatg.CodFam = '086' AND Almmmatg.SubFam = '001' THEN DO
            ASSIGN Faccpedi.AcuBon[10] = Faccpedi.AcuBon[10] + (Facdpedi.CanPed * Facdpedi.Factor * facdpedi.implin).
        */
        IF facdpedi.codmat = x-articulo-ICBPER THEN DO:
            /* Inafecto */
            ASSIGN Faccpedi.AcuBon[10] = Faccpedi.AcuBon[10] + facdpedi.implin.
        END.
        ELSE DO:
            IF NOT FacDPedi.AftIgv THEN FacCPedi.ImpExo = FacCPedi.ImpExo + FacDPedi.ImpLin.
    
            IF NOT FacDPedi.AftIgv THEN x-Dto2xExonerados = x-Dto2xExonerados + FacDPedi.ImpDto2.
            ELSE x-Dto2xAfectosIgv = x-Dto2xAfectosIgv + FacDPedi.ImpDto2.
        END.
    
    END.
    
    ASSIGN
        FacCPedi.ImpIgv = ROUND(F-IGV,2)
        FacCPedi.ImpIsc = ROUND(F-ISC,2).
    ASSIGN
        FacCPedi.ImpVta = FacCPedi.ImpTot - FacCPedi.ImpExo - FacCPedi.ImpIgv - Faccpedi.AcuBon[10].
    ASSIGN
        FacCPedi.ImpBrt = FacCPedi.ImpVta + FacCPedi.ImpDto
        FacCPedi.Importe[1] = FacCPedi.ImpTot.    /* Guardamos el importe original */
    /* RHC 06/05/2014 En caso tenga descuento por Encarte */
    IF Faccpedi.ImpDto2 > 0 THEN DO:
        ASSIGN
            Faccpedi.ImpTot = Faccpedi.ImpTot - Faccpedi.ImpDto2
            Faccpedi.ImpIgv = Faccpedi.ImpIgv -  ~
                ROUND(x-Dto2xAfectosIgv / ( 1 + Faccpedi.PorIgv / 100) * Faccpedi.PorIgv / 100, 2)
            Faccpedi.ImpExo = Faccpedi.ImpExo - x-Dto2xExonerados
            FacCPedi.ImpVta = FacCPedi.ImpTot - FacCPedi.ImpExo - FacCPedi.ImpIgv
            FacCPedi.ImpBrt = FacCPedi.ImpVta + FacCPedi.ImpDto.
    END.
    /* ******************************************** */
    /* RHC 15/08/19 IMPUESTO A LA BOLSA DE PLASTICO */
    /* ******************************************** */
    ASSIGN
        Faccpedi.ImpTot = Faccpedi.ImpTot /*+ Faccpedi.AcuBon[10]*/.


END.
