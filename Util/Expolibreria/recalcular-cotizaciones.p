DISABLE TRIGGERS FOR LOAD OF faccpedi.
DISABLE TRIGGERS FOR LOAD OF facdpedi.

DEF TEMP-TABLE ResumenxLinea
    FIELD codmat LIKE almmmatg.codmat
    FIELD codfam LIKE almmmatg.codfam
    FIELD subfam LIKE almmmatg.subfam
    FIELD canped LIKE facdpedi.canped
    INDEX Llave01 AS PRIMARY /*UNIQUE*/ codmat codfam subfam.

DEFINE TEMP-TABLE ITEM LIKE FacDPedi.
DEF TEMP-TABLE ErroresxLinea LIKE ResumenxLinea.

DEF VAR s-codmon AS INT NO-UNDO.
DEF VAR s-CodCli AS CHAR NO-UNDO.
DEF VAR s-tpocmb AS DEC NO-UNDO.
DEF VAR s-cndvta AS CHAR NO-UNDO.
DEF VAR s-porigv AS DEC NO-UNDO.
DEF VAR s-nrodec AS INT NO-UNDO.
DEF VAR s-flgigv AS LOG NO-UNDO.
DEFINE VARIABLE X-CANPED AS DECI NO-UNDO.
DEFINE VARIABLE S-UNDVTA AS CHAR NO-UNDO.
DEFINE VARIABLE F-FACTOR AS DECI NO-UNDO.
DEFINE VARIABLE F-PREBAS LIKE Almmmatg.PreBas NO-UNDO.
DEFINE VARIABLE F-PREVTA AS DECI NO-UNDO.
DEFINE VARIABLE Y-DSCTOS AS DECI NO-UNDO.
DEFINE VARIABLE Z-DSCTOS AS DECI NO-UNDO.
DEFINE VARIABLE F-DSCTOS LIKE Almmmatg.PorMax NO-UNDO.
DEFINE VARIABLE x-tipdto AS CHAR NO-UNDO.
DEFINE VARIABLE s-Cmpbnte AS CHAR NO-UNDO.
DEFINE VARIABLE x-imptot LIKE faccpedi.imptot NO-UNDO.
DEFINE VARIABLE F-IGV AS DECIMAL NO-UNDO.
DEFINE VARIABLE F-ISC AS DECIMAL NO-UNDO.
DEFINE VARIABLE f-FleteUnitario AS DEC DECIMALS 6 NO-UNDO.
DEFINE VARIABLE x-Dto2xExonerados AS DEC NO-UNDO.
DEFINE VARIABLE x-Dto2xAfectosIgv AS DEC NO-UNDO.
DEFINE VAR x-fecha-desde AS DATE.

DEF VAR s-codcia  AS INT    INIT 001        NO-UNDO.
DEF VAR s-coddiv  AS CHAR   INIT '00015'    NO-UNDO.
DEF VAR pcoddiv   AS CHAR   INIT '50015'    NO-UNDO.
DEF VAR cl-codcia AS INT    INIT 000        NO-UNDO.
DEF VAR s-coddoc  AS CHAR   INIT "COT"      NO-UNDO.
DEF VAR s-tpoped  AS CHAR   INIT "E"        NO-UNDO.
DEF VAR s-PorPercepcion AS  DEC INIT 0      NO-UNDO.

s-coddiv = "10060".     /* Division */
pcoddiv = "20060".      /* Lista de Precios */
x-fecha-desde = 09/19/2018.

FOR EACH faccpedi WHERE codcia = s-codcia
    AND coddiv = s-coddiv
    AND coddoc = s-coddoc
    AND tpoped = s-tpoped
    AND flgest <> 'A'
    AND Faccpedi.Libre_c01 = pCodDiv
    AND fchped >= x-fecha-desde /*DATE(06,01,2018)*/ :
    DISPLAY faccpedi.fchped faccpedi.nroped.
    PAUSE 0.
    x-ImpTot = Faccpedi.ImpTot.
    /* FILTROS */
    FIND gn-clie WHERE gn-clie.codcia = cl-codcia
        AND gn-clie.codcli = faccpedi.codcli
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE gn-clie THEN NEXT.
    /* ************ */
    ASSIGN
        S-CODMON = FacCPedi.CodMon
        S-CODCLI = FacCPedi.CodCli
        S-TPOCMB = FacCPedi.TpoCmb
        S-CNDVTA = FacCPedi.FmaPgo
        s-PorIgv = Faccpedi.porigv
        s-NroDec = (IF Faccpedi.Libre_d01 <= 0 THEN 4 ELSE Faccpedi.Libre_d01)
        s-FlgIgv = Faccpedi.FlgIgv
        s-Cmpbnte = Faccpedi.cmpbnte.
    FOR EACH Facdpedi OF Faccpedi, FIRST Almmmatg OF Facdpedi NO-LOCK /*WHERE Almmmatg.codfam = '012'*/ :
        ASSIGN
            x-CanPed = Facdpedi.CanPed
            s-UndVta = Facdpedi.UndVta.

        RUN vta2/PrecioMayorista-Cred-v2 (
            s-TpoPed,
            pCodDiv,
            s-CodCli,
            s-CodMon,
            INPUT-OUTPUT s-UndVta,
            OUTPUT f-Factor,
            Almmmatg.CodMat,
            s-CndVta,
            x-CanPed,
            s-NroDec,
            OUTPUT f-PreBas,
            OUTPUT f-PreVta,
            OUTPUT f-Dsctos,
            OUTPUT y-Dsctos,
            OUTPUT z-Dsctos,
            OUTPUT x-TipDto,
            OUTPUT f-FleteUnitario,
            FacDPedi.TipVta,
            FALSE
            ).
        ASSIGN 
            Facdpedi.Factor = F-FACTOR
            Facdpedi.PorDto = F-DSCTOS      /* Ambos descuentos afectan */
            Facdpedi.PorDto2 = 0            /* el precio unitario */
            Facdpedi.PreBas = F-PreBas 
            Facdpedi.PreVta[1] = F-PreVta     /* CONTROL DE PRECIO DE LISTA */
            Facdpedi.UndVta = s-UndVta
            Facdpedi.PreUni = F-PREVTA
            Facdpedi.Libre_d02 = f-FleteUnitario    /* Flete Unitario */
            Facdpedi.Por_Dsctos[1] = 0
            Facdpedi.Por_Dsctos[2] = z-Dsctos
            Facdpedi.Por_Dsctos[3] = y-Dsctos
            Facdpedi.AftIgv = Almmmatg.AftIgv
            Facdpedi.AftIsc = Almmmatg.AftIsc
            Facdpedi.Libre_c04 = x-TipDto
            Facdpedi.ImpIsc = 0
            Facdpedi.ImpIgv = 0.
        ASSIGN
            Facdpedi.ImpLin = ROUND ( Facdpedi.CanPed * Facdpedi.PreUni * 
                          ( 1 - Facdpedi.Por_Dsctos[1] / 100 ) *
                          ( 1 - Facdpedi.Por_Dsctos[2] / 100 ) *
                          ( 1 - Facdpedi.Por_Dsctos[3] / 100 ), 2 ).
        IF Facdpedi.Por_Dsctos[1] = 0 AND Facdpedi.Por_Dsctos[2] = 0 AND Facdpedi.Por_Dsctos[3] = 0 
            THEN Facdpedi.ImpDto = 0.
            ELSE Facdpedi.ImpDto = Facdpedi.CanPed * Facdpedi.PreUni - Facdpedi.ImpLin.
        /* RHC 04/08/2015 Si existe f-FleteUnitario se recalcula el Descuento */
        IF f-FleteUnitario > 0 THEN DO:

            /* Ic - 28Set2018, si el flete debe considerarse, segun Karin Roden.... */
            RUN gn/factor-porcentual-flete(INPUT facdpedi.codmat, INPUT-OUTPUT f-FleteUnitario, INPUT s-TpoPed, INPUT f-factor, INPUT s-CodMon).

            /* El flete afecta el monto final */
            IF Facdpedi.ImpDto = 0 THEN DO:       /* NO tiene ningun descuento */
                ASSIGN
                    Facdpedi.PreUni = ROUND(f-PreVta + f-FleteUnitario, s-NroDec)  /* Incrementamos el PreUni */
                    Facdpedi.ImpLin = Facdpedi.CanPed * Facdpedi.PreUni.
            END.
            ELSE DO:      /* CON descuento promocional o volumen */
                ASSIGN
                    Facdpedi.ImpLin = Facdpedi.ImpLin + (Facdpedi.CanPed * f-FleteUnitario)
                    Facdpedi.PreUni = ROUND( (Facdpedi.ImpLin + Facdpedi.ImpDto) / Facdpedi.CanPed, s-NroDec).
            END.
        END.
        /* ***************************************************************** */
        ASSIGN
            Facdpedi.ImpLin = ROUND(Facdpedi.ImpLin, 2)
            Facdpedi.ImpDto = ROUND(Facdpedi.ImpDto, 2).
        IF Facdpedi.AftIsc 
        THEN Facdpedi.ImpIsc = ROUND(Facdpedi.PreBas * Facdpedi.CanPed * (Almmmatg.PorIsc / 100),4).
        ELSE Facdpedi.ImpIsc = 0.
        IF Facdpedi.AftIgv 
        THEN Facdpedi.ImpIgv = Facdpedi.ImpLin - ROUND( Facdpedi.ImpLin  / ( 1 + (s-PorIgv / 100) ), 4 ).
        ELSE Facdpedi.ImpIgv = 0.

         /* ********* */
         /* RHC 07/11/2013 CALCULO DE PERCEPCION */
         ASSIGN
             Facdpedi.CanSol = 0
             Facdpedi.CanApr = 0.
         FIND FIRST Vtatabla WHERE Vtatabla.codcia = s-codcia
             AND Vtatabla.tabla = 'CLNOPER'
             AND VtaTabla.Llave_c1 = s-CodCli
             NO-LOCK NO-ERROR.
         IF NOT AVAILABLE Vtatabla THEN DO:
             IF gn-clie.Libre_L01 = YES AND gn-clie.RucOld <> "SI" THEN s-Porpercepcion = 0.5.
             IF gn-clie.Libre_L01 = NO AND gn-clie.RucOld <> "SI" THEN s-Porpercepcion = 2.
             /* Ic 04 Julio 2013 
                 gn-clie.Libre_L01   : PERCEPCTOR
                 gn-clie.RucOld      : RETENEDOR
             */
             IF s-Cmpbnte = "BOL" THEN s-Porpercepcion = 2.
             FIND Almsfami OF Almmmatg NO-LOCK NO-ERROR.
             IF AVAILABLE Almsfami AND Almsfami.Libre_c05 = "SI" THEN
                 ASSIGN
                 Facdpedi.CanSol = s-PorPercepcion
                 Facdpedi.CanApr = ROUND(Facdpedi.implin * s-PorPercepcion / 100, 2).
         END.
    END.
    RUN Descuentos-Finales-01.
    RUN Descuentos-Finales-02.
    /* RHC DESCUENTOS ESPECIALES SOLO CAMPAÑA */
    RUN Descuentos-solo-campana.
    /* RHC 06/07/17 Descuentos por saldo resumido solo para Expolibreria */
    RUN Descuentos-Finales-04.
    /* **************************************************************** */
    /* RHC 18/11/2015 RUTINA ESPECIAL PARA LISTA DE PRECIOS DE TERCEROS */
    /* **************************************************************** */
    RUN Lista-Terceros NO-ERROR.
    IF ERROR-STATUS:ERROR THEN UNDO, RETURN 'ADM-ERROR'.
    /* **************************************************************** */
    /* CALCULAMOS TOTAL COTIZACION */
    ASSIGN
      FacCPedi.ImpDto = 0
      FacCPedi.ImpIgv = 0
      FacCPedi.ImpIsc = 0
      FacCPedi.ImpTot = 0
      FacCPedi.ImpExo = 0
      FacCPedi.Importe[3] = 0
      F-IGV = 0
      F-ISC = 0
      x-Dto2xExonerados = 0
      x-Dto2xAfectosIgv = 0.

    /* VENTAS INAFECTAS A IGV */
    IF FacCPedi.FlgIgv = NO THEN DO:
        FacCPedi.PorIgv = 0.00.
        FOR EACH FacDPedi OF FacCPedi:
            ASSIGN
                FacDPedi.AftIgv = NO
                FacDPedi.ImpIgv = 0.00.
        END.
    END.
    FOR EACH FacDPedi OF FacCPedi:
        ASSIGN
            F-Igv = F-Igv + FacDPedi.ImpIgv
            F-Isc = F-Isc + FacDPedi.ImpIsc
            FacCPedi.ImpTot = FacCPedi.ImpTot + FacDPedi.ImpLin.
        /* Importe Inafecto o Exonerado */
        IF FacDPedi.ImpIgv = 0 THEN FacCPedi.ImpExo = FacCPedi.ImpExo + FacDPedi.ImpLin.
    END.
    ASSIGN
        FacCPedi.ImpIgv = ROUND(F-IGV,2)
        FacCPedi.ImpIsc = ROUND(F-ISC,2)
        FacCPedi.ImpVta = ROUND( (FacCPedi.ImpTot - FacCPedi.ImpExo) / (1 + FacCPedi.PorIgv / 100), 2)
        FacCPedi.ImpBrt = FacCPedi.ImpVta /*+ FacCPedi.ImpIsc*/ + FacCPedi.ImpDto /*+ FacCPedi.ImpExo*/
        FacCPedi.Importe[1] = FacCPedi.ImpTot.    /* Guardamos el importe original */
    IF FacCPedi.FlgIgv = NO 
        THEN ASSIGN
            FacCPedi.ImpVta = FacCPedi.ImpExo
            FacCPedi.ImpBrt = FacCPedi.ImpExo.
    /* RHC 17/02/2016 En caso tenga descuento por LISTA EXPRESS */
    /* Se supone que el descuento se aplica a todos los productos por igual */
    IF Faccpedi.PorDto > 0 AND Faccpedi.ImpDto2 = 0 THEN DO:
        Faccpedi.ImpDto2 = ROUND(Faccpedi.ImpTot * Faccpedi.PorDto / 100, 2).
    END.
    IF Faccpedi.ImpDto2 > 0 AND Faccpedi.PorDto = 0 THEN DO:
        Faccpedi.PorDto = Faccpedi.ImpDto2 / Faccpedi.ImpTot * 100.
    END.
    IF Faccpedi.ImpDto2 > 0 THEN DO:
        x-Dto2xExonerados = Faccpedi.ImpExo * Faccpedi.PorDto / 100.
        x-Dto2xAfectosIgv = (Faccpedi.ImpTot - Faccpedi.ImpExo) * Faccpedi.PorDto / 100.
        IF Faccpedi.ImpExo = 0 THEN 
            ASSIGN
            x-Dto2xExonerados = 0
            x-Dto2xAfectosIgv = Faccpedi.ImpDto2.
        /* Guardamos Importe Dcto Lista Express pero SIN IGV, se va al TCK Ccbcdocu.Libre_d01 */
        ASSIGN
            Faccpedi.Importe[3] = ROUND(x-Dto2xExonerados + (x-Dto2xAfectosIgv / (1 + Faccpedi.PorIgv / 100)), 2).
        ASSIGN
            Faccpedi.ImpTot = Faccpedi.ImpTot - Faccpedi.ImpDto2
            Faccpedi.ImpIgv = Faccpedi.ImpIgv -  ~
            ROUND(x-Dto2xAfectosIgv / ( 1 + Faccpedi.PorIgv / 100) * Faccpedi.PorIgv / 100, 2)
            Faccpedi.ImpExo = Faccpedi.ImpExo - x-Dto2xExonerados
            FacCPedi.ImpVta = FacCPedi.ImpTot - FacCPedi.ImpExo - FacCPedi.ImpIgv
            FacCPedi.ImpBrt = FacCPedi.ImpVta + FacCPedi.ImpDto.
        DEF VAR x-ARepartir AS DEC NO-UNDO.
        x-ARepartir = Faccpedi.ImpDto2.
        FOR EACH Facdpedi OF Faccpedi WHERE Facdpedi.codmat <> '044939'   /* Menos FLETE */
            BREAK BY Facdpedi.codcia:
            ASSIGN
                Facdpedi.PorDto2 = Faccpedi.PorDto
                Facdpedi.ImpDto2 = ROUND ( Facdpedi.ImpLin * Facdpedi.PorDto2 / 100, 2).
            x-ARepartir = x-ARepartir - Facdpedi.ImpDto2.
            IF LAST-OF(Facdpedi.CodCia) THEN ASSIGN Facdpedi.ImpDto2 = Facdpedi.ImpDto2 + x-ARepartir.
        END.
    END.
    /* CALCULAMOS LA PERCEPCION */
    RUN vta2/percepcion-por-pedido ( ROWID(Faccpedi)).
    /* RHC 22/07/2016 TRANSFERENCIAS GRATUITAS */
    IF FacCPedi.FmaPgo = "900" THEN
        ASSIGN
        FacCPedi.ImpBrt = 0
        FacCPedi.ImpExo = FacCPedi.ImpTot
        FacCPedi.ImpDto = 0
        FacCPedi.ImpVta = 0
        FacCPedi.ImpIgv = 0.
    /* *************************************** */
    DISPLAY faccpedi.fchped faccpedi.nroped x-imptot faccpedi.imptot.
    PAUSE 0.
END.

PROCEDURE Descuentos-Finales-01:

    {vta2/descuentoxvolumenxlinearesumida.i}

END PROCEDURE.

PROCEDURE Descuentos-Finales-02:

    {vta2/descuentoxvolumenxsaldosresumidav2.i}

END PROCEDURE.

PROCEDURE Descuentos-Finales-04:

    {vta2/expodtoxvolxsaldoresumido.i}

END PROCEDURE.

PROCEDURE Descuentos-solo-campana:

        {vta2/descuento-solo-campana.i}

END PROCEDURE.


PROCEDURE Lista-Terceros:

    DEFINE VARIABLE f-FleteUnitario AS DEC DECIMALS 6 NO-UNDO.
    DEFINE VARIABLE S-UNDVTA AS CHAR NO-UNDO.
    DEFINE VARIABLE F-FACTOR AS DECI NO-UNDO.
    DEFINE VARIABLE F-PREVTA AS DECI NO-UNDO.
    DEFINE VARIABLE Y-DSCTOS AS DECI NO-UNDO.
    DEFINE VARIABLE Z-DSCTOS AS DECI NO-UNDO.
    DEFINE VARIABLE F-DSCTOS LIKE Almmmatg.PorMax NO-UNDO.
    DEFINE VARIABLE x-TipDto AS CHAR NO-UNDO.

    /* 1er Filtro: La lista de precios NO debe ser precios de FERIA */
    FIND gn-divi WHERE gn-divi.codcia = s-codcia AND gn-divi.coddiv = pCodDiv NO-LOCK.
    IF GN-DIVI.CanalVenta = 'FER' THEN RETURN.
    /* 2do Filtro: La venta debe ser una venta Normal o Provincias */
    IF LOOKUP(s-TpoPed, 'P,N') = 0 THEN RETURN.
    /* Verificamos el cliente de acuerdo a la división de origen */
    /*IF LOOKUP(s-CodDiv, '00018,00019') = 0 THEN DO:     /* NI PROVINCIAS NI MESA REDONDA */*/
    /* RHC 29/02/2016 Quitamos */
    IF LOOKUP(s-CodDiv, '00018') = 0 THEN DO:     /* NO PROVINCIAS  */
        /* Buscamos clientes VIP */
        FIND FacTabla WHERE FacTabla.CodCia = s-codcia
            AND FacTabla.Tabla = "VIP3ROS"
            AND FacTabla.Codigo = Faccpedi.CodCli
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE FacTabla THEN RETURN.
    END.
    /* ************************************************************* */
    /* SE VA A DAR HASTA DOS VUELTAS PARA DETERMINAR LA LISTA A USAR */
    /* ************************************************************* */
    /* Actualizamos datos del temporal */
    EMPTY TEMP-TABLE ITEM.
    FOR EACH Facdpedi OF Faccpedi NO-LOCK WHERE Facdpedi.Libre_c05 <> "OF":
        CREATE ITEM.
        BUFFER-COPY Facdpedi TO ITEM.
    END.
    /* ******************************* */
    DEF VAR x-Ciclos AS INT NO-UNDO.
    DEF VAR x-ImpTot AS DEC NO-UNDO.
    DEF VAR x-ListaTerceros AS INT INIT 0 NO-UNDO.  /* Lista por defecto */
    DEF VAR x-ListaAnterior AS INT NO-UNDO.
    DEF VAR F-PREBAS AS DEC NO-UNDO.
    DEF VAR S-TPOCMB AS DEC NO-UNDO.
    FIND gn-clie WHERE gn-clie.codcia = cl-codcia
        AND gn-clie.codcli = Faccpedi.codcli
        NO-LOCK.
    x-ListaAnterior = gn-clie.Libre_d01.
    /* *********** */
    IF x-ListaAnterior > 3 THEN x-ListaAnterior = 3.    /* Valor Máximo */
    /* RHC 14/12/2015 Tomamos el valor por defecto en el cliente, puede ser de 0 a 3 */
    x-ListaTerceros = x-ListaAnterior.
    /* ***************************************************************************** */
    FIND FIRST FacTabla WHERE FacTabla.CodCia = s-codcia
        AND FacTabla.Tabla = 'RLP3ROS' NO-LOCK NO-ERROR.
    IF NOT AVAILABLE FacTabla THEN RETURN.
    DO x-Ciclos = 1 TO 2:
        /* Tomamos el importe final */
        x-ImpTot = 0.
        FOR EACH ITEM:
            x-ImpTot = x-ImpTot + ITEM.ImpLin.
        END.
        IF Faccpedi.codmon = 2 THEN x-ImpTot = x-ImpTot * Faccpedi.TpoCmb.
        /* Decidimos cual lista tomar */
        DEF VAR k AS INT NO-UNDO.
        DO k = 1 TO 3:
            IF k < 3 AND x-ImpTot >= FacTabla.Valor[k] AND x-ImpTot < FacTabla.Valor[k + 1] THEN DO:
                x-ListaTerceros = k.
                LEAVE.
            END.
            IF k = 3 AND x-ImpTot >= FacTabla.Valor[k] THEN x-ListaTerceros = k.
        END.
        /* Tomamos el mejor */
        x-ListaTerceros = MAXIMUM(x-ListaAnterior,x-ListaTerceros).
        IF x-ListaTerceros = 0 THEN RETURN. /* El importe de venta NO llega al mínimo necesario */
        IF x-ListaTerceros > 3 THEN x-ListaTerceros = 3.    /* Valor Máximo */
        /* Actualizamos Precios de Venta */
        FOR EACH ITEM WHERE ITEM.Libre_c05 <> "OF", 
            FIRST Almmmatg OF ITEM NO-LOCK,
            FIRST ListaTerceros OF ITEM NO-LOCK:
            IF ListaTerceros.PreOfi[x-ListaTerceros] = 0 THEN NEXT.
            F-PREBAS = ListaTerceros.PreOfi[x-ListaTerceros].
            S-TPOCMB = Almmmatg.TpoCmb.
            IF Faccpedi.CodMon = 1 THEN DO:
                IF Almmmatg.MonVta = 1 THEN ASSIGN F-PREBAS = F-PREBAS /** F-FACTOR*/.
                ELSE ASSIGN F-PREBAS = F-PREBAS * S-TPOCMB /** F-FACTOR*/.
            END.
            IF Faccpedi.CodMon = 2 THEN DO:
                IF Almmmatg.MonVta = 2 THEN ASSIGN F-PREBAS = F-PREBAS /** F-FACTOR*/.
                ELSE ASSIGN F-PREBAS = (F-PREBAS / S-TPOCMB) /** F-FACTOR*/.
            END.
            ASSIGN
                ITEM.Por_Dsctos[1] = 0
                ITEM.Por_Dsctos[2] = 0
                ITEM.Por_Dsctos[3] = 0
                ITEM.PreBas = F-PREBAS
                ITEM.PreUni = F-PREBAS
                ITEM.Libre_c04 = "LP3ROS"
                ITEM.Libre_d01 = x-ListaTerceros.
            /* Recalculamos registro */
            /*MESSAGE 'la cagada' f-prebas.*/
            ASSIGN
                ITEM.ImpLin = ROUND ( ITEM.CanPed * ITEM.PreUni * 
                              ( 1 - ITEM.Por_Dsctos[1] / 100 ) *
                              ( 1 - ITEM.Por_Dsctos[2] / 100 ) *
                              ( 1 - ITEM.Por_Dsctos[3] / 100 ), 2 ).
            /* ****************************************** */
            /* RHC 15/12/2015 AGREGAMOS EL FLETE UNITARIO */
            /* ****************************************** */
            ASSIGN
                ITEM.ImpDto = 0
                ITEM.Libre_d02 = 0
                f-FleteUnitario = 0.
            RUN vta2/PrecioMayorista-Cred-v2 (
                Faccpedi.TpoPed,
                pCodDiv,
                Faccpedi.CodCli,
                Faccpedi.CodMon,
                INPUT-OUTPUT s-UndVta,
                OUTPUT f-Factor,
                ITEM.CodMat,
                Faccpedi.FmaPgo,
                ITEM.CanPed,
                Faccpedi.Libre_d01,
                OUTPUT f-PreBas,
                OUTPUT f-PreVta,
                OUTPUT f-Dsctos,
                OUTPUT y-Dsctos,
                OUTPUT z-Dsctos,
                OUTPUT x-TipDto,
                OUTPUT f-FleteUnitario,
                ITEM.TipVta,
                NO
                ).
            IF RETURN-VALUE <> 'ADM-ERROR' AND f-FleteUnitario > 0 THEN DO:
                ASSIGN
                    ITEM.Libre_d02 = f-FleteUnitario.
                /* El flete afecta el monto final */
                IF ITEM.ImpDto = 0 THEN DO:       /* NO tiene ningun descuento */
                    ASSIGN
                        ITEM.PreUni = ROUND(ITEM.PreUni + ITEM.Libre_d02, s-NroDec)  /* Incrementamos el PreUni */
                        ITEM.ImpLin = ITEM.CanPed * ITEM.PreUni.
                END.
                ELSE DO:      /* CON descuento promocional o volumen */
                    ASSIGN
                        ITEM.ImpLin = ITEM.ImpLin + (ITEM.CanPed * f-FleteUnitario)
                        ITEM.PreUni = ROUND( (ITEM.ImpLin + ITEM.ImpDto) / ITEM.CanPed, s-NroDec).
                END.
            END.
            /* ****************************************** */
            /* ****************************************** */
            ASSIGN
                ITEM.ImpLin = ROUND(ITEM.ImpLin, 2)
                ITEM.ImpDto = ROUND(ITEM.ImpDto, 2).
            IF ITEM.AftIsc 
            THEN ITEM.ImpIsc = ROUND(ITEM.PreBas * ITEM.CanPed * (Almmmatg.PorIsc / 100),4).
            ELSE ITEM.ImpIsc = 0.
            IF ITEM.AftIgv 
            THEN ITEM.ImpIgv = ITEM.ImpLin - ROUND( ITEM.ImpLin  / ( 1 + (s-PorIgv / 100) ), 4 ).
            ELSE ITEM.ImpIgv = 0.
        END.
    END.
    IF x-ListaTerceros = 0 THEN RETURN.
    /* Ahora sí grabamos la información */
    DO TRANSACTION ON ERROR UNDO, RETURN ERROR ON STOP UNDO, RETURN ERROR:
        FOR EACH ITEM WHERE ITEM.Libre_c04 = "LP3ROS", FIRST Almmmatg OF ITEM NO-LOCK,
            FIRST Almsfami OF Almmmatg NO-LOCK:
            FIND Facdpedi OF Faccpedi WHERE Facdpedi.codmat = ITEM.codmat
                AND Facdpedi.Libre_c05 <> "OF" EXCLUSIVE-LOCK NO-ERROR.
            IF NOT AVAILABLE Facdpedi THEN DO:
                MESSAGE 'NO se pudo bloquear el registro del código:' ITEM.codmat
                    VIEW-AS ALERT-BOX ERROR.
                UNDO, RETURN ERROR.
            END.
            BUFFER-COPY ITEM TO Facdpedi.
            /* RHC 07/11/2013 CALCULO DE PERCEPCION */
            DEF VAR s-PorPercepcion AS DEC INIT 0 NO-UNDO.
            ASSIGN
                Facdpedi.CanSol = 0
                Facdpedi.CanApr = 0.
            FIND FIRST Vtatabla WHERE Vtatabla.codcia = s-codcia
                AND Vtatabla.tabla = 'CLNOPER'
                AND VtaTabla.Llave_c1 = s-CodCli
                NO-LOCK NO-ERROR.
            IF NOT AVAILABLE Vtatabla THEN DO:
                IF gn-clie.Libre_L01 = YES AND gn-clie.RucOld <> "SI" THEN s-Porpercepcion = 0.5.
                IF gn-clie.Libre_L01 = NO AND gn-clie.RucOld <> "SI" THEN s-Porpercepcion = 2.
                /* Ic 04 Julio 2013 
                    gn-clie.Libre_L01   : PERCEPCTOR
                    gn-clie.RucOld      : RETENEDOR
                */
                IF s-Cmpbnte = "BOL" THEN s-Porpercepcion = 2.
                IF Almsfami.Libre_c05 = "SI" THEN
                    ASSIGN
                    Facdpedi.CanSol = s-PorPercepcion
                    Facdpedi.CanApr = ROUND(Facdpedi.implin * s-PorPercepcion / 100, 2).
            END.
        END.
        ASSIGN
            FacCPedi.TipBon[10] = x-ListaTerceros.      /* Control de Lista de Terceros */
    END.

END PROCEDURE.
