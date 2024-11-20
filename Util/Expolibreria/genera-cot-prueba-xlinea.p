DEF VAR s-codcia AS INTE INIT 001 NO-UNDO.
DEF VAR cl-codcia AS INT INIT 000 NO-UNDO.
DEF VAR s-coddiv AS CHAR INIT '10060' NO-UNDO.
DEF VAR pcoddiv  AS CHAR INIT '10060' NO-UNDO.
DEF VAR s-codcli AS CHAR INIT 'SYS00000001' NO-UNDO.
DEF VAR x-fmapgo AS CHAR INIT '001' NO-UNDO.
DEF VAR s-coddoc AS CHAR INIT 'COT' NO-UNDO.
DEF VAR s-nroser AS INT INIT 160 NO-UNDO.
DEF VAR s-codalm AS CHAR INIT '60' NO-UNDO.
DEF VAR s-TpoPed AS CHAR INIT 'E' NO-UNDO.
DEF VAR x-clfcli AS CHAR INIT 'C' NO-UNDO.
DEF VAR s-clfcli AS CHAR NO-UNDO.
DEF VAR s-codven AS CHAR INIT '020' NO-UNDO.
DEF VAR s-PorIgv AS DEC INIT 18 NO-UNDO.
DEF VAR s-user-id AS CHAR INIT 'VTA-15' NO-UNDO.
DEF VAR s-codmon AS INT INIT 1 NO-UNDO.
DEF VAR s-nrodec AS INT INIT 4 NO-UNDO.
DEF VAR s-flgigv AS LOG INIT YES NO-UNDO.
DEF VAR s-tpocmb AS DEC INIT 1 NO-UNDO.
DEF VAR s-Cmpbnte AS CHAR INIT 'BOL' NO-UNDO.
DEF VAR s-PorPercepcion AS DEC INIT 0 NO-UNDO.
DEF VAR s-codfam AS CHAR INIT '010' NO-UNDO.
DEF VAR x-codfam AS CHAR NO-UNDO.

DEFINE VARIABLE S-UNDBAS AS CHAR NO-UNDO.
DEFINE VARIABLE S-UNDVTA AS CHAR NO-UNDO.
DEFINE VARIABLE F-FACTOR AS DECI NO-UNDO.
DEFINE VARIABLE F-PREBAS LIKE Almmmatg.PreBas NO-UNDO.
DEFINE VARIABLE F-DSCTOS LIKE Almmmatg.PorMax NO-UNDO.
DEFINE VARIABLE X-CANPED AS DECI NO-UNDO.
DEFINE VARIABLE F-PREVTA AS DECI NO-UNDO.
DEFINE VARIABLE I-NroItm AS INTE NO-UNDO.
DEFINE VARIABLE Y-DSCTOS AS DECI NO-UNDO.
DEFINE VARIABLE Z-DSCTOS AS DECI NO-UNDO.
DEFINE VARIABLE x-TipDto AS CHAR NO-UNDO.
DEFINE VARIABLE f-FleteUnitario AS DEC DECIMALS 6 NO-UNDO.

DEF VAR k AS INT NO-UNDO.
DEF VAR j AS INT NO-UNDO.
DEF VAR I-NITEM AS INT NO-UNDO.

DEF TEMP-TABLE ResumenxLinea
    FIELD codmat LIKE almmmatg.codmat
    FIELD codfam LIKE almmmatg.codfam
    FIELD subfam LIKE almmmatg.subfam
    FIELD canped LIKE facdpedi.canped
    INDEX Llave01 AS PRIMARY /*UNIQUE*/ codmat codfam subfam.

DEF TEMP-TABLE ErroresxLinea LIKE ResumenxLinea.

FIND gn-clie WHERE gn-clie.codcia = cl-codcia AND gn-clie.codcli = s-codcli.
DO  k = 1 TO NUM-ENTRIES(s-codfam):
    x-codfam = ENTRY(k, s-codfam).
    FIND almtfami WHERE almtfami.codcia = s-codcia
        AND almtfami.codfam = x-codfam
        NO-LOCK.
    FIND TcmbCot WHERE  TcmbCot.Codcia = 0
        AND  (TcmbCot.Rango1 <= TODAY - TODAY + 1
        AND   TcmbCot.Rango2 >= TODAY - TODAY + 1)
        NO-LOCK NO-ERROR.
    IF AVAIL TcmbCot THEN S-TPOCMB = TcmbCot.TpoCmb.  

    DO j = 1 TO NUM-ENTRIES(x-clfcli):
        s-clfcli = ENTRY(j, x-clfcli).
        ASSIGN
            gn-clie.clfcli = s-ClfCli.
        /* Creamos una cotizacion x cada forma de pago */
        {vtagn/i-faccorre-01.i &Codigo = s-coddoc &Serie = s-nroser}
        CREATE Faccpedi.
        ASSIGN 
            FacCPedi.CodCia = S-CODCIA
            FacCPedi.CodDiv = S-CODDIV
            FacCPedi.CodDoc = s-coddoc 
            FacCPedi.CodAlm = s-CodAlm    /* Lista de Almacenes Válidos de Venta */
            FacCPedi.FchPed = TODAY 
            FacCPedi.NroPed = STRING(FacCorre.NroSer,"999") + STRING(FacCorre.Correlativo,"999999")
            FacCPedi.TpoPed = s-TpoPed
            FacCPedi.Glosa  = "CLIENTE CLASIFICACION: " + s-ClfCli
            FacCPedi.FlgEst = "P".    /* APROBADO */
        ASSIGN
            Faccpedi.codcli = s-codcli
            Faccpedi.ruccli = s-codcli
            Faccpedi.nomcli = 'FAMILIA: ' + x-codfam + ' ' + almtfami.desfam
            Faccpedi.codven = s-codven
            Faccpedi.fchped = TODAY
            Faccpedi.fchven = TODAY + 7
            Faccpedi.fmapgo = x-fmapgo
            Faccpedi.usuario = s-user-id
            Faccpedi.codmon = s-codmon
            Faccpedi.tpocmb = s-tpocmb
            FacCPedi.PorIgv = s-PorIgv
            FacCPedi.Hora = STRING(TIME,"HH:MM")
            FacCPedi.Libre_c01 = pCodDiv
            Faccpedi.cmpbnte = s-Cmpbnte
            Faccpedi.flgigv = s-flgigv
            Faccpedi.libre_d01 = s-nrodec.
        ASSIGN
            FacCorre.Correlativo = FacCorre.Correlativo + 1.
        I-NITEM = 0.
        FOR EACH vtacatventa NO-LOCK WHERE vtacatventa.codcia = s-codcia AND vtacatventa.coddiv = pcoddiv,
            EACH almcatvtac OF vtacatventa NO-LOCK,
            EACH almcatvtad OF almcatvtac NO-LOCK ,
            FIRST Almmmatg OF Almcatvtad NO-LOCK WHERE Almmmatg.codfam = x-codfam
            BY AlmCatVtaD.NroSec:
            I-NITEM = I-NITEM + 1.
            CREATE Facdpedi.
            ASSIGN
                Facdpedi.NroItm = I-NITEM
                FacDPedi.CodCia = FacCPedi.CodCia
                FacDPedi.CodDiv = FacCPedi.CodDiv
                FacDPedi.coddoc = FacCPedi.coddoc
                FacDPedi.NroPed = FacCPedi.NroPed
                FacDPedi.FchPed = FacCPedi.FchPed
                FacDPedi.Hora   = FacCPedi.Hora 
                FacDPedi.FlgEst = FacCPedi.FlgEst
                Facdpedi.codmat = Almcatvtad.codmat
                Facdpedi.undvta = Almmmatg.undbas
                Facdpedi.canped = 100
                Facdpedi.Factor = 1.
            ASSIGN 
                F-FACTOR = 1
                X-CANPED = 1.
            RUN vta2/PrecioMayorista-Cred-v2 (
                s-TpoPed,
                pCodDiv,
                s-CodCli,
                s-CodMon,
                INPUT-OUTPUT s-UndVta,
                OUTPUT f-Factor,
                Facdpedi.codmat,
                x-fmapgo,
                x-CanPed,
                s-NroDec,
                OUTPUT f-PreBas,
                OUTPUT f-PreVta,
                OUTPUT f-Dsctos,
                OUTPUT y-Dsctos,
                OUTPUT z-Dsctos,
                OUTPUT x-TipDto,
                OUTPUT f-FleteUnitario,
                "",
                NO
                ).
            ASSIGN
                Facdpedi.UndVta = s-UndVta
                Facdpedi.PreUni = f-PreVta
                Facdpedi.Libre_d02 = f-FleteUnitario    /* Flete Unitario */
                Facdpedi.Factor = F-FACTOR
                Facdpedi.PorDto = f-Dsctos
                Facdpedi.PreBas = F-PreBas 
                Facdpedi.PreVta[1] = F-PreVta     /* CONTROL DE PRECIO DE LISTA */
                Facdpedi.AftIgv = Almmmatg.AftIgv
                Facdpedi.AftIsc = Almmmatg.AftIsc
                Facdpedi.Libre_c04 = x-TipDto
                Facdpedi.Por_Dsctos[2] = z-Dsctos
                Facdpedi.Por_Dsctos[3] = y-Dsctos
                Facdpedi.Libre_c04 = x-TipDto.
            IF Facdpedi.AlmDes = "" THEN Facdpedi.AlmDes = ENTRY(1, s-CodAlm).
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

        {vta2/graba-totales-cotizacion-cred.i}

    END.
END.


PROCEDURE Descuentos-Finales-01:

    {vta2/descuentoxvolumenxlinearesumida.i}

END PROCEDURE.

PROCEDURE Descuentos-Finales-02:

    {vta2/descuentoxvolumenxsaldosresumida.i}

END PROCEDURE.

