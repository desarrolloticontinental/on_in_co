DEF VAR s-codcia AS INT INIT 001.
DEF VAR s-coddoc AS CHAR INIT 'P/M'.
DEF VAR s-nroser AS INT INIT 123.
DEF VAR s-coddiv AS CHAR INIT '00023'.
DEF VAR x-Rpta AS CHAR.
DEF VAR x-Correlativo LIKE Faccorre.correlativo NO-UNDO.
DEFINE VARIABLE s-codref   AS CHAR INITIAL "C/M".
DEFINE VARIABLE s-PorIgv   LIKE Faccpedi.PorIgv.
DEF VAR s-codalm AS CHAR INIT "10".
DEF VAR s-user-id AS CHAR INIT 'ADMIN'.
DEF VAR s-codmon AS INT INIT 1.

DEFINE VARIABLE S-TPOCMB  AS DEC.
DEFINE VARIABLE S-UNDVTA AS CHAR NO-UNDO.
DEFINE VARIABLE S-FLGSIT  AS CHAR.
DEFINE VARIABLE s-codbko AS CHAR.        /*Banco*/
DEFINE VARIABLE s-Tarjeta AS CHAR.       /*Tarjeta*/
DEFINE VARIABLE s-CodPro AS CHAR.
DEFINE VARIABLE s-NroVale AS CHAR.
DEFINE VARIABLE F-PREVTA AS DECI NO-UNDO.
DEFINE VARIABLE Y-DSCTOS AS DECI NO-UNDO.
DEFINE VARIABLE Z-DSCTOS AS DECI NO-UNDO.
DEFINE VARIABLE X-TIPDTO AS CHAR NO-UNDO.
DEFINE VARIABLE S-CODIGV  AS INT INIT 1.
DEFINE VARIABLE I-NPEDI AS INTEGER NO-UNDO INIT 0.

FIND FacCfgGn WHERE FacCfgGn.CodCia = S-CODCIA NO-LOCK NO-ERROR.

DISABLE TRIGGERS FOR LOAD OF faccpedi.
DISABLE TRIGGERS FOR LOAD OF facdpedi.

DO TRANSACTION ON ERROR UNDO, RETURN ERROR ON STOP UNDO, RETURN ERROR:
    {vtagn/i-faccorre-01.i &Codigo = s-CodDoc &Serie = s-NroSer}

    CREATE faccpedi.
    ASSIGN
        x-Correlativo = Faccorre.Correlativo.
    ASSIGN 
      Faccpedi.CodCia = S-CODCIA
      Faccpedi.CodDoc = s-coddoc 
      Faccpedi.CodRef = s-codref
      Faccpedi.FchPed = TODAY 
      Faccpedi.PorIgv = FacCfgGn.PorIgv 
      Faccpedi.NroPed = STRING(s-NroSer,"999") + STRING(x-Correlativo,"999999")
      Faccpedi.CodDiv = S-CODDIV
      Faccpedi.TipVta = '1'
      Faccpedi.FlgEst = "P"
      Faccpedi.FlgEnv = NO
      Faccpedi.CodMon = s-CodMon
      Faccpedi.codcli = '11111111111'
      Faccpedi.flgigv = YES
      Faccpedi.fchped = TODAY
      Faccpedi.fmapgo = '000'.
    ASSIGN
        s-PorIgv = FacCfgGn.PorIgv .
    /* LA VARIABLE s-codalm ES EN REALIDAD UNA LISTA DE ALMACENES VALIDOS */
    ASSIGN
        FacCPedi.CodAlm = ENTRY (1, s-CodAlm).
    /* ****************************************************************** */
    ASSIGN
      FacCorre.Correlativo = x-Correlativo + 1.

    /* Actualizamos la hora cuando lo vuelve a modificar */
    ASSIGN
        Faccpedi.Hora   = STRING(TIME,"HH:MM")
        Faccpedi.Usuario = S-USER-ID.

    /* Detalle del Pedido */
    FOR EACH vtalistamingn NO-LOCK WHERE vtalistamingn.codcia = s-codcia
        AND vtalistamingn.preofi > 0:
        DISPLAY vtalistamingn.codmat.
        PAUSE 0.
        I-NPEDI = I-NPEDI + 1.
        CREATE Facdpedi.
        ASSIGN
            Facdpedi.CodCia = Faccpedi.CodCia
            Facdpedi.CodDiv = Faccpedi.CodDiv
            Facdpedi.coddoc = Faccpedi.coddoc
            Facdpedi.NroPed = Faccpedi.NroPed
            Facdpedi.FchPed = Faccpedi.FchPed
            Facdpedi.Hora   = Faccpedi.Hora 
            Facdpedi.FlgEst = Faccpedi.FlgEst
            Facdpedi.NroItm = 1
            Facdpedi.CodMat = VtaListaMinGn.codmat
            Facdpedi.canped = 1
            Facdpedi.factor = 1
            Facdpedi.preuni = vtalistamingn.preofi.
    END.
    RUN Recalcular.

    /* Grabamos Totales */
    RUN Graba-Totales.

END.



PROCEDURE recalcular:

    DEFINE VARIABLE F-FACTOR AS DECI NO-UNDO.
    DEFINE VARIABLE F-PREBAS LIKE Almmmatg.PreBas NO-UNDO.
    DEFINE VARIABLE F-DSCTOS LIKE Almmmatg.PorMax NO-UNDO.
    DEFINE VARIABLE X-CANPED AS DECI NO-UNDO.

    FOR EACH Facdpedi OF Faccpedi, FIRST Almmmatg OF Facdpedi NO-LOCK:
        F-FACTOR = Facdpedi.Factor.
        x-CanPed = Facdpedi.CanPed.
        RUN vta2/Precio-Utilex-Menor (
                            s-CodDiv,
                            s-CodMon,
                            s-TpoCmb,
                            OUTPUT s-UndVta,
                            OUTPUT f-Factor,
                            Almmmatg.CodMat,
                            x-CanPed,
                            4,
                            s-flgsit,       /* s-codbko, */
                            s-codbko,
                            s-tarjeta,
                            s-codpro,
                            s-NroVale,
                            OUTPUT f-PreBas,
                            OUTPUT f-PreVta,
                            OUTPUT f-Dsctos,
                            OUTPUT y-Dsctos,
                            OUTPUT z-Dsctos,
                            OUTPUT x-TipDto
                            ).
        ASSIGN 
            Facdpedi.Factor = f-Factor
            Facdpedi.UndVta = s-UndVta
            Facdpedi.PreUni = F-PREVTA
            Facdpedi.PreBas = F-PreBas 
            Facdpedi.PorDto = F-DSCTOS  /* Ambos descuentos afectan */
            Facdpedi.PorDto2 = 0        /* el precio unitario */
            Facdpedi.ImpDto2 = 0
            Facdpedi.Por_Dsctos[2] = z-Dsctos
            Facdpedi.Por_Dsctos[3] = Y-DSCTOS 
            Facdpedi.AftIgv = ( IF s-CodIgv = 1 THEN Almmmatg.AftIgv ELSE NO )
            Facdpedi.AftIsc = Almmmatg.AftIsc
            Facdpedi.ImpIsc = 0
            Facdpedi.ImpIgv = 0
            Facdpedi.Libre_c04 = X-TIPDTO.

            ASSIGN
                facdpedi.ImpLin = facdpedi.CanPed * facdpedi.PreUni * 
                              ( 1 - facdpedi.Por_Dsctos[1] / 100 ) *
                              ( 1 - facdpedi.Por_Dsctos[2] / 100 ) *
                              ( 1 - facdpedi.Por_Dsctos[3] / 100 )
                facdpedi.ImpDto2 = ROUND ( facdpedi.ImpLin * facdpedi.PorDto2 / 100, 2).
            IF facdpedi.Por_Dsctos[1] = 0 AND facdpedi.Por_Dsctos[2] = 0 AND facdpedi.Por_Dsctos[3] = 0 
                THEN facdpedi.ImpDto = 0.
                ELSE facdpedi.ImpDto = facdpedi.CanPed * facdpedi.PreUni - facdpedi.ImpLin.
            ASSIGN
                facdpedi.ImpLin = ROUND(facdpedi.ImpLin, 2)
                facdpedi.ImpDto = ROUND(facdpedi.ImpDto, 2).
            IF facdpedi.AftIsc 
            THEN facdpedi.ImpIsc = ROUND(facdpedi.PreBas * facdpedi.CanPed * (Almmmatg.PorIsc / 100),4).
            IF facdpedi.AftIgv 
            THEN facdpedi.ImpIgv = facdpedi.ImpLin - ROUND( facdpedi.ImpLin  / ( 1 + (FacCfgGn.PorIgv / 100) ), 4 ).

    END.

END PROCEDURE.


PROCEDURE Graba-Totales:

    DEFINE VARIABLE F-IGV AS DECIMAL NO-UNDO.
    DEFINE VARIABLE F-ISC AS DECIMAL NO-UNDO.
    DEFINE VARIABLE X-STANDFORD AS DECIMAL NO-UNDO.
    DEFINE VARIABLE X-LINEA1 AS DECIMAL NO-UNDO.
    DEFINE VARIABLE X-OTROS AS DECIMAL NO-UNDO.
    DEFINE VARIABLE Y-IMPTOT AS DECIMAL NO-UNDO.
    DEFINE VARIABLE x-ImpDto2 AS DEC NO-UNDO.

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
      X-STANDFORD = 0
      X-LINEA1 = 0
      X-OTROS = 0
      Y-IMPTOT = 0.
      x-ImpDto2 = 0.

    FOR EACH FacDPedi OF FacCPedi NO-LOCK: 
      F-Igv = F-Igv + FacDPedi.ImpIgv.
      F-Isc = F-Isc + FacDPedi.ImpIsc.
      x-ImpDto2 = x-ImpDto2 + FacDPedi.ImpDto2.

      FacCPedi.ImpTot = FacCPedi.ImpTot + FacDPedi.ImpLin.
      FacCPedi.ImpDto2 = FacCPedi.ImpDto2 + FacDPedi.ImpDto2.

      IF NOT FacDPedi.AftIgv THEN FacCPedi.ImpExo = FacCPedi.ImpExo + FacDPedi.ImpLin.
      IF FacDPedi.AftIgv = YES
      THEN FacCPedi.ImpDto = FacCPedi.ImpDto + ROUND(FacDPedi.ImpDto / (1 + FacCPedi.PorIgv / 100), 2).
      ELSE FacCPedi.ImpDto = FacCPedi.ImpDto + FacDPedi.ImpDto.
      /******************Identificacion de Importes para Descuento**********/
      FIND Almmmatg WHERE Almmmatg.Codcia = S-CODCIA AND 
                           Almmmatg.Codmat = FacDPedi.CodMat NO-LOCK NO-ERROR.
      IF AVAILABLE Almmmatg THEN DO:
          IF Almmmatg.CodFam = "002" AND Almmmatg.SubFam = "012" AND TRIM(Almmmatg.Desmar) = "STANDFORD" 
          THEN X-STANDFORD = X-STANDFORD + FacDPedi.ImpLin.
          IF FacDPedi.Por_Dsctos[3] = 0 THEN DO:
             IF Almmmatg.CodFam = "001" 
             THEN X-LINEA1 = X-LINEA1 + FacDPedi.ImpLin.
             ELSE X-OTROS = X-OTROS + FacDPedi.ImpLin.
          END.                
      END.
      /*********************************************************************/
    END.
    Y-IMPTOT = ( X-LINEA1 + X-OTROS ) .   
    ASSIGN
      FacCPedi.ImpIgv = ROUND(F-IGV,2)
      FacCPedi.ImpIsc = ROUND(F-ISC,2).
    ASSIGN
        FacCPedi.ImpVta = FacCPedi.ImpTot - FacCPedi.ImpExo - FacCPedi.ImpIgv.
    ASSIGN
        FacCPedi.ImpBrt = FacCPedi.ImpVta /*+ FacCPedi.ImpIsc*/ + FacCPedi.ImpDto /*+ FacCPedi.ImpExo*/
        FacCPedi.Importe[3] = IF Y-IMPTOT > FacCPedi.ImpTot THEN FacCPedi.ImpTot ELSE Y-IMPTOT.


END PROCEDURE.
