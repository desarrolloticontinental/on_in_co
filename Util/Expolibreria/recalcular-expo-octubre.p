DISABLE TRIGGERS FOR LOAD OF faccpedi.
DISABLE TRIGGERS FOR LOAD OF facdpedi.

    DEF TEMP-TABLE ResumenxLinea
        FIELD codfam LIKE almmmatg.codfam
        FIELD subfam LIKE almmmatg.subfam
        FIELD canped LIKE facdpedi.canped
        INDEX Llave01 AS PRIMARY UNIQUE codfam subfam.

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
DEFINE VARIABLE x-imptot LIKE faccpedi.imptot NO-UNDO.

DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR s-coddiv AS CHAR INIT '00015' NO-UNDO.
DEF VAR pcoddiv  AS CHAR INIT '00015' NO-UNDO.
DEF VAR cl-codcia AS INT INIT 000 NO-UNDO.
DEF VAR s-coddoc AS CHAR INIT "COT" NO-UNDO.
DEF VAR s-tpoped AS CHAR INIT "E" NO-UNDO.

FOR EACH faccpedi WHERE codcia = s-codcia
    AND coddiv = s-coddiv
    AND coddoc = s-coddoc
    AND tpoped = s-tpoped
    AND fchped >= 09/30/2013
    AND FacCPedi.Libre_c01 = pCodDiv:
    x-ImpTot = Faccpedi.ImpTot.
    /*
    DISPLAY faccpedi.fchped faccpedi.nroped x-imptot faccpedi.imptot.
    PAUSE 0.
    NEXT.
    */
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
        s-FlgIgv = Faccpedi.FlgIgv.
    FOR EACH Facdpedi OF Faccpedi, FIRST Almmmatg OF Facdpedi NO-LOCK:
        ASSIGN
            x-CanPed = Facdpedi.CanPed
            s-UndVta = Facdpedi.UndVta.
        RUN vta2/PrecioListaxMayorCredito (
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
            FacDPedi.TipVta,
            TRUE
            ).
        ASSIGN 
            Facdpedi.Factor = F-FACTOR
            Facdpedi.PorDto = F-DSCTOS      /* Ambos descuentos afectan */
            Facdpedi.PorDto2 = 0            /* el precio unitario */
            Facdpedi.PreBas = F-PreBas 
            Facdpedi.PreVta[1] = F-PreVta     /* CONTROL DE PRECIO DE LISTA */
            Facdpedi.UndVta = s-UndVta
            Facdpedi.PreUni = F-PREVTA
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
        ASSIGN
            Facdpedi.ImpLin = ROUND(Facdpedi.ImpLin, 2)
            Facdpedi.ImpDto = ROUND(Facdpedi.ImpDto, 2).
        IF Facdpedi.AftIsc 
        THEN Facdpedi.ImpIsc = ROUND(Facdpedi.PreBas * Facdpedi.CanPed * (Almmmatg.PorIsc / 100),4).
        ELSE Facdpedi.ImpIsc = 0.
        IF Facdpedi.AftIgv 
        THEN Facdpedi.ImpIgv = Facdpedi.ImpLin - ROUND( Facdpedi.ImpLin  / ( 1 + (s-PorIgv / 100) ), 4 ).
        ELSE Facdpedi.ImpIgv = 0.
    END.
    RUN Descuentos-Finales.
    RUN graba-totales.
    DISPLAY faccpedi.fchped faccpedi.nroped x-imptot faccpedi.imptot.
    PAUSE 0.
END.
RETURN.

PROCEDURE graba-totales:

    {vta2/graba-totales-cotizacion-cred.i}

END PROCEDURE.


PROCEDURE Descuentos-Finales:

    {vta2/descuentoxvolumenxlinearesumida.i}


END PROCEDURE.
