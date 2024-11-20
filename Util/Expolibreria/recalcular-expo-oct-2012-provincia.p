DISABLE TRIGGERS FOR LOAD OF faccpedi.
DISABLE TRIGGERS FOR LOAD OF facdpedi.

DEF VAR x-porcent AS DEC NO-UNDO.
DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR s-coddiv AS CHAR INIT '00018' NO-UNDO.
DEF VAR cl-codcia AS INT INIT 000 NO-UNDO.
DEF VAR s-coddoc AS CHAR INIT "COT" NO-UNDO.
DEF VAR s-nroser AS INT NO-UNDO.
DEF VAR s-codmon AS INT NO-UNDO.
DEF VAR s-CodCli AS CHAR NO-UNDO.
DEF VAR s-porigv AS DEC NO-UNDO.
DEF VAR s-cndvta AS CHAR NO-UNDO.
DEF VAR s-tpocmb AS DEC NO-UNDO.
DEF VAR s-nrodec AS INT NO-UNDO.
DEF VAR s-flgigv AS LOG NO-UNDO.
DEF VAR s-UndVta AS CHAR NO-UNDO.
DEF VAR s-TpoPed AS CHAR NO-UNDO.
DEF VAR f-Factor LIKE Facdpedi.factor NO-UNDO.
DEF VAR x-imptot AS DEC NO-UNDO.
DEFINE VARIABLE F-PREBAS LIKE Almmmatg.PreBas NO-UNDO.
DEFINE VARIABLE F-DSCTOS LIKE Almmmatg.PorMax NO-UNDO.
DEFINE VARIABLE X-CANPED AS DECI NO-UNDO.
DEFINE VARIABLE F-PREVTA AS DECI NO-UNDO.
DEFINE VARIABLE Y-DSCTOS AS DECI NO-UNDO.
DEFINE VARIABLE Z-DSCTOS AS DECI NO-UNDO.
DEFINE VARIABLE x-TipDto AS CHAR NO-UNDO.

FOR EACH faccpedi WHERE codcia = s-codcia
    AND coddiv = s-coddiv
    AND coddoc = s-coddoc
    AND nroped BEGINS '018'
    AND fchped >= 10/10/2012:
    /* FILTROS */
    FIND gn-clie WHERE gn-clie.codcia = cl-codcia
        AND gn-clie.codcli = faccpedi.codcli
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE gn-clie THEN NEXT.
    DISPLAY faccpedi.fchped faccpedi.nroped.
    PAUSE 0.
    /* 4 DECIMALES */
    ASSIGN
        Faccpedi.Libre_d01 = 4.
    /* ************ */
    ASSIGN
        S-CODMON = FacCPedi.CodMon
        S-CODCLI = FacCPedi.CodCli
        S-TPOCMB = FacCPedi.TpoCmb
        S-CNDVTA = FacCPedi.FmaPgo
        s-PorIgv = Faccpedi.porigv
        s-NroDec = (IF Faccpedi.Libre_d01 <= 0 THEN 2 ELSE Faccpedi.Libre_d01)
        s-FlgIgv = Faccpedi.FlgIgv
        s-TpoPed = Faccpedi.TpoPed.
    FOR EACH Facdpedi OF Faccpedi, FIRST Almmmatg OF Facdpedi NO-LOCK:
        ASSIGN
            x-CanPed = Facdpedi.CanPed
            s-UndVta = Facdpedi.UndVta.
        RUN vta2/PrecioMayorista-Cred-01 (
            s-TpoPed,
            s-CodDiv,
            s-CodCli,
            s-CodMon,
            INPUT-OUTPUT s-UndVta,
            OUTPUT f-Factor,
            Facdpedi.codmat,
            s-CndVta,
            x-CanPed,
            s-NroDec,
            OUTPUT f-PreBas,
            OUTPUT f-PreVta,
            OUTPUT f-Dsctos,
            OUTPUT y-Dsctos,
            OUTPUT z-Dsctos,
            OUTPUT x-TipDto,
            FALSE
            ).
        IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.
        ASSIGN 
            Facdpedi.Factor = F-FACTOR
            Facdpedi.PorDto = f-Dsctos
            Facdpedi.PreBas = F-PreBas 
            Facdpedi.UndVta = s-UndVta
            Facdpedi.PreUni = F-PREVTA
            Facdpedi.Por_Dsctos[2] = z-Dsctos
            Facdpedi.Por_Dsctos[3] = y-Dsctos
            Facdpedi.AftIgv = Almmmatg.AftIgv
            Facdpedi.AftIsc = Almmmatg.AftIsc
            Facdpedi.Libre_c04 = x-TipDto.
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
    x-ImpTot = Faccpedi.ImpTot.
    {vta2/graba-totales-cotizacion-cred.i}
    DISPLAY x-imptot faccpedi.imptot.
    /*PAUSE 0.*/
END.


/*
    DISPLAY 
        F-PREVTA @ Facdpedi.PreUni 
        z-Dsctos @ Facdpedi.Por_Dsctos[2]
        y-Dsctos @ Facdpedi.Por_Dsctos[3]
        WITH BROWSE {&BROWSE-NAME}.
*/
