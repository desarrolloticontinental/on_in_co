DISABLE TRIGGERS FOR LOAD OF faccpedi.
DISABLE TRIGGERS FOR LOAD OF facdpedi.

DEF VAR x-canped AS DEC NO-UNDO.
DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR s-coddiv AS CHAR INIT '00000' NO-UNDO.      /* EXPOLIBRERIA */
DEF VAR s-codcli AS CHAR NO-UNDO.
DEF VAR s-codmov AS INT NO-UNDO.
DEF VAR s-tpocmb AS DEC NO-UNDO.
DEF VAR s-codmon AS INT NO-UNDO.
DEF VAR F-FACTOR AS DECI NO-UNDO INIT 1.
DEF VAR s-CndVta AS CHAR NO-UNDO.
DEF VAR F-PREVTA AS DECI NO-UNDO.
DEF VAR Y-DSCTOS AS DECI NO-UNDO.
DEF VAR Z-DSCTOS AS DECI NO-UNDO.       /* DESCUENTO EXPOLIBRERIA */
DEF VAR F-PREBAS LIKE Almmmatg.PreBas NO-UNDO.
DEF VAR F-DSCTOS LIKE Almmmatg.PorMax NO-UNDO.
DEFINE VARIABLE F-IGV AS DECIMAL NO-UNDO INIT 0.
DEFINE VARIABLE F-ISC AS DECIMAL NO-UNDO INIT 0.
DEF VAR s-undvta AS CHAR NO-UNDO.
DEF VAR s-undbas AS CHAR NO-UNDO.

/* ULTIMA EXPO desde el 25/10/2010 hasa el 26/10/2010 */
FOR EACH faccpedi WHERE codcia = s-codcia
    AND coddiv = s-coddiv
    AND coddoc = 'COT'
    AND nroped BEGINS '040'
    AND fchped >= 10/25/2010:
    DISPLAY coddiv coddoc nroped fchped usuario.
    PAUSE 0.
    ASSIGN
        s-codcli = Faccpedi.codcli
        s-codmon = Faccpedi.codmon
        s-tpocmb = Faccpedi.tpocmb
        s-codmon = Faccpedi.codmon
        s-cndvta = Faccpedi.fmapgo.
    FOR EACH facdpedi OF faccpedi, FIRST almmmatg OF facdpedi NO-LOCK:
        FIND Almtconv WHERE Almtconv.CodUnid  = Almmmatg.UndBas 
            AND Almtconv.Codalter = Almmmatg.Chr__01 
            NO-LOCK NO-ERROR.
        x-CanPed = Facdpedi.CanPed.
        f-Factor = Facdpedi.Factor.
    ASSIGN 
        S-UNDVTA = Almmmatg.CHR__01
        S-UNDBAS = Almmmatg.UndBas
        F-FACTOR = 1
        X-CANPED = 1.
    RUN vtagn/PrecioListaMayorista (
                        s-CodDiv,
                        s-CodCli,
                        s-CodMon,
                        s-TpoCmb,
                        OUTPUT s-UndVta,
                        OUTPUT f-Factor,
                        Almmmatg.CodMat,
                        s-CndVta,
                        x-CanPed,
                        4,
                        OUTPUT f-PreBas,
                        OUTPUT f-PreVta,
                        OUTPUT f-Dsctos,
                        OUTPUT y-Dsctos,
                        OUTPUT z-Dsctos
                        ).
        ASSIGN
            Facdpedi.pordto = f-dsctos
            Facdpedi.preuni = f-prevta
            Facdpedi.por_dsctos[1] = z-dsctos.
        ASSIGN 
            FacDPEDI.PreBas = F-PreBas 
            FacDPEDI.Por_DSCTOS[2] = Almmmatg.PorMax
            FacDPEDI.Por_Dsctos[3] = Y-DSCTOS 
            FacDPEDI.ImpDto = ROUND( FacDPEDI.PreUni * FacDPEDI.CanPed * (FacDPEDI.Por_Dsctos[1] / 100),4 )
            FacDPEDI.ImpLin = ROUND( FacDPEDI.PreUni * FacDPEDI.CanPed , 2 ) - FacDPEDI.ImpDto.
        IF FacDPEDI.AftIsc 
        THEN FacDPEDI.ImpIsc = ROUND(FacDPEDI.PreBas * FacDPEDI.CanPed * (Almmmatg.PorIsc / 100),4).
        IF FacDPEDI.AftIgv 
        THEN FacDPEDI.ImpIgv = FacDPEDI.ImpLin - ROUND(FacDPEDI.ImpLin  / (1 + (Faccpedi.PorIgv / 100)),4).
    END.
    RUN graba-totales.
END.

PROCEDURE graba-totales:
/* ******************** */

{vtamay/graba-totales.i}

END PROCEDURE.
