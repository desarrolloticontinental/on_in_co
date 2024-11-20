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


DEF VAR x-codmat AS CHAR FORMAT 'x(6)'.
DEF VAR x-linea AS CHAR FORMAT 'x(50)'.
DEF TEMP-TABLE detalle
    FIELD nroped LIKE faccpedi.nroped
    FIELD codmat LIKE facdpedi.codmat.

INPUT FROM c:\tmp\precios.prn.
REPEAT :
    IMPORT UNFORMATTED x-linea.
    x-codmat = SUBSTRING(x-linea,1,6).
    FOR EACH facdpedi NO-LOCK WHERE facdpedi.codcia = 1
        AND facdpedi.coddoc = 'cot'
        AND facdpedi.codmat = x-codmat,
        FIRST faccpedi OF facdpedi NO-LOCK WHERE faccpedi.coddiv = '00000'
        AND faccpedi.nroped BEGINS '040'
        AND faccpedi.flgest <> 'A'
        AND faccpedi.fchped >= 10/25/2010:
        CREATE detalle.
        ASSIGN
            detalle.nroped = faccpedi.nroped
            detalle.codmat = facdpedi.codmat.
    END.
END.
INPUT CLOSE.

FOR EACH detalle BREAK BY detalle.nroped BY detalle.codmat:
    FIND faccpedi WHERE faccpedi.codcia = 1
        AND faccpedi.coddiv = '00000'
        AND faccpedi.coddoc = 'cot'
        AND faccpedi.nroped = detalle.nroped.
    ASSIGN
        s-codcli = Faccpedi.codcli
        s-codmon = Faccpedi.codmon
        s-tpocmb = Faccpedi.tpocmb
        s-codmon = Faccpedi.codmon
        s-cndvta = Faccpedi.fmapgo.
    FIND facdpedi OF faccpedi WHERE facdpedi.codmat = detalle.codmat.
    FIND Almmmatg OF facdpedi NO-LOCK.
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
    IF LAST-OF(detalle.nroped) THEN RUN graba-totales.
END.
RETURN.


PROCEDURE graba-totales:
/* ******************** */

{vtamay/graba-totales.i}

END PROCEDURE.

