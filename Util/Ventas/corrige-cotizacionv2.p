
FIND FIRST faccpedi WHERE codcia = 1 AND coddoc = 'cot' AND nroped = '035100383'.

/* DEFINE VAR lNuevoPrecio AS DEC. */
/* DEFINE VAR lOldPrecio AS DEC.   */
/*                                 */
/* lNuevoPrecio = 24.75.           */

DEFINE VAR lCodMats AS CHAR.

/* ICBPER */
DEFINE VAR x-articulo-ICBPER AS CHAR.

x-articulo-ICBPER = '099268'.


lCodMats = "040365,040364,040366,033618,033619,067943,037954,037959,067945".

IF AVAILABLE faccpedi THEN DO:
    /*DISPLAY coddiv nomcli.*/
    /*UPDATE fmapgo WITH 1 COL.*/
    FOR EACH facdpedi OF faccpedi WHERE LOOKUP(facdpedi.codmat,lCodMats) = 0,
        FIRST almmmatg OF facdpedi NO-LOCK BY facdpedi.codmat:
        /*lOldPrecio = facdpedi.preuni.*/
        /*DISPLAY facdpedi.codmat almmmatg.desmat facdpedi.canped lOldPrecio .*/
                
        /*ASSIGN facdpedi.preuni = lNuevoPrecio.*/
        /*
        UPDATE facdpedi.codmat almmmatg.desmat FacDPedi.CanPed FacDPedi.Por_Dsctos[1] FacDPedi.Por_Dsctos[2] FacDPedi.Por_Dsctos[3] FacDPedi.PreUni
            WITH 1 COL.
        */
        UPDATE facdpedi.codmat almmmatg.desmat FacDPedi.CanPed FacDPedi.PreUni WITH 100 COL.
        
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
        THEN Facdpedi.ImpIgv = Facdpedi.ImpLin - ROUND( Facdpedi.ImpLin  / ( 1 + (Faccpedi.PorIgv / 100) ), 4 ).
        ELSE Facdpedi.ImpIgv = 0.
    END.

    {vta2/graba-totales-cotizacion-cred.i}

END.
ELSE DO:
    MESSAGE "No existe".
END.
