FIND FIRST faccpedi WHERE codcia = 1 AND coddoc = 'cot' AND nroped = '124062610'.
DISPLAY coddiv nomcli.
FOR EACH facdpedi OF faccpedi WHERE codmat = '072242',
    FIRST almmmatg OF facdpedi NO-LOCK:
    DISPLAY facdpedi.codmat facdpedi.canped.
    UPDATE FacDPedi.Por_Dsctos[1] FacDPedi.Por_Dsctos[2] FacDPedi.Por_Dsctos[3] FacDPedi.PreUni facdpedi.implin 
        WITH 1 COL.
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

