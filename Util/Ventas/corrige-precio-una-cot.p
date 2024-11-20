
FIND FIRST faccpedi WHERE codcia = 1 
    AND coddoc = 'cot' 
    AND nroped = '018143795'.
DEF VAR x-linea AS CHAR.
INPUT FROM d:\tmp\revilla.prn.
REPEAT:
    IMPORT UNFORMATTED x-linea.
    IF x-linea = '' THEN LEAVE.
    FIND facdpedi OF faccpedi WHERE codmat = SUBSTRING(x-linea,1,10)
        NO-ERROR.
    IF AVAILABLE facdpedi THEN DO:
        FIND FIRST Almmmatg OF Facdpedi NO-LOCK.
        ASSIGN
            FacDPedi.Por_Dsctos[1] = 0
            FacDPedi.Por_Dsctos[2] = 0
            FacDPedi.Por_Dsctos[3] = 0
            FacDPedi.PreUni = DECIMAL(SUBSTRING(x-linea,11,10)).
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
END.
    
INPUT CLOSE.
{vta2/graba-totales-cotizacion-cred.i}
