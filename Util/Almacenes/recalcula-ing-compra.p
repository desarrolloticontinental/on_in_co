FIND almcmov WHERE codcia = 1 AND codalm = '11'
    AND tipmov = 'i'
    AND codmov = 02
    AND nrodoc = 78321.

ASSIGN almcmov.impmn1 = 0 almcmov.impmn2 = 0.
FOR EACH almdmov OF almcmov:
    almdmov.IMPCTO = ROUND(CANDES * PREUNI,2).
    IF Almcmov.codmon = 1 
        THEN Almcmov.ImpMn1 = Almcmov.ImpMn1 + Almdmov.ImpMn1.
    ELSE Almcmov.ImpMn2 = Almcmov.ImpMn2 + Almdmov.ImpMn2.
END.
IF Almcmov.codmon = 1 
    THEN Almcmov.ImpMn2 = ROUND(Almcmov.ImpMn1 / Almcmov.tpocmb, 2).
ELSE Almcmov.ImpMn1 = ROUND(Almcmov.ImpMn2 * Almcmov.tpocmb, 2).
