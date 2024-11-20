DEF VAR x-Importe LIKE Lg-docmp.preuni NO-UNDO.
DEF VAR s-codcia AS INT INIT 001 NO-UNDO.

FIND lg-cocmp WHERE codcia = 1 AND nrodoc = 101766.

FOR EACH lg-docmp OF lg-cocmp WHERE lg-docmp.codmat = '037454':
    DISPLAY codmat.
    UPDATE preuni lg-docmp.Dsctos[1] lg-docmp.Dsctos[2] lg-docmp.Dsctos[3].
    FIND Almmmatg WHERE Almmmatg.CodCia = S-CODCIA AND
         Almmmatg.CodMat = lg-docmp.Codmat NO-LOCK NO-ERROR.
    ASSIGN 
        x-Importe = lg-docmp.CanPedi * lg-docmp.PreUni.
    ASSIGN
        lg-docmp.ImpTot = ROUND(x-Importe * 
                                (1 - (lg-docmp.Dsctos[1] / 100)) *
                                (1 - (lg-docmp.Dsctos[2] / 100)) *
                                (1 - (lg-docmp.Dsctos[3] / 100)) *
                                (1 + (lg-docmp.IgvMat / 100)), 2).
END.
FIND LAST LG-CFGIGV NO-LOCK NO-ERROR.
ASSIGN 
    LG-COCmp.ImpTot = 0
    LG-COCmp.ImpExo = 0.
FOR EACH Lg-docmp OF Lg-cocmp NO-LOCK:
    LG-COCmp.ImpTot = LG-COCmp.ImpTot + Lg-docmp.ImpTot.
    IF Lg-docmp.IgvMat = 0 THEN LG-COCmp.ImpExo = LG-COCmp.ImpExo + Lg-docmp.ImpTot.
END.
ASSIGN 
    LG-COCmp.ImpIgv = (LG-COCmp.ImpTot - LG-COCmp.ImpExo) - 
    ROUND((LG-COCmp.ImpTot - LG-COCmp.ImpExo) / (1 + LG-CFGIGV.PorIgv / 100),2)
    LG-COCmp.ImpBrt = LG-COCmp.ImpTot - LG-COCmp.ImpExo - LG-COCmp.ImpIgv
    LG-COCmp.ImpDto = 0
    LG-COCmp.ImpNet = LG-COCmp.ImpBrt - LG-COCmp.ImpDto.

