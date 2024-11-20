DEF VAR x-linea AS CHAR.
DEF VAR x-preuni AS DEC DECIMALS 4.
DEF TEMP-TABLE detalle
    FIELD nrodoc LIKE lg-cocmp.nrodoc
    FIELD prowid AS ROWID.

INPUT FROM c:\tmp\ordenes_oo.prn.
REPEAT :
    IMPORT UNFORMATTED x-linea.
    IF x-linea = '' THEN LEAVE.
    FIND lg-cocmp WHERE codcia = 001
        AND nrodoc = INTEGER(SUBSTRING(x-linea,31))
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE lg-cocmp OR lg-cocmp.codpro <> '51135890' THEN NEXT.
    FIND lg-docmp WHERE lg-docmp.codcia = lg-cocmp.codcia
        AND lg-docmp.coddiv = lg-cocmp.coddiv
        AND LG-COCmp.TpoDoc = lg-cocmp.tpodoc
        AND lg-docmp.nrodoc = INTEGER(SUBSTRING(x-linea,31))
        AND lg-docmp.codmat = SUBSTRING(x-linea,1,6)
        NO-ERROR.
    IF NOT AVAILABLE lg-docmp THEN NEXT.
    FIND FIRST detalle WHERE detalle.nrodoc = lg-cocmp.nrodoc NO-LOCK NO-ERROR.
    IF NOT AVAILABLE detalle THEN DO:
        CREATE detalle.
        ASSIGN detalle.nrodoc = lg-cocmp.nrodoc prowid = ROWID(lg-cocmp).
    END.
    x-preuni = DECIMAL(SUBSTRING(x-linea,21,10)).
    ASSIGN 
        lg-docmp.preuni = x-preuni
        lg-docmp.ImpTot = ROUND(lg-docmp.CanPedi * lg-docmp.PreUni * 
                       (1 - (lg-docmp.Dsctos[1] / 100)) *
                       (1 - (lg-docmp.Dsctos[2] / 100)) *
                       (1 - (lg-docmp.Dsctos[3] / 100)) *
                       (1 + (lg-docmp.IgvMat / 100)), 2).

END.
FOR EACH detalle, FIRST lg-cocmp WHERE ROWID(lg-cocmp) = detalle.prowid:
    DISPLAY detalle.nrodoc lg-cocmp.nrodoc.
    PAUSE 0.
    ASSIGN 
      LG-COCmp.ImpTot = 0
      LG-COCmp.ImpExo = 0.
    FOR EACH Lg-docmp OF Lg-cocmp NO-LOCK:
        LG-COCmp.ImpTot = LG-COCmp.ImpTot + Lg-docmp.ImpTot.
        IF Lg-docmp.IgvMat = 0 THEN LG-COCmp.ImpExo = LG-COCmp.ImpExo + Lg-docmp.ImpTot.
    END.
    FIND LAST LG-CFGIGV NO-LOCK NO-ERROR.
    ASSIGN 
        LG-COCmp.ImpIgv = (LG-COCmp.ImpTot - LG-COCmp.ImpExo) - 
                  ROUND((LG-COCmp.ImpTot - LG-COCmp.ImpExo) / (1 + LG-CFGIGV.PorIgv / 100),2)
        LG-COCmp.ImpBrt = LG-COCmp.ImpTot - LG-COCmp.ImpExo - LG-COCmp.ImpIgv
        LG-COCmp.ImpDto = 0
        LG-COCmp.ImpNet = LG-COCmp.ImpBrt - LG-COCmp.ImpDto.
    FOR EACH almcmov NO-LOCK WHERE almcmov.codcia = lg-cocmp.codcia
        AND almcmov.tipmov = 'i'
        AND almcmov.codmov = 02
        AND almcmov.codalm = lg-cocmp.codalm
        AND almcmov.fchdoc >= lg-cocmp.fchdoc                                     
        AND INTEGER(almcmov.nrorf1) = lg-cocmp.nrodoc,
        EACH almdmov OF almcmov,
        FIRST lg-docmp OF lg-cocmp WHERE lg-docmp.codmat = almdmov.codmat:
        ASSIGN
            Almdmov.PreLis = lg-docmp.PreUni
            Almdmov.PreUni = ROUND(lg-docmp.PreUni * (1 - (lg-docmp.Dsctos[1] / 100)) * 
                                            (1 - (lg-docmp.Dsctos[2] / 100)) * 
                                            (1 - (lg-docmp.Dsctos[3] / 100)),4)
            Almdmov.ImpCto = ROUND(Almdmov.CanDes * Almdmov.PreUni,2).

    END.
END.

