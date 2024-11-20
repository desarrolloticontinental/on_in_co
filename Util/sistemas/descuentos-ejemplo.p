OUTPUT TO d:\almmmatg.d.
FOR EACH almmmatg NO-LOCK WHERE codcia = 1
    AND codfam = '001' 
    AND subfam = '045'
    AND dtovolr[1] > 0:
    EXPORT DELIMITER '|' codmat dtovolr dtovold.
END.
OUTPUT CLOSE.

OUTPUT TO d:\vtalistamay.d.
FOR EACH almmmatg NO-LOCK WHERE codcia = 1
    AND codfam = '001' 
    AND subfam = '045',
    EACH vtalistamay OF almmmatg NO-LOCK WHERE vtalistamay.dtovolr[1] > 0:
    DISPLAY vtalistamay.coddiv almmmatg.codmat vtalistamay.dtovolr
        vtalistamay.dtovold.
END.
OUTPUT CLOSE.

OUTPUT TO d:\vtadctoprom.d.
FOR EACH vtadctoprom NO-LOCK WHERE 
    VtaDctoProm.CodCia = 1 AND
    TODAY >= VtaDctoProm.FchIni AND 
    TODAY <= VtaDctoProm.FchFin,
    FIRST almmmatg OF vtadctoprom NO-LOCK WHERE almmmatg.codcia = 1
    AND almmmatg.codfam = '001' 
    AND almmmatg.subfam = '004':
    EXPORT DELIMITER '|' 
        VtaDctoProm.CodDiv 
        VtaDctoProm.CodMat 
        VtaDctoProm.FchIni 
        VtaDctoProm.FchFin 
        VtaDctoProm.Descuento.
END.
OUTPUT CLOSE.
