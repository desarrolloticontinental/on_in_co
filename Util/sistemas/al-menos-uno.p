DEF STREAM almmmatg.
DEF STREAM vtalistamay.
DEF STREAM vtadctoprom.

OUTPUT STREAM almmmatg TO d:\almmmatg.txt.
OUTPUT STREAM vtalistamay TO d:\vtalistamay.txt.
OUTPUT STREAM vtadctoprom TO d:\vtadctoprom.txt.

DEF VAR x-1 AS LOG.
DEF VAR x-2 AS LOG.
DEF VAR x-3 AS LOG.

RLOOP:
FOR EACH almmmatg NO-LOCK WHERE almmmatg.codcia = 1 AND Almmmatg.DtoVolD[1] > 0:
    FIND FIRST vtalistamay OF almmmatg WHERE VtaListaMay.DtoVolD[1] > 0 NO-LOCK NO-ERROR.
    IF AVAILABLE vtalistamay THEN DO:
        FIND FIRST vtadctoprom OF almmmatg WHERE VtaDctoProm.Descuento > 0
            AND TODAY >= VtaDctoProm.FchIni
            AND TODAY <= VtaDctoProm.FchFin
            NO-LOCK NO-ERROR.
        IF AVAILABLE vtadctoprom THEN DO:
            EXPORT STREAM almmmatg DELIMITER '|' almmmatg.codmat almmmatg.dtovolr almmmatg.dtovold.
            EXPORT STREAM vtalistamay DELIMITER '|' vtalistamay.coddiv vtalistamay.codmat vtalistamay.dtovolr vtalistamay.dtovold.
            EXPORT STREAM vtadctoprom DELIMITER '|' VtaDctoProm.CodDiv VtaDctoProm.CodMat VtaDctoProm.FchIni VtaDctoProm.FchFin VtaDctoProm.Descuento.
            LEAVE RLOOP.
        END.
    END.
END.
