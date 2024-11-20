DEF BUFFER detalle FOR cb-dmov.

FOR EACH cb-cmov NO-LOCK WHERE
        cb-cmov.CodCia = 001 AND
        cb-cmov.Periodo = 2012 AND
        cb-cmov.nromes >= 01 AND
        cb-cmov.CodOpe = '002',
        EACH cb-dmov WHERE
        cb-dmov.CodCia = cb-cmov.CodCia AND
        cb-dmov.Periodo = cb-cmov.Periodo AND
        cb-dmov.NroMes = cb-cmov.NroMes AND
        cb-dmov.CodOpe = cb-cmov.CodOpe AND
        cb-dmov.NroAst = cb-cmov.NroAst AND
        cb-dmov.chr_01 <> "" :
    /*DISPLAY chr_01 cb-dmov.coddoc cb-dmov.nrodoc fchdoc fchast.*/
    ciclo:
    FOR EACH Cp-tpro NO-LOCK  WHERE
        Cp-tpro.CODCIA = 000 AND
        Cp-tpro.CORRELATIVO = YES AND
         Cp-tpro.CodDoc = cb-dmov.coddoc:
        FOR EACH DETALLE NO-LOCK WHERE
            DETALLE.CODCIA  = cb-cmov.codcia AND
            DETALLE.PERIODO = cb-cmov.periodo AND
            DETALLE.NROMES <= cb-cmov.nromes  AND
            DETALLE.CODOPE  = CP-TPRO.CODOPE AND
            DETALLE.CODCTA  = CP-TPRO.CODCTA AND
            DETALLE.CodAux  = cb-dmov.codaux   AND
            DETALLE.CODDOC  = CP-TPRO.CODDOC AND
            detalle.nrodoc = cb-dmov.nrodoc:
            cb-dmov.fchdoc = detalle.fchdoc.
            LEAVE ciclo.
        END.
    END.
END.

