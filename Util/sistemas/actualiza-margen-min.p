
DEF VAR X-CTOUND AS DECI NO-UNDO.
DEF VAR F-FACTOR AS DECI NO-UNDO.

DISABLE TRIGGERS FOR LOAD OF Vtalistamingn.

FOR EACH Vtalistamingn EXCLUSIVE-LOCK WHERE Vtalistamingn.codcia = 1 AND Vtalistamingn.DEC__01 = ?,
    FIRST Almmmatg OF Vtalistamingn NO-LOCK:
    ASSIGN
        X-CTOUND = Almmmatg.CtoTot
        F-FACTOR = 1.
    /****   Busca el Factor de conversión   ****/
    FIND Almtconv WHERE Almtconv.CodUnid = Almmmatg.UndBas
        AND  Almtconv.Codalter = Almmmatg.CHR__01
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Almtconv THEN F-FACTOR = 0.
    ELSE F-FACTOR = Almtconv.Equival.
    IF x-ctound = ? OR x-ctound = 0 OR F-FACTOR = 0 THEN VtaListaMinGn.Dec__01 = 0.
    ELSE VtaListaMinGn.Dec__01 = ROUND(((((VtaListaMinGn.PreOfi / F-FACTOR) / X-CTOUND) - 1) * 100), 6).
END.
