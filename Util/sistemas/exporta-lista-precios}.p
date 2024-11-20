/* Exportacion de listas de precios */
OUTPUT TO d:\almmmatg.d.
FOR EACH almmmatg NO-LOCK WHERE codcia = 001 AND tpoart <> "D".
    EXPORT almmmatg.
END.
OUTPUT CLOSE.

OUTPUT TO d:\vtalistamay.d.
FOR EACH vtalistamay NO-LOCK WHERE codcia = 001:
    EXPORT vtalistamay.
END.
OUTPUT CLOSE.

OUTPUT TO d:\vtalistamingn.d.
FOR EACH vtalistamingn NO-LOCK WHERE codcia = 001:
    EXPORT vtalistamingn.
END.
OUTPUT CLOSE.

