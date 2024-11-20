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

OUTPUT TO d:\gn-divi.d.
FOR EACH gn-divi NO-LOCK WHERE codcia = 001:
    EXPORT gn-divi.
END.
OUTPUT CLOSE.

OUTPUT TO d:\vtatabla.d.
FOR EACH vtatabla NO-LOCK:
    EXPORT vtatabla.
END.
OUTPUT CLOSE.

OUTPUT TO d:\factabla.d.
FOR EACH factabla NO-LOCK:
    EXPORT factabla.
END.
OUTPUT CLOSE.

OUTPUT TO d:\almtfami.d.
FOR EACH almtfami NO-LOCK:
    EXPORT almtfami.
END.
OUTPUT CLOSE.

OUTPUT TO d:\almsfami.d.
FOR EACH almsfami NO-LOCK:
    EXPORT almsfami.
END.
OUTPUT CLOSE.
