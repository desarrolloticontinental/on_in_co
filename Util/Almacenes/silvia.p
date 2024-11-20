DEF VAR x-codmat AS CHAR FORMAT 'x(8)' NO-UNDO.
INPUT FROM d:\tmp\silvia.prn.
OUTPUT TO d:\tmp\silvia.txt.
DEF VAR x-ctotot LIKE almmmatg.ctotot NO-UNDO.
DEF VAR x-preuti LIKE vtalistamingn.preofi NO-UNDO.
DEF VAR x-preofi LIKE almmmatg.preofi NO-UNDO.
REPEAT :
    IMPORT x-codmat.
    IF x-codmat = '' THEN LEAVE.
    FIND almmmatg WHERE codcia = 1 AND codmat = x-codmat NO-LOCK NO-ERROR.
    IF NOT AVAILABLE almmmatg THEN NEXT.
    FIND FIRST almtfami OF almmmatg NO-LOCK.
    FIND FIRST almsfami OF almmmatg NO-LOCK.
    FIND vtalistamingn OF almmmatg NO-LOCK NO-ERROR.
    IF NOT AVAILABLE vtalistamingn THEN NEXT.
    ASSIGN
        x-ctotot = almmmatg.ctotot * (IF almmmatg.monvta = 1 THEN 1 ELSE almmmatg.tpocmb)
        x-preuti = VtaListaMinGn.PreOfi * (IF almmmatg.monvta = 1 THEN 1 ELSE almmmatg.tpocmb)
        x-preofi = Almmmatg.PreOfi * (IF almmmatg.monvta = 1 THEN 1 ELSE almmmatg.tpocmb).
    PUT UNFORMATTED
        almmmatg.codmat '|'
        almmmatg.desmat '|'
        almmmatg.desmar '|'
        almmmatg.codfam + ' ' + almtfami.desfam '|'
        almmmatg.subfam + ' ' + almsfami.dessub '|'
        x-ctotot '|'
        x-preofi '|'
        x-preuti
        SKIP.
END.
INPUT CLOSE.
OUTPUT CLOSE.

