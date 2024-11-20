DEF VAR x-items AS INT.
OUTPUT TO d:\tmp\resumen.txt.
FOR EACH faccpedi NO-LOCK WHERE codcia = 1
    AND LOOKUP(coddoc, 'o/d,otr') > 0
    AND fchent >= DATE(06,12,2019)
    AND fchent <= DATE(06,17,2019)
    AND divdes = '00000'
    AND flgest <> 'A'
    BREAK BY divdes BY fchent:
    IF FIRST-OF(faccpedi.divdes) OR FIRST-OF(faccpedi.fchent) THEN x-items = 0.
    FOR EACH facdpedi OF faccpedi NO-LOCK:
        x-items = x-items + 1.
    END.
    IF LAST-OF(faccpedi.divdes) OR LAST-OF(faccpedi.fchent) THEN
        PUT UNFORMATTED faccpedi.divdes '|' faccpedi.fchent '|' x-items SKIP.
END.
OUTPUT CLOSE.

