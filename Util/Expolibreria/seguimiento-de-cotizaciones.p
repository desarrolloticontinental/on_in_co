DEF BUFFER bcpedido FOR faccpedi.
DEF BUFFER bdpedido FOR facdpedi.
DEF BUFFER bcorden FOR faccpedi.
DEF BUFFER bdorden FOR facdpedi.

    DEF VAR s-coddiv AS CHAR INIT '00018'.

FOR EACH faccpedi WHERE codcia = 1
    AND coddoc = 'cot'
    AND codcli = '20486506351'
    AND coddiv = s-coddiv
    AND fchped >= 10/01/2012
    AND flgest <> 'A',
    EACH facdpedi OF faccpedi /*WHERE codmat = '002140'*/.
    DISPLAY faccpedi.nroped faccpedi.flgest codmat canped canate
        WITH STREAM-IO NO-BOX NO-LABELS.
    FOR EACH bcpedido WHERE bcpedido.codcia = 1
        AND bcpedido.coddiv = s-coddiv
        AND bcpedido.coddoc = 'ped'
        AND bcpedido.codref = faccpedi.coddoc
        AND bcpedido.nroref = faccpedi.nroped
        AND bcpedido.flgest <> 'A',
        EACH bdpedido OF bcpedido WHERE bdpedido.codmat = facdpedi.codmat.
        DISPLAY bcpedido.coddoc bcpedido.nroped bcpedido.flgest
            bdpedido.canped bdpedido.canate WITH STREAM-IO NO-BOX NO-LABELS.
        FOR EACH bcorden WHERE bcorden.codcia = 1
            AND bcorden.coddoc = 'o/d'
            AND bcorden.coddiv = s-coddiv
            AND bcorden.codref = bcpedido.coddoc
            AND bcorden.nroref = bcpedido.nroped,
            EACH bdorden OF bcorden WHERE bdorden.codmat = facdpedi.codmat.
            DISPLAY bcorden.coddoc bcorden.nroped bcorden.flgest
            bdorden.canped bdorden.canate WITH STREAM-IO NO-BOX NO-LABELS.

        END.
    END.
END.

