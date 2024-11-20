DEF BUFFER pedido FOR faccpedi.


FOR EACH faccpedi NO-LOCK WHERE codcia = 1
    AND coddoc = 'o/d'
    AND divdes = '00040'
    AND fchped >= 01/01/2020,
    FIRST pedido WHERE pedido.codcia = 1
    AND pedido.coddoc = faccpedi.codref 
    AND pedido.nroped = faccpedi.nroref
    AND pedido.fchent <> faccpedi.fchent:
    DISPLAY faccpedi.coddoc faccpedi.nroped faccpedi.fchent
        pedido.coddoc pedido.nroref pedido.fchent
        WITH STREAM-IO NO-BOX WIDTH 320.
    PAUSE 0.
    pedido.fchent = faccpedi.fchent.
END.
