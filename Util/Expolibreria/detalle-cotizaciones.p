DEF VAR X AS CHAR FORMAT 'x(20)'.
OUTPUT TO c:\tmp\expoene2013.txt.
FOR EACH faccpedi NO-LOCK WHERE codcia = 1
    AND coddoc = 'cot'
    AND coddiv = '10015'
    AND flgest <> 'a',
    EACH facdpedi OF faccpedi NO-LOCK,
    FIRST almmmatg OF facdpedi NO-LOCK:
    RUN vta2/p-faccpedi-flgest (faccpedi.flgest,
                                faccpedi.coddoc,
                                OUTPUT X).

    DISPLAY 
        faccpedi.fchped
        faccpedi.coddoc
        faccpedi.nroped
        fchent
        faccpedi.codcli
        nomcli
        X   COLUMN-LABEL 'Estado'
        facdpedi.codmat
        desmat
        codfam
        subfam
        undvta
        canped  COLUMN-LABEL 'Pedido'
        (IF canate > 0 THEN canate ELSE 0) @ canate  COLUMN-LABEL 'Atendido'
        implin
        almmmatg.pesmat  COLUMN-LABEL 'Peso Unitario'
        almmmatg.libre_d02   COLUMN-LABEL 'Vol. Unitario'
        WITH stream-io NO-BOX WIDTH 320.
END.
