DEF VAR x-tpomrg AS CHAR.
OUTPUT TO d:\productos.txt.
PUT UNFORMATTED 'CODIGO|DESCRIPCION|TIPO|CATCONTA|ALM' SKIP.
FOR EACH almmmatg NO-LOCK WHERE codcia = 1
    AND tpoart <> 'd'
    /*AND LOOKUP(catconta[1], 'MC,MI,PM') > 0*/
    AND TRUE <> (almacenes > ''):
    x-tpomrg = 'Ambos'.
    IF almmmatg.tpomrg = '1' THEN x-tpomrg = 'Mayoristas'.
    IF almmmatg.tpomrg = '2' THEN x-tpomrg = 'Minoristas'.

    PUT UNFORMATTED 
        almmmatg.codmat '|'
        almmmatg.desmat '|'
        x-tpomrg '|'
        almmmatg.catconta[1] '|'
        '' SKIP.

END.
OUTPUT CLOSE.

