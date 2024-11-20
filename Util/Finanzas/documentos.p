OUTPUT TO c:\tmp\docu-oct-nov-2011.txt.
FOR EACH ccbcdocu NO-LOCK WHERE codcia = 1
    AND LOOKUP(coddoc, 'fac,bol') > 0
    AND fchdoc >= 10/01/2011
    AND fchdoc <= 11/30/2011
    AND flgest <> 'A':
    DISPLAY 
        coddiv FORMAT 'x(5)' LABEL 'División'
        coddoc LABEL 'Código'
        nrodoc LABEL 'Número'
        fchdoc LABEL 'Emisión'
        fchvto LABEL 'Vencimiento'
        fchcan LABEL 'Cancelación' WHEN fchcan <> ?
        imptot LABEL 'Importe'
        fmapgo LABEL 'Condición de Venta'
        WITH STREAM-IO NO-BOX NO-UNDERLINE WIDTH 320.
END.
OUTPUT CLOSE.

