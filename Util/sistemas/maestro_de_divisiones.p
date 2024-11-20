OUTPUT TO d:\tmp\divisiones.txt.
PUT UNFORMATTED 
    'ESTADO|CODIGO|NOMBRE|DIRECCION|DIRECCION2|CANAL DE VENTA|AJUSTE POR FLETE'
    SKIP.
FOR EACH gn-divi NO-LOCK WHERE codcia = 001:
    PUT UNFORMATTED
        campo-log[1] FORMAT 'Inactivo/Activo' '|'
        coddiv '|'
        desdiv '|'
        dirdiv '|'
        faxdiv '|'
        canalventa '|'
        campo-log[4] FORMAT 'Si/No' '|'
        SKIP.
END.
OUTPUT CLOSE.

