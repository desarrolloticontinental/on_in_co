DEF VAR s-codcia AS INT INIT 001.
DEF VAR s-coddiv AS CHAR INIT '00067'.
DEF BUFFER b-tabla FOR vtatabla.

OUTPUT TO d:\tmp\factor-flete.txt.
PUT UNFORMATTED 
    'DIVISION|LINEA|FACTOR|SUBLINEA|FACTOR' SKIP.
FOR EACH gn-divi NO-LOCK WHERE gn-divi.codcia = 001,
    EACH vtatabla NO-LOCK WHERE vtatabla.codcia = s-codcia
    AND vtatabla.tabla = "DIVFACXLIN"
    AND vtatabla.llave_c1 = gn-divi.coddiv,
    EACH b-tabla NO-LOCK WHERE b-tabla.codcia = s-codcia
    AND b-tabla.tabla = "DIVFACXSLIN"
    AND b-tabla.llave_c1 = s-coddiv
    AND b-tabla.llave_c2 = vtatabla.llave_c2:
    PUT UNFORMATTED
        gn-divi.coddiv '|'
        vtatabla.llave_c2 '|'
        VtaTabla.Valor[1] '|'
        b-tabla.llave_c3  '|'
        b-tabla.valor[1]
        SKIP.

END.
