
OUTPUT TO d:\estadisticas.txt.
FOR EACH logtabla NO-LOCK WHERE codcia = 1
    AND evento = 'RUN-PROGRAM'
    AND dia >= DATE(01,01,2023)
    AND INDEX('vta2/d-listaprecios-contado-v2.w|vtamay/c-conped.w|vtagn/c-conped.w|vtagn/d-almmmate-02.w|vta2/d-listaprecio-contado-v2.w|vtagn/d-almmmate-02-v2.w|vtagn/d-mostrador-sunat-v2.w', valorllave) > 0:
    PUT UNFORMATTED
        tabla ';'
        dia ';'
        hora ';'
        usuario ';'
        valorllave ';'
        (IF INDEX(valorllave,'c-conped') > 0 THEN 'F7'
            ELSE (IF INDEX(valorllave,'d-almmmate') > 0 THEN 'F8'
                ELSE (IF INDEX(valorllave,'d-listaprecios') > 0 THEN 'F8'
                    ELSE (IF INDEX(valorllave,'d-mostrador') > 0 THEN '+CODIGOS'
                        ELSE ''))))
        SKIP.

END.
OUTPUT CLOSE.
