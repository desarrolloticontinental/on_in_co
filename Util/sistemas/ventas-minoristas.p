
OUTPUT TO d:\vtaminorista.txt.
PUT UNFORMATTED
    'COD|NRO|FECHA|ENCARTE|CLIENTE ASOCIADO|COD DEL PERSONAL|'
    'ARTICULO|CANTIDAD|PRE UNI|%DCTO1|%DCTO2|%DCTO3|IMPORTE|IMPORTE DCTO ENCARTE'
    SKIP.
FOR EACH gn-divi NO-LOCK WHERE gn-divi.codcia = 1
    AND gn-divi.canalventa = 'MIN',
    EACH faccpedi WHERE faccpedi.codcia = 1
    AND faccpedi.coddoc = 'p/m'
    AND faccpedi.coddiv = gn-divi.coddiv
    AND faccpedi.flgest = 'C'
    AND faccpedi.fchped >= 01/01/2019,
    EACH facdpedi OF faccpedi NO-LOCK:
    PUT UNFORMATTED
        faccpedi.coddoc             "|"
        faccpedi.nroped             "|"
        faccpedi.fchped             "|"
        faccpedi.libre_c05          "|"
        faccpedi.libre_c03          "|"
        faccpedi.libre_c04          "|"
        facdpedi.codmat             "|"
        facdpedi.canped             "|"
        facdpedi.preuni             "|"
        FacDPedi.Por_Dsctos[1]      "|"
        FacDPedi.Por_Dsctos[2]      "|"
        FacDPedi.Por_Dsctos[3]      "|"
        facdpedi.implin             "|"
        facdpedi.impdto2
        SKIP.
END.
