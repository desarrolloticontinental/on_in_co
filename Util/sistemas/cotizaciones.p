SESSION:DATE-FORMAT = "ymd".
OUTPUT TO d:\tmp\cotizaciones.txt.
FOR EACH faccpedi NO-LOCK WHERE codcia = 1
    AND coddoc = 'cot'
    AND fchped >= DATE(07,01,2018)
    AND fchped <= DATE(03,31,2019)
    AND flgest <> 'A',
    FIRST gn-divi NO-LOCK WHERE gn-divi.codcia = faccpedi.codcia 
    AND gn-divi.coddiv = faccpedi.libre_c01 
    AND gn-divi.canalventa = 'FER',
    EACH facdpedi OF faccpedi NO-LOCK:
    EXPORT DELIMITER "~029" 
        faccpedi.coddiv
        faccpedi.nroped
        faccpedi.fchped
        faccpedi.fchent
        faccpedi.codven
        faccpedi.codcli
        facdpedi.codmat
        facdpedi.undvta
        facdpedi.canped
        facdpedi.implin
        facdpedi.canate
        faccpedi.libre_c01.
END.
OUTPUT CLOSE.

