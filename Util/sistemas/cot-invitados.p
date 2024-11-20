DEF BUFFER eventos FOR gn-divi.

DEF STREAM Reporte.
OUTPUT STREAM Reporte TO d:\cotizaciones.txt.
PUT STREAM Reporte UNFORMATTED 'ORIGEN|LISTA|DOC|NUMERO|CLIENTE|NOMBRE|IMPORTE' SKIP.

FOR EACH gn-divi NO-LOCK WHERE gn-divi.codcia = 1,
    EACH faccpedi NO-LOCK WHERE faccpedi.codcia = 1 AND
    faccpedi.coddiv = gn-divi.coddiv AND
    faccpedi.coddoc = 'COT' AND
    faccpedi.fchped >= DATE(10,01,2019) AND
    faccpedi.flgest <> 'A',
    FIRST eventos NO-LOCK WHERE eventos.codcia = 1 AND
    eventos.coddiv = faccpedi.libre_c01 AND
    eventos.canalventa = "FER",
    FIRST vtaclivip NO-LOCK WHERE VtaCliVip.CodCia = 0 AND 
    VtaCliVip.CodCli = Faccpedi.codcli AND
    VtaCliVip.CodDiv = faccpedi.libre_c01:
    DISPLAY vtaclivip.coddiv vtaclivip.codcli. PAUSE 0.
    PUT STREAM Reporte UNFORMATTED
        faccpedi.coddiv '|'
        faccpedi.libre_c01 '|'
        faccpedi.coddoc '|'
        faccpedi.nroped '|'
        faccpedi.codcli '|'
        faccpedi.nomcli '|'
        faccpedi.imptot
        SKIP.
END.
OUTPUT STREAM Reporte CLOSE.

/*
FOR EACH eventos NO-LOCK WHERE eventos.codcia = 1 AND eventos.canalventa = 'FER',
    EACH vtaclivip NO-LOCK WHERE VtaCliVip.CodCia = 0 AND 
    VtaCliVip.CodDiv = eventos.CodDiv:
    DISPLAY vtaclivip.coddiv vtaclivip.codcli.
    PAUSE 0.
    FOR EACH gn-divi NO-LOCK WHERE gn-divi.codcia = 1,
        EACH faccpedi NO-LOCK WHERE faccpedi.codcia = 1 AND
        faccpedi.coddiv = gn-divi.coddiv AND
        faccpedi.coddoc = 'COT' AND
        faccpedi.codcli = VtaCliVip.CodCli AND
        faccpedi.fchped >= DATE(10,01,2019) AND
        faccpedi.flgest <> 'A':
        IF faccpedi.libre_c01 <> eventos.coddiv THEN NEXT.
        PUT STREAM Reporte UNFORMATTED
            faccpedi.coddiv '|'
            faccpedi.libre_c01 '|'
            faccpedi.coddoc '|'
            faccpedi.nroped '|'
            faccpedi.codcli '|'
            faccpedi.nomcli '|'
            faccpedi.imptot
            SKIP.
    END.
END.
*/


