
define variable lasd as character.
DEFINE VARIABLE X-DEBE LIKE cb-dmov.impmn1.
DEFINE VARIABLE X-HABER LIKE cb-dmov.impmn1.


OUTPUT TO D:\SIE\SINUSD.TXT.
for each cb-ctas where
    codcia = 0 and
    codcta begins "42" and
    cb-ctas.AftDcb no-lock,
    each cb-dmov where
    cb-dmov.codcia = 1 and
    periodo = 2007 and
    nromes >= 1 and
    nromes <= 12 and
    cb-dmov.codcta = cb-ctas.codcta no-lock:
    lasd = substring(cb-dmov.codcta,length(cb-dmov.codcta),1).
    if lasd = "2" and impmn2 = 0 then
        display
            cb-dmov.coddiv
            periodo
            nromes
            CB-DMOV.codope
            nroast
            cb-dmov.codcta
            codaux
            CB-DMOV.CODDOC
            NRODOC
            impmn1 WHEN NOT tpomov @ X-DEBE         COLUMN-LABEL "DEBE"
            impmn1 WHEN tpomov @ X-HABER    COLUMN-LABEL "HABER"
            tpomov
            with stream-io WIDTH 230.
end.
OUTPUT CLOSE.
