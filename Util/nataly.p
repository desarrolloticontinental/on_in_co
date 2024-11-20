def temp-table t-cco
    field cco like cb-auxi.codaux
    field descco like cb-auxi.nomaux
    field impo_smn as dec
    field impo_sme as dec
    field impo_cmn as dec
    field impo_cme as dec.


for each lg-coser no-lock where codcia = 001
        and flgsit <> 'A'
        and coddoc = 'O/S'
        and fchdoc >= 10/01/2006,
        each lg-doser of lg-coser no-lock:
    find t-cco where t-cco.cco = lg-doser.cco exclusive-lock no-error.
    if not available t-cco then do:
        create t-cco.
        t-cco.cco = lg-doser.cco.
        find cb-auxi where cb-auxi.codcia = 000
            and cb-auxi.codaux = t-cco.cco
            no-lock no-error.
        if available cb-auxi then t-cco.descco = cb-auxi.nomaux.
    end.
    if lg-coser.codmon = 1
    then t-cco.impo_smn = t-cco.impo_smn + lg-doser.imptot.
    else t-cco.impo_sme = t-cco.impo_sme + lg-doser.imptot.
end.
for each lg-coser no-lock where codcia = 001
        and flgsit <> 'A'
        and coddoc = 'OCA'
        and fchdoc >= 10/01/2006,
        each lg-doser of lg-coser no-lock:
    find t-cco where t-cco.cco = lg-doser.cco exclusive-lock no-error.
    if not available t-cco then do:
        create t-cco.
        t-cco.cco = lg-doser.cco.
        find cb-auxi where cb-auxi.codcia = 000
            and cb-auxi.codaux = t-cco.cco
            no-lock no-error.
        if available cb-auxi then t-cco.descco = cb-auxi.nomaux.
    end.
    if lg-coser.codmon = 1
    then t-cco.impo_cmn = t-cco.impo_cmn + lg-doser.imptot.
    else t-cco.impo_cme = t-cco.impo_cme + lg-doser.imptot.
end.
        
output to c:\tmp\nataly01.txt.    
for each t-cco by t-cco.cco:
    display
        t-cco.cco
        t-cco.descco
        t-cco.impo_smn
        t-cco.impo_sme
        t-cco.impo_cmn
        t-cco.impo_cme
        with stream-io no-box no-underline width 320.
end.    
output close.
