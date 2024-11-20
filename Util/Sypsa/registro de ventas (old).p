DEF STREAM cabecera.
DEF STREAM detalle.

OUTPUT STREAM cabecera TO c:\tmp\ccbcdocu.txt.
OUTPUT STREAM detalle TO c:\tmp\ccbddocu.txt.

    PUT STREAM cabecera UNFORMATTED
        'coddoc|' +
        'nrodoc|' +
        'fchdoc|' +
        'codcli|' +
        'nomcli|' +
        'dircli|' +
        'ruccli|' +
        'codant|' +
        'codped|' +
        'nroped|' +
        'nroord|' +
        'impbrt|' +
        'impexo|' +
        'porigv|' +
        'impigv|' +
        'impdto|' +
        'imptot|' +
        'sdoact|' +
        'flgest|' +
        'usuario|' +
        'usrdscto|' +
        'codmon|' +
        'tpocmb|' +
        'codalm|' +
        'lugent|' +
        'tipo|' +
        'codmov|' +
        'codven|' +
        'impvta|' +
        'fchcan|' +
        'glosa|' +
        'codref|' +
        'nroref|' +
        'fchvto|' +
        'coddiv|' +
        'fmapgo|' +
        'flgcbd|' +
        'fchanu|' +
        'usuanu|' +
        'coddpto|' +
        'codprov|' +
        'coddist|' +
        'impcto|' +
        'nrocard|' +
        'sede|' +
        'divori'
        SKIP.

    PUT STREAM detalle UNFORMATTED
        'CodDoc|' +
        'NroDoc|' +
        'NroItm|' +
        'UndVta|' +
        'codmat|' +
        'PreUni|' +
        'PorDto|' +
        'PorDto2|' +
        'ImpDto|' +
        'ImpLin|' +
        'CanDes|' +
        'AftIgv|' +
        'PreBas|' +
        'ImpIgv|' +
        'Factor|' +
        'AlmDes|' +
        'Por_Dsctos[1]|' +
        'Por_Dsctos[2]|' +
        'Por_Dsctos[3]|' +
        'CodDiv|' +
        'FchDoc|' +
        'ImpCto|' +
        'ImpDto2'
        SKIP.

FOR EACH ccbcdocu NO-LOCK WHERE codcia = 001
    AND fchdoc >= 06/01/2012
    AND fchdoc <= TODAY - 1
    AND LOOKUP(coddoc, 'fac,bol,tck') > 0:
    PUT STREAM cabecera
        coddoc '|'
        nrodoc '|'
        fchdoc '|'
        codcli '|'
        nomcli '|'
        dircli '|'
        ruccli '|'
        codant '|'
        codped '|'
        nroped '|'
        nroord '|'
        impbrt '|'
        impexo '|'
        porigv '|'
        impigv '|'
        impdto '|'
        imptot '|'
        sdoact '|'
        flgest '|'
        usuario '|'
        usrdscto '|'
        codmon '|'
        tpocmb FORMAT '>,>>9.9999' '|'
        codalm '|'
        lugent '|'
        tipo '|'
        codmov '|'
        codven '|'
        impvta '|'
        fchcan '|'
        glosa '|'
        codref '|'
        nroref '|'
        fchvto '|'
        coddiv FORMAT 'x(5)' '|'
        fmapgo '|'
        flgcbd '|'
        fchanu '|'
        usuanu '|'
        coddpto '|'
        codprov '|'
        coddist '|'
        impcto '|'
        nrocard '|'
        sede '|'
        divori
        SKIP.
    FOR EACH ccbddocu OF ccbcdocu NO-LOCK:
        PUT STREAM detalle
            CcbDDocu.CodDoc '|'
            CcbDDocu.NroDoc '|'
            CcbDDocu.NroItm  '|'
            CcbDDocu.UndVta  '|'
            CcbDDocu.codmat  '|'
            CcbDDocu.PreUni  '|'
            CcbDDocu.PorDto '|'
            CcbDDocu.PorDto2  '|'
            CcbDDocu.ImpDto  '|'
            CcbDDocu.ImpLin  '|'
            CcbDDocu.CanDes  '|'
            CcbDDocu.AftIgv  '|'
            CcbDDocu.PreBas  '|'
            CcbDDocu.ImpIgv  '|'
            CcbDDocu.Factor  '|'
            CcbDDocu.AlmDes  '|'
            CcbDDocu.Por_Dsctos[1]  '|'
            CcbDDocu.Por_Dsctos[2]  '|'
            CcbDDocu.Por_Dsctos[3]  '|'
            CcbDDocu.CodDiv  FORMAT 'x(5)' '|'
            CcbDDocu.FchDoc  '|'
            CcbDDocu.ImpCto  '|'
            CcbDDocu.ImpDto2
            SKIP.
    END.
END.
OUTPUT STREAM cabecera CLOSE.
OUTPUT STREAM detalle CLOSE.

