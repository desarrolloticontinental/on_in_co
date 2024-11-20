DISABLE TRIGGERS FOR LOAD OF integral.faccpedi.
DISABLE TRIGGERS FOR LOAD OF integral.facdpedi.

FOR EACH integral.faccpedi EXCLUSIVE-LOCK WHERE integral.faccpedi.codcia = 1
    AND integral.faccpedi.coddiv = '00519'
    AND integral.faccpedi.coddoc = 'cot'
    AND integral.faccpedi.deliverygroup = "6",
    FIRST base.faccpedi NO-LOCK WHERE base.faccpedi.codcia = integral.faccpedi.codcia
    AND base.faccpedi.coddiv = integral.faccpedi.coddiv
    AND base.faccpedi.coddoc = integral.faccpedi.coddoc
    AND base.faccpedi.nroped = integral.faccpedi.nroped:
    FOR EACH integral.facdpedi OF integral.faccpedi EXCLUSIVE-LOCK,
        FIRST base.facdpedi OF base.faccpedi NO-LOCK WHERE base.facdpedi.codmat = integral.facdpedi.codmat:
        ASSIGN
            integral.facdpedi.preuni = base.facdpedi.preuni
            integral.facdpedi.pordto = base.facdpedi.pordto
            integral.facdpedi.pordto2 = base.facdpedi.pordto2
            integral.facdpedi.impdto  = base.facdpedi.impdto
            integral.facdpedi.implin = base.facdpedi.implin
            integral.facdpedi.aftigv = base.facdpedi.aftigv
            integral.facdpedi.aftisc = base.facdpedi.aftisc
            integral.facdpedi.prebas = base.facdpedi.prebas
            integral.facdpedi.impigv = base.facdpedi.impigv
            integral.facdpedi.impisc = base.facdpedi.impisc
            integral.facdpedi.por_dsctos[1] = base.facdpedi.por_dsctos[1]
            integral.facdpedi.por_dsctos[2] = base.facdpedi.por_dsctos[2]
            integral.facdpedi.por_dsctos[3] = base.facdpedi.por_dsctos[3]
            integral.facdpedi.impdto2 = base.facdpedi.impdto2
            integral.facdpedi.libre_d01 = base.facdpedi.libre_d01
            integral.facdpedi.libre_d02 = base.facdpedi.libre_d02
            integral.facdpedi.libre_d03 = base.facdpedi.libre_d03
            integral.facdpedi.libre_d04 = base.facdpedi.libre_d04
            integral.facdpedi.libre_d05 = base.facdpedi.libre_d05
            integral.facdpedi.preuniweb = base.facdpedi.preuniweb
            integral.facdpedi.implinweb = base.facdpedi.implinweb
            .
    END.
    ASSIGN
        integral.faccpedi.impbrt = base.faccpedi.impbrt
        integral.faccpedi.impexo = base.faccpedi.impexo
        integral.faccpedi.porigv = base.faccpedi.porigv
        integral.faccpedi.impdto = base.faccpedi.impdto
        integral.faccpedi.imptot = base.faccpedi.imptot
        integral.faccpedi.impigv = base.faccpedi.impigv
        integral.faccpedi.impisc = base.faccpedi.impisc
        integral.faccpedi.impvta = base.faccpedi.impvta
        integral.faccpedi.impfle = base.faccpedi.impfle
        integral.faccpedi.pordto = base.faccpedi.pordto
        integral.faccpedi.flgigv = base.faccpedi.flgigv
        integral.faccpedi.libre_d01 = base.faccpedi.libre_d01
        integral.faccpedi.libre_d02 = base.faccpedi.libre_d02
        integral.faccpedi.impdto2 = base.faccpedi.impdto2
        .
    
END.

