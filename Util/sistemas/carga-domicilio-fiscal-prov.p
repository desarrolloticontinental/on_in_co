/* Carga direccion fiscal */
FOR EACH gn-prov NO-LOCK WHERE codcia = 000:
    FIND FIRST gn-provd WHERE gn-provd.codcia = gn-prov.codcia AND
        gn-provd.codpro = gn-prov.codpro AND
        gn-provd.sede = '@@@' NO-LOCK NO-ERROR.
    IF AVAILABLE gn-provd THEN NEXT.
    DISPLAY gn-prov.codpro FORMAT 'x(11)'. PAUSE 0.
    CREATE gn-provd.
    BUFFER-COPY gn-prov TO gn-provd
        ASSIGN 
        gn-provd.sede = '@@@'
        gn-provd.DomFiscal = YES.
END.

FOR EACH gn-provd WHERE gn-provd.codcia = 000 AND
    TRUE <> (gn-provd.codpos > ''):
    DISPLAY gn-provd.codpro FORMAT 'x(11)'. PAUSE 0.
    FIND TabDistr WHERE TabDistr.CodDepto = gn-provd.CodDept AND
        TabDistr.CodProvi = gn-provd.CodProv AND
        TabDistr.CodDistr = gn-provd.CodDist 
        NO-LOCK NO-ERROR.
    IF AVAILABLE TabDistr THEN gn-provd.Codpos = TabDistr.CodPos.
END.
