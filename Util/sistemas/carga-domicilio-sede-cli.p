/* Carga direccion fiscal */
FOR EACH gn-clie NO-LOCK WHERE codcia = 000:
    FIND FIRST gn-clied WHERE gn-clied.codcia = gn-clie.codcia AND
        gn-clied.codcli = gn-clie.codcli AND
        gn-clied.sede = '@@@' NO-LOCK NO-ERROR.
    IF AVAILABLE gn-clied THEN NEXT.
    DISPLAY gn-clie.codcli FORMAT 'x(11)'. PAUSE 0.
    CREATE gn-clied.
    BUFFER-COPY gn-clie TO gn-clied
        ASSIGN 
        gn-clied.sede = '@@@'
        Gn-ClieD.DomFiscal = YES.
END.

FOR EACH gn-clied WHERE gn-clied.codcia = 000 AND
    TRUE <> (gn-clied.codpos > ''):
    DISPLAY gn-clieD.codcli FORMAT 'x(11)'. PAUSE 0.
    FIND TabDistr WHERE TabDistr.CodDepto = Gn-ClieD.CodDept AND
        TabDistr.CodProvi = Gn-ClieD.CodProv AND
        TabDistr.CodDistr = Gn-ClieD.CodDist 
        NO-LOCK NO-ERROR.
    IF AVAILABLE TabDistr THEN Gn-ClieD.Codpos = TabDistr.CodPos.
END.

