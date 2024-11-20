FOR EACH gn-clie EXCLUSIVE-LOCK WHERE codcia = 0
    AND libre_c01 = "N" 
    AND flgsit = "C"
    AND dni > ''
    AND FlagUsuario = "SYSTEM":
    DISPLAY codcli dni flgsit fching FlagFecha WITH STREAM-IO.
    PAUSE 0.
    ASSIGN
        gn-clie.flgsit = "A"
        gn-clie.usuario = "SYSTEM"
        gn-clie.fchact = TODAY.
    IF gn-clie.Flgsit = 'C'
        THEN gn-clie.fchces = TODAY.
    ELSE gn-clie.fchces = ?.
    /* RHC 25.10.04 Historico */
    RUN lib/logtabla ("gn-clie", STRING(gn-clie.codcia, '999') + '|' +
                      STRING(gn-clie.codcli, 'x(11)'), "WRITE").
END.

