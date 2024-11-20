OUTPUT TO c:\tmp\hr-servicios.txt.
FOR EACH di-rutac NO-LOCK WHERE codcia = 1
    AND DI-RutaC.CodDoc = 'H/R'
    AND DI-RutaC.FchDoc >= 12/01/2009
    AND flgest <> 'A',
    EACH di-rutad OF di-rutac NO-LOCK,
    FIRST ccbcdocu NO-LOCK WHERE ccbcdocu.codcia = 1
    AND ccbcdocu.coddoc = di-rutad.codref
    AND ccbcdocu.nrodoc = di-rutad.nroref
    AND ( ccbcdocu.tpofac = 'A' OR ccbcdocu.tpofac = 'S' ):
    DISPLAY 
        ccbcdocu.coddoc 
        "|"
        ccbcdocu.nrodoc 
        "|"
        ccbcdocu.nomcli
        "|"
        ccbcdocu.codcli
        "|"
        ccbcdocu.codmon
        "|"
        ccbcdocu.imptot
        "|"
        DI-RutaC.CodDiv FORMAT 'x(5)'
        "|"
        DI-RutaC.CodVeh FORMAT 'x(10)'
        "|"
        DI-RutaC.FchDoc 
        "|"
        DI-RutaC.Nomtra 
        "|"
        DI-RutaC.NroDoc 
        "|"
        DI-RutaC.usuario
        WITH STREAM-IO NO-BOX WIDTH 320.
END.
