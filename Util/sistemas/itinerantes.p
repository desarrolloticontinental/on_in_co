OUTPUT TO d:\revisar.txt.
PUT UNFORMATTED 
    'doc|nro|fecha|partida|dir partida|llegada|des llegada|dir llegada|'
    'articulo|descripcion|marca|cantidad|unidad|linea|sublinea' 
    SKIP.
FOR EACH ccbcdocu NO-LOCK WHERE codcia = 1
    AND tpofac = 'I'
    AND coddoc = "G/R"
    AND flgest <> 'A',
    EACH ccbrdocu NO-LOCK WHERE CcbRdocu.CodCia = CcbCDocu.CodCia
    AND CcbRdocu.CodDoc = CcbCDocu.CodDoc
    AND CcbRdocu.NroDoc = CcbCDocu.NroDoc,
    FIRST almmmatg OF ccbrdoc NO-LOCK,
    FIRST almtfami OF almmmatg NO-LOCK,
    FIRST almsfami OF almmmatg NO-LOCK:
    PUT UNFORMATTED
    ccbcdocu.coddoc   "|"
    ccbcdocu.nrodoc   "|"
    ccbcdocu.fchdoc   "|"
    ccbcdocu.codped   "|"
    ccbcdocu.dircli   "|"
    ccbcdocu.codant   "|"
    ccbcdocu.nomcli   "|"
    ccbcdocu.lugent2  "|"
    ccbrdocu.codmat   "|"
    ccbrdocu.desmat   "|"
    ccbrdocu.desmar   "|"
    ccbrdocu.candes   "|"
    ccbrdocu.undvta   "|"
    almmmatg.codfam " " almtfami.desfam "|"
    almmmatg.subfam " " AlmSFami.dessub 
    SKIP.
END.
