DEF TEMP-TABLE Detalle
    FIELD codcli LIKE faccpedi.codcli
    FIELD nomcli LIKE faccpedi.nomcli
    FIELD dircli LIKE faccpedi.dircli
    FIELD codpos LIKE gn-clie.codpos
    FIELD nompos LIKE almtabla.nombre
    FIELD coddept LIKE gn-clie.coddept
    FIELD nomdepto LIKE tabdepto.nomdepto
    FIELD codprov LIKE gn-clie.codprov
    FIELD nomprovi LIKE tabprovi.nomprovi
    FIELD coddist LIKE gn-clie.coddist
    FIELD nomdistr LIKE tabdistr.nomdistr.

OUTPUT TO c:\tmp\cli-expo.txt.
FOR EACH faccpedi NO-LOCK WHERE codcia = 1
    AND coddiv = '00015'
    AND coddoc = 'cot'
    AND fchped >= 11/30/09
    AND flgest <> 'a':
    FIND detalle WHERE detalle.codcli = faccpedi.codcli
        NO-LOCK NO-ERROR.
    IF AVAILABLE detalle THEN NEXT.
    CREATE detalle.
    ASSIGN
        detalle.codcli = faccpedi.codcli
        detalle.nomcli = faccpedi.nomcli
        detalle.dircli = faccpedi.dircli
        detalle.codpos = faccpedi.codpos.
    FIND gn-clie WHERE gn-clie.codcia = 000
        AND gn-clie.codcli = faccpedi.codcli
        NO-LOCK NO-ERROR.
    IF AVAILABLE gn-clie THEN
        ASSIGN
        detalle.coddept = gn-clie.coddept
        detalle.codprov = gn-clie.codprov
        detalle.coddist = gn-clie.coddist.
    FIND TabDepto WHERE TabDepto.CodDepto = detalle.coddept NO-LOCK NO-ERROR.
    IF AVAILABLE TabDepto THEN detalle.nomdepto = TabDepto.NomDepto.
    FIND Tabprovi WHERE Tabprovi.CodDepto = detalle.CodDept
        AND Tabprovi.Codprovi = detalle.codprov NO-LOCK NO-ERROR.
    IF AVAILABLE Tabprovi THEN detalle.nomprovi = Tabprovi.Nomprovi.
    FIND Tabdistr WHERE Tabdistr.CodDepto = detalle.CodDept
        AND Tabdistr.Codprovi = detalle.codprov
        AND Tabdistr.Coddistr = detalle.coddist NO-LOCK NO-ERROR.
    IF AVAILABLE Tabdistr THEN detalle.nomdistr = Tabdistr.Nomdistr.
    FIND almtabla WHERE almtabla.Tabla = 'CP' 
        AND almtabla.Codigo = detalle.codpos NO-LOCK NO-ERROR.
    IF AVAILABLE almtabla THEN detalle.nompos = almtabla.nombre.
END.


FOR EACH detalle:
    DISPLAY
        detalle.codcli COLUMN-LABEL 'Cliente'
        '|'
        detalle.nomcli COLUMN-LABEL 'Nombre'
        '|'
        detalle.dircli COLUMN-LABEL 'Direccion'
        '|'
        detalle.codpos COLUMN-LABEL 'CodPostal'
        '|'
        detalle.nompos COLUMN-LABEL 'Postal'
        '|'
        detalle.nomdepto COLUMN-LABEL 'Departamento'
        '|'
        detalle.nomprovi COLUMN-LABEL 'Provincia'
        '|'
        detalle.nomdistr COLUMN-LABEL 'Distrito'
        WITH STREAM-IO NO-BOX NO-UNDERLINE NO-LABELS WIDTH 320.
END.
OUTPUT CLOSE.

