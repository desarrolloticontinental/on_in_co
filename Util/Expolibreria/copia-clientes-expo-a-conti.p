DEF TEMP-TABLE t-clie LIKE gn-clie.

DEF VAR s-coddiv AS CHAR INIT "10015" NO-UNDO.

INPUT FROM c:\tmp\expo\gn-clie.d.
REPEAT :
    CREATE t-clie.
    IMPORT t-clie.
END.
INPUT CLOSE.

MESSAGE '1ra parte' VIEW-AS ALERT-BOX.
FOR EACH t-clie WHERE t-clie.codcia = 000 AND t-clie.codcli <> "":
    FIND gn-clie OF t-clie NO-LOCK NO-ERROR.
    IF NOT AVAILABLE gn-clie THEN DO:
        CREATE gn-clie.
        BUFFER-COPY t-clie TO gn-clie.
    END.
END.

MESSAGE '2da parte' VIEW-AS ALERT-BOX.
/* actualizamos los clientes asistentes en la expo ene 2013 */
FOR EACH expasist NO-LOCK WHERE codcia = 001
    AND coddiv = s-coddiv
    AND estado[1] = "C"
    AND fecpro >= 01/01/2013,
    FIRST t-clie NO-LOCK WHERE t-clie.codcia = 000
    AND t-clie.codcli = expasist.codcli:
    FIND gn-clie OF t-clie EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE gn-clie THEN NEXT.
    ASSIGN
        gn-clie.NomCli       = t-clie.nomcli
        gn-clie.Ruc          = t-clie.ruc
        gn-clie.DirCli       = t-clie.dircli
        gn-clie.Referencias  = t-clie.referencias
        gn-clie.RepLeg[3]    = t-clie.repleg[3]
        gn-clie.Telfnos[1]   = t-clie.telfnos[1]
        gn-clie.DirEnt       = t-clie.dirent
        gn-clie.Telfnos[3]   = t-clie.telfnos[3]
        gn-clie.E-Mail       = t-clie.e-mail
        gn-clie.NroCard      = t-clie.nrocard.
END.
