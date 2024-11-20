FOR EACH almubimat EXCLUSIVE-LOCK:
    DELETE almubimat.
END.
FOR EACH logubimat EXCLUSIVE-LOCK:
    DELETE logubimat.
END.

FOR EACH almmmate NO-LOCK WHERE almmmate.codcia = 1
    AND almmmate.codubi > '':
    CREATE almubimat.
    ASSIGN
        almubimat.CodCia = 1
        almubimat.CodAlm = almmmate.codalm
        almubimat.CodMat = almmmate.codmat
        almubimat.CodUbi = almmmate.codubi.
    FIND FIRST almtubic WHERE almtubic.CodCia = 1
        AND almtubic.CodAlm = almmmate.codalm
        AND almtubic.CodUbi = almmmate.codubi
        NO-LOCK NO-ERROR.
    IF AVAILABLE almtubic THEN almubimat.codzona = almtubic.codzona.
/*     CREATE logubimat.                            */
/*     BUFFER-COPY almubimat TO logubimat           */
/*         ASSIGN                                   */
/*         logubimat.Evento = "CREATE"              */
/*         logubimat.Fecha = TODAY                  */
/*         logubimat.Hora = STRING(TIME,'HH:MM:SS') */
/*         logubimat.Usuario = 'SYSTEM'.            */
END.
