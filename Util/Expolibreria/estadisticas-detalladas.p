DEF VAR x-nompro LIKE gn-prov.nompro.
DEF VAR x-nomcli LIKE gn-clie.nomcli.

DEF TEMP-TABLE detalle
    FIELD codcia LIKE faccpedi.codcia
    FIELD coddoc LIKE faccpedi.coddoc
    FIELD nroped LIKE faccpedi.nroped
    FIELD fchped LIKE faccpedi.fchped FORMAT '99/99/9999'
    FIELD codcli LIKE faccpedi.codcli 
    FIELD nomcli LIKE faccpedi.nomcli FORMAT 'x(40)'
    FIELD estado AS CHAR FORMAT 'x(15)'
    FIELD evento AS CHAR FORMAT 'x(10)'
    FIELD codven LIKE faccpedi.codven FORMAT 'x(3)'
    FIELD codmat LIKE facdpedi.codmat FORMAT 'x(6)'
    FIELD canped LIKE facdpedi.canped FORMAT '>>>,>>9.99'
    FIELD canate LIKE facdpedi.canate FORMAT '->>>,>>9.99'
    FIELD undvta LIKE facdpedi.undvta FORMAT 'x(4)'
    FIELD implin LIKE facdpedi.implin FORMAT '>>>,>>9.99'.

/* EXPO 2010 */
FOR EACH faccpedi NO-LOCK WHERE codcia = 1
    AND coddiv = '00015'
    AND coddoc = 'cot'
    AND flgest <> 'A'
    AND ( Faccpedi.fchped >= 10/25/2010 AND Faccpedi.fchped <= 03/31/2011 )
    AND (Faccpedi.fchped < 01/01/2011 OR Faccpedi.UsrSac = "*"):
    DISPLAY faccpedi.nroped faccpedi.fchped.
    PAUSE 0.
    FOR EACH facdpedi OF faccpedi NO-LOCK,
        FIRST almmmatg OF facdpedi NO-LOCK:
        CREATE detalle.
        ASSIGN
            detalle.codcia = faccpedi.codcia
            detalle.evento = "EXPO2010"
            detalle.coddoc = faccpedi.coddoc
            detalle.nroped = faccpedi.nroped
            detalle.fchped = faccpedi.fchped
            detalle.codcli = faccpedi.codcli
            detalle.nomcli = faccpedi.nomcli
            detalle.codven = faccpedi.codven
            detalle.codmat = facdpedi.codmat.
        ASSIGN
            detalle.undvta = almmmatg.undbas
            detalle.canped = facdpedi.canped * facdpedi.factor 
            detalle.canate = facdpedi.canate * facdpedi.factor 
            detalle.implin = facdpedi.implin.
        CASE FaccPedi.FlgEst:
            WHEN "A" THEN detalle.estado = "ANULADO".
            WHEN "C" THEN detalle.estado = "ATENDIDO".
            WHEN "P" THEN detalle.estado = "PENDIENTE".
            WHEN "V" THEN detalle.estado = "VENCIDO".
            WHEN "X" THEN detalle.estado = "CERRADA".
       END CASE.         
    END.
END.
/*
/* EXPO 2011 */
FOR EACH faccpedi NO-LOCK WHERE codcia = 1
    AND coddiv = '00015'
    AND coddoc = 'cot'
    AND flgest <> 'A'
    AND (Faccpedi.fchped >= 01/01/2011 AND Faccpedi.fchped <= 03/31/2011
         AND Faccpedi.UsrSac <> '*'):
    DISPLAY faccpedi.nroped faccpedi.fchped.
    PAUSE 0.
    FOR EACH facdpedi OF faccpedi NO-LOCK,
        FIRST almmmatg OF facdpedi NO-LOCK:
        CREATE detalle.
        ASSIGN
            detalle.codcia = faccpedi.codcia
            detalle.evento = "EXPO2011"
            detalle.coddoc = faccpedi.coddoc
            detalle.nroped = faccpedi.nroped
            detalle.fchped = faccpedi.fchped
            detalle.codcli = faccpedi.codcli
            detalle.nomcli = faccpedi.nomcli
            detalle.codven = faccpedi.codven
            detalle.codmat = facdpedi.codmat.
        ASSIGN
            detalle.undvta = almmmatg.undbas
            detalle.canped = facdpedi.canped * facdpedi.factor 
            detalle.canate = facdpedi.canate * facdpedi.factor 
            detalle.implin = facdpedi.implin.
        CASE FaccPedi.FlgEst:
            WHEN "A" THEN detalle.estado = "ANULADO".
            WHEN "C" THEN detalle.estado = "ATENDIDO".
            WHEN "P" THEN detalle.estado = "PENDIENTE".
            WHEN "V" THEN detalle.estado = "VENCIDO".
            WHEN "X" THEN detalle.estado = "CERRADA".
       END CASE.         
    END.
END.

/* PROVINCIAS */
FOR EACH faccpedi NO-LOCK WHERE codcia = 1
    AND coddiv = '00000'
    AND coddoc = 'cot'
    AND flgest <> 'A'
    AND nroped BEGINS '040'
    AND fchped >= 10/25/2010
    AND fchped <= 03/31/2011:
    DISPLAY faccpedi.nroped faccpedi.fchped.
    PAUSE 0.
    FOR EACH facdpedi OF faccpedi NO-LOCK,
        FIRST almmmatg OF facdpedi NO-LOCK:
        CREATE detalle.
        ASSIGN
            detalle.codcia = faccpedi.codcia
            detalle.evento = "PROVINCIAS"
            detalle.coddoc = faccpedi.coddoc
            detalle.nroped = faccpedi.nroped
            detalle.fchped = faccpedi.fchped
            detalle.codcli = faccpedi.codcli
            detalle.nomcli = faccpedi.nomcli
            detalle.codven = faccpedi.codven
            detalle.codmat = facdpedi.codmat.
        ASSIGN
            detalle.undvta = almmmatg.undbas
            detalle.canped = facdpedi.canped * facdpedi.factor 
            detalle.canate = facdpedi.canate * facdpedi.factor 
            detalle.implin = facdpedi.implin.
        CASE FaccPedi.FlgEst:
            WHEN "A" THEN detalle.estado = "ANULADO".
            WHEN "C" THEN detalle.estado = "ATENDIDO".
            WHEN "P" THEN detalle.estado = "PENDIENTE".
            WHEN "V" THEN detalle.estado = "VENCIDO".
            WHEN "X" THEN detalle.estado = "CERRADA".
       END CASE.         
    END.
END.
*/
OUTPUT TO c:\tmp\expo2010.txt.
FOR EACH detalle NO-LOCK,
    FIRST almmmatg OF detalle NO-LOCK,
    FIRST gn-ven OF detalle NO-LOCK:
    FIND gn-prov WHERE gn-prov.codcia = 0
        AND gn-prov.codpro = almmmatg.codpr1 NO-LOCK NO-ERROR.
    IF AVAILABLE gn-prov THEN x-nompro = gn-prov.nompro.
    ELSE x-nompro = ''.
    DISPLAY
        detalle.evento '|'
        detalle.coddoc '|' detalle.nroped '|' detalle.fchped '|'
        detalle.codcli detalle.nomcli '|' detalle.estado '|'
        detalle.codven FORMAT 'x(3)' gn-ven.nomven FORMAT 'x(30)' '|'
        almmmatg.chr__02 FORMAT 'x(1)' '|'
        detalle.codmat  FORMAT 'x(6)' almmmatg.desmat '|'
        almmmatg.desmar FORMAT 'x(15)' '|'
        almmmatg.codfam '|' 
        almmmatg.subfam '|'
        detalle.canped FORMAT '>>>>>9.99' '|'
        detalle.canate FORMAT '->>>>>9.99' '|'
        detalle.undvta '|'
        detalle.implin FORMAT '>>>>>9.99' '|'
        WITH STREAM-IO NO-BOX NO-LABELS NO-UNDERLINE WIDTH 320.
END.
OUTPUT CLOSE.

MESSAGE 'fin'.
