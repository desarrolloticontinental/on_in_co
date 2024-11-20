DEF BUFFER b-clie FOR gn-clie.
DEF VAR x-direc LIKE gn-clie.dircli NO-UNDO.
DEF VAR x-nom LIKE gn-clie.nomcli NO-UNDO.
DEF VAR x-apepat AS CHAR NO-UNDO.
DEF VAR x-apemat AS CHAR NO-UNDO.
DEF VAR x-nombre AS CHAR NO-UNDO.
DEF VAR x-repleg AS CHAR NO-UNDO.
DEF VAR x-repleg2 AS CHAR NO-UNDO.
DEF VAR x-dirent AS CHAR NO-UNDO.
DEF VAR x-contac AS CHAR NO-UNDO.
DEF VAR x-refer AS CHAR NO-UNDO.
   
DEF VAR i AS INTE NO-UNDO.

FOR EACH b-clie NO-LOCK WHERE b-clie.codcia = 0:
    i = i + 1.
    IF (i MODULO 1000) = 0 THEN DO:
        DISPLAY b-clie.codcli. PAUSE 0.
    END.
    FIND gn-clie WHERE ROWID(gn-clie) = ROWID(b-clie) EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
    IF AVAILABLE gn-clie THEN DO:
        /*
        RUN lib/limpiar-texto (gn-clie.nomcli, '', OUTPUT x-nom).
        RUN lib/limpiar-texto (gn-clie.dircli, '', OUTPUT x-direc).
        RUN lib/limpiar-texto (gn-clie.apepat, '', OUTPUT x-apepat).
        RUN lib/limpiar-texto (gn-clie.apemat, '', OUTPUT x-apemat).
        RUN lib/limpiar-texto (gn-clie.nombre, '', OUTPUT x-nombre).
        RUN lib/limpiar-texto (gn-clie.repleg[1], '', OUTPUT x-repleg).
        RUN lib/limpiar-texto (gn-clie.repleg[4], '', OUTPUT x-repleg2).
        RUN lib/limpiar-texto (gn-clie.dirent, '', OUTPUT x-dirent).
        RUN lib/limpiar-texto (gn-clie.contac, '', OUTPUT x-contac).
        */
        RUN lib/limpiar-texto (gn-clie.Referencias, '', OUTPUT x-refer).
        /*
        gn-clie.nomcli = x-nom.
        gn-clie.dircli = x-direc.
        gn-clie.apepat = x-apepat.
        gn-clie.apemat = x-apemat.
        gn-clie.nombre = x-nombre.
        gn-clie.repleg[1] = x-repleg.
        gn-clie.repleg[4] = x-repleg2.
        gn-clie.dirent = x-dirent.
        gn-clie.contac = x-contac.
        */
        gn-clie.referencias = x-refer.
        RELEASE gn-clie.
    END.
END.

