OUTPUT TO c:\tmp\provincias.txt.
FOR EACH ccbcdocu NO-LOCK WHERE codcia = 001
    AND coddoc = 'fac'
    AND fchdoc >= 07/01/2013
    AND divori = '00018'
    AND flgest <> 'a'
    AND acubon[5] > 0,
    EACH ccbddocu OF ccbcdocu NO-LOCK WHERE ccbddocu.ImpDcto_Adelanto[5] > 0,
    FIRST almmmatg OF ccbddocu NO-LOCK.
    DISPLAY
        ccbcdocu.fchdoc
        ccbcdocu.coddoc
        ccbcdocu.nrodoc
        ccbcdocu.codcli
        ccbcdocu.nomcli
        ccbddocu.codmat
        almmmatg.desmat
        almmmatg.codfam
        almmmatg.subfam
        ccbddocu.candes
        ccbddocu.undvta
        ccbddocu.implin
        ccbddocu.PorDcto_Adelanto[5] COLUMN-LABEL '% Percepcion'
        ccbddocu.ImpDcto_Adelanto[5] COLUMN-LABEL 'Percepcion'
        WITH STREAM-IO NO-BOX NO-UNDERLINE WIDTH 320.

END.
OUTPUT CLOSE.

