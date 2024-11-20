OUTPUT TO c:\tmp\unitario.txt.
FOR EACH ccbcdocu NO-LOCK WHERE codcia = 1
    AND (coddoc = 'fac' OR coddoc = 'bol'
         OR coddoc = 'tck')
    AND fchdoc >= 01/01/10
    AND flgest <> 'a'
    AND codped = 'p/m',
    FIRST faccpedm NO-LOCK WHERE faccpedm.codcia = 1
    AND faccpedm.coddoc = ccbcdocu.codped
    AND faccpedm.nroped = ccbcdocu.nroped:
    FOR EACH ccbddocu OF ccbcdocu NO-LOCK WHERE codmat = '017891':
        DISPLAY
            ccbcdocu.coddiv
            ccbcdocu.fchdoc
            ccbcdocu.coddoc
            ccbcdocu.nrodoc
            (IF ccbcdocu.codmon = 1 THEN 'S/.' ELSE 'US$')
            'con tarjeta' WHEN faccpedm.flgsit = 'T' 
            codmat
            preuni / factor COLUMN-LABEL 'Unitario!por unidad base'
            ccbddocu.undvta
            ccbddocu.candes
            WITH STREAM-IO NO-BOX WIDTH 200.
    END.
END.


