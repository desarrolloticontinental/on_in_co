OUTPUT TO c:\tmp\ordenes.txt.
FOR EACH faccpedi NO-LOCK WHERE codcia = 1
    AND coddiv ='00018'
    AND coddoc = 'o/d'
    AND flgest = 'P':
    DISPLAY
        coddoc
        nroped
        fchped
        codalm
        codcli
        nomcli
        imptot
        WITH STREAM-IO NO-BOX width 320.
END.
OUTPUT CLOSE.

