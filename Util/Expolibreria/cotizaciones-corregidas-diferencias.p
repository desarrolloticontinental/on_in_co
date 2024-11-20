
DEF BUFFER b-cpedi FOR faccpedi.
OUTPUT TO c:\tmp\expo2010.txt.
FOR EACH faccpedi NO-LOCK WHERE codcia = 1
    AND coddoc = 'cot'
    AND coddiv = '00015'
    AND fchped >= 11/30/09
    AND flgest <> 'a',
    FIRST b-cpedi NO-LOCK WHERE b-cpedi.codcia = faccpedi.codcia
    AND b-cpedi.coddoc = 'cel'
    AND b-cpedi.coddiv = faccpedi.coddiv
    AND b-cpedi.nroped = faccpedi.nroped:
    DISPLAY
        faccpedi.nroped
        '|'
        faccpedi.fchped
        '|'
        faccpedi.codcli
        '|'
        faccpedi.nomcli
        '|'
        faccpedi.fmapgo
        '|'
        faccpedi.codven
        '|'
        b-cpedi.imptot
        '|'
        faccpedi.imptot
        WITH STREAM-IO NO-BOX NO-LABELS WIDTH 320.
END.
OUTPUT CLOSE.

