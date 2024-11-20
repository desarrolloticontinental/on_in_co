OUTPUT TO c:\tmp\licencias.txt.
FOR EACH almtabla WHERE tabla = 'lc':
    DISPLAY
        almtabla.Codigo '|'
        almtabla.Nombre
        WITH STREAM-IO NO-BOX NO-LABELS WIDTH 320.
END.
