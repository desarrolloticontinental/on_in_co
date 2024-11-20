OUTPUT TO c:\tmp\almacenes.txt.
FOR EACH almacen NO-LOCK WHERE codcia = 1:
    PUT
        codalm '|'
         Almacen.Descripcion
        SKIP.
END.
OUTPUT CLOSE.

