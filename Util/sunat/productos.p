OUTPUT TO c:\tmp\productos.txt.
FOR EACH almmmatg NO-LOCK WHERE codcia = 1:
    PUT
        codmat '|'
        desmat '|'
        undstk
        SKIP.
END.
OUTPUT CLOSE.

