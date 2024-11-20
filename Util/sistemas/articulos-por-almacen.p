DEFINE VARIABLE cDesFam AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cSubFam AS CHARACTER   NO-UNDO.

    
OUTPUT TO "d:\tmp\articulos-por-almacen.txt".
PUT UNFORMATTED
    "Almacen|Ubicacion|Codigo|Descripcion|Unidad|Peso en kg|Volumen en cm3" 
    SKIP.

FOR EACH almmmate NO-LOCK WHERE almmmate.codcia = 1 AND
    almmmate.codalm = '11',
    FIRST almmmatg OF almmmate NO-LOCK :
    PUT UNFORMATTED
        almmmate.codalm "|"
        almmmate.codubi "|"
        almmmatg.codmat "|"
        almmmatg.desmat "|"
        Almmmatg.UndStk "|"
        almmmatg.pesmat "|"
        almmmatg.libre_d02
        SKIP.
END.
OUTPUT TO CLOSE.
