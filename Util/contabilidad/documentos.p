OUTPUT TO c:\tmp\documentos.txt.
FOR EACH cb-tabl WHERE tabla = '02':
    PUT 
        cb-tabl.Codigo '|'
        cb-tabl.Nombre FORMAT 'x(50)'
        SKIP.
END.
