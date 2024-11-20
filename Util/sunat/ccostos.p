OUTPUT TO c:\tmp\ccosto.txt.
FOR EACH cb-auxi WHERE clfaux = 'cco'
AND codaux <> '':
    PUT
        codaux '|'
        nomaux
        SKIP.
END.
OUTPUT CLOSE.

