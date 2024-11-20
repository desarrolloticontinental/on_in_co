OUTPUT TO d:\desactivados.txt.
FOR EACH logtabla NO-LOCK WHERE codcia = 1 and 
    tabla = 'gn-clie' and 
    evento = 'desactivado',
    FIRST gn-clie NO-LOCK WHERE gn-clie.codcia = 0 AND 
    gn-clie.codcli = ENTRY(2,valorllave,'|'):
    PUT UNFORMATTED
        gn-clie.codcli '|'
        gn-clie.nomcli 
        SKIP.
END.
