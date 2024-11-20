OUTPUT TO c:\tmp\clientes.txt.
FOR EACH gn-clie NO-LOCK WHERE codcia = 000:
    PUT
        gn-clie.codcli '|'
        gn-clie.nomcli
        SKIP.
END.
FOR EACH gn-prov NO-LOCK WHERE gn-prov.codcia = 000:
    PUT
        gn-prov.codpro '|'
        gn-prov.nompro
        SKIP.
END.
OUTPUT CLOSE.

