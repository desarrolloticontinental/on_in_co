DEF TEMP-TABLE t-prov LIKE gn-prov.

FOR EACH cb-dmov NO-LOCK WHERE codcia = 001
    AND periodo >= 2011 AND periodo <= 2012
    AND nromes >= 01 AND nromes <= 12
    AND clfaux = '@PV',
    FIRST gn-prov NO-LOCK WHERE gn-prov.codcia = 000
    AND gn-prov.codpro = cb-dmov.codaux:
    FIND t-prov WHERE t-prov.codcia = gn-prov.codcia
        AND t-prov.codpro = gn-prov.codpro
        NO-ERROR.
    IF NOT AVAILABLE t-prov THEN DO:
        CREATE t-prov.
        BUFFER-COPY gn-prov TO t-prov.
    END.
END.

OUTPUT TO c:\tmp\proveedores.txt.
FOR EACH t-prov NO-LOCK:
    DISPLAY t-prov.codpro t-prov.nompro.
END.
OUTPUT CLOSE.

