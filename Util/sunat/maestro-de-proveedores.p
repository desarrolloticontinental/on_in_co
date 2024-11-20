OUTPUT TO c:\tmp\proveedores.txt.
FOR EACH gn-prov NO-LOCK WHERE codcia = 000:
    DISPLAY
         gn-prov.CodPro gn-prov.NomPro gn-prov.Ruc
        WITH STREAM-IO NO-BOX WIDTH 200.
END.
