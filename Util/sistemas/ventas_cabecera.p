OUTPUT TO d:\cabecera.txt.
PUT UNFORMATTED
    "DateKey" ";"   
    "CodDiv"  ";"
    "CodDoc"  ";"
    "NroDoc"  ";"        
    "CodCli"  ";"    
    "FmaPgo"  ";"
    "Tipo" 
    SKIP.
FOR EACH ventas_cabecera NO-LOCK WHERE datekey >= 01/01/2023:
    PUT UNFORMATTED 
        Ventas_Cabecera.DateKey ";"
        Ventas_Cabecera.CodDiv ";"
        Ventas_Cabecera.CodDoc ";"
        Ventas_Cabecera.NroDoc ";"
        Ventas_Cabecera.CodCli ";"
        Ventas_Cabecera.FmaPgo ";"
        Ventas_Cabecera.Tipo
        SKIP.
END.
