DEF TEMP-TABLE t-detalle 
    FIELD datekey LIKE Ventas_Detalle.DateKey
    FIELD coddiv LIKE Ventas_Detalle.CodDiv
    FIELD coddoc LIKE Ventas_Detalle.CodDoc
    FIELD nrodoc LIKE Ventas_Detalle.NroDoc
    FIELD codmat LIKE Ventas_Detalle.CodMat
    FIELD cantidad LIKE Ventas_Detalle.Cantidad
    FIELD Impnacsigv LIKE Ventas_Detalle.Impnacsigv
    FIELD Promnacsigv LIKE Ventas_Detalle.Promnacsigv
    FIELD codfam LIKE Ventas_Detalle.CodFam
    FIELD subfam LIKE Ventas_Detalle.SubFam
    .

FOR EACH ventas_detalle NO-LOCK WHERE datekey >= 01/01/2023
    AND Impnacsigv <> 0:
    CREATE t-detalle.
    BUFFER-COPY ventas_detalle TO t-detalle.
END.

OUTPUT TO d:\detalle.txt.
PUT UNFORMATTED
    "DateKey" ";"   
    "CodDiv"  ";"
    "CodDoc"  ";"
    "NroDoc"  ";"        
    "CodMat"  ";"    
    "Cantidad"  ";"
    "Importe" ";"
    "Costo" ";"
    "Linea" ";"
    "SubLinea"
    SKIP.
FOR EACH t-detalle NO-LOCK:
    FIND FIRST gn-divi WHERE gn-divi.codcia = 1
        AND gn-divi.coddiv = t-detalle.CodDiv NO-LOCK NO-ERROR.
    IF NOT AVAILABLE gn-divi THEN NEXT.
    FIND FIRST almmmatg WHERE almmmatg.codcia = 1
        AND almmmatg.codmat = t-detalle.codmat NO-LOCK NO-ERROR.
    IF NOT AVAILABLE almmmatg THEN NEXT.
    FIND FIRST almtfami WHERE almtfami.codcia = 1
        AND almtfami.codfam = t-detalle.CodFam 
        AND Almtfami.SwComercial = YES
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE almtfami THEN NEXT.
    FIND FIRST almsfami WHERE almsfami.codcia = 1
        AND almsfami.codfam = t-detalle.CodFam
        AND almsfami.subfam = t-detalle.SubFam 
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE almsfami THEN NEXT.
    PUT UNFORMATTED 
        t-detalle.DateKey          ";"
        t-detalle.CodDiv           ";"
        t-detalle.CodDoc           ";"
        t-detalle.NroDoc           ";"
        t-detalle.CodMat           ";"
        t-detalle.Cantidad         ";"
        t-detalle.ImpNacSIGV       ";"
        t-detalle.PromNacSIGV      ";"
        t-detalle.CodFam           ";"
        t-detalle.SubFam
        SKIP.
END.
