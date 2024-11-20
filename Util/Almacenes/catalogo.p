DEFINE VARIABLE cDesFam AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cSubFam AS CHARACTER   NO-UNDO.

    
OUTPUT TO "d:\tmp\catalogo.txt".
PUT UNFORMATTED
    "CodMat|Descripcion|CodMar|Marca|Familia|DesFam|SubFamilia|DesSub|Unidad|Costo Soles|Pre Ofi Soles|UndOfi|Proveedor|Nom Proveedor" 
    "|Empaque Master|Empaque Inner|Min Vta May|Min Vta Min"
    SKIP.

FOR EACH almmmatg WHERE almmmatg.codcia = 1 AND almmmatg.tpoart <> "D" NO-LOCK,
    FIRST almtfami OF almmmatg NO-LOCK,
    FIRST almsfami OF almmmatg NO-LOCK:
    FIND gn-prov WHERE gn-prov.codcia = 000
        AND gn-prov.codpro = almmmatg.codpr1 NO-LOCK NO-ERROR.
    PUT UNFORMATTED
        almmmatg.codmat "|"
        almmmatg.desmat "|"
        almmmatg.codmar "|"
        almmmatg.desmar "|"
        almmmatg.codfam "|"
        Almtfami.desfam "|"
        almmmatg.subfam "|"
        AlmSFami.dessub "|"
        Almmmatg.UndStk "|"
        (IF almmmatg.monvta = 1 THEN almmmatg.ctotot ELSE almmmatg.ctotot * almmmatg.tpocmb) "|"
        (IF almmmatg.monvta = 1 THEN almmmatg.preofi ELSE almmmatg.preofi * almmmatg.tpocmb) "|"
        almmmatg.CHR__01 "|"
        almmmatg.codpr1 "|"
        (IF AVAILABLE gn-prov THEN gn-prov.nompro ELSE "") '|'
        Almmmatg.CanEmp "|"
        Almmmatg.StkRep "|"
        Almmmatg.DEC__03 "|"
        Almmmatg.StkMin
        SKIP.
END.
OUTPUT TO CLOSE.
