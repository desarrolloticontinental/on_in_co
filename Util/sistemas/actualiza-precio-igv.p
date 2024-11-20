DISABLE TRIGGERS FOR LOAD OF Almmmatg.
DISABLE TRIGGERS FOR LOAD OF VtaListaMay.
DISABLE TRIGGERS FOR LOAD OF VtaListaMinGn.
DISABLE TRIGGERS FOR LOAD OF Almmmatp.
DISABLE TRIGGERS FOR LOAD OF VtaTabla.
DISABLE TRIGGERS FOR LOAD OF VtaDTabla.

FIND FIRST Sunat_Fact_Electr_Taxs  WHERE Sunat_Fact_Electr_Taxs.TaxTypeCode = "IGV"
    AND Sunat_Fact_Electr_Taxs.Disabled = NO
    AND TODAY >= Sunat_Fact_Electr_Taxs.Start_Date 
    AND TODAY <= Sunat_Fact_Electr_Taxs.End_Date 
    AND Sunat_Fact_Electr_Taxs.Tax > 0
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE Sunat_Fact_Electr_Taxs THEN RETURN.

DEF VAR k AS INTE NO-UNDO.

/* Precios por volumen */
FOR EACH almmmatg EXCLUSIVE-LOCK WHERE codcia = 1 AND Almmmatg.PreVta[1] > 0,
    FIRST Almtfami OF Almmmatg NO-LOCK WHERE Almtfami.SwComercial = YES:
    ASSIGN
        Almmmatg.TasaImpuesto = Sunat_Fact_Electr_Taxs.Tax
        Almmmatg.ImporteUnitarioSinImpuesto = ROUND(Almmmatg.PreOfi / ( 1 + Sunat_Fact_Electr_Taxs.Tax / 100), 4)
        Almmmatg.ImporteUnitarioImpuesto = Almmmatg.PreOfi - Almmmatg.ImporteUnitarioSinImpuesto
        Almmmatg.ImporteUnitarioSinImpuesto_A = ROUND(Almmmatg.Prevta[2] / ( 1 + Sunat_Fact_Electr_Taxs.Tax / 100), 4)
        Almmmatg.ImporteUnitarioImpuesto_A = Almmmatg.Prevta[2] - Almmmatg.ImporteUnitarioSinImpuesto_A
        Almmmatg.ImporteUnitarioSinImpuesto_B = ROUND(Almmmatg.Prevta[3] / ( 1 + Sunat_Fact_Electr_Taxs.Tax / 100), 4)
        Almmmatg.ImporteUnitarioImpuesto_B = Almmmatg.Prevta[3] - Almmmatg.ImporteUnitarioSinImpuesto_B
        Almmmatg.ImporteUnitarioSinImpuesto_C = ROUND(Almmmatg.Prevta[4] / ( 1 + Sunat_Fact_Electr_Taxs.Tax / 100), 4)
        Almmmatg.ImporteUnitarioImpuesto_C = Almmmatg.Prevta[4] - Almmmatg.ImporteUnitarioSinImpuesto_C
        .
    DO k = 1 TO 10:
        ASSIGN
            Almmmatg.TasaImpuesto = Sunat_Fact_Electr_Taxs.Tax
            Almmmatg.DtoVolPSinImpuesto[k] = ROUND(Almmmatg.DtoVolP[k] / ( 1 + Sunat_Fact_Electr_Taxs.Tax / 100), 4)
            Almmmatg.DtoVolPImpuesto[k] = Almmmatg.DtoVolP[k] - Almmmatg.DtoVolPSinImpuesto[k]
            .
    END.
END.

FOR EACH VtaListaMay EXCLUSIVE-LOCK WHERE codcia = 1:
    ASSIGN
        VtaListaMay.TasaImpuesto = Sunat_Fact_Electr_Taxs.Tax
        VtaListaMay.ImporteUnitarioSinImpuesto = ROUND(VtaListaMay.PreOfi / ( 1 + Sunat_Fact_Electr_Taxs.Tax / 100), 4)
        VtaListaMay.ImporteUnitarioImpuesto = VtaListaMay.PreOfi - VtaListaMay.ImporteUnitarioSinImpuesto
        .
    DO k = 1 TO 10:
        ASSIGN
            VtaListaMay.TasaImpuesto = Sunat_Fact_Electr_Taxs.Tax
            VtaListaMay.DtoVolPSinImpuesto[k] = ROUND(VtaListaMay.DtoVolP[k] / ( 1 + Sunat_Fact_Electr_Taxs.Tax / 100), 4)
            VtaListaMay.DtoVolPImpuesto[k] = VtaListaMay.DtoVolP[k] - VtaListaMay.DtoVolPSinImpuesto[k]
            .
    END.
END.

FOR EACH VtaListaMinGn EXCLUSIVE-LOCK WHERE codcia = 1:
    ASSIGN
        VtaListaMinGn.TasaImpuesto = Sunat_Fact_Electr_Taxs.Tax
        VtaListaMinGn.ImporteUnitarioSinImpuesto = ROUND(VtaListaMinGn.PreOfi / ( 1 + Sunat_Fact_Electr_Taxs.Tax / 100), 4)
        VtaListaMinGn.ImporteUnitarioImpuesto = VtaListaMinGn.PreOfi - VtaListaMinGn.ImporteUnitarioSinImpuesto
        .
    DO k = 1 TO 10:
        ASSIGN
            VtaListaMinGn.TasaImpuesto = Sunat_Fact_Electr_Taxs.Tax
            VtaListaMinGn.DtoVolPSinImpuesto[k] = ROUND(VtaListaMinGn.DtoVolP[k] / ( 1 + Sunat_Fact_Electr_Taxs.Tax / 100), 4)
            VtaListaMinGn.DtoVolPImpuesto[k] = VtaListaMinGn.DtoVolP[k] - VtaListaMinGn.DtoVolPSinImpuesto[k]
            .
    END.
END.

FOR EACH Almmmatp EXCLUSIVE-LOCK WHERE codcia = 1:
    ASSIGN
        Almmmatp.TasaImpuesto = Sunat_Fact_Electr_Taxs.Tax
        Almmmatp.ImporteUnitarioSinImpuesto = ROUND(Almmmatp.PreOfi / ( 1 + Sunat_Fact_Electr_Taxs.Tax / 100), 4)
        Almmmatp.ImporteUnitarioImpuesto = Almmmatp.PreOfi - Almmmatp.ImporteUnitarioSinImpuesto
        .
    DO k = 1 TO 10:
        ASSIGN
            Almmmatp.TasaImpuesto = Sunat_Fact_Electr_Taxs.Tax
            Almmmatp.DtoVolPSinImpuesto[k] = ROUND(Almmmatp.DtoVolP[k] / ( 1 + Sunat_Fact_Electr_Taxs.Tax / 100), 4)
            Almmmatp.DtoVolPImpuesto[k] = Almmmatp.DtoVolP[k] - Almmmatp.DtoVolPSinImpuesto[k]
            .
    END.
    DO k = 1 TO 10:
        ASSIGN
            Almmmatp.TasaImpuesto = Sunat_Fact_Electr_Taxs.Tax
            Almmmatp.PromPrecioSinImpuesto[k] = ROUND(Almmmatp.PromPrecio[k] / ( 1 + Sunat_Fact_Electr_Taxs.Tax / 100), 4)
            Almmmatp.PromPrecioImpuesto[k] = Almmmatp.PromPrecio[k] -  Almmmatp.PromPrecioSinImpuesto[k]
            .
    END.
END.

FOR EACH Vtatabla EXCLUSIVE-LOCK WHERE codcia = 1 AND tabla = 'REMATES':
    ASSIGN
        VtaTabla.TasaImpuesto = Sunat_Fact_Electr_Taxs.Tax
        VtaTabla.ImporteUnitarioSinImpuesto = ROUND(VtaTabla.Valor[1] / ( 1 + Sunat_Fact_Electr_Taxs.Tax / 100), 4)
        VtaTabla.ImporteUnitarioImpuesto = VtaTabla.Valor[1] - VtaTabla.ImporteUnitarioSinImpuesto
        .
END.

FOR EACH Vtadtabla EXCLUSIVE-LOCK WHERE VtaDTabla.CodCia = 1
    AND VtaDTabla.Tabla = "UTILEX-ENCARTE"
    AND VtaDTabla.Tipo = "M"
    AND VtaDTabla.Libre_d02 > 0:
    ASSIGN
        VtaDTabla.TasaImpuesto = Sunat_Fact_Electr_Taxs.Tax
        VtaDTabla.ImporteUnitarioSinImpuesto = ROUND(VtaDTabla.Libre_d02 / ( 1 + Sunat_Fact_Electr_Taxs.Tax / 100), 4)
        VtaDTabla.ImporteUnitarioImpuesto = VtaDTabla.Libre_d02 - VtaDTabla.ImporteUnitarioSinImpuesto
        .
END.
