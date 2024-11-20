DISABLE TRIGGERS FOR LOAD OF Vtadctoprom.
DISABLE TRIGGERS FOR LOAD OF Vtadctoprommin.

FIND FIRST Sunat_Fact_Electr_Taxs  WHERE Sunat_Fact_Electr_Taxs.TaxTypeCode = "IGV"
    AND Sunat_Fact_Electr_Taxs.Disabled = NO
    AND TODAY >= Sunat_Fact_Electr_Taxs.Start_Date 
    AND TODAY <= Sunat_Fact_Electr_Taxs.End_Date 
    AND Sunat_Fact_Electr_Taxs.Tax > 0
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE Sunat_Fact_Electr_Taxs THEN RETURN.

FOR EACH VtaDctoProm EXCLUSIVE-LOCK WHERE VtaDctoProm.CodCia = 001:
    ASSIGN
        VtaDctoProm.TasaImpuesto = Sunat_Fact_Electr_Taxs.Tax
        VtaDctoProm.ImporteUnitarioSinImpuesto = ROUND(VtaDctoProm.Precio / ( 1 + Sunat_Fact_Electr_Taxs.Tax / 100), 4)
        VtaDctoProm.ImporteUnitarioImpuesto = VtaDctoProm.Precio - VtaDctoProm.ImporteUnitarioSinImpuesto
        .
END.

FOR EACH VtaDctoPromMin EXCLUSIVE-LOCK WHERE VtaDctoPromMin.CodCia = 001:
    ASSIGN
        VtaDctoPromMin.TasaImpuesto = Sunat_Fact_Electr_Taxs.Tax
        VtaDctoPromMin.ImporteUnitarioSinImpuesto = ROUND(VtaDctoPromMin.Precio / ( 1 + Sunat_Fact_Electr_Taxs.Tax / 100), 4)
        VtaDctoPromMin.ImporteUnitarioImpuesto = VtaDctoPromMin.Precio - VtaDctoPromMin.ImporteUnitarioSinImpuesto
        .
END.

