/* Limpieza de tablas antes del evento */
DISABLE TRIGGERS FOR LOAD OF vtacatventa.
FOR EACH vtacatventa EXCLUSIVE WHERE codcia = 1 AND 
    LOOKUP(coddiv, '20015,20018,20060,20065,20067') > 0:
    DELETE vtacatventa.
END.
DISABLE TRIGGERS FOR LOAD OF almcatvtac.
DISABLE TRIGGERS FOR LOAD OF almcatvtad.
FOR EACH almcatvtac EXCLUSIVE WHERE codcia = 1 AND 
    LOOKUP(coddiv, '20015,20018,20060,20065,20067') > 0:
    FOR EACH almcatvtad OF almcatvtac EXCLUSIVE:
        DELETE almcatvtad.
    END.
    DELETE almcatvtac.
END.
