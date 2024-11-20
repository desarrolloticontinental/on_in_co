/* Limpieza de tablas antes del evento */
DISABLE TRIGGERS FOR LOAD OF vtalistamay.
FOR EACH vtalistamay EXCLUSIVE WHERE codcia = 1 AND 
    LOOKUP(coddiv, '20015,20018,20060,20065,20067') > 0:
    DELETE vtalistamay.
END.

DISABLE TRIGGERS FOR LOAD OF vtadctoprom.
FOR EACH vtadctoprom EXCLUSIVE WHERE codcia = 1 AND 
    LOOKUP(coddiv, '20015,20018,20060,20065,20067') > 0:
    DELETE vtadctoprom.
END.
