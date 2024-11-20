/* CARGA EMPAQUE MASTER, EMPAQUE INNER, MINIMO VENTA EXPO */
DEF VAR s-codcia AS INT.
DEF VAR s-coddiv AS CHAR.

s-codcia = 001.
s-coddiv = '20015'.
DISABLE TRIGGERS FOR LOAD OF almmmatg.

FOR EACH vtacatventa NO-LOCK WHERE vtacatventa.codcia = s-codcia
    AND vtacatventa.coddiv = s-coddiv:
    FOR EACH almcatvtad OF vtacatventa NO-LOCK, 
        FIRST almmmatg OF almcatvtad EXCLUSIVE-LOCK WHERE 
        (almcatvtad.libre_d02 <> almmmatg.stkrep OR
         almcatvtad.libre_d03 <> almmmatg.canemp):
        ASSIGN
            almmmatg.stkrep = almcatvtad.libre_d02
            almmmatg.stkmax = almcatvtad.libre_d02
            almmmatg.canemp = almcatvtad.libre_d03.
    END.
END.

