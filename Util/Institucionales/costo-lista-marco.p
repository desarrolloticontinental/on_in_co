DISABLE TRIGGERS FOR LOAD OF almmmatg.

FOR EACH almmmatp NO-LOCK WHERE codcia = 001,
    FIRST almmmatg OF almmmatp:
    DISPLAY almmmatp.codmat.
    PAUSE 0.
    /* LO expresamos a la moneda de venta del campo MONVTA */
    ASSIGN
        Almmmatg.DEC__02     = Almmmatp.MonVta
        Almmmatg.CtoLisMarco = Almmmatp.CtoTot / 1.18
        Almmmatg.CtoTotMarco = Almmmatp.CtoTot.
END.
      
      
      
