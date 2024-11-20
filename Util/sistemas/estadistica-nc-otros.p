
FOR EACH gn-divi NO-LOCK WHERE codcia = 001,
    EACH Ventas_Detalle EXCLUSIVE-LOCK WHERE Ventas_Detalle.CodDiv = gn-divi.coddiv AND
    Ventas_Detalle.CodDoc = "N/C" AND
    Ventas_Detalle.DateKey >= DATE(10,01,2016),
    FIRST CcbCDocu NO-LOCK WHERE CcbCDocu.CodCia = 001 AND
    CcbCDocu.CodDoc = Ventas_Detalle.CodDoc AND
    CcbCDocu.NroDoc = Ventas_Detalle.NroDoc:
    IF Ccbcdocu.codcia = 1 and Ccbcdocu.coddoc = 'N/C' and Ccbcdocu.cndcre = "N" THEN DO:
        FIND CcbTabla WHERE CcbTabla.CodCia = 001 AND
            CcbTabla.Tabla = "N/C" AND
            CcbTabla.Codigo = Ccbcdocu.codcta
            NO-LOCK NO-ERROR.
        IF AVAILABLE CcbTabla AND CcbTabla.Libre_c02 = "NO" THEN DO:
            DISPLAY ccbcdocu.coddoc ccbcdocu.nrodoc ccbcdocu.fchdoc WITH STREAM-IO. 
            PAUSE 0.
            ASSIGN
                Ventas_Detalle.PromExtSIGV = 0
                Ventas_Detalle.PromExtCIGV = 0
                Ventas_Detalle.PromNacSIGV = 0
                Ventas_Detalle.PromNacCIGV = 0.
        END.
    END.
END.
    
