DISABLE TRIGGERS FOR LOAD OF vtacdocu.
DEF VAR x-importe AS DEC.
DEF VAR x-items AS INT.
DEF VAR x-peso AS DEC.
DEF VAR x-volumen AS DEC.

FOR EACH vtacdocu EXCLUSIVE-LOCK WHERE codcia = 1
    AND codped = 'hpk'
    AND fchped >= DATE(03,01,2022):
    ASSIGN
        x-importe = 0      /* Importe */
        x-Items = 0          /* Items */
        x-Peso = 0           /* Peso */
        x-Volumen = 0.       /* Volumen */
    FOR EACH Vtaddocu OF Vtacdocu NO-LOCK,FIRST Almmmatg OF Vtaddocu NO-LOCK:
        ASSIGN
            x-importe = x-importe + Vtaddocu.ImpLin
            x-items = x-Items + 1
            x-Peso = x-Peso + (Vtaddocu.CanPed * Vtaddocu.Factor * Almmmatg.PesMat)
            x-Volumen = x-Volumen + (Vtaddocu.CanPed * Vtaddocu.Factor * Almmmatg.Libre_d02 / 10000000).
    END.
    IF x-items <> vtacdocu.items THEN DO:
        DISPLAY vtacdocu.coddiv vtacdocu.fchped vtacdocu.nroped vtacdocu.libre_d01 x-importe
            vtacdocu.items x-items vtacdocu.peso x-peso
            vtacdocu.volumen x-volumen WITH STREAM-IO NO-BOX WIDTH 320.
        PAUSE 0.
        vtacdocu.libre_d01 = x-importe.
        vtacdocu.items = x-items.
        vtacdocu.peso = x-peso.
        vtacdocu.volumen = x-volumen.
    END.
END.
