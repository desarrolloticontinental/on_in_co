DEF VAR F-PRECIO AS DEC NO-UNDO.      
def VAR x-inicio AS DATETIME.
DEF VAR x-fin AS DATETIME.

RUN LIMPIA.
DISPLAY 'LIMPIA OK' WITH 1 DOWN STREAM-IO. PAUSE 0.

x-inicio = NOW.
RUN LIMA.
DISPLAY 'LIMA OK' WITH 1 DOWN STREAM-IO. PAUSE 0.

RUN UTILEX.
DISPLAY 'UTILEX OK' WITH 1 DOWN STREAM-IO. PAUSE 0.

RUN DIVISION.
DISPLAY 'DIVISION OK' WITH 1 DOWN STREAM-IO. PAUSE 0.
x-fin = NOW.

MESSAGE x-inicio SKIP x-fin VIEW-AS ALERT-BOX WARNING.

RETURN.


PROCEDURE LIMPIA:

    FOR EACH vtadctoprom EXCLUSIVE-LOCK:
        DELETE vtadctoprom.
    END.

END PROCEDURE.

PROCEDURE LIMA:

    FOR EACH VtaTabla NO-LOCK WHERE VtaTabla.CodCia = 001
            AND VtaTabla.Tabla = "DTOPROLIMA" 
            AND (TODAY >= Vtatabla.rango_fecha[1] AND TODAY <= Vtatabla.rango_fecha[2]),
        FIRST GN-DIVI NO-LOCK WHERE GN-DIVI.CodCia = VtaTabla.CodCia
            AND GN-DIVI.CodDiv = VtaTabla.Llave_c2 
            AND gn-divi.ventamayorista = 1,     /* Mayorista General */
        FIRST Almmmatg NO-LOCK WHERE Almmmatg.codcia = 001
            AND Almmmatg.codmat = VtaTabla.Llave_c1:
        /* TODO EN SOLES */
        F-PRECIO = Almmmatg.Prevta[1].
        IF Almmmatg.MonVta = 2 THEN F-PRECIO = F-PRECIO * Almmmatg.TpoCmb.
        CREATE VtaDctoProm.
        ASSIGN
            VtaDctoProm.CodCia = Almmmatg.codcia
            VtaDctoProm.CodDiv = Vtatabla.llave_c2
            VtaDctoProm.CodMat = Almmmatg.codmat
            VtaDctoProm.Descuento = Vtatabla.valor[1]
    /*         VtaDctoProm.DescuentoMR  */
    /*         VtaDctoProm.DescuentoVIP */
            VtaDctoProm.FchFin = Vtatabla.rango_fecha[2]
            VtaDctoProm.FchIni = Vtatabla.rango_fecha[1].
        ASSIGN
            VtaDctoProm.Precio = ROUND(F-PRECIO * (1 - (VtaDctoProm.Descuento / 100)), 4)
    /*         VtaDctoProm.PrecioMR  */
    /*         VtaDctoProm.PrecioVIP */
            .
    END.


END PROCEDURE.

PROCEDURE UTILEX:

    FOR EACH VtaListaMinGn NO-LOCK WHERE VtaListaMinGn.codcia = 001,
        FIRST Almmmatg OF VtaListaMinGn NO-LOCK,
        EACH VtaTabla NO-LOCK WHERE VtaTabla.CodCia = VtaListaMinGn.CodCia
            AND VtaTabla.Llave_c1 = VtaListaMinGn.codmat
            AND VtaTabla.Tabla = "DTOPROUTILEX" 
            AND (TODAY >= Vtatabla.rango_fecha[1] AND TODAY <= Vtatabla.rango_fecha[2]),
        FIRST GN-DIVI NO-LOCK WHERE GN-DIVI.CodCia = VtaTabla.CodCia
            AND GN-DIVI.CodDiv = VtaTabla.Llave_c2 
            AND LOOKUP(gn-divi.canalventa, 'MIN') > 0:  /* Utilex */
        /* TODO EN SOLES */
        F-PRECIO = VtaListaMinGn.PreOfi.
        IF Almmmatg.MonVta = 2 THEN F-PRECIO = F-PRECIO * Almmmatg.TpoCmb.
        CREATE VtaDctoProm.
        ASSIGN
            VtaDctoProm.CodCia = VtaListaMinGn.codcia
            VtaDctoProm.CodDiv = Vtatabla.llave_c2
            VtaDctoProm.CodMat = VtaListaMinGn.codmat
            VtaDctoProm.Descuento = Vtatabla.valor[1]
            VtaDctoProm.FchFin = Vtatabla.rango_fecha[2]
            VtaDctoProm.FchIni = Vtatabla.rango_fecha[1].
        ASSIGN
            VtaDctoProm.Precio = ROUND(F-PRECIO * (1 - (VtaDctoProm.Descuento / 100)), 4)
            .
    END.

END PROCEDURE.

PROCEDURE DIVISION:

    FOR EACH VtaListaMay NO-LOCK WHERE VtaListaMay.codcia = 001
            AND (TODAY >= VtaListaMay.PromFchD AND TODAY <= VtaListaMay.PromFchH),
        FIRST Almmmatg OF VtaListaMay NO-LOCK,
        FIRST GN-DIVI NO-LOCK WHERE GN-DIVI.CodCia = VtaTabla.CodCia
            AND GN-DIVI.CodDiv = VtaTabla.Llave_c2 
            AND gn-divi.ventamayorista = 2:     /* Lista Específica */
        /* TODO EN SOLES */
        F-PRECIO = VtaListaMay.PreOfi.
        IF Almmmatg.MonVta = 2 THEN F-PRECIO = F-PRECIO * Almmmatg.TpoCmb.
        CREATE VtaDctoProm.
        ASSIGN
            VtaDctoProm.CodCia = Almmmatg.codcia
            VtaDctoProm.CodDiv = VtaListaMay.CodDiv
            VtaDctoProm.CodMat = Almmmatg.codmat
            VtaDctoProm.Descuento = Vtalistamay.PromDto
            VtaDctoProm.DescuentoMR = Vtalistamay.libre_d02
            VtaDctoProm.DescuentoVIP = Vtalistamay.libre_d01
            VtaDctoProm.FchFin = VtaListaMay.PromFchH 
            VtaDctoProm.FchIni = VtaListaMay.PromFchD.
        ASSIGN
            VtaDctoProm.Precio = ROUND(F-PRECIO * (1 - (VtaDctoProm.Descuento / 100)), 4)
            VtaDctoProm.PrecioMR = ROUND(F-PRECIO * (1 - (VtaDctoProm.DescuentoMR / 100)), 4)
            VtaDctoProm.PrecioVIP = ROUND(F-PRECIO * (1 - (VtaDctoProm.DescuentoVIP / 100)), 4)
            .
    END.

END PROCEDURE.
