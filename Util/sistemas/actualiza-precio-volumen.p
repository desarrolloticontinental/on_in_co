DISABLE TRIGGERS FOR LOAD OF Almmmatg.
DISABLE TRIGGERS FOR LOAD OF VtaListaMay.
DISABLE TRIGGERS FOR LOAD OF VtaListaMinGn.
DISABLE TRIGGERS FOR LOAD OF Almmmatp.

DEF VAR x-Item AS INTE NO-UNDO.
DEF VAR f-PrecioListaVta AS DECI NO-UNDO.

/* Precios por volumen */
FOR EACH almmmatg EXCLUSIVE-LOCK WHERE codcia = 1 AND Almmmatg.PreVta[1] > 0,
    FIRST Almtfami OF Almmmatg NO-LOCK WHERE Almtfami.SwComercial = YES:
    ASSIGN
        f-PrecioListaVta = Almmmatg.PreVta[1].
    DO x-Item = 1 TO 10:
        IF Almmmatg.DtoVolD[x-Item] <> 0 THEN
            ASSIGN Almmmatg.DtoVolP[x-Item] = ROUND(f-PrecioListaVta * (1 - (Almmmatg.DtoVolD[x-Item]  / 100)), 4).
    END.
END.

DEF VAR f-PrecioLista AS DECI NO-UNDO.
FOR EACH VtaListaMay EXCLUSIVE-LOCK WHERE codcia = 1:
    f-PrecioLista = VtaListaMay.PreOfi.
    /* Actualizamos Precio por Volumen */
    DO x-Item = 1 TO 10:
        IF VtaListaMay.DtoVolD[x-Item] <> 0 THEN
            ASSIGN VtaListaMay.DtoVolP[x-Item] = ROUND(f-PrecioLista * (1 - (VtaListaMay.DtoVolD[x-Item]  / 100)), 4).
    END.

END.

FOR EACH VtaListaMinGn EXCLUSIVE-LOCK WHERE codcia = 1:
    f-PrecioLista = VtaListaMinGn.PreOfi.
    /* Actualizamos Precio por Volumen */
    DO x-Item = 1 TO 10:
        IF VtaListaMinGn.DtoVolD[x-Item] <> 0 THEN
            ASSIGN VtaListaMinGn.DtoVolP[x-Item] = ROUND(f-PrecioLista * (1 - (VtaListaMinGn.DtoVolD[x-Item]  / 100)), 4).
    END.
END.

FOR EACH Almmmatp EXCLUSIVE-LOCK WHERE codcia = 1:
    f-PrecioLista = Almmmatp.PreOfi.
    /* Actualizamos Precio por Volumen */
    DO x-Item = 1 TO 10:
        IF Almmmatp.DtoVolD[x-Item] <> 0 THEN
            ASSIGN Almmmatp.DtoVolP[x-Item] = ROUND(f-PrecioLista * (1 - (Almmmatp.DtoVolD[x-Item]  / 100)), 4).
    END.
END.
