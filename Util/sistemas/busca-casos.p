DEF BUFFER cotizacion FOR faccpedi.
DEF VAR s-codcia AS INTE INIT 001.


    FOR EACH COTIZACION NO-LOCK WHERE COTIZACION.codcia = s-codcia
        AND COTIZACION.coddoc = "COT"
        AND COTIZACION.lista_de_precios = '20015'
        AND COTIZACION.FchPed >= DATE(10,01,2022)
        AND COTIZACION.FchPed <= TODAY
        AND COTIZACION.FlgEst <> "A":
        /* Barremos las configuraciones válidas */
        FOR EACH PriCDtoVolAcu NO-LOCK WHERE PriCDtoVolAcu.CodCia = s-codcia 
            AND PriCDtoVolAcu.Inactivo = NO
            AND PriCDtoVolAcu.Desde <= COTIZACION.FchPed
            AND PriCDtoVolAcu.Hasta >= COTIZACION.FchPed 
            AND CAN-FIND(FIRST PriDDtoVolAcu OF PriCDtoVolAcu WHERE 
                         PriDDtoVolAcu.Tabla = 'DIVISION' AND
                         PriDDtoVolAcu.Llave_c1 = COTIZACION.Lista_de_Precios NO-LOCK):
            /* Si encontramos al menos una COT lo guardamos */
            FOR EACH PriDDtoVolAcu OF PriCDtoVolAcu NO-LOCK WHERE PriDDtoVolAcu.Tabla = "SKU":
                FIND FIRST facdpedi OF cotizacion WHERE facdpedi.codmat = PriDDtoVolAcu.llave_c1 NO-LOCK NO-ERROR.
                IF AVAILABLE facdpedi THEN DISPLAY cotizacion.coddoc cotizacion.nroped.

            END.
        END.
    END.
