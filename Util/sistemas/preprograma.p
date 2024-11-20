
/* Si es una división PADRE se toma como viene */
/* Si es una división HIJO se toma a partir de una venta de la división PADRE */
DEF VAR s-coddiv AS CHAR.
DEF VAR s-codcia AS INTE.
DEF VAR s-codcli AS CHAR.
DEF VAR pMensaje AS CHAR.

s-coddiv = '00018'.
s-codcia = 001.
s-codcli = '10035793521'.

DEF TEMP-TABLE ITEM LIKE facdpedi.
DEF TEMP-TABLE T-CPEDI LIKE faccpedi.
DEF TEMP-TABLE T-CPEDI-2 LIKE faccpedi.
DEF TEMP-TABLE T-DPEDI LIKE facdpedi.
DEF TEMP-TABLE T-CDOCU LIKE ccbcdocu.
DEF TEMP-TABLE T-CDOCU-2 LIKE ccbcdocu.
DEF TEMP-TABLE T-VtamDctoVolTime LIKE VtamDctoVolTime.

DEF BUFFER COTIZACION FOR Faccpedi.
DEF BUFFER PEDIDO FOR Faccpedi.

EMPTY TEMP-TABLE T-CPEDI.
EMPTY TEMP-TABLE T-CPEDI-2.
EMPTY TEMP-TABLE T-DPEDI.
EMPTY TEMP-TABLE T-CDOCU.
EMPTY TEMP-TABLE T-CDOCU-2.
EMPTY TEMP-TABLE T-VtamDctoVolTime.

/* Cargamos COTIZACIONES y COMPROBANTES */
FIND faccpedi WHERE faccpedi.codcia = s-codcia AND faccpedi.coddoc = 'cot' AND faccpedi.nroped = '218000225' NO-LOCK.
/* Sin PROMOCIONES */
FOR EACH facdpedi OF faccpedi NO-LOCK WHERE Facdpedi.Libre_c05 <> 'OF':
    CREATE ITEM.
    BUFFER-COPY facdpedi TO ITEM.
END.

/* Determinamos cuales son las divisiones a barrer */
DEF VAR x-Divisiones AS CHAR NO-UNDO.
DEF VAR x-FchIni AS DATE NO-UNDO.
DEF VAR x-FchFin AS DATE NO-UNDO.

/* ******************************************************************************************************************* */
/* Buscamos si tiene algún descuento activo */
/* ******************************************************************************************************************* */
FIND FIRST VtamDctoVolTime WHERE VtamDctoVolTime.CodDiv = s-coddiv AND
    VtamDctoVolTime.FlgEst = "Activo" AND 
    TODAY >= VtamDctoVolTime.FchIni AND
    TODAY <= VtamDctoVolTime.FchFin
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE VtamDctoVolTime THEN DO:
    FIND FIRST VtasDctoVolTime WHERE VtasDctoVolTime.CodDiv = s-coddiv AND
        CAN-FIND(FIRST VtamDctoVolTime OF VtasDctoVolTime WHERE VtamDctoVolTime.FlgEst = "Activo" AND 
                 TODAY >= VtamDctoVolTime.FchIni AND
                 TODAY <= VtamDctoVolTime.FchFin NO-LOCK)
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE VtasDctoVolTime THEN RETURN.
END.
x-Divisiones = s-CodDiv.        /* Valor por defecto */

/* ******************************************************************************************************************* */
/* MASTER-TRANSACTION         */
/* ******************************************************************************************************************* */

RUN Carga-Temporales.
RUN Carga-Descuentos.
RUN Graba-Descuentos (OUTPUT pMensaje).
IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
    MESSAGE pMensaje VIEW-AS ALERT-BOX ERROR.
END.
/* ******************************************************************************************************************* */
/* ******************************************************************************************************************* */

RETURN.

/* *********************** */
PROCEDURE Carga-Temporales:
/* *********************** */
    /* ******************************************************************************************************************* */
    /* Cargamos los padres e hijos */
    /* ******************************************************************************************************************* */
    RLOOP:
    FOR EACH VtamDctoVolTime NO-LOCK WHERE VtamDctoVolTime.CodDiv = s-coddiv AND
        VtamDctoVolTime.FlgEst = "Activo" AND 
        TODAY >= VtamDctoVolTime.FchIni AND
        TODAY <= VtamDctoVolTime.FchFin:
        /* *************************************************** */
        /* Buscamos que al menos un artículo se esté cotizando */
        /* *************************************************** */
        FIND FIRST ITEM WHERE CAN-FIND(FIRST VtaaDctoVolTime OF VtamDctoVolTime WHERE VtaaDctoVolTime.CodMat = ITEM.codmat NO-LOCK)
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE ITEM THEN NEXT RLOOP.
        /* *************************************************** */
        FOR EACH VtasDctoVolTime OF VtamDctoVolTime NO-LOCK:
            IF LOOKUP(VtasDctoVolTime.CodDiv, x-Divisiones) = 0 THEN x-Divisiones = x-Divisiones + ',' + VtasDctoVolTime.CodDiv.
        END.
        IF NOT CAN-FIND(FIRST T-VtamDctoVolTime OF VtamDctoVolTime NO-LOCK) THEN DO:
            CREATE T-VtamDctoVolTime.
            BUFFER-COPY VtamDctoVolTime TO T-VtamDctoVolTime.
        END.
    END.

    DEF BUFFER B-VtasDctoVolTime FOR VtasDctoVolTime.
    /* Cargamos padres e hijos */
    RLOOP:
    FOR EACH VtasDctoVolTime NO-LOCK WHERE VtasDctoVolTime.CodDiv = s-coddiv:
        FOR EACH VtamDctoVolTime OF VtasDctoVolTime NO-LOCK WHERE VtamDctoVolTime.FlgEst = "Activo" AND 
            TODAY >= VtamDctoVolTime.FchIni AND
            TODAY <= VtamDctoVolTime.FchFin:
            /* *************************************************** */
            /* Buscamos que al menos un artículo se esté cotizando */
            /* *************************************************** */
            FIND FIRST ITEM WHERE CAN-FIND(FIRST VtaaDctoVolTime OF VtamDctoVolTime WHERE VtaaDctoVolTime.CodMat = ITEM.codmat NO-LOCK)
                NO-LOCK NO-ERROR.
            IF NOT AVAILABLE ITEM THEN NEXT.
            /* *************************************************** */
            IF LOOKUP(VtamDctoVolTime.CodDiv, x-Divisiones) = 0 THEN x-Divisiones = x-Divisiones + ',' + VtamDctoVolTime.CodDiv.
            FOR EACH B-VtasDctoVolTime OF VtamDctoVolTime NO-LOCK:
                IF LOOKUP(B-VtasDctoVolTime.CodDiv, x-Divisiones) = 0 THEN x-Divisiones = x-Divisiones + ',' + B-VtasDctoVolTime.CodDiv.
            END.
            IF NOT CAN-FIND(FIRST T-VtamDctoVolTime OF VtamDctoVolTime NO-LOCK) THEN DO:
                CREATE T-VtamDctoVolTime.
                BUFFER-COPY VtamDctoVolTime TO T-VtamDctoVolTime.
            END.
        END.
    END.
    /* ******************************************************************************************************************* */
    /* Cargamos temporales */
    /* ******************************************************************************************************************* */
    DEF VAR k AS INTE NO-UNDO.
    DEF VAR x-CodDiv AS CHAR NO-UNDO.

    /* Cargamos comprobantes (FAC BOL) */
    DO k = 1 TO NUM-ENTRIES(x-Divisiones):
        x-CodDiv = ENTRY(k, x-Divisiones).
        FOR EACH T-VtamDctoVolTime NO-LOCK:
            RUN Carga-Comprobante-Cotizaciones (x-CodDiv).
        END.
    END.
    /* ******************************************************************************************************************* */
    /* Cargamos solo N/C por devolución de mercadería */
    /* ******************************************************************************************************************* */
    FOR EACH T-CDOCU NO-LOCK:
        FOR EACH ccbcdocu NO-LOCK WHERE ccbcdocu.codcia = s-codcia AND
            ccbcdocu.coddoc = "N/C" AND
            ccbcdocu.codref = T-CDOCU.coddoc AND
            ccbcdocu.nroref = T-CDOCU.nrodoc AND
            ccbcdocu.cndcre = "D" AND
            ccbcdocu.flgest <> 'A':
            CREATE T-CDOCU-2.
            BUFFER-COPY ccbcdocu TO T-CDOCU-2.
        END.
    END.
    FOR EACH T-CDOCU-2 NO-LOCK:
        CREATE T-CDOCU.
        BUFFER-COPY T-CDOCU-2 TO T-CDOCU.
    END.
    /* ******************************************************************************************************************* */

END PROCEDURE.

/* ************************************** */
PROCEDURE Carga-Comprobante-Cotizaciones:
/* ************************************** */

DEF INPUT PARAMETER pCodDiv AS CHAR.
/* OJO: debe tomar en cuenta la COT que se está creando o actualizando,
        mejor correr el proceso una vez grabada la cotización */
FOR EACH COTIZACION NO-LOCK WHERE COTIZACION.codcia = s-codcia AND
    COTIZACION.codcli = s-codcli AND
    COTIZACION.coddoc = "COT" AND
    COTIZACION.fchped >= T-VtamDctoVolTime.FchIni AND
    COTIZACION.fchped <= T-VtamDctoVolTime.FchFin AND
    COTIZACION.coddiv = pCodDiv AND
    COTIZACION.flgest <> "A",
    EACH PEDIDO NO-LOCK WHERE PEDIDO.codcia = COTIZACION.codcia AND
    PEDIDO.coddoc = "PED" AND
    PEDIDO.codref = COTIZACION.coddoc AND
    PEDIDO.nroref = COTIZACION.nroped AND
    PEDIDO.flgest <> "A":
    FOR EACH ccbcdocu NO-LOCK WHERE ccbcdocu.codcia = s-codcia AND
        ccbcdocu.codped = PEDIDO.coddoc AND
        ccbcdocu.nroped = PEDIDO.nroped AND
        (ccbcdocu.coddoc = "FAC" OR ccbcdocu.coddoc = "BOL") AND
        ccbcdocu.flgest <> "A":
        IF NOT CAN-FIND(FIRST T-CDOCU OF ccbcdocu NO-LOCK) THEN DO:
            CREATE T-CDOCU.
            BUFFER-COPY ccbcdocu TO T-CDOCU.
        END.
        IF NOT CAN-FIND(FIRST T-CPEDI OF COTIZACION NO-LOCK) THEN DO:
            CREATE T-CPEDI.
            BUFFER-COPY COTIZACION TO T-CPEDI.
        END.
    END.
END.

END PROCEDURE.


/* *************************** */
PROCEDURE Carga-Descuentos:
/* *************************** */

/* Acumulamos cantidades por cada comprobante */
DEF VAR x-Signo AS INTE INIT 1 NO-UNDO.

FOR EACH T-CDOCU NO-LOCK,
    EACH Ccbddocu OF T-CDOCU NO-LOCK,
    FIRST ITEM NO-LOCK WHERE ITEM.codmat = Ccbddocu.codmat:
    x-Signo = (IF T-CDOCU.CodDoc = "N/C" THEN -1 ELSE 1).
    FIND FIRST T-DPEDI WHERE T-DPEDI.codmat = Ccbddocu.codmat EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE T-DPEDI THEN DO:
        CREATE T-DPEDI.
        ASSIGN
            T-DPEDI.codmat = Ccbddocu.codmat.
    END.
    ASSIGN
        T-DPEDI.canped = T-DPEDI.canped + (Ccbddocu.candes * Ccbddocu.factor * x-Signo).
END.

/* Limpiamos los productos del T-DPEDI que no estén en el ITEM */
FOR EACH T-DPEDI EXCLUSIVE-LOCK:
    IF NOT CAN-FIND(FIRST ITEM WHERE ITEM.codmat = T-DPEDI.codmat NO-LOCK) THEN DELETE T-DPEDI.
END.

/* Calculamos % de descuento de acuerdo al importe acumulado */
DEF VAR x-Acumulado LIKE T-DPEDI.canped NO-UNDO.
DEF VAR x-PorDto AS DECI NO-UNDO.

FOR EACH T-VtamDctoVolTime NO-LOCK, FIRST VtamDctoVolTime OF T-VtamDctoVolTime NO-LOCK:
    /* Barremos cada Pack */
    FOR EACH VtapDctoVolTime OF VtamDctoVolTime NO-LOCK:
        x-Acumulado = 0.
        x-PorDto = 0.
        /* Por cada Pack acumulamos cantidades */
        FOR EACH ITEM NO-LOCK, FIRST T-DPEDI NO-LOCK WHERE T-DPEDI.codmat = ITEM.codmat, 
            FIRST VtaaDctoVolTime OF VtapDctoVolTime WHERE VtaaDctoVolTime.CodMat = ITEM.codmat:
            x-Acumulado = x-Acumulado + T-DPEDI.canped.
        END.
        IF x-Acumulado = 0 THEN NEXT.   /* Pasamos al siguiente Pack */
        /* Buscamos la escala */
        FOR EACH VtaeDctoVolTime OF VtapDctoVolTime NO-LOCK BY VtaeDctoVolTime.Percent:
            IF x-Acumulado >= VtaeDctoVolTime.Qty THEN x-PorDto = VtaeDctoVolTime.Percent.
        END.
        /* Actualizamos descuento */
        FOR EACH ITEM NO-LOCK, FIRST T-DPEDI EXCLUSIVE-LOCK WHERE T-DPEDI.codmat = ITEM.codmat, 
            FIRST VtaaDctoVolTime OF VtapDctoVolTime WHERE VtaaDctoVolTime.CodMat = ITEM.codmat:
            T-DPEDI.PorDto = x-PorDto.
            T-DPEDI.Libre_c01 = "*".        /* Marcamos */
        END.
    END.
END.

END PROCEDURE.


/* ************************ */
PROCEDURE Graba-Descuentos:
/* ************************ */

DEF OUTPUT PARAMETER pMensaje AS CHAR NO-UNDO.

FOR EACH T-DPEDI NO-LOCK WHERE T-DPEDI.Libre_c01 = "*":
    FIND Facdpedi OF Faccpedi WHERE Facdpedi.codmat = T-DPEDI.codmat EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
    IF ERROR-STATUS:ERROR = YES THEN DO:
        {lib/mensaje-de-error.i &MensajeError="pMensaje"}
        RETURN 'ADM-ERROR'.
    END.
    ASSIGN
        FacDPedi.Por_Dsctos[1] = 0
        FacDPedi.Por_Dsctos[2] = 0
        FacDPedi.Por_Dsctos[3] = T-DPEDI.PorDto
        FacDPedi.Libre_c04 = "DVXTIME".
END.

RETURN 'OK'.


END PROCEDURE.
