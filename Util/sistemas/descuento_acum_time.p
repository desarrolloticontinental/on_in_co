DEF NEW SHARED VAR s-codcia AS INT INIT 001.   
DEF NEW SHARED VAR cl-codcia AS INT INIT 0.   
DEF NEW SHARED VAR s-user-id AS CHAR INIT 'ADMIN'.
DEF NEW SHARED VAR s-coddiv AS CHAR INIT '00002'.   

DEFINE BUFFER COTIZACION FOR FacCPedi.
DEFINE BUFFER PEDIDO FOR FacCPedi.

FIND faccpedi WHERE codcia = 1 AND coddoc = 'cot' AND nroped = '218000375' NO-LOCK.

DEF VAR pcoddiv AS CHAR.
DEF VAR pCodCli AS CHAR.
DEF VAR X-TIPDTO AS CHAR INIT "DXVACUMTIME" NO-UNDO.
DEF VAR x-Forma AS CHAR INIT 'UNICO' NO-UNDO.   /* OJO: Valor por defecto */
DEF VAR pMensaje AS CHAR NO-UNDO.

pcoddiv = faccpedi.coddiv.
pCodCli = faccpedi.codcli.

DEF TEMP-TABLE T-VtamDctoVolTime LIKE VtamDctoVolTime.
DEF TEMP-TABLE T-VtapDctoVolTime LIKE VtapDctoVolTime.
DEF TEMP-TABLE ITEM LIKE Facdpedi.

EMPTY TEMP-TABLE T-VtamDctoVolTime.
EMPTY TEMP-TABLE T-VtapDctoVolTime.

FOR EACH facdpedi OF faccpedi NO-LOCK:
    CREATE ITEM.
    BUFFER-COPY facdpedi TO ITEM.
END.

/* 1ro por el PADRE */
RLOOP:
FOR EACH VtamDctoVolTime NO-LOCK WHERE VtamDctoVolTime.CodDiv = pCodDiv AND
    VtamDctoVolTime.FlgEst = "Activo" AND 
    TODAY >= VtamDctoVolTime.FchIni AND
    TODAY <= VtamDctoVolTime.FchFin:
    FOR EACH VtapDctoVolTime OF VtamDctoVolTime NO-LOCK, 
        EACH VtaaDctoVolTime OF VtapDctoVolTime NO-LOCK:
        FIND FIRST ITEM WHERE ITEM.codmat = VtaaDctoVolTime.codmat NO-LOCK NO-ERROR.
        IF NOT AVAILABLE ITEM THEN NEXT.
        IF NOT CAN-FIND(FIRST T-VtamDctoVolTime OF VtamDctoVolTime NO-LOCK) THEN DO:
            CREATE T-VtamDctoVolTime.
            BUFFER-COPY VtamDctoVolTime TO T-VtamDctoVolTime.
        END.
        IF NOT CAN-FIND(FIRST T-VtapDctoVolTime OF VtapDctoVolTime NO-LOCK) THEN DO:
            CREATE T-VtapDctoVolTime.
            BUFFER-COPY VtapDctoVolTime TO T-VtapDctoVolTime.
        END.
    END.
END.
/* 2do por los HIJOS */
RLOOP:
FOR EACH VtasDctoVolTime NO-LOCK WHERE VtasDctoVolTime.CodDiv = pCodDiv,
    FIRST VtamDctoVolTime NO-LOCK WHERE VtamDctoVolTime.IdMaster = VtasDctoVolTime.IdMaster AND
        VtamDctoVolTime.FlgEst = "Activo" AND 
        TODAY >= VtamDctoVolTime.FchIni AND
        TODAY <= VtamDctoVolTime.FchFin:
    FOR EACH VtapDctoVolTime OF VtamDctoVolTime NO-LOCK, EACH VtaaDctoVolTime OF VtapDctoVolTime NO-LOCK:
        FIND FIRST ITEM WHERE ITEM.codmat = VtaaDctoVolTime.codmat NO-LOCK NO-ERROR.
        IF NOT AVAILABLE ITEM THEN NEXT.
        IF NOT CAN-FIND(FIRST T-VtamDctoVolTime OF VtamDctoVolTime NO-LOCK) THEN DO:
            CREATE T-VtamDctoVolTime.
            BUFFER-COPY VtamDctoVolTime TO T-VtamDctoVolTime.
        END.
        IF NOT CAN-FIND(FIRST T-VtapDctoVolTime OF VtapDctoVolTime NO-LOCK) THEN DO:
            CREATE T-VtapDctoVolTime.
            BUFFER-COPY VtapDctoVolTime TO T-VtapDctoVolTime.
        END.
    END.
END.
/* ******************************************************************************************************************* */
DEFINE TEMP-TABLE T-CDOCU NO-UNDO LIKE CcbCDocu.
DEFINE TEMP-TABLE T-CDOCU-2 NO-UNDO LIKE CcbCDocu.
DEFINE TEMP-TABLE T-CPEDI NO-UNDO LIKE FacCPedi.
DEFINE TEMP-TABLE T-DPEDI NO-UNDO LIKE FacDPedi.

DEF VAR x-Divisiones AS CHAR NO-UNDO.
FOR EACH T-VtamDctoVolTime NO-LOCK, EACH T-VtapDctoVolTime OF T-VtamDctoVolTime NO-LOCK:
    /* Por cada PACK tomamos las COTIZACIONES válidas */
    EMPTY TEMP-TABLE T-CDOCU.
    EMPTY TEMP-TABLE T-CDOCU-2.
    EMPTY TEMP-TABLE T-CPEDI.
    EMPTY TEMP-TABLE T-DPEDI.
    /* Divisiones válidas PADRE + HIJOS */
    x-Divisiones = pCodDiv.     /* Valor por defecto */
    IF LOOKUP(T-VtamDctoVolTime.CodDiv, x-Divisiones) = 0 THEN x-Divisiones = x-Divisiones + ',' + T-VtamDctoVolTime.CodDiv.
    FOR EACH VtasDctoVolTime OF T-VtamDctoVolTime NO-LOCK:
        IF LOOKUP(VtasDctoVolTime.CodDiv, x-Divisiones) = 0 THEN x-Divisiones = x-Divisiones + ',' + VtasDctoVolTime.CodDiv.
    END.
    RUN DXVACUMTIME_Carga-Temporales (T-VtamDctoVolTime.CodDiv, 
                                      x-Divisiones, 
                                      pCodCli, 
                                      T-VtamDctoVolTime.FchIni, 
                                      T-VtamDctoVolTime.FchFin).
    /* *************************************************************************************************** */
    /* 11/10/2023: Si no hay histórico y no pertenece a una división PADRE entonces NO se aplica descuento */
    IF NOT CAN-FIND(FIRST T-CDOCU NO-LOCK) AND T-VtamDctoVolTime.CodDiv <> pCodDiv THEN NEXT.
    /* *************************************************************************************************** */
    MESSAGE 'continua el proceso'.
    /*
    RUN DXVACUMTIME_Carga-Descuentos.
    RUN DXVACUMTIME_Graba-Descuentos (INPUT X-TIPDTO, x-Forma, OUTPUT pMensaje).
    IF RETURN-VALUE = 'ADM-ERROR' THEN RETURN 'ADM-ERROR'.
    */
END.



/* *********************************** */
PROCEDURE DXVACUMTIME_Carga-Temporales:
/* *********************************** */
    DEF INPUT PARAMETER pDivisionPadre AS CHAR.
    DEF INPUT PARAMETER x-Divisiones AS CHAR.
    DEF INPUT PARAMETER pCodCli AS CHAR.
    DEF INPUT PARAMETER pFchIni AS DATE.
    DEF INPUT PARAMETER pFchFin AS DATE.

    DEF VAR x-PuntoControl AS LOG NO-UNDO.

    x-PuntoControl = NO.        /* Mientras sea NO no vale esa COT */

    RLOOP:
    FOR EACH COTIZACION NO-LOCK WHERE COTIZACION.codcia = s-codcia AND
        COTIZACION.codcli = pCodCli AND
        COTIZACION.coddoc = "COT" AND
        COTIZACION.fchped >= pFchIni AND
        COTIZACION.fchped <= pFchFin AND
        LOOKUP(COTIZACION.coddiv, x-Divisiones) > 0 AND         /* Todas las divisiones involucradas */
        COTIZACION.flgest <> "A"
        BY COTIZACION.FchPed:                                   /* OJO: Importante ordenar por histórico */
        /* Debe tener al menos un producto afecto a descuento perteneciente al PACK*/
        FIND FIRST Facdpedi OF COTIZACION WHERE CAN-FIND(FIRST VtaaDctoVolTime OF T-VtapDctoVolTime WHERE VtaaDctoVolTime.CodMat = Facdpedi.CodMat NO-LOCK)
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE Facdpedi THEN NEXT.        /* Pasamos a la siguiente COTIZACION */
        FOR EACH PEDIDO NO-LOCK WHERE PEDIDO.codcia = COTIZACION.codcia AND
            PEDIDO.coddoc = "PED" AND
            PEDIDO.codref = COTIZACION.coddoc AND
            PEDIDO.nroref = COTIZACION.nroped AND
            PEDIDO.flgest <> "A":
            /* Debe tener al menos un producto válido perteneciente al PACK */
            FIND FIRST Facdpedi OF PEDIDO WHERE CAN-FIND(FIRST VtaaDctoVolTime OF T-VtapDctoVolTime WHERE VtaaDctoVolTime.CodMat = Facdpedi.CodMat NO-LOCK)
                NO-LOCK NO-ERROR.
            IF NOT AVAILABLE Facdpedi THEN NEXT.    /* Pasamos al siguiente PEDIDO */
            FOR EACH ccbcdocu NO-LOCK WHERE ccbcdocu.codcia = s-codcia AND
                ccbcdocu.codped = PEDIDO.coddoc AND
                ccbcdocu.nroped = PEDIDO.nroped AND
                (ccbcdocu.coddoc = "FAC" OR ccbcdocu.coddoc = "BOL") AND
                ccbcdocu.flgest <> "A":
                /* Debe tener al menos un producto válido perteneciente al PACK */
                FIND FIRST Ccbddocu OF Ccbcdocu WHERE CAN-FIND(FIRST VtaaDctoVolTime OF T-VtapDctoVolTime WHERE VtaaDctoVolTime.CodMat = Ccbddocu.CodMat NO-LOCK)
                    NO-LOCK NO-ERROR.
                IF NOT AVAILABLE Ccbddocu THEN NEXT.    /* Pasamos al siguiente COMPROBANTE */
                IF x-PuntoControl = NO AND COTIZACION.CodDiv <> pDivisionPadre THEN NEXT RLOOP.   /* División Hija */
                IF COTIZACION.CodDiv = pDivisionPadre THEN x-PuntoControl = YES.              /* División Padre */
                /* Almacenamos los comprobantes válidos */
                IF NOT CAN-FIND(FIRST T-CDOCU OF ccbcdocu NO-LOCK) THEN DO:
                    CREATE T-CDOCU.
                    BUFFER-COPY ccbcdocu TO T-CDOCU.
                END.
                /* Almacenamos las cotizaciones válidas */
                IF NOT CAN-FIND(FIRST T-CPEDI OF COTIZACION NO-LOCK) THEN DO:
                    CREATE T-CPEDI.
                    BUFFER-COPY COTIZACION TO T-CPEDI.
                END.
            END.
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


END PROCEDURE.
