    DEF SHARED VAR s-codcia AS INT INIT 001.
    DEF SHARED VAR cl-codcia AS INT init 000.
    DEFINE BUFFER B-DIVI FOR GN-DIVI.
    DEFINE BUFFER COTIZACION FOR FacCPedi.
    DEFINE BUFFER PEDIDO FOR FacCPedi.

DEF VAR x-fchent AS DATE.
DEF VAR x-error AS CHAR.
FIND faccpedi WHERE codcia = 1 and coddoc = 'o/d' and nroped = '067004593'
    NO-LOCK.
RUN p-fchent-v3 (faccpedi.codalm,
                    TODAY,
                    "23:59:00",
                    faccpedi.codcli,
                    faccpedi.coddiv,
                    faccpedi.codpos,
                    faccpedi.coddoc,
                    faccpedi.nroped,
                    0,
                    0,
                    INPUT-OUTPUT x-fchent,
                    OUTPUT x-error).

MESSAGE x-fchent.
PROCEDURE p-fchent-v3:

    DEF INPUT PARAMETER pCodAlm AS CHAR.    /* Almacén Despacho */
    DEF INPUT PARAMETER pFechaBase AS DATE.
    DEF INPUT PARAMETER pHoraBase AS CHAR.
    DEF INPUT PARAMETER pCodCli AS CHAR.
    DEF INPUT PARAMETER pDivOri AS CHAR.    /* División Solicitante */
    DEF INPUT PARAMETER pUbigeo AS CHAR.
    DEF INPUT PARAMETER pCodDoc AS CHAR.
    DEF INPUT PARAMETER pNroPed AS CHAR.
    DEF INPUT PARAMETER pNroSKU AS INT.
    DEF INPUT PARAMETER pPeso   AS DEC.
    DEF INPUT-OUTPUT PARAMETER pFchEnt AS DATE.
    DEF OUTPUT PARAMETER pError AS CHAR NO-UNDO.
    pError = ''.    /* Valor por Defecto */
    IF pFchEnt = ? THEN pFchEnt = pFechaBase.
    IF pFchEnt < pFechaBase THEN pFchEnt = pFechaBase.


    /* *************************************************************************** */
    /* DETERMINAMOD SI EL ALMACEN DE DESPACHO ES UN CENTRO DE DISTRIBUCION         */
    /* *************************************************************************** */
    DEF VAR s-CentroDistribucion AS LOG INIT NO NO-UNDO.

    FIND Almacen WHERE Almacen.codcia = s-codcia
        AND Almacen.codalm = pCodAlm
        AND CAN-FIND(FIRST gn-divi WHERE gn-divi.codcia = s-codcia
                     AND gn-divi.coddiv = Almacen.coddiv 
                     AND gn-divi.Campo-Log[5] = YES NO-LOCK)
        NO-LOCK NO-ERROR.
    IF AVAILABLE Almacen THEN s-CentroDistribucion = YES.

    /* *************************************************************************** */
    /* REGLA GENERAL: DIAS ANTES DE DESPACHAR */
    /* *************************************************************************** */
    DEF VAR x-Minimo AS INT INIT 2 NO-UNDO.   /* Mínimo 2 días antes de la hora de corte */
    DEF VAR cCodDiv AS CHAR NO-UNDO.

    FIND Almacen WHERE Almacen.codcia = s-codcia
        AND Almacen.codalm = pCodAlm
        NO-LOCK NO-ERROR.
    cCodDiv = Almacen.CodDiv.   /* Valor por Defecto */
    /* Definimos la hora de corte */
    DEF VAR x-HoraCorte AS CHAR NO-UNDO.
    IF s-CentroDistribucion = YES THEN x-HoraCorte = "18:00:00".
    ELSE x-HoraCorte = "17:00:00".
    /* ************************************** */
    /* RHC 01/02/2018 Buscamos lo configurado */
    /* ************************************** */
    FIND VtaTabla WHERE VtaTabla.CodCia = s-CodCia
        AND VtaTabla.Tabla = 'CANALHR'
        AND VtaTabla.Llave_c1 = cCodDiv
        AND VtaTabla.Llave_c2 = pDivOri
        NO-LOCK NO-ERROR.
    IF AVAILABLE VtaTabl THEN
        ASSIGN
        x-Minimo    = VtaTabla.Valor[1]
        x-HoraCorte = VtaTabla.Libre_c01.
    IF NUM-ENTRIES(pHoraBase,':') < 3 THEN pHoraBase = pHoraBase + ':00'.
    /* ************************************** */

    IF s-CentroDistribucion = YES THEN DO:
        /* **************************************************************************** */
        /* LAS EXCEPCIONES NO TIENEN HORA DE CORTE PERO POR LO MENOS UN DIA DE ATENCION */
        /* **************************************************************************** */
        DEF VAR x-Excepciones AS LOG INIT NO NO-UNDO.
        /* *************************************************************************** */
        /* FIN DE EXCEPCIONES                                                          */
        /* *************************************************************************** */
    END.
    /* *************************************************************************** */
    /* *************************************************************************** */
    /* RUTINA SOLO PARA CENTRO DE DISTRIBUCION */
    /* *************************************************************************** */
    /* *************************************************************************** */
    /* Buscamos día programado para el Centro de Distribución */
    /* El parámetro pUbigeo puede ser:
        150103  Departamento, 
        Provincia y Distrito (DDPPdd)
        L03     Código Postal
        CR      Cliente Recoge
        P0      Provincias
        */
    /* *************************************************************************** */
    /* DETERMINAMOS LA ZONA GEOGRAFICA DEL DESPACHO */
    /* *************************************************************************** */
    DEF VAR cZonaGHR AS CHAR NO-UNDO.
    DEF VAR cSubZonaGHR AS CHAR NO-UNDO.
    CASE pUbigeo:
        WHEN "P0" THEN ASSIGN cZonaGHR = "02" cSubZonaGHR = "01".     /* LIMA CENTRO (AGENCIAS) */
        WHEN "CR" THEN ASSIGN cZonaGHR = "CR" cSubZonaGHR = "CR".     /* CLIENTE RECOGE */
        OTHERWISE DO:
            /* Primero suponemos que es un Código Postal */
            FIND Almtabla WHERE Almtabla.Tabla = "CP" AND Almtabla.Codigo = pUbigeo
                NO-LOCK NO-ERROR.
            IF AVAILABLE Almtabla THEN DO:
                /* CODIGO POSTAL */
                FIND TabDistr WHERE TabDistr.CodPos = Almtabla.Codigo NO-LOCK NO-ERROR.
                IF NOT AVAILABLE TabDistr THEN DO:
                    pError = "NO está configurado el código postal " + pUbigeo + CHR(10) +
                        " en la programación de Pre-Hojas de Ruta".
                    RETURN.
                END.
                pUbigeo = TRIM(TabDistr.CodDepto) + TRIM(TabDistr.CodProvi) + TRIM(TabDistr.CodDistr).
            END.
            /* UBIGEO */
            FIND FIRST VtaDTabla WHERE VtaDTabla.CodCia = s-CodCia
                AND VtaDTabla.Tabla = 'SZGHR'
                AND VtaDTabla.LlaveDetalle = "D"
                AND VtaDTabla.Libre_c01 = SUBSTRING(pUbigeo,1,2)
                AND VtaDTabla.Libre_c02 = SUBSTRING(pUbigeo,3,2)
                AND VtaDTabla.Libre_c03 = SUBSTRING(pUbigeo,5,2)
                NO-LOCK NO-ERROR.
            IF NOT AVAILABLE VtaDTabla THEN DO:
                pError = "NO está configurado el código de ubigeo " + pUbigeo + CHR(10) +
                    " en la programación de Pre-Hojas de Ruta".
                RETURN.
            END.
            ASSIGN
                cZonaGHR    = VtaDTabla.Llave
                cSubZonaGHR = VtaDTabla.Tipo.
        END.
    END CASE.
    /* RHC 19/12/17 La programación x CANAL es opcional */
    DEF VAR x-CanalHR AS LOG INIT YES NO-UNDO.
    FIND FIRST VtaTabla WHERE VtaTabla.CodCia = s-codcia
        AND VtaTabla.Tabla = "CANALHRXDIA"
        AND VtaTabla.Llave_c1 = cCodDiv
        AND VtaTabla.Llave_c2 = pDivOri
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE VtaTabla THEN x-CanalHR = NO.  /* NO Hay programacion por CANAL DE VENTA */
    /* Obligatorio cronograma de despacho por CD */
    FIND FIRST VtaTabla WHERE VtaTabla.CodCia = s-codcia
        AND VtaTabla.Tabla = "SZGHRXDIA"
        AND VtaTabla.Llave_c1 = cCodDiv
        AND VtaTabla.Llave_c2 = cZonaGHR
        AND VtaTabla.Llave_c3 = cSubZonaGHR
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE VtaTabla THEN DO:
        pError = "NO está configurado el cronograma de despacho del ubigeo: " + pUbigeo + CHR(10) +
            " en la programación de Despachos".
        RETURN.
    END.
    /* AHORA SI DETERMINAMOS LA FECHA DE PARTIDA */
    IF pHoraBase > x-HoraCorte THEN x-Minimo = x-Minimo + 1.
    IF pFchEnt - pFechaBase < x-Minimo THEN pFchEnt = pFechaBase + x-Minimo.
    /* Si es domingo corre un día */
    IF WEEKDAY(pFchEnt) = 1 THEN pFchEnt = pFchEnt + 1.     
    /* *************************************************************************** */
    /* BUSCAMOS DIA DE DESPACHO */
    /* *************************************************************************** */
    DEF VAR fFchProg AS DATE NO-UNDO.
    DEF VAR iNroSKU AS INT NO-UNDO.
    DEF VAR iNroPed AS INT NO-UNDO.     /* Número de pedidos despachados del día */
    DEF VAR dPeso   AS DEC NO-UNDO.

    DEF BUFFER B-TABLA FOR VtaTabla.
    FIND B-TABLA WHERE B-TABLA.codcia = s-codcia 
        AND B-TABLA.Tabla = 'CDSKU'
        AND B-TABLA.Llave_c1 = cCodDiv
        NO-LOCK NO-ERROR.
    fFchProg = pFchEnt - 1.     /* OJO: Punto de partida */


    PRINCIPAL:
    REPEAT:
        fFchProg = fFchProg + 1.
        /* ************************************************************** */
        IF WEEKDAY(fFchProg) = 1 THEN NEXT.     /* Salta el domingo */
        /* ************************************************************** */
        /* RHC 16/12/17 CONFIGURADO COMO FERIADO, SALTA AL DIA SIGUIENTE  */
        /* ************************************************************** */
        FIND FacTabla WHERE FacTabla.CodCia = s-codcia
            AND FacTabla.Tabla = 'SZGHRFERIADO'
            AND FacTabla.Codigo = cCodDiv
            NO-LOCK NO-ERROR.
        IF AVAILABLE FacTabla AND FacTabla.Campo-L[WEEKDAY(fFchProg) - 1] = YES THEN NEXT.
        /* ************************************************************** */
        /* Día de despacho de acuerdo al Canal de Venta */
        IF x-CanalHR = YES THEN DO:
            FIND VtaTabla WHERE VtaTabla.CodCia = s-codcia
                AND VtaTabla.Tabla = "CANALHRXDIA"
                AND VtaTabla.Llave_c1 = cCodDiv
                AND VtaTabla.Llave_c2 = pDivOri
                AND VtaTabla.Valor[1] = WEEKDAY(fFchProg)
                NO-LOCK NO-ERROR.
            IF NOT AVAILABLE VtaTabla THEN NEXT.
        END.
        /* Día de despacho de acuerdo al cronograma logístico */
        FIND VtaTabla WHERE VtaTabla.CodCia = s-codcia
            AND VtaTabla.Tabla = "SZGHRXDIA"
            AND VtaTabla.Llave_c1 = cCodDiv
            AND VtaTabla.Llave_c2 = cZonaGHR
            AND VtaTabla.Llave_c3 = cSubZonaGHR
            AND VtaTabla.Valor[1] = WEEKDAY(fFchProg)
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE VtaTabla THEN NEXT.    /* NO tiene programado ese día */
                                                /* Pasa al siguiente día */
        IF NOT AVAILABLE B-TABLA  THEN LEAVE PRINCIPAL.   /* NO tiene minimo de SKU, fecha OK */
        /* NO tiene mínimos definidos, fecha OK */
        IF B-TABLA.Valor[1] = 0 
            AND B-TABLA.Valor[2] = 0 
            AND B-TABLA.Valor[3] = 0 
            AND B-TABLA.Valor[4] = 0
            AND B-TABLA.Valor[5] = 0
            THEN LEAVE PRINCIPAL. 
        /* **************************************************************************************** */
        /* VERIFICAMOS LA CAPACIDAD CONFIGURADA PARA EL CD */
        /* **************************************************************************************** */
        ASSIGN
            iNroSKU = 0
            iNroPed = 0
            dPeso   = 0.
        /* POR ORDENES DE DESPACHO CREDITO */
        FOR EACH Faccpedi NO-LOCK WHERE Faccpedi.codcia = s-codcia
            AND Faccpedi.divdes = cCodDiv
            AND Faccpedi.coddoc = 'O/D'
            AND Faccpedi.fchent = fFchProg
            AND Faccpedi.flgest = 'P':
            /* Acumulamos */
            /* ************************************************************************** */
            /* RHC 22/01/18 Control de pedidos por FERias (Expolibreria) */
            /* ************************************************************************** */
            FIND FIRST PEDIDO WHERE PEDIDO.codcia = Faccpedi.codcia
                AND PEDIDO.coddiv = Faccpedi.coddiv
                AND PEDIDO.coddoc = Faccpedi.codref
                AND PEDIDO.nroped = Faccpedi.nroref
                NO-LOCK NO-ERROR.
            IF AVAILABLE PEDIDO THEN DO:
                FIND FIRST COTIZACION WHERE COTIZACION.codcia = PEDIDO.codcia
                    AND COTIZACION.coddiv = PEDIDO.coddiv
                    AND COTIZACION.coddoc = PEDIDO.codref
                    AND COTIZACION.nroped = PEDIDO.nroref
                    NO-LOCK NO-ERROR.
                IF AVAILABLE COTIZACION THEN DO:
                    FIND B-DIVI WHERE B-DIVI.codcia = s-codcia 
                        AND B-DIVI.coddiv = COTIZACION.Libre_c01      /* Lista de Precios */
                        NO-LOCK NO-ERROR.
                    IF AVAILABLE B-DIVI AND B-DIVI.CanalVenta = "FER" THEN iNroPed = iNroPed + 1.
                END.
            END.
            /* ************************************************************************** */
            FOR EACH Facdpedi OF Faccpedi NO-LOCK, FIRST Almmmatg OF Facdpedi NO-LOCK:
                iNroSKU = iNroSKU + 1.
                dPeso = dPeso + ((Facdpedi.canped * Facdpedi.factor) * Almmmatg.Pesmat).
            END.
        END.
        /* POR ORDENES DE DESPACHO MOSTRADOR */
        FOR EACH Faccpedi NO-LOCK WHERE Faccpedi.codcia = s-codcia
            AND Faccpedi.divdes = cCodDiv
            AND Faccpedi.coddoc = 'O/M'
            AND Faccpedi.fchent = fFchProg
            AND Faccpedi.flgest = 'P':
            /* Acumulamos */
            FOR EACH Facdpedi OF Faccpedi NO-LOCK, FIRST Almmmatg OF Facdpedi NO-LOCK:
                iNroSKU = iNroSKU + 1.
                dPeso = dPeso + ((Facdpedi.canped * Facdpedi.factor) * Almmmatg.Pesmat).
            END.
        END.
        /* POR ORDENES DE TRANSFERENCIA */
        FOR EACH Faccpedi NO-LOCK WHERE Faccpedi.codcia = s-codcia
            AND Faccpedi.divdes = cCodDiv
            AND Faccpedi.coddoc = 'OTR'
            AND Faccpedi.fchent = fFchProg
            AND Faccpedi.flgest = 'P':
            /* Acumulamos */
            FOR EACH Facdpedi OF Faccpedi NO-LOCK, FIRST Almmmatg OF Facdpedi NO-LOCK:
                iNroSKU = iNroSKU + 1.
                dPeso = dPeso + ((Facdpedi.canped * Facdpedi.factor) * Almmmatg.Pesmat).
            END.
        END.
        
        /* RHC 22/01/2018 Control de # de Pedidos Expolibreria */
        IF B-TABLA.Valor[5] > 0 AND pCodDoc = "O/D" THEN DO:
            FIND Faccpedi WHERE Faccpedi.codcia = s-codcia
                AND Faccpedi.coddoc = pCodDoc
                AND Faccpedi.nroped = pNroPed
                NO-LOCK NO-ERROR.
            IF AVAILABLE Faccpedi THEN DO:
                FIND B-DIVI WHERE B-DIVI.codcia = s-codcia 
                    AND B-DIVI.coddiv = Faccpedi.Libre_c01      /* Lista de Precios */
                    NO-LOCK NO-ERROR.
                IF AVAILABLE B-DIVI AND B-DIVI.CanalVenta = "FER" THEN DO:
                    IF iNroPed > B-TABLA.Valor[5] THEN NEXT PRINCIPAL.
                END.
            END.
        END.
        /* INCREMENTAMOS DATOS EXTERNOS en 10% */
        iNroSKU = iNroSKU + pNroSKU.
        dPeso = dPeso + pPeso.
        
        IF WEEKDAY(fFchProg) >= 2 AND WEEKDAY(fFchProg) <= 6 THEN DO:   /* Lunes a Viernes */
            IF B-TABLA.Valor[1] > 0 AND iNroSKU > B-TABLA.Valor[1] * 1.10 THEN NEXT.
            IF B-TABLA.Valor[3] > 0 AND dPeso > B-TABLA.Valor[3] * 1.10   THEN NEXT.
            LEAVE PRINCIPAL.      /* Fecha OK */
        END.
        ELSE DO:                                                        /* Sábado y Domingo */
            IF B-TABLA.Valor[2] > 0 AND iNroSKU > B-TABLA.Valor[2] * 1.10 THEN NEXT.
            IF B-TABLA.Valor[4] > 0 AND dPeso > B-TABLA.Valor[4] * 1.10   THEN NEXT.
            LEAVE PRINCIPAL.      /* Fecha OK */
        END.
        
    END.


pFchEnt = fFchProg.
END PROCEDURE.
