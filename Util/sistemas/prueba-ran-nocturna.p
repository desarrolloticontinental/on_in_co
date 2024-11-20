DEF NEW SHARED VAR s-codcia AS INTE INIT 001.
DEF VAR s-Reposicion AS LOG NO-UNDO.


FIND Almcfggn WHERE Almcfggn.codcia = s-codcia NO-LOCK NO-ERROR.
CASE TRUE:
    WHEN NOT AVAILABLE Almcfggn OR AlmCfgGn.Temporada = '' THEN RETURN ERROR.
    WHEN Almcfggn.Temporada = "C" THEN s-Reposicion = YES.
    WHEN Almcfggn.Temporada = "NC" THEN s-Reposicion = NO.
END CASE.

DEF VAR x-Clasificaciones AS CHAR NO-UNDO.
DEF VAR k AS INT NO-UNDO.
DEF VAR j AS INT NO-UNDO.

DEF VAR s-TipoCalculo AS CHAR.
DEF VAR pOk AS LOG NO-UNDO.
DEF VAR txtFechaEntrega AS DATE NO-UNDO.
DEF VAR FILL-IN-ImpMin AS DEC INIT 0 NO-UNDO.   /* VALOR POR DEFINIR */

DEF VAR pFchEnt AS DATE NO-UNDO.
DEF VAR pCodDiv AS CHAR NO-UNDO.
DEF VAR pVtaPuntual AS LOG INIT NO NO-UNDO.

DEF VAR LocalFechaProceso AS DATE NO-UNDO.
LocalFechaProceso = TODAY.

txtFechaEntrega  = LocalFechaProceso + 2.

DEFINE BUFFER B-PARAM FOR RepAutomParam.
DEF VAR pMensaje AS CHAR NO-UNDO.
DEF VAR s-CodAlm AS CHAR NO-UNDO.
DEF NEW SHARED VAR s-CodDiv AS CHAR.
DEFINE TEMP-TABLE T-DREPO NO-UNDO LIKE RepAutomDetail.
DEFINE TEMP-TABLE T-GENER NO-UNDO LIKE TabGener.
DEFINE TEMP-TABLE T-MATE NO-UNDO LIKE Almmmate.
DEFINE TEMP-TABLE T-MATE-2 NO-UNDO LIKE Almmmate
       FIELD DesStkMax AS DEC
       FIELD DesStkDis AS DEC
       FIELD DesStkTra AS DEC.
DEFINE TEMP-TABLE T-MATG NO-UNDO LIKE Almmmatg.
DEF VAR pOcurrencia LIKE RepAutomParam.Ocurrencia NO-UNDO.
DEF VAR FILL-IN-PorStkMax AS DEC NO-UNDO.
DEFINE BUFFER B-CREPO FOR almcrepo.
DEFINE BUFFER B-DREPO FOR almdrepo.
DEFINE BUFFER B-MATE FOR Almmmate.
DEFINE BUFFER B-MATE2 FOR Almmmate.
DEFINE BUFFER B-MATG FOR Almmmatg.
DEFINE TEMP-TABLE tmp-tabla NO-UNDO
    FIELD t-CodAlm LIKE Almacen.codalm  FORMAT 'x(3)'
    FIELD t-CodDoc LIKE FacDPedi.CodDoc FORMAT "XXX"
    FIELD t-Nroped LIKE FacDPedi.NroPed FORMAT "XXX-XXXXXXXX"
    FIELD t-CodDiv LIKE FacCPedi.CodDiv FORMAT 'x(5)'
    FIELD t-FchPed LIKE FacDPedi.FchPed
    FIELD t-NomCli LIKE FacCPedi.NomCli COLUMN-LABEL "Cliente" FORMAT "x(35)"
    FIELD t-CodMat LIKE FacDPedi.codmat
    FIELD t-Canped LIKE FacDPedi.CanPed.

DEF VAR s-TipMov AS CHAR INIT "RAN" NO-UNDO.
DEF VAR s-coddoc AS CHAR INIT 'RAN' NO-UNDO.    /* Reposición Automática Nocturna */

FOR EACH B-PARAM NO-LOCK WHERE B-PARAM.CodCia = s-CodCia 
    AND B-PARAM.Dia = WEEKDAY(LocalFechaProceso)
    AND B-PARAM.Orden = 12:
    FIND Almacen WHERE Almacen.codcia = s-codcia AND Almacen.codalm = B-PARAM.CodAlm NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Almacen THEN DO:
        pMensaje = 'ERROR: en el almacen ' + B-PARAM.CodAlm.
        PUT UNFORMATTED NOW ' ' pMensaje SKIP.
        NEXT.
    END.
    RUN Proceso-Principal.
    IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
        IF TRUE <> (pMensaje > '') THEN pMensaje = 'No se pudo generar la reposicion'.
        NEXT.
    END.
END.


{alm/i-reposicionautomaticav51.i}



/* ************************************************************************************************* */
PROCEDURE Proceso-Principal:
/* ************************************************************************************************* */

pMensaje = ''.
DO TRANSACTION ON ERROR UNDO, RETURN 'ADM-ERROR' ON STOP UNDO, RETURN 'ADM-ERROR':
    FIND FIRST RepAutomParam WHERE ROWID(RepAutomParam) = ROWID(B-PARAM) NO-LOCK.
    ASSIGN
        s-CodAlm = RepAutomParam.CodAlm.
    FIND Almacen WHERE Almacen.codcia = s-codcia AND Almacen.codalm = B-PARAM.CodAlm NO-LOCK NO-ERROR.
    ASSIGN
        s-CodDiv = Almacen.CodDiv
        pCodDiv  = Almacen.CodDiv.
    RUN gn/fAlmPrincipal (INPUT s-CodAlm, OUTPUT pOk).
    IF pOk = YES THEN s-TipoCalculo = "GRUPO".
    ELSE s-TipoCalculo = "TIENDA".
    
    /* ********************************************************************************************** */
    /* 1ro. Cargamos la reposición automática */
    /* ********************************************************************************************** */
    x-Clasificaciones = ''.
    j = 0.
    DO k = 1 TO NUM-ENTRIES(RepAutomParam.Clasificacion):
        j = j + 1.
        x-Clasificaciones = x-Clasificaciones + (IF j = 1 THEN '' ELSE ',') +
            ENTRY(k, RepAutomParam.Clasificacion).
    END.
    
    EMPTY TEMP-TABLE T-DREPO.
    EMPTY TEMP-TABLE T-GENER.
    EMPTY TEMP-TABLE T-MATE.
    EMPTY TEMP-TABLE T-MATE-2.
    EMPTY TEMP-TABLE T-MATG.

    /* ********************************************************************************************** */
    /* ********************************************************************************************** */
    PUT UNFORMATTED NOW ' a. Carga Temporales' SKIP.
    RUN Carga-Temporal.
    /* ********************************************************************************************** */

    /* ********************************************************************************************** */
    /* ********************************************************************************************** */
    pOcurrencia = "".
    RUN GENERAR-PEDIDO.
/*     IF RETURN-VALUE = 'ADM-ERROR' THEN DO: */
/*         pMensaje = pOcurrencia.            */
/*         RETURN 'ADM-ERROR'.                */
/*     END.                                   */
END.
RETURN 'OK'.


END PROCEDURE.


/* ************************************************************************************************* */
PROCEDURE Carga-Temporal:
/* ************************************************************************************************* */


    x-Clasificaciones = REPLACE(x-Clasificaciones, 'N', 'XA,XB,XC,XD,XE,XF,NA,NB,NC,ND,NE,NF,,').

/* *********************************************************************** */
/* 1ro. CARGAMOS LOS PRODUCTOS ******************************************* */
/* Tabla: T-MATG ********************************************************* */
/* *********************************************************************** */
DEF VAR x-Linea AS CHAR NO-UNDO.
EMPTY TEMP-TABLE T-MATG.
FOR EACH Almmmatg NO-LOCK WHERE Almmmatg.codcia = s-codcia AND almmmatg.codmat = '043968'
    AND Almmmatg.tpoart <> "D"
    AND (TRUE <> (RepAutomParam.Lineas > "") OR LOOKUP(Almmmatg.codfam, RepAutomParam.Lineas) > 0)
    AND (TRUE <> (RepAutomParam.SubLineas > "") OR LOOKUP(Almmmatg.subfam, RepAutomParam.SubLineas) > 0)
    AND (TRUE <> (RepAutomParam.Proveedores > "") OR LOOKUP(Almmmatg.codpr1, RepAutomParam.Proveedores) > 0)
    AND (TRUE <> (RepAutomParam.Marcas > "") OR LOOKUP(Almmmatg.desmar, RepAutomParam.Marcas) > 0):
    /* Filtro por Clasificación */
    FIND FIRST FacTabla WHERE FacTabla.codcia = Almmmatg.codcia 
        AND FacTabla.tabla = 'RANKVTA' 
        AND FacTabla.codigo = Almmmatg.codmat
        NO-LOCK NO-ERROR.
    IF AVAILABLE FacTabla THEN DO:
        IF s-Reposicion = YES /* Campaña */ THEN DO:
            IF RepAutomParam.Tipo = "General" AND LOOKUP(factabla.campo-c[1],x-Clasificaciones) = 0 THEN NEXT.
            IF RepAutomParam.Tipo = "Utilex" AND LOOKUP(factabla.campo-c[2],x-Clasificaciones) = 0 THEN NEXT.
            IF RepAutomParam.Tipo = "Mayorista" AND LOOKUP(factabla.campo-c[3],x-Clasificaciones) = 0 THEN NEXT.
        END.
        ELSE DO:
            /* No Campaña */
            IF RepAutomParam.Tipo = "General" AND LOOKUP(factabla.campo-c[4],x-Clasificaciones) = 0 THEN NEXT.
            IF RepAutomParam.Tipo = "Utilex" AND LOOKUP(factabla.campo-c[5],x-Clasificaciones) = 0 THEN NEXT.
            IF RepAutomParam.Tipo = "Mayorista" AND LOOKUP(factabla.campo-c[6],x-Clasificaciones) = 0 THEN NEXT.
        END.
    END.
    FIND FIRST T-MATG OF Almmmatg EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE T-MATG THEN CREATE T-MATG.
    BUFFER-COPY Almmmatg TO T-MATG.
END.

/* *********************************************************************** */
/* 2do DEFINIMOS LA CANTIDAD A REPONER *********************************** */
/* Tabla: T-MATE ********************************************************* */
/* *********************************************************************** */
EMPTY TEMP-TABLE T-MATE.
/* Barremos producto por producto */
FILL-IN-PorStkMax = RepAutomParam.PorStkMax.
/* ******************************************************************************** */
/* ******************************************************************************** */
/* 26/02/2022: Oportunidad de Optimizar cuando se especifica un sector */
/* ******************************************************************************** */
/* ******************************************************************************** */
IF RepAutomParam.Sector > '' THEN DO:
    FOR EACH T-MATG, 
        FIRST Almmmate NO-LOCK WHERE Almmmate.codcia = s-codcia AND 
            Almmmate.codalm = RepAutomParam.AlmPed AND
            Almmmate.codmat = T-MATG.CodMat,
        FIRST Almtubic OF Almmmate NO-LOCK:
        IF LOOKUP(TRIM(Almtubic.CodZona), RepAutomParam.Sector) = 0  THEN DELETE T-MATG.
    END.
END.
/* ******************************************************************************** */
/* ******************************************************************************** */
/* ******************************************************************************** */
FOR EACH T-MATG NO-LOCK:
    /* ******************************************************************************** */
    /* RHC 08/05/2017 SE VA A GENERAR UNA TABLA CON LA INFORMACION DE ALMMMATE PERO CON 
        LOS VALORES DEL CD (EJ.ATE) SI FUERA EL CASO */
    /* ******************************************************************************** */
    RUN ARTICULOS-A-SOLICITAR (INPUT T-MATG.CodMat, "AUT").    /* T-MATE */
END.
/* ******************************************************************************** */
/* ******************************************************************************** */
/* ******************************************************************************** */
/* CARGAMOS LA CANTIDAD A REPONER ************************************************* */
/* ******************************************************************************** */
EMPTY TEMP-TABLE T-DREPO.
FOR EACH T-MATG NO-LOCK, EACH T-MATE OF T-MATG NO-LOCK:
    RUN CARGA-REPOSICION.
END.
FOR EACH T-DREPO EXCLUSIVE-LOCK WHERE T-DREPO.canreq <= 0 AND LOOKUP(T-DREPO.AlmPed, '997,998') = 0:
    DELETE T-DREPO.
END.
FOR EACH T-DREPO EXCLUSIVE-LOCK, FIRST B-MATG OF T-DREPO NO-LOCK:
    RUN DATOS-FINALES.
END.
/* ******************************************************************************** */
/* ******************************************************************************** */
/* RHC 02/08/2016 Datos adicionales */
DEF VAR x-Total AS DEC NO-UNDO.
DEF VAR x-Item  AS INT INIT 1 NO-UNDO.
DEF VAR pComprometido AS DEC NO-UNDO.

FOR EACH T-DREPO EXCLUSIVE-LOCK BY T-DREPO.CodMat :
    T-DREPO.ITEM    = x-Item.
    x-Item = x-Item + 1.
END.


END PROCEDURE.



/* ************************************************************************************************* */
PROCEDURE CARGA-REPOSICION:
/* ************************************************************************************************* */

/* *************************************************************** */
/* 1ra parte: DETERMINAMOS SI EL PRODUCTO NECESITA REPOSICION O NO */
/* *************************************************************** */
DEF VAR x-StockMaximo AS DEC NO-UNDO.
DEF VAR x-Empaque AS DEC NO-UNDO.
DEF VAR x-StkAct AS DEC NO-UNDO.
DEF VAR pAReponer   AS DEC NO-UNDO.
DEF VAR pReposicion AS DEC NO-UNDO.
DEF VAR dFactor AS DEC NO-UNDO.
DEF VAR p      AS INT NO-UNDO.
DEF VAR k      AS INT NO-UNDO.

ASSIGN
    x-StockMaximo   = T-MATE.StkMin     /* Stock Maximo + Seguridad */
    x-Empaque       = T-MATE.StkMax     /* Empaque */
    x-StkAct        = T-MATE.StkAct.    /* Stock Actual - Stock Comprometido */

IF x-Empaque <= 0 OR x-Empaque = ? THEN x-Empaque = 1.   /* RHC 29/02/2016 */
/* INCREMENTAMOS LOS PEDIDOS POR REPOSICION EN TRANSITO + TRANSITO COMPRAS  */
x-StkAct = x-StkAct + T-MATE.StkRep + T-MATE.StkActCbd.

IF x-StkAct < 0 THEN x-StkAct = 0.
IF x-StkAct >= x-StockMaximo THEN RETURN.   /* NO VA */

/* ********************* Cantidad de Reposicion ******************* */
/* Definimos el stock maximo */
/* RHC 05/06/2014 Cambio de filosofía, ahora es el % del stock mínimo */
IF x-StkAct >= (x-StockMaximo * RepAutomParam.PorRep / 100) THEN RETURN.    /* NO VA */

/* Se va a reponer en cantidades múltiplo del valor T-MATE.StkMax (EMPAQUE) */
pAReponer = x-StockMaximo - x-StkAct.
pReposicion = pAReponer.    /* RHC 16/06/2017 */

IF pReposicion <= 0 THEN RETURN.    /* NO VA */
pAReponer = pReposicion.    /* OJO */

/* ***************************************************************************** */
/* 2da parte: DISTRIBUIMOS LA CANTIDAD A REPONER ENTRE LOS ALMACENES DE DESPACHO */
/* ***************************************************************************** */
DEF VAR pComprometido AS DEC NO-UNDO.
DEF VAR x-TipMat AS CHAR NO-UNDO.
DEF VAR x-StockDisponible AS DEC NO-UNDO.
DEF VAR x-CanReq AS DEC NO-UNDO.
DEF VAR x-Tolerancia AS DEC NO-UNDO.
DEF VAR x-Item AS INT NO-UNDO.
DEF VAR x-StkMax AS DEC NO-UNDO.

DEF VAR x-ControlDespacho AS LOG INIT NO NO-UNDO.

/* RHC 05/09/2017 Acumulado de CANTIDAD GENERADA */
DEF VAR x-Total-CanGen AS DEC NO-UNDO.
x-Total-CanGen = 0.

IF T-MATG.Chr__02 = "P" THEN x-TipMat = "P". 
ELSE x-TipMat = "T".

    FIND FIRST Almacen WHERE Almacen.codcia = s-codcia 
        AND Almacen.codalm = RepAutomParam.almped NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Almacen THEN RETURN.

    /* CARGAMOS T-MATE-2 CON EL RESUMEN POR CD O SOLO EL DE LA TIENDA */
    RUN RESUMEN-POR-DESPACHO (RepAutomParam.almped, T-MATE.codmat).     /* T-MATE-2 */
    FIND FIRST T-MATE-2 WHERE T-MATE-2.codcia = s-codcia
        AND T-MATE-2.codalm = RepAutomParam.AlmPed
        AND T-MATE-2.codmat = T-MATE.CodMat NO-LOCK NO-ERROR.
    IF NOT AVAILABLE T-MATE-2 THEN RETURN.
    

    /* Incluye Transferencias en Tránsito y Compras en Tránsito */
    x-StockDisponible = T-MATE-2.StkAct - T-MATE-2.StkMin + T-MATE-2.StkRep + T-MATE-2.StkActCbd.

    IF x-StockDisponible <= 0 THEN RETURN.

    /* Se solicitará la reposición de acuerdo al empaque del producto */
    x-CanReq = MINIMUM(x-StockDisponible, pReposicion).
    /* redondeamos al entero superior */
    IF x-CanReq <> TRUNCATE(x-CanReq,0) THEN x-CanReq = TRUNCATE(x-CanReq,0) + 1.
    /* *************************************************** */
    /* RHC 06/07/17 Corregido                              */
    /* *************************************************** */
    IF (x-CanReq / x-Empaque) <> TRUNCATE(x-CanReq / x-Empaque,0) 
        THEN x-CanReq = (TRUNCATE(x-CanReq / x-Empaque,0) + 1) * x-Empaque.
    x-Tolerancia = x-StockMaximo * 1.1.     /* Maximo Solicitante + 10% Tolerancia */
    REPEAT WHILE ( (x-CanReq + x-Total-CanGen) + x-StkAct) > x-Tolerancia:
        x-CanReq = x-CanReq - x-Empaque.
    END.
    /* No debe superar el stock disponible */
    REPEAT WHILE x-CanReq > x-StockDisponible:
        x-CanReq = x-CanReq - x-Empaque.
    END.
    
    IF x-CanReq <= 0 AND x-StockDisponible > 0 THEN x-ControlDespacho = YES.
    IF x-CanReq <= 0 THEN RETURN.    /* Menos que la cantidad por empaque */

    /* ******************** RHC 18/02/2014 FILTRO POR IMPORTE DE REPOSICION ********************* */
    IF T-MATG.MonVta = 2 THEN DO:
        IF x-CanReq * T-MATG.CtoTot * T-MATG.TpoCmb < FILL-IN-ImpMin THEN RETURN.
    END.
    ELSE IF x-CanReq * T-MATG.CtoTot < FILL-IN-ImpMin THEN RETURN.
    IF x-CanReq = ? THEN RETURN.
    /* ****************************************************************************************** */
    CREATE T-DREPO.
    ASSIGN
        T-DREPO.Origen = 'AUT'
        T-DREPO.CodCia = s-codcia 
        T-DREPO.CodAlm = s-codalm 
        T-DREPO.Item = x-Item
        T-DREPO.AlmPed = RepAutomParam.almped
        T-DREPO.CodMat = T-MATE.codmat
        T-DREPO.CanReq = pAReponer
        T-DREPO.CanGen = x-CanReq.
    /* RHC 03/07/17 Redondear al empaque */
    IF T-DREPO.CanGen MODULO x-Empaque > 0 THEN DO:
        T-DREPO.CanGen = ( TRUNCATE(T-DREPO.CanGen / x-Empaque, 0) + 1 ) * x-Empaque.
    END.
    /* RHC Acumulamos */
    x-Total-CanGen = x-Total-CanGen + T-DREPO.CanGen.
    /* Del Almacén Solicitante */
    ASSIGN                 
        T-DREPO.SolStkAct = T-MATE.StkAct + T-MATE.StkComprometido
        T-DREPO.SolStkCom = T-MATE.StkComprometido
        T-DREPO.SolStkDis = T-MATE.StkAct
        T-DREPO.SolStkMax = T-MATE.StkMin
        T-DREPO.SolStkTra = T-MATE.StkRep
        T-DREPO.SolCmpTra = T-MATE.StkActCbd
        .
    /* Del Almacén de Despacho */
    ASSIGN
        T-DREPO.DesStkDis = T-MATE-2.StkAct
        T-DREPO.PorcReposicion = (IF T-DREPO.CanReq <> 0 THEN (T-DREPO.CanGen / T-DREPO.CanReq * 100) ELSE 0).
    /* Datos Adicionales */
    ASSIGN
        T-DREPO.ClfGral = fClasificacion(T-DREPO.CodMat).
    ASSIGN
        x-Item = x-Item + 1
        pReposicion = pReposicion - T-DREPO.CanGen.
    

RELEASE T-DREPO.


END PROCEDURE.


/* ************************************************************************************************* */
PROCEDURE GENERAR-PEDIDO:
/* ************************************************************************************************* */

    DEF VAR n-Items AS INT NO-UNDO.
  DEF VAR k AS INT NO-UNDO.
  DEF VAR x-TotPeso  AS DEC NO-UNDO.
  DEF VAR x-TotVolu  AS DEC NO-UNDO.
  DEF VAR x-ImpTotal AS DEC NO-UNDO.
  DEF VAR x-TotItems AS DEC NO-UNDO.
  DEF VAR x-PreUni AS DEC NO-UNDO.

  DEF VAR pComprometido AS DEC NO-UNDO.
  DEF VAR fDisponible AS DEC NO-UNDO.
  DEF VAR pMensaje AS CHAR NO-UNDO.

  pOcurrencia = "".
  
  RLOOP:
  DO TRANSACTION ON ERROR UNDO, RETURN "ADM-ERROR" ON STOP UNDO, RETURN "ADM-ERROR":
      IF NOT CAN-FIND(FIRST Faccorre WHERE Faccorre.codcia = s-codcia 
                      AND Faccorre.coddoc = s-coddoc 
                      AND Faccorre.flgest = YES 
                      AND Faccorre.coddiv = pCodDiv
                      NO-LOCK)
          THEN DO:
          pOcurrencia = "NO definido el correlativo doc: " + s-coddoc + " división: " + pcoddiv.
          RETURN 'ADM-ERROR'.
      END.
      /* Depuramos el temporal */
      FOR EACH T-DREPO WHERE T-DREPO.AlmPed = '998':
          DELETE T-DREPO.
      END.
      FOR EACH T-DREPO WHERE T-DREPO.AlmPed <> RepAutomParam.AlmPed:
          DELETE T-DREPO.
      END.
      IF RepAutomParam.Sector > '' THEN DO:
          FOR EACH T-DREPO NO-LOCK:
              MESSAGE 'sector' t-drepo.codzona RepAutomParam.Sector VIEW-AS ALERT-BOX.
              IF LOOKUP(T-DREPO.CodZona, RepAutomParam.Sector) = 0 THEN DELETE T-DREPO.
          END.
      END.
      IF NOT CAN-FIND(FIRST T-DREPO NO-LOCK) THEN DO:
          pOcurrencia = "NO hay registros generados".
          RETURN 'ADM-ERROR'.
      END.
      /* Generación */
      n-Items = 0.
      FOR EACH T-DREPO NO-LOCK,
          FIRST Almmmatg OF T-DREPO NO-LOCK,
          FIRST Almmmate OF Almmmatg WHERE Almmmate.CodAlm = s-CodAlm NO-LOCK,
          FIRST Almtfami OF Almmmatg NO-LOCK,
          FIRST AlmSFami OF Almmmatg NO-LOCK 
          BREAK BY T-DREPO.AlmPed BY Almmmatg.DesMar BY Almmmatg.DesMat
          ON ERROR UNDO, THROW:
          /* *********************************************************** */
          /* *********************************************************** */
          MESSAGE t-drepo.codmat t-drepo.canreq VIEW-AS ALERT-BOX.
          RETURN.
          DELETE T-DREPO.
          n-Items = n-Items + 1.
      END.
      IF AVAILABLE FacCorre THEN RELEASE Faccorre.
      IF AVAILABLE Almcrepo THEN RELEASE almcrepo.
      IF AVAILABLE Almdrepo THEN RELEASE almdrepo.
      IF AVAILABLE RepAutomDetail THEN RELEASE RepAutomDetail.
  END.
  IF TRUE <> (pOcurrencia > '') THEN pOcurrencia = "Proceso OK".
  IF pMensaje > '' THEN pOcurrencia = pOcurrencia + ' ' + pMensaje.
  RETURN 'OK'.


END PROCEDURE.
