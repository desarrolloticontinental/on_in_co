&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
/* Connected Databases 
          integral         PROGRESS
*/


/* Temp-Table and Buffer definitions                                    */
DEFINE BUFFER B-CREPO FOR almcrepo.
DEFINE BUFFER B-DREPO FOR almdrepo.
DEFINE BUFFER B-MATE FOR Almmmate.
DEFINE BUFFER B-MATE2 FOR Almmmate.
DEFINE BUFFER B-MATG FOR Almmmatg.
DEFINE BUFFER B-PARAM FOR RepAutomParam.
DEFINE TEMP-TABLE REPOSICIONES NO-UNDO LIKE almcrepo
       FIELD RowidRepAuto AS ROWID.
DEFINE TEMP-TABLE T-DREPO NO-UNDO LIKE RepAutomDetail.
DEFINE TEMP-TABLE T-GENER NO-UNDO LIKE TabGener.
DEFINE TEMP-TABLE T-MATE NO-UNDO LIKE Almmmate.
DEFINE TEMP-TABLE T-MATE-2 NO-UNDO LIKE Almmmate
       FIELD DesStkMax AS DEC
       FIELD DesStkDis AS DEC
       FIELD DesStkTra AS DEC.
DEFINE TEMP-TABLE T-MATG NO-UNDO LIKE Almmmatg.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEF NEW SHARED VAR s-codcia AS INT INIT 001.
DEF NEW SHARED VAR cl-codcia AS INT INIT 000.
DEF NEW SHARED VAR pv-codcia AS INT INIT 000.
DEF NEW SHARED VAR s-user-id AS CHAR INIT 'SYSTEM'.
DEF NEW SHARED VAR s-CodDiv AS CHAR.

FIND FIRST AlmCfgGn WHERE almcfggn.codcia = s-codcia NO-LOCK NO-ERROR.
IF NOT AVAILABLE AlmCfgGn THEN RETURN ERROR.

DEF VAR s-Reposicion AS LOG.     /* Campaña Yes, No Campaña No */
DEF VAR s-CodAlm AS CHAR NO-UNDO.
DEF VAR pMensaje AS CHAR NO-UNDO.

DEFINE TEMP-TABLE tmp-tabla NO-UNDO
    FIELD t-CodAlm LIKE Almacen.codalm  FORMAT 'x(3)'
    FIELD t-CodDoc LIKE FacDPedi.CodDoc FORMAT "XXX"
    FIELD t-Nroped LIKE FacDPedi.NroPed FORMAT "XXX-XXXXXXXX"
    FIELD t-CodDiv LIKE FacCPedi.CodDiv FORMAT 'x(5)'
    FIELD t-FchPed LIKE FacDPedi.FchPed
    FIELD t-NomCli LIKE FacCPedi.NomCli COLUMN-LABEL "Cliente" FORMAT "x(35)"
    FIELD t-CodMat LIKE FacDPedi.codmat
    FIELD t-Canped LIKE FacDPedi.CanPed.

DISABLE TRIGGERS FOR LOAD OF Almdrepo.
DISABLE TRIGGERS FOR LOAD OF Almcrepo.
DISABLE TRIGGERS FOR LOAD OF Faccorre.
DISABLE TRIGGERS FOR LOAD OF Faccpedi.
DISABLE TRIGGERS FOR LOAD OF Facdpedi.
DISABLE TRIGGERS FOR LOAD OF Vtacdocu.
DISABLE TRIGGERS FOR LOAD OF Vtaddocu.
DISABLE TRIGGERS FOR LOAD OF VtadTrkPed.
DISABLE TRIGGERS FOR LOAD OF VtacTrkPed.
DISABLE TRIGGERS FOR LOAD OF Di-RutaC.
DISABLE TRIGGERS FOR LOAD OF Di-RutaD.

DEF VAR FILL-IN-PorStkMax AS DEC NO-UNDO.

DEF TEMP-TABLE T-DREPO-2 LIKE T-DREPO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-fFaltanteGrupo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fFaltanteGrupo Procedure 
FUNCTION fFaltanteGrupo RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
   Temp-Tables and Buffers:
      TABLE: B-CREPO B "?" ? INTEGRAL almcrepo
      TABLE: B-DREPO B "?" ? INTEGRAL almdrepo
      TABLE: B-MATE B "?" ? INTEGRAL Almmmate
      TABLE: B-MATE2 B "?" ? INTEGRAL Almmmate
      TABLE: B-MATG B "?" ? INTEGRAL Almmmatg
      TABLE: B-PARAM B "?" ? INTEGRAL RepAutomParam
      TABLE: REPOSICIONES T "?" NO-UNDO INTEGRAL almcrepo
      ADDITIONAL-FIELDS:
          FIELD RowidRepAuto AS ROWID
      END-FIELDS.
      TABLE: T-DREPO T "?" NO-UNDO INTEGRAL RepAutomDetail
      TABLE: T-GENER T "?" NO-UNDO INTEGRAL TabGener
      TABLE: T-MATE T "?" NO-UNDO INTEGRAL Almmmate
      TABLE: T-MATE-2 T "?" NO-UNDO INTEGRAL Almmmate
      ADDITIONAL-FIELDS:
          FIELD DesStkMax AS DEC
          FIELD DesStkDis AS DEC
          FIELD DesStkTra AS DEC
      END-FIELDS.
      TABLE: T-MATG T "?" NO-UNDO INTEGRAL Almmmatg
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 11
         WIDTH              = 62.72.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
/* OJO: SE VAN A CREAR PRE REPOSICIONES AUTOMATICAS */
DEF VAR s-TipMov AS CHAR INIT "RAN" NO-UNDO.
DEF VAR s-coddoc AS CHAR INIT 'RAN' NO-UNDO.    /* Reposición Automática Nocturna */

DEF VAR pOcurrencia LIKE RepAutomParam.Ocurrencia NO-UNDO.

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
PUT UNFORMATTED NOW ' INICIO' SKIP. 
/* 1ro. Limpiamos información del día */
RUN Limpia-Base.
IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
    PUT UNFORMATTED 'Error al limpiar base: ' NOW SKIP.
    QUIT.
END.

/* 2do. Proceso Principal */
/* Anular RAN Pendientes */
PUT UNFORMATTED NOW ' ANULA RANs PENDIENTES DE APROBACION' SKIP.
RUN Rechaza-RAN.
IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
    PUT UNFORMATTED NOW ' Error al ANULAR RAN' SKIP.
    QUIT.
END.

/* 3ro. Proceso Principal */
PUT NOW ' GENERACION DE REPOSICIONES AUTOMATICAS:' SKIP.
FOR EACH B-PARAM NO-LOCK WHERE B-PARAM.CodCia = s-CodCia 
    AND B-PARAM.Dia = WEEKDAY(LocalFechaProceso)
    AND B-PARAM.FechaInicio = ?
    /*AND B-PARAM.Aprobar = YES*/
    BY  B-PARAM.Orden:
    PUT UNFORMATTED NOW ' ' B-PARAM.Orden ' ' B-PARAM.CodAlm ' ' B-PARAM.AlmPed ' ' B-PARAM.Sector SKIP.
    FIND Almacen WHERE Almacen.codcia = s-codcia AND Almacen.codalm = B-PARAM.CodAlm NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Almacen THEN DO:
        pMensaje = 'ERROR: en el almacen ' + B-PARAM.CodAlm.
        PUT UNFORMATTED NOW ' ' pMensaje SKIP.
        NEXT.
    END.

    /* Borramos la tabla temporal */
    EMPTY TEMP-TABLE reposiciones.

    RUN Proceso-Principal.
    IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
        IF TRUE <> (pMensaje > '') THEN pMensaje = 'No se pudo generar la reposicion'.
        PUT UNFORMATTED NOW ' ERROR: ' pMensaje SKIP.
        NEXT.
    END.
    /* RHC 02/03/2020 Control de hora */
    /*IF STRING(TIME, 'HH:MM:SS') > '09:00:00' THEN LEAVE.*/

    /* 4to Generación de R/A, PHR y HPK */
    PUT NOW ' GENERACION DE OTR, PHR y HPK:' SKIP.
    FOR EACH REPOSICIONES NO-LOCK,
        FIRST B-CREPO NO-LOCK WHERE B-CREPO.CodCia = REPOSICIONES.codcia
        AND B-CREPO.CodAlm = REPOSICIONES.codalm
        AND B-CREPO.TipMov = REPOSICIONES.tipmov
        AND B-CREPO.NroSer = REPOSICIONES.nroser
        AND B-CREPO.NroDoc = REPOSICIONES.nrodoc:
        PUT UNFORMATTED NOW ' ' REPOSICIONES.codalm ' ' REPOSICIONES.tipmov ' ' REPOSICIONES.nroser ' ' REPOSICIONES.nrodoc SKIP.
        /* ********************************************************************************* */
        /* RHC GENERACION AUTOMATICA DE R/A Y OTR EN UN SOLO PASO */
        /* Si no se puede generar la R/A y/o la OTR NO se detiene el proceso */
        /* pMensaje almacena los documentos que se van generando o el error */
        /* ********************************************************************************* */
        RUN GENERAR-RA-OTR (INPUT ROWID(B-CREPO), OUTPUT pMensaje).
        /* ********************************************************************************* */
        /* ********************************************************************************* */
    END.
END.

/*
/* 4to Generación de R/A, PHR y HPK */
PUT NOW ' GENERACION DE OTR, PHR y HPK:' SKIP.
FOR EACH REPOSICIONES NO-LOCK,
    FIRST B-CREPO NO-LOCK WHERE B-CREPO.CodCia = REPOSICIONES.codcia
    AND B-CREPO.CodAlm = REPOSICIONES.codalm
    AND B-CREPO.TipMov = REPOSICIONES.tipmov
    AND B-CREPO.NroSer = REPOSICIONES.nroser
    AND B-CREPO.NroDoc = REPOSICIONES.nrodoc:
    PUT UNFORMATTED NOW ' ' REPOSICIONES.codalm ' ' REPOSICIONES.tipmov ' ' REPOSICIONES.nroser ' ' REPOSICIONES.nrodoc SKIP.
    /* ********************************************************************************* */
    /* RHC GENERACION AUTOMATICA DE R/A Y OTR EN UN SOLO PASO */
    /* Si no se puede generar la R/A y/o la OTR NO se detiene el proceso */
    /* pMensaje almacena los documentos que se van generando o el error */
    /* ********************************************************************************* */
    RUN GENERAR-RA-OTR (INPUT ROWID(B-CREPO), OUTPUT pMensaje).
    /* ********************************************************************************* */
    /* ********************************************************************************* */
END.
*/


PUT UNFORMATTED NOW ' FIN' SKIP.
QUIT.

{alm/i-reposicionautomaticav51.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-CARGA-REPOSICION) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CARGA-REPOSICION Procedure 
PROCEDURE CARGA-REPOSICION :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


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
/*         T-DREPO.DesStkAct = T-MATE-2.StkAct + T-MATE-2.StkComprometido */
/*         T-DREPO.DesStkCom = T-MATE-2.StkComprometido                   */
        T-DREPO.DesStkDis = T-MATE-2.StkAct
/*         T-DREPO.DesStkMax = T-MATE-2.StkMin    */
/*         T-DREPO.DesCmpTra = T-MATE-2.StkActCbd */
        T-DREPO.PorcReposicion = (IF T-DREPO.CanReq <> 0 THEN (T-DREPO.CanGen / T-DREPO.CanReq * 100) ELSE 0).
    /* Datos Adicionales */
/*     ASSIGN                                                                              */
/*         T-DREPO.GrpStkDis = T-MATE-2.DesStkDis                                          */
/*         T-DREPO.FSGrupo = T-MATE-2.DesStkDis - T-MATE-2.DesStkMax + T-MATE-2.DesStkTra. */
    ASSIGN
        T-DREPO.ClfGral = fClasificacion(T-DREPO.CodMat).
    ASSIGN
        x-Item = x-Item + 1
        pReposicion = pReposicion - T-DREPO.CanGen.

RELEASE T-DREPO.

END PROCEDURE.

/*
FOR EACH Almrepos NO-LOCK WHERE Almrepos.CodCia = s-codcia
    AND Almrepos.TipMat = x-TipMat      /* Propios o Terceros */
    AND Almrepos.CodAlm = T-MATE.codalm 
    AND (Almrepos.AlmPed > '' AND Almrepos.AlmPed <> T-MATE.codalm)
    BY Almrepos.Orden:
    FIND FIRST Almacen WHERE Almacen.codcia = Almrepos.codcia 
        AND Almacen.codalm = Almrepos.almped NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Almacen THEN NEXT.

    /* CARGAMOS T-MATE-2 CON EL RESUMEN POR CD O SOLO EL DE LA TIENDA */
    RUN RESUMEN-POR-DESPACHO (Almrepos.almped, T-MATE.codmat).
    FIND FIRST T-MATE-2 WHERE T-MATE-2.codcia = s-codcia
        AND T-MATE-2.codalm = Almrepos.AlmPed
        AND T-MATE-2.codmat = T-MATE.CodMat NO-LOCK NO-ERROR.
    IF NOT AVAILABLE T-MATE-2 THEN NEXT.

    /* Incluye Transferencias en Tránsito y Compras en Tránsito */
    x-StockDisponible = T-MATE-2.StkAct - T-MATE-2.StkMin + T-MATE-2.StkRep + T-MATE-2.StkActCbd.
    IF x-StockDisponible <= 0 THEN NEXT.

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
    IF x-CanReq <= 0 THEN NEXT.    /* Menos que la cantidad por empaque */

    /* ******************** RHC 18/02/2014 FILTRO POR IMPORTE DE REPOSICION ********************* */
    IF T-MATG.MonVta = 2 THEN DO:
        IF x-CanReq * T-MATG.CtoTot * T-MATG.TpoCmb < FILL-IN-ImpMin THEN NEXT.
    END.
    ELSE IF x-CanReq * T-MATG.CtoTot < FILL-IN-ImpMin THEN NEXT.
    IF x-CanReq = ? THEN NEXT.
    /* ****************************************************************************************** */
    CREATE T-DREPO.
    ASSIGN
        T-DREPO.Origen = 'AUT'
        T-DREPO.CodCia = s-codcia 
        T-DREPO.CodAlm = s-codalm 
        T-DREPO.Item = x-Item
        T-DREPO.AlmPed = Almrepos.almped
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
        T-DREPO.DesStkAct = T-MATE-2.StkAct + T-MATE-2.StkComprometido
        T-DREPO.DesStkCom = T-MATE-2.StkComprometido
        T-DREPO.DesStkDis = T-MATE-2.StkAct
        T-DREPO.DesStkMax = T-MATE-2.StkMin
        T-DREPO.DesCmpTra = T-MATE-2.StkActCbd
        T-DREPO.PorcReposicion = (IF T-DREPO.CanReq <> 0 THEN (T-DREPO.CanGen / T-DREPO.CanReq * 100) ELSE 0).
    /* Datos Adicionales */
    ASSIGN
        T-DREPO.GrpStkDis = T-MATE-2.DesStkDis
        T-DREPO.FSGrupo = T-MATE-2.DesStkDis - T-MATE-2.DesStkMax + T-MATE-2.DesStkTra.
    ASSIGN
        T-DREPO.ClfGral = fClasificacion(T-DREPO.CodMat).
    ASSIGN
        x-Item = x-Item + 1
        pReposicion = pReposicion - T-DREPO.CanGen.
    IF pReposicion <= 0 THEN LEAVE.
END.
RELEASE T-DREPO.
*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Carga-Temporal) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Carga-Temporal Procedure 
PROCEDURE Carga-Temporal :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

x-Clasificaciones = REPLACE(x-Clasificaciones, 'N', 'XA,XB,XC,XD,XE,XF,NA,NB,NC,ND,NE,NF,,').

/* *********************************************************************** */
/* 1ro. CARGAMOS LOS PRODUCTOS ******************************************* */
/* Tabla: T-MATG ********************************************************* */
/* *********************************************************************** */
DEF VAR x-Linea AS CHAR NO-UNDO.
EMPTY TEMP-TABLE T-MATG.
FOR EACH Almmmatg NO-LOCK WHERE Almmmatg.codcia = s-codcia
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

/* ******************************************************************************** */
/* 01-09-2023: Separa los artículos en Master y Saldos */
/* ******************************************************************************** */
/*RUN Separa-Articulos.*/
/* ******************************************************************************** */
/* ******************************************************************************** */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Genera-OTR-por-RA) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Genera-OTR-por-RA Procedure 
PROCEDURE Genera-OTR-por-RA :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF INPUT PARAMETER pTipMov AS CHAR.
  DEF INPUT PARAMETER pNroDoc AS CHAR.
  DEF OUTPUT PARAMETER pMensaje AS CHAR NO-UNDO.
  DEF OUTPUT PARAMETER pMensaje2 AS CHAR NO-UNDO.

  DEF VAR pFechaEntrega AS DATE NO-UNDO.

  pMensaje  = ''.
  pMensaje2 = ''.
  
  DEF VAR hMaster AS HANDLE NO-UNDO.
  RUN gn/master-library PERSISTENT SET hMaster.
    
  DEF BUFFER REPOSICION FOR Almcrepo.
  FOR EACH REPOSICION EXCLUSIVE-LOCK WHERE REPOSICION.codcia = s-CodCia AND
      REPOSICION.tipmov = "A" AND
      REPOSICION.codref = pTipMov AND
      REPOSICION.nroref = pNroDoc:
      /* ********************************************************************************************* */
      /* RHC 21/02/2018 Consistencia del Stock Origen Disponible */
      /* ********************************************************************************************* */
      FIND Almacen WHERE Almacen.codcia = s-codcia AND Almacen.codalm = REPOSICION.codalm NO-LOCK.
      FIND gn-divi WHERE gn-divi.codcia = s-codcia AND gn-divi.coddiv = Almacen.coddiv NO-LOCK.
      /* *********************************************************************** */
    ASSIGN
        REPOSICION.FlgEst = "C"        /* RHC 28/11/17 CERRADO */
        REPOSICION.FchApr = TODAY
        REPOSICION.FlgSit = 'A'       /* Aprobado */
        REPOSICION.HorApr = STRING(TIME, 'HH:MM')
        REPOSICION.UsrApr = s-user-id.
    /* Ic - 14Set2016, Aprobacion y Generacion de OTR automaticamente */
    RUN genera-OTR IN hMaster ("R/A",
                               ROWID(REPOSICION),
                               NO,      /* CrossDocking */
                               "",
                               OUTPUT pFechaEntrega,
                               OUTPUT pMensaje,
                               OUTPUT pMensaje2).
    IF RETURN-VALUE = "ADM-ERROR" THEN DO:
        IF TRUE <> (pMensaje > '') THEN pMensaje = "NO se pudo generar la OTR para el doc: " +
            STRING(almcrepo.NroSer, '999') + "-" + STRING(almcrepo.NroDoc).
        UNDO, RETURN 'ADM-ERROR'.
    END.
    ASSIGN
        REPOSICION.Fecha = pFechaEntrega
        REPOSICION.CrossDocking = NO
        REPOSICION.AlmacenXD = "".
    /* 28/03/2022 Actualizar log de la HPK */
    FOR EACH Faccpedi NO-LOCK WHERE Faccpedi.codcia = s-codcia AND
        Faccpedi.coddoc = "OTR" AND
        Faccpedi.codref = "R/A" AND
        Faccpedi.nroref = STRING(REPOSICION.NroSer,'999') + STRING(REPOSICION.NroDoc,"999999") AND
        Faccpedi.flgest <> "A",
        EACH Vtacdocu EXCLUSIVE-LOCK WHERE Vtacdocu.codcia = s-codcia AND
        Vtacdocu.codped = 'HPK' AND
        Vtacdocu.codref = Faccpedi.coddoc AND       /* OTR */
        Vtacdocu.nroref = Faccpedi.nroped AND
        Vtacdocu.flgest = 'P' AND
        Vtacdocu.flgsit = 'T':
        /* ****************************************************** */
        /* 07/04/2022: Esta parte viene de /triggers/w-vtacdocu.p */
        /* ****************************************************** */
        ASSIGN
            Vtacdocu.Libre_d01 = 0      /* Importe */
            Vtacdocu.Items = 0          /* Items */
            Vtacdocu.Peso = 0           /* Peso */
            Vtacdocu.Volumen = 0.       /* Volumen */
        FOR EACH Vtaddocu OF Vtacdocu NO-LOCK,FIRST Almmmatg OF Vtaddocu NO-LOCK:
            ASSIGN
                Vtacdocu.Libre_d01 = Vtacdocu.Libre_d01 + Vtaddocu.ImpLin
                Vtacdocu.Items = Vtacdocu.Items + 1
                Vtacdocu.Peso = Vtacdocu.Peso + (Vtaddocu.CanPed * Vtaddocu.Factor * Almmmatg.PesMat)
                Vtacdocu.Volumen = Vtacdocu.Volumen + (Vtaddocu.CanPed * Vtaddocu.Factor * Almmmatg.Libre_d02 / 1000000).
        END.
        /* ****************************************************** */
        /* ****************************************************** */
        RUN gn/p-log-status-pedidos (Vtacdocu.CodPed,
                                     Vtacdocu.NroPed,
                                     'TRCKHPK',
                                     'PK_SEM',
                                     '',
                                     ?).
    END.
  END.
  DELETE PROCEDURE hMaster.
  RETURN 'OK'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GENERAR-PEDIDO) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GENERAR-PEDIDO Procedure 
PROCEDURE GENERAR-PEDIDO :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

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
              IF LOOKUP(T-DREPO.CodZona, RepAutomParam.Sector) = 0 THEN DELETE T-DREPO.
          END.
      END.
      IF NOT CAN-FIND(FIRST T-DREPO NO-LOCK) THEN DO:
          pOcurrencia = "NO hay registros generados".
          RETURN 'ADM-ERROR'.
      END.
      /* Generación */
      /* Se va a crear en almcrepo, almdrepo y repautomdetail */
      {lib/lock-genericov3.i
          &Tabla="FacCorre"
          &Alcance="FIRST"
          &Condicion="Faccorre.codcia = s-codcia ~
            AND Faccorre.coddoc = s-coddoc ~
            AND Faccorre.flgest = YES ~
            AND Faccorre.coddiv = pCodDiv"
          &Bloqueo="EXCLUSIVE-LOCK NO-ERROR"
          &Accion="RETRY"
          &Mensaje="NO"
          &txtMensaje="pOcurrencia"
          &TipoError="UNDO, RETURN 'ADM-ERROR'"
          }
      
      n-Items = 0.
      FOR EACH T-DREPO NO-LOCK,
          FIRST Almmmatg OF T-DREPO NO-LOCK,
          FIRST Almmmate OF Almmmatg WHERE Almmmate.CodAlm = s-CodAlm NO-LOCK,
          FIRST Almtfami OF Almmmatg NO-LOCK,
          FIRST AlmSFami OF Almmmatg NO-LOCK 
          BREAK BY T-DREPO.AlmPed BY Almmmatg.DesMar BY Almmmatg.DesMat
          ON ERROR UNDO, THROW:
          IF FIRST-OF(T-DREPO.AlmPed) THEN DO:
              pOcurrencia = "".
              CREATE Almcrepo.
              ASSIGN
                  almcrepo.AlmPed = T-DREPO.Almped
                  almcrepo.CodAlm = s-codalm
                  almcrepo.CodCia = s-codcia
                  almcrepo.FchDoc = LocalFechaProceso
                  almcrepo.FchVto = LocalFechaProceso + 7
                  almcrepo.Fecha = pFchEnt    /* Ic 13May2015*/
                  almcrepo.Hora = STRING(TIME, 'HH:MM')
                  almcrepo.NroDoc = Faccorre.correlativo
                  almcrepo.NroSer = Faccorre.nroser
                  almcrepo.TipMov = s-TipMov          /* OJO: Manual Automático */
                  almcrepo.Usuario = s-user-id
                  almcrepo.Glosa = RepAutomParam.Glosa
                  Almcrepo.FlgEst = "X".    /* En proceso */
              ASSIGN
                  almcrepo.VtaPuntual     = pVtaPuntual
                  almcrepo.MotReposicion  = RepAutomParam.Motivo.
              ASSIGN
                  Faccorre.correlativo = Faccorre.correlativo + 1
                  n-Items = 0.
              /* RHC 21/04/2016 Almacén de despacho CD? */
              IF CAN-FIND(FIRST TabGener WHERE TabGener.CodCia = s-codcia
                          AND TabGener.Clave = "ZG"
                          AND TabGener.Libre_c01 = Almcrepo.AlmPed    /* Almacén de Despacho */
                          AND TabGener.Libre_l01 = YES                /* CD */
                          NO-LOCK)
                  THEN Almcrepo.FlgSit = "G".   /* Por Autorizar por Abastecimientos */
              /* ************************************** */
              /* Datos de Control */
              ASSIGN
                  RepAutomParam.NroDoc = Almcrepo.nrodoc
                  RepAutomParam.NroSer = Almcrepo.nroser
                  RepAutomParam.TipMov = s-TipMov.
              ASSIGN
                  x-TotPeso  = 0
                  x-TotVolu  = 0
                  x-ImpTotal = 0
                  x-TotItems = 0.
          END.
          /* *********************************************************** */
          /* RHC 06/02/2020 Volvemos a consistenciar el stock disponible */
          /* *********************************************************** */
          /* 18/03/2022 Redundante */
/*           RUN gn/stock-comprometido-v2 (INPUT T-DREPO.CodMat,                     */
/*                                         INPUT Almcrepo.AlmPed,                    */
/*                                         NO,                                       */
/*                                         OUTPUT pComprometido).                    */
/*           FIND B-MATE WHERE B-MATE.codcia = s-codcia                              */
/*               AND B-MATE.codalm = Almcrepo.AlmPed                                 */
/*               AND B-MATE.codmat = T-DREPO.CodMat                                  */
/*               NO-LOCK NO-ERROR.                                                   */
/*           IF AVAILABLE B-MATE THEN fDisponible = (B-MATE.StkAct - pComprometido). */
/*           ELSE fDisponible = 0.00.                                                */
/*                                                                                   */
/*           IF fDisponible <= 0 OR fDisponible < T-DREPO.CanGen THEN DO:            */
/*               DELETE T-DREPO.                                                     */
/*               NEXT.                                                               */
/*           END.                                                                    */
          /* *********************************************************** */
          /* *********************************************************** */
          CREATE Almdrepo.
          BUFFER-COPY T-DREPO TO Almdrepo
              ASSIGN
              almdrepo.ITEM   = n-Items + 1
              almdrepo.CodCia = almcrepo.codcia
              almdrepo.CodAlm = almcrepo.codalm
              almdrepo.TipMov = almcrepo.tipmov
              almdrepo.NroSer = almcrepo.nroser
              almdrepo.NroDoc = almcrepo.nrodoc
              almdrepo.CanReq = almdrepo.cangen
              almdrepo.CanApro = almdrepo.cangen.
          
          /* Acumulamos Totales */
          FIND LAST Almstkge WHERE AlmStkge.CodCia = s-codcia
              AND AlmStkge.codmat = Almdrepo.codmat
              AND AlmStkge.Fecha <= LocalFechaProceso
              NO-LOCK NO-ERROR.
          IF AVAILABLE Almstkge THEN x-PreUni = AlmStkge.CtoUni.
          ELSE x-PreUni = 0.
          ASSIGN
              x-TotPeso  = x-TotPeso + Almmmatg.PesMat * Almdrepo.CanReq
              x-TotVolu  = x-TotVolu + Almmmatg.Libre_d02 * Almdrepo.CanReq / 1000000
              x-ImpTotal = x-ImpTotal + x-PreUni *  Almdrepo.CanReq
              x-TotItems = x-TotItems + 1.
          /* ****************** */
          CREATE RepAutomDetail.
          BUFFER-COPY T-DREPO TO RepAutomDetail
              ASSIGN
              RepAutomDetail.NroDoc = RepAutomParam.NroDoc
              RepAutomDetail.NroSer = RepAutomParam.NroSer
              RepAutomDetail.TipMov = RepAutomParam.TipMov.
          ASSIGN
              RepAutomDetail.FaltanteGrupo = fFaltanteGrupo().
          IF LAST-OF(T-DREPO.AlmPed) THEN DO:
              ASSIGN
                  Almcrepo.Libre_c01 = STRING(x-TotItems) + '|' +
                                        STRING(x-TotPeso) + '|' +
                                        STRING(x-TotVolu) + '|' +
                                        STRING(x-ImpTotal) + '|' +
                                        "TotItems|TotPeso|TotVolu|ImpTotal"
                  Almcrepo.FlgEst = "P".    /* Proceso Concluido */
              /* ********************************************************************************* */
              /* RHC GENERACION AUTOMATICA DE R/A Y OTR EN UN SOLO PASO */
              /* Si no se puede generar la R/A y/o la OTR NO se detiene el proceso */
              /* pMensaje almacena los documentos que se van generando o el error */
              /* ********************************************************************************* */
              IF RepAutomParam.Aprobar = YES THEN DO:
                  /* 28/02/2022 Cambia el FlgEst a "X" */
                  ASSIGN
                      Almcrepo.FlgEst = "X".        /* OJO: En proceso nuevamente */
                  CREATE REPOSICIONES.
                  BUFFER-COPY Almcrepo TO REPOSICIONES.
                  ASSIGN
                      REPOSICIONES.RowidRepAuto = ROWID(RepAutomParam).    /* OJO */
                  pOcurrencia = "PRIMERA PARTE OK".
              END.
/*               IF RepAutomParam.Aprobar = YES THEN DO:          */
/*                   PUT UNFORMATTED 'Genera RA y OTR ' NOW SKIP. */
/*                   RUN GENERAR-RA-OTR (OUTPUT pMensaje).        */
/*               END.                                             */
              /* ********************************************************************************* */
              /* ********************************************************************************* */
          END.
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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GENERAR-RA-OTR) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GENERAR-RA-OTR Procedure 
PROCEDURE GENERAR-RA-OTR :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF INPUT  PARAMETER pRowid AS ROWID.
  DEF OUTPUT PARAMETER pMensaje AS CHAR NO-UNDO.

  DEFINE VAR hProc AS HANDLE NO-UNDO.

  /* RHC 04/10/2016 CONSISTENCIA DE FECHA DE ENTREGA */
  DEF VAR pFchEnt AS DATE NO-UNDO.
  pFchEnt = TODAY + 2.

  /* OJO con la hora */
  /* LA RUTINA VA A DECIDIR SI EL CALCULO ES POR UBIGEO O POR GPS */
  DEF VAR pUbigeo AS CHAR NO-UNDO.
  DEF VAR pLongitud AS DEC NO-UNDO.
  DEF VAR pLatitud AS DEC NO-UNDO.
  DEF VAR pPeso AS DEC NO-UNDO.
  DEF VAR pNroSKU AS INT NO-UNDO.
  DEF VAR pMensaje2 AS CHAR NO-UNDO.
  DEF VAR pMensaje3 AS CHAR NO-UNDO.
  DEF VAR pCodAlm AS CHAR NO-UNDO.
  DEF VAR pTipMov AS CHAR NO-UNDO.
  DEF VAR pNroDoc AS CHAR NO-UNDO.
  DEF VAR pCuenta AS INTE NO-UNDO.

  /* 28/92/2022: Bloqueamos RepAutomDetail */
  DO TRANSACTION ON ERROR UNDO, RETURN 'ADM-ERROR' ON STOP UNDO, RETURN 'ADM-ERROR':
      {lib/lock-genericov3.i
          &Tabla="Almcrepo"
          &Alcance="FIRST"
          &Condicion="ROWID(Almcrepo) = pRowid"
          &Bloqueo="EXCLUSIVE-LOCK NO-ERROR"
          &Accion="RETRY"
          &Mensaje="NO"
          &txtMensaje="pMensaje"
          &TipoError="UNDO, RETURN 'ADM-ERROR'"
          &Intentos="10"
          }
      ASSIGN
          Almcrepo.FlgEst = "P".    /* OJO */

      FIND FIRST RepAutomParam WHERE ROWID(RepAutomParam) = REPOSICIONES.RowidRepAuto 
          EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
      IF ERROR-STATUS:ERROR = YES THEN DO:
          {lib/mensaje-de-error.i &CuentaError="pCuenta" &MensajeError="pMensaje"}
          RETURN 'ADM-ERROR'.
      END.

      RUN logis/p-datos-sede-auxiliar ("@ALM",
                                       s-CodAlm,              /* Solicitante */
                                       "",                    /* Sede */
                                       OUTPUT pUbigeo,        /* Ej 150101 */
                                       OUTPUT pLongitud,
                                       OUTPUT pLatitud).

      RUN logis/p-fecha-entrega-ubigeo.p (
          s-CodAlm,                     /* Almacén de despacho */
          TODAY,                        /* Fecha base */
          STRING(TIME,'HH:MM:SS'),      /* Hora base */
          "",                           /* Cliente */
          s-CodDiv,                     /* División solicitante */
          pUbigeo,                      /* Ubigeo: CR es cuando el cliente recoje  */
          "R/A",                        /* Documento actual */
          "",
          pNroSKU,
          pPeso,
          INPUT-OUTPUT pFchEnt,
          OUTPUT pMensaje).
      IF pMensaje > '' THEN DO:
          ASSIGN
              RepAutomParam.FechaFin = TODAY
              RepAutomParam.HoraFin = STRING(TIME, 'HH:MM:SS')
              RepAutomParam.Ocurrencia = "ERROR"
              RepAutomParam.Glosa = "ERROR R/A: " + pMensaje.
          RETURN 'ADM-ERROR'.
      END.
      /* *************************************************************************************** */
      /* 1ro Generamos la R/A */
      /* *************************************************************************************** */
      RUN alm/almacen-library PERSISTENT SET hProc.
      ASSIGN
          pCodAlm = Almcrepo.CodAlm
          pTipMov = Almcrepo.TipMov
          pNroDoc = STRING(Almcrepo.NroSer, '999') + STRING(Almcrepo.NroDoc).
      PUT NOW '    a. Genera-RA-por-RAN' SKIP.
      RUN Genera-RA-por-RAN IN hProc (INPUT pCodAlm,
                                      INPUT pTipMov,
                                      INPUT pNroDoc,
                                      INPUT pFchEnt,
                                      INPUT "",
                                      INPUT Almcrepo.VtaPuntual,
                                      INPUT Almcrepo.MotReposicion,
                                      OUTPUT pMensaje,
                                      OUTPUT pMensaje2).
      IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
          pMensaje = "ERROR R/A: " + pMensaje.
          PUT UNFORMATTED NOW ' ' pMensaje SKIP.
          ASSIGN
              RepAutomParam.FechaFin = TODAY
              RepAutomParam.HoraFin = STRING(TIME, 'HH:MM:SS')
              RepAutomParam.Ocurrencia = "ERROR"
              RepAutomParam.Glosa = pMensaje.
          RETURN 'ADM-ERROR'.
      END.
      DELETE PROCEDURE hProc.
      /* *************************************************************************************** */
      /* 2do. Generamos la OTR */
      /* *************************************************************************************** */
      PUT NOW '    b. Genera-OTR-por-RA' SKIP.
      RUN Genera-OTR-por-RA (INPUT pTipMov,
                             INPUT pNroDoc,
                             OUTPUT pMensaje,
                             OUTPUT pMensaje2).
      IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
          pMensaje = "ERROR R/A: " + pMensaje.
          PUT UNFORMATTED NOW ' ' pMensaje SKIP.
          ASSIGN
              RepAutomParam.FechaFin = TODAY
              RepAutomParam.HoraFin = STRING(TIME, 'HH:MM:SS')
              RepAutomParam.Ocurrencia = "ERROR"
              RepAutomParam.Glosa = pMensaje.
          RETURN 'ADM-ERROR'.
      END.
      pMensaje2 = pMensaje2 + ' ' + pMensaje3.
      /* *************************************************************************************** */
      ASSIGN
          RepAutomParam.FechaFin = TODAY
          RepAutomParam.HoraFin = STRING(TIME, 'HH:MM:SS')
          RepAutomParam.Ocurrencia = 'Proceso OK'.
      RELEASE RepAutomParam.
      RELEASE Almcrepo.
  END.
  RETURN 'OK'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Limpia-Base) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Limpia-Base Procedure 
PROCEDURE Limpia-Base :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FOR EACH RepAutomParam EXCLUSIVE-LOCK WHERE RepAutomParam.CodCia = s-CodCia 
    AND RepAutomParam.Dia = WEEKDAY(LocalFechaProceso)
    AND RepAutomParam.FechaInicio <> LocalFechaProceso
    ON ERROR UNDO, RETURN 'ADM-ERROR' ON STOP UNDO, RETURN 'ADM-ERROR':
    ASSIGN
        RepAutomParam.FechaFin = ?
        RepAutomParam.FechaInicio = ?
        RepAutomParam.HoraFin = ''
        RepAutomParam.HoraInicio = ''
        RepAutomParam.NroDoc = 0
        RepAutomParam.NroSer = 0
        RepAutomParam.Ocurrencia = ''
        RepAutomParam.TipMov = ''.
END.
RETURN 'OK'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-p-articulo-en-transito) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE p-articulo-en-transito Procedure 
PROCEDURE p-articulo-en-transito :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER s-codcia AS INT.
DEFINE INPUT PARAMETER s-codalm AS CHAR.
DEFINE INPUT PARAMETER s-codmat AS CHAR.
DEFINE OUTPUT PARAMETER x-Total AS DEC.
    
x-Total = 0.

FOR EACH Almcrepo NO-LOCK WHERE Almcrepo.codcia = s-codcia
    AND Almcrepo.codalm = s-codalm
    AND Almcrepo.flgest = 'P',
    FIRST Almacen NO-LOCK WHERE Almacen.codcia = s-codcia
    AND Almacen.codalm = Almcrepo.almped:
    FOR EACH Almdrepo OF Almcrepo NO-LOCK WHERE Almdrepo.codmat = s-CodMat
        AND almdrepo.CanApro > almdrepo.CanAten:
            x-Total = x-Total + (Almdrepo.CanApro - Almdrepo.CanAten).
    END.
END.

/* G/R por Transferencia en Tránsito */
FOR EACH Almdmov USE-INDEX Almd02 NO-LOCK WHERE Almdmov.codcia = s-codcia
    AND Almdmov.codmat = s-CodMat
    AND Almdmov.fchdoc >= LocalFechaProceso - 30
    AND Almdmov.tipmov = "S"
    AND Almdmov.codmov = 03,
    FIRST Almcmov OF Almdmov NO-LOCK WHERE Almcmov.flgest <> "A" 
    AND Almcmov.flgsit = "T" ,
    FIRST Almacen OF Almcmov NO-LOCK:
    IF Almcmov.CrossDocking = NO  AND Almcmov.AlmDes    <> s-CodAlm THEN NEXT.
    IF Almcmov.CrossDocking = YES AND Almcmov.AlmacenXD <> s-CodAlm THEN NEXT.

        x-Total = x-Total + Almdmov.CanDes.
END.

FOR EACH Facdpedi USE-INDEX Llave02 NO-LOCK WHERE Facdpedi.codcia = s-codcia
    AND Facdpedi.codmat = s-CodMat
    AND Facdpedi.coddoc = 'OTR'     /* Orden de Transferencia */
    AND Facdpedi.flgest = 'P':
    FIND FIRST Faccpedi OF Facdpedi NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Faccpedi THEN NEXT.
    IF Faccpedi.CrossDocking = NO AND NOT (Faccpedi.flgest = 'P' AND Faccpedi.CodCli = s-CodAlm)
        THEN NEXT.
    IF Faccpedi.CrossDocking = YES AND NOT (Faccpedi.flgest = 'P' AND Faccpedi.AlmacenXD = s-CodAlm)
        THEN NEXT.
        x-Total = x-Total + FacDPedi.Factor * (FacDPedi.CanPed - FacDPedi.CanAte).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Proceso-Principal) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proceso-Principal Procedure 
PROCEDURE Proceso-Principal :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       Ya viene B-PARAM con el puntero correcto
------------------------------------------------------------------------------*/

pMensaje = ''.
DO TRANSACTION ON ERROR UNDO, RETURN 'ADM-ERROR' ON STOP UNDO, RETURN 'ADM-ERROR':
    {lib/lock-genericov3.i ~
        &Tabla="RepAutomParam" ~
        &Alcance="FIRST" ~
        &Condicion="ROWID(RepAutomParam) = ROWID(B-PARAM)" ~
        &Boqueo="EXCLUSIVE-LOCK NO-ERROR" ~
        &Accion="RETRY" ~
        &Mensaje="NO" ~
        &txtMensaje="pMensaje" ~
        &TipoError="UNDO, RETURN 'ADM-ERROR'"}
    ASSIGN
        RepAutomParam.FechaInicio = LocalFechaProceso
        RepAutomParam.HoraInicio = STRING(TIME, 'HH:MM:SS').
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
    PUT UNFORMATTED NOW ' b. Genera Reposicion' SKIP.
    pOcurrencia = "".
    RUN GENERAR-PEDIDO.
    IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
        pMensaje = pOcurrencia.
        /* Solo se graba con el error */
        ASSIGN
            RepAutomParam.FechaFin = LocalFechaProceso
            RepAutomParam.HoraFin = STRING(TIME, 'HH:MM:SS')
            RepAutomParam.Ocurrencia = "ERROR"
            RepAutomParam.Glosa = pMensaje.
        RETURN 'ADM-ERROR'.
        /*UNDO, RETURN 'ADM-ERROR'.*/
    END.
    ASSIGN
        RepAutomParam.FechaFin = LocalFechaProceso
        RepAutomParam.HoraFin = STRING(TIME, 'HH:MM:SS')
        RepAutomParam.Ocurrencia = pOcurrencia.
END.
RETURN 'OK'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Rechaza-RAN) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Rechaza-RAN Procedure 
PROCEDURE Rechaza-RAN :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF BUFFER B-CREPO FOR Almcrepo.

FOR EACH Almcrepo NO-LOCK WHERE Almcrepo.codcia = s-codcia AND
    Almcrepo.TipMov = "RAN" AND
    Almcrepo.FlgEst = "P" AND
    Almcrepo.FchDoc < TODAY:
    {lib/lock-genericov3.i ~
        &Tabla="B-CREPO" ~
        &Alcance="FIRST" ~
        &Condicion="ROWID(B-CREPO) = ROWID(Almcrepo)" ~
        &Bloqueo="EXCLUSIVE-LOCK NO-ERROR" ~
        &Accion="RETRY" ~
        &Mensaje="NO" ~
        &TipoError="UNDO, RETURN 'ADM-ERROR'" ~
        &Intentos="15"}
    ASSIGN
        B-CREPO.FecAct = TODAY
        B-CREPO.HorAct = STRING(TIME, 'HH:MM:SS')
        B-CREPO.UsrAct = s-user-id
        B-CREPO.FlgEst = "A".
    FOR EACH almdrepo EXCLUSIVE-LOCK WHERE almdrepo.CodCia = almcrepo.codcia AND
        almdrepo.CodAlm = almcrepo.codalm AND
        almdrepo.TipMov = almcrepo.tipmov AND
        almdrepo.NroSer = almcrepo.nroser AND
        almdrepo.NroDoc = almcrepo.nrodoc:
        DELETE almdrepo.
    END.
    FOR EACH RepAutomDetail EXCLUSIVE-LOCK WHERE RepAutomDetail.CodCia = Almcrepo.codcia AND
        RepAutomDetail.CodAlm = Almcrepo.codalm AND
        RepAutomDetail.TipMov = Almcrepo.tipmov AND
        RepAutomDetail.NroSer = Almcrepo.nroser AND
        RepAutomDetail.NroDoc = Almcrepo.nrodoc:
        DELETE RepAutomDetail.
    END.
END.
IF AVAILABLE(B-CREPO) THEN RELEASE B-CREPO.
RETURN "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Separa-Articulos) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Separa-Articulos Procedure 
PROCEDURE Separa-Articulos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

EMPTY TEMP-TABLE T-DREPO-2.

FOR EACH T-DREPO:
    CREATE T-DREPO-2.
    BUFFER-COPY T-DREPO TO T-DREPO-2.
END.
EMPTY TEMP-TABLE T-DREPO.

DEF VAR x-CanGen AS DECI NO-UNDO.
DEF VAR x-CanMaster AS DECI NO-UNDO.

FOR EACH T-DREPO-2 WHERE NO-LOCK, FIRST Almmmatg OF T-DREPO-2 NO-LOCK:
    IF Almmmatg.CanEmp <= 0 OR T-DREPO-2.CanGen < Almmmatg.CanEmp THEN DO:
        CREATE T-DREPO.
        BUFFER-COPY T-DREPO-2 TO T-DREPO
            ASSIGN T-DREPO.Origen = "Saldo".
    END.
    ELSE DO:
        x-CanGen = T-DREPO-2.CanGen.
        /* Se va a fraccionar en 2 partes: Una en Master y lo que queda en Saldo */
        x-CanMaster = TRUNCATE(x-CanGen / Almmmatg.CanEmp, 0).
        /* Primero el Master */
        CREATE T-DREPO.
        BUFFER-COPY T-DREPO-2 TO T-DREPO
            ASSIGN 
            T-DREPO.CanGen = x-CanMaster * Almmmatg.CanEmp
            T-DREPO.Origen = "Master".
        x-CanGen = x-CanGen - T-DREPO.CanGen.
        /* Segundo el Saldo */
        IF x-CanGen > 0 THEN DO:
            /* Marcamos el anterior */
            T-DREPO.Sector = "*".
            CREATE T-DREPO.
            BUFFER-COPY T-DREPO-2 TO T-DREPO
                ASSIGN 
                T-DREPO.CanGen = x-CanGen
                T-DREPO.Origen = "Saldo".
            /* Marcamos el actual */
            T-DREPO.Sector = "*".
        END.
    END.
END.
EMPTY TEMP-TABLE T-DREPO-2.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-fFaltanteGrupo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fFaltanteGrupo Procedure 
FUNCTION fFaltanteGrupo RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  /* Buscamos a qué grupo pertenece */
  DEF VAR x-Grupo AS CHAR NO-UNDO.
  DEF VAR x-Acumulado AS DEC NO-UNDO.
  DEF VAR x-libre_d01 AS DEC NO-UNDO.
  DEF VAR x-libre_d02 AS DEC NO-UNDO.
  DEF VAR x-libre_d03 AS DEC NO-UNDO.
  DEF VAR x-libre_d04 AS DEC NO-UNDO.
  DEF VAR pStkComprometido AS DEC NO-UNDO.

  DEF BUFFER B-MATE FOR Almmmate.

  EMPTY TEMP-TABLE tmp-tabla.

  FIND FIRST TabGener WHERE TabGener.Clave = "ZG"
      AND TabGener.CodCia = s-codcia 
      AND TabGener.Libre_c01 = Almcrepo.CodAlm
      NO-LOCK NO-ERROR.
  IF NOT AVAILABLE TabGener THEN RETURN "".

  x-Grupo = TabGener.Codigo.
  FOR EACH TabGener NO-LOCK WHERE TabGener.CodCia = s-codcia 
      AND TabGener.Clave = "ZG"
      AND TabGener.Codigo = x-Grupo,
      FIRST B-MATE NO-LOCK WHERE B-MATE.codcia = s-codcia
      AND B-MATE.CodMat = Almdrepo.CodMat
      AND B-MATE.CodAlm = TabGener.Libre_c01:
      pStkComprometido = fStockComprometido(B-MATE.CodMat, B-MATE.CodAlm).
      x-libre_d01 = B-MATE.StkAct - pStkComprometido.
      /* En Tránsito */
      RUN alm/p-articulo-en-transito (
          B-MATE.CodCia,
          B-MATE.CodAlm,
          B-MATE.CodMat,
          INPUT-OUTPUT TABLE tmp-tabla,
          OUTPUT x-libre_d02).
      /* Compras en tránsito */
      x-Libre_d04 = 0.
      FOR EACH OOComPend WHERE OOComPend.CodAlm = Almmmate.codalm AND 
          OOComPend.CodMat = Almmmate.codmat NO-LOCK:
          x-Libre_d04 = x-Libre_d04 + (OOComPend.CanPed - OOComPend.CanAte).
      END.
      x-libre_d03 = (x-libre_d01 + x-libre_d02 + x-Libre_d04) - B-MATE.StkMin.
      x-Acumulado = x-Acumulado + x-libre_d03.
  END.
  RETURN (IF x-Acumulado >= 0 THEN "NO" ELSE "SI").

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

