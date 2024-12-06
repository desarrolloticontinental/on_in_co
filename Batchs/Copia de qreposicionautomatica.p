&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
/* Connected Databases 
          integral         PROGRESS
*/


/* Temp-Table and Buffer definitions                                    */
DEFINE BUFFER B-MATE FOR Almmmate.
DEFINE BUFFER B-MATE2 FOR Almmmate.
DEFINE BUFFER B-MATG FOR Almmmatg.
DEFINE TEMP-TABLE T-DREPO LIKE RepAutomDetail.
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

DEF VAR s-codcia AS INT INIT 001 NO-UNDO.

FIND FIRST AlmCfgGn WHERE almcfggn.codcia = s-codcia NO-LOCK NO-ERROR.
IF NOT AVAILABLE AlmCfgGn THEN RETURN ERROR.


DEF VAR s-Reposicion AS LOG.     /* Campa�a Yes, No Campa�a No */
DEF VAR s-CodAlm AS CHAR NO-UNDO.
DEF VAR s-CodDiv AS CHAR NO-UNDO.

DEFINE TEMP-TABLE tmp-tabla
    FIELD t-CodAlm LIKE Almacen.codalm  FORMAT 'x(3)'
    FIELD t-CodDoc LIKE FacDPedi.CodDoc FORMAT "XXX"
    FIELD t-Nroped LIKE FacDPedi.NroPed FORMAT "XXX-XXXXXXXX"
    FIELD t-CodDiv LIKE FacCPedi.CodDiv FORMAT 'x(5)'
    FIELD t-FchPed LIKE FacDPedi.FchPed
    FIELD t-NomCli LIKE FacCPedi.NomCli COLUMN-LABEL "Cliente" FORMAT "x(35)"
    FIELD t-CodMat LIKE FacDPedi.codmat
    FIELD t-Canped LIKE FacDPedi.CanPed.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
   Temp-Tables and Buffers:
      TABLE: B-MATE B "?" ? INTEGRAL Almmmate
      TABLE: B-MATE2 B "?" ? INTEGRAL Almmmate
      TABLE: B-MATG B "?" ? INTEGRAL Almmmatg
      TABLE: T-DREPO T "?" ? INTEGRAL RepAutomDetail
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
         HEIGHT             = 6.92
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
/* OJO: SE VAN A CREAR PRE REPOSICIONES AUTOMATICAS */
DEF VAR s-TipMov AS CHAR INIT "RAN" NO-UNDO.
DEF VAR s-coddoc AS CHAR INIT 'RAN' NO-UNDO.    /* Reposici�n Autom�tica Nocturna */
DEF VAR s-User-Id AS CHAR INIT "ADMIN" NO-UNDO.

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

DEF VAR pMensaje AS CHAR NO-UNDO.
DEF VAR pFchEnt AS DATE NO-UNDO.
DEF VAR pCodDiv AS CHAR NO-UNDO.
DEF VAR pVtaPuntual AS LOG INIT NO NO-UNDO.

txtFechaEntrega  = TODAY + 2.

FOR EACH RepAutomParam EXCLUSIVE-LOCK WHERE RepAutomParam.CodCia = s-CodCia
    AND RepAutomParam.Dia = WEEKDAY(TODAY):
    s-CodAlm = RepAutomParam.CodAlm.
    FIND Almacen WHERE Almacen.codcia = s-codcia AND Almacen.codalm = s-codalm NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Almacen THEN RETURN ERROR.
    s-CodDiv = Almacen.CodDiv.
    pCodDiv  = Almacen.CodDiv.
    RUN gn/fAlmPrincipal (INPUT s-CodAlm, OUTPUT pOk).
    IF pOk = YES THEN s-TipoCalculo = "GRUPO".
    ELSE s-TipoCalculo = "TIENDA".
    /* 1ro. Cargamos la reposici�n autom�tica */
    x-Clasificaciones = ''.
    j = 0.
    DO k = 1 TO NUM-ENTRIES(RepAutomParam.Clasificacion):
        j = j + 1.
        x-Clasificaciones = x-Clasificaciones + (IF j = 1 THEN '' ELSE ',') +
            ENTRY(k, RepAutomParam.Clasificacion).
    END.

    RUN Carga-Temporal.

    RUN GENERAR-PEDIDO.

END.
RETURN.

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
/* RHC 05/06/2014 Cambio de filosof�a, ahora es el % del stock m�nimo */
IF x-StkAct >= (x-StockMaximo * RepAutomParam.PorRep / 100) THEN RETURN.    /* NO VA */

/* Se va a reponer en cantidades m�ltiplo del valor T-MATE.StkMax (EMPAQUE) */
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
FOR EACH Almrepos NO-LOCK WHERE Almrepos.CodCia = s-codcia
    AND Almrepos.CodAlm = T-MATE.codalm 
    AND Almrepos.AlmPed > ''
    AND Almrepos.AlmPed <> T-MATE.codalm
    AND Almrepos.TipMat = x-TipMat      /* Propios o Terceros */
    AND pReposicion > 0 /* <<< OJO <<< */
    BY Almrepos.Orden:
    FIND FIRST Almacen WHERE Almacen.codcia = Almrepos.codcia AND Almacen.codalm = Almrepos.almped
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Almacen THEN NEXT.

    /* CARGAMOS T-MATE-2 CON EL RESUMEN POR CD O SOLO EL DE LA TIENDA */
    RUN RESUMEN-POR-DESPACHO (Almrepos.almped, T-MATE.codmat).
    FIND FIRST T-MATE-2 WHERE T-MATE-2.codcia = s-codcia
        AND T-MATE-2.codalm = Almrepos.AlmPed
        AND T-MATE-2.codmat = T-MATE.CodMat NO-LOCK NO-ERROR.
    IF NOT AVAILABLE T-MATE-2 THEN NEXT.

    /* Incluye Transferencias en Tr�nsito y Compras en Tr�nsito */
    x-StockDisponible = T-MATE-2.StkAct - T-MATE-2.StkMin + T-MATE-2.StkRep + T-MATE-2.StkActCbd.
    IF x-StockDisponible <= 0 THEN NEXT.
    /* Se solicitar� la reposici�n de acuerdo al empaque del producto */
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
    /* Del Almac�n Solicitante */
    ASSIGN                 
        T-DREPO.SolStkAct = T-MATE.StkAct + T-MATE.StkComprometido
        T-DREPO.SolStkCom = T-MATE.StkComprometido
        T-DREPO.SolStkDis = T-MATE.StkAct
        T-DREPO.SolStkMax = T-MATE.StkMin
        T-DREPO.SolStkTra = T-MATE.StkRep
        T-DREPO.SolCmpTra = T-MATE.StkActCbd
        .
    /* Del Almac�n de Despacho */
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

END PROCEDURE.

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
    /* Filtro por Clasificaci�n */
    FIND FIRST FacTabla WHERE FacTabla.codcia = Almmmatg.codcia 
        AND FacTabla.tabla = 'RANKVTA' 
        AND FacTabla.codigo = Almmmatg.codmat
        NO-LOCK NO-ERROR.
    IF AVAILABLE FacTabla THEN DO:
        IF s-Reposicion = YES /* Campa�a */ THEN DO:
            IF RepAutomParam.Tipo = "General" AND LOOKUP(factabla.campo-c[1],x-Clasificaciones) = 0 THEN NEXT.
            IF RepAutomParam.Tipo = "Utilex" AND LOOKUP(factabla.campo-c[2],x-Clasificaciones) = 0 THEN NEXT.
            IF RepAutomParam.Tipo = "Mayorista" AND LOOKUP(factabla.campo-c[3],x-Clasificaciones) = 0 THEN NEXT.
        END.
        ELSE DO:
            /* No Campa�a */
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
FOR EACH T-MATG NO-LOCK:
    /* ******************************************************************************** */
    /* RHC 08/05/2017 SE VA A GENERAR UNA TABLA CON LA INFORMACION DE ALMMMATE PERO CON 
        LOS VALORES DEL CD (EJ.ATE) SI FUERA EL CASO */
    /* ******************************************************************************** */
    RUN ARTICULOS-A-SOLICITAR (INPUT T-MATG.CodMat).
    IF RETURN-VALUE = 'ADM-ERROR' THEN RETURN.
END.
/* Depuraci�n final */
FOR EACH T-MATE, FIRST B-MATE OF T-MATE NO-LOCK:
    /* RHC 04/0/17 Primero lo afectamos por el "% de Stock Maximo" */
    T-MATE.StkMin = T-MATE.StkMin * RepAutomParam.PorStkMax / 100.
    T-MATE.StkMax = B-MATE.StkMax.    /* Empaque */
    IF T-MATE.StkMin - (T-MATE.StkAct + T-MATE.StkRep + T-MATE.StkActCbd) <= 0 THEN DELETE T-MATE.
END.
/* ******************************************************************************** */
/* ******************************************************************************** */
/* ******************************************************************************** */
/* CARGAMOS LA CANTIDAD A REPONER ************************************************* */
/* ******************************************************************************** */
EMPTY TEMP-TABLE T-DREPO.
FOR EACH T-MATG NO-LOCK, 
    EACH T-MATE OF T-MATG NO-LOCK:
    RUN CARGA-REPOSICION.
END.

FOR EACH T-DREPO WHERE T-DREPO.canreq <= 0 AND LOOKUP(T-DREPO.AlmPed, '997,998') = 0:
    DELETE T-DREPO.
END.
FOR EACH T-DREPO, FIRST B-MATG OF T-DREPO NO-LOCK:
    RUN DATOS-FINALES.
END.
/* ******************************************************************************** */
/* ******************************************************************************** */
/* RHC 02/08/2016 Datos adicionales */
DEF VAR x-Total AS DEC NO-UNDO.
DEF VAR x-Item  AS INT INIT 1 NO-UNDO.
DEF VAR pComprometido AS DEC NO-UNDO.

FOR EACH T-DREPO BY T-DREPO.CodMat:
    T-DREPO.ITEM    = x-Item.
    x-Item = x-Item + 1.
END.


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

  /* RHC 04/10/2016 CONSISTENCIA DE FECHA DE ENTREGA */
  pMensaje = "".
  pFchEnt = txtFechaEntrega.

  /* OJO con la hora */
  RUN gn/p-fchent (TODAY, STRING(TIME,'HH:MM:SS'), pFchEnt, s-CodDiv, s-CodAlm, OUTPUT pMensaje).
  IF pMensaje > '' THEN RETURN ERROR.

  IF txtFechaEntrega < TODAY THEN RETURN ERROR.

  IF RepAutomParam.Motivo = "Seleccione un motivo" THEN RETURN ERROR.

  DEF VAR n-Items AS INT NO-UNDO.
  DEF VAR k AS INT NO-UNDO.
  DEF BUFFER B-MATG FOR Almmmatg.

  DO TRANSACTION ON ERROR UNDO, RETURN "ADM-ERROR" ON STOP UNDO, RETURN "ADM-ERROR":
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
          &txtMensaje="pMensaje"
          &TipoError="UNDO, RETURN 'ADM-ERROR'"
          }
      /* Depuramos el temporal */
      FOR EACH T-DREPO WHERE T-DREPO.AlmPed = '998':
          DELETE T-DREPO.
      END.
      FOR EACH T-DREPO WHERE T-DREPO.AlmPed <> RepAutomParam.AlmPed:
          DELETE T-DREPO.
      END.
      IF RepAutomParam.Sector > '' THEN DO:
          FOR EACH T-DREPO NO-LOCK,
              FIRST Almmmate NO-LOCK WHERE Almmmate.codcia = s-codcia
              AND Almmmate.codmat = T-DREPO.codmat 
              AND Almmmate.codalm = T-DREPO.almped:
              IF LOOKUP(Almmmate.CodUbi, RepAutomParam.Sector) = 0 THEN DELETE T-DREPO.
          END.
      END.

      n-Items = 0.
      FOR EACH T-DREPO NO-LOCK,
          FIRST Almmmatg OF T-DREPO NO-LOCK,
          FIRST Almmmate OF Almmmatg WHERE Almmmate.CodAlm = s-CodAlm NO-LOCK,
          FIRST Almtfami OF Almmmatg NO-LOCK,
          FIRST AlmSFami OF Almmmatg NO-LOCK 
          BREAK BY T-DREPO.AlmPed BY Almmmatg.DesMar BY Almmmatg.DesMat:
          IF FIRST-OF(T-DREPO.AlmPed) THEN DO:
              CREATE Almcrepo.
              ASSIGN
                  almcrepo.AlmPed = T-DREPO.Almped
                  almcrepo.CodAlm = s-codalm
                  almcrepo.CodCia = s-codcia
                  almcrepo.FchDoc = TODAY
                  almcrepo.FchVto = TODAY + 7
                  almcrepo.Fecha = pFchEnt    /* Ic 13May2015*/
                  almcrepo.Hora = STRING(TIME, 'HH:MM')
                  almcrepo.NroDoc = Faccorre.correlativo
                  almcrepo.NroSer = Faccorre.nroser
                  almcrepo.TipMov = s-TipMov          /* OJO: Manual Autom�tico */
                  almcrepo.Usuario = s-user-id
                  almcrepo.Glosa = RepAutomParam.Glosa.
              ASSIGN
                  almcrepo.VtaPuntual     = pVtaPuntual
                  almcrepo.MotReposicion  = RepAutomParam.Motivo.
              ASSIGN
                  Faccorre.correlativo = Faccorre.correlativo + 1
                  n-Items = 0.
              /* RHC 21/04/2016 Almac�n de despacho CD? */
              IF CAN-FIND(FIRST TabGener WHERE TabGener.CodCia = s-codcia
                          AND TabGener.Clave = "ZG"
                          AND TabGener.Libre_c01 = Almcrepo.AlmPed    /* Almac�n de Despacho */
                          AND TabGener.Libre_l01 = YES                /* CD */
                          NO-LOCK)
                  THEN Almcrepo.FlgSit = "G".   /* Por Autorizar por Abastecimientos */
              /* ************************************** */
              /* Datos de Control */
              ASSIGN
                  RepAutomParam.NroDoc = Almcrepo.nrodoc
                  RepAutomParam.NroSer = Almcrepo.nroser
                  RepAutomParam.TipMov = s-TipMov.
          END.
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
          DELETE T-DREPO.
          n-Items = n-Items + 1.
      END.
  END.
  RELEASE Faccorre.
  RELEASE almcrepo.
  RELEASE almdrepo.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

