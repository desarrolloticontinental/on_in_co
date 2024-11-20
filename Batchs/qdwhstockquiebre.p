&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       : Optimizado
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEF NEW SHARED VAR s-codcia AS INT INIT 001.
DEF NEW SHARED VAR pv-codcia AS INT INIT 000.

DEF VAR s-clave AS CHAR INIT 'ALM_QUIEBRE' NO-UNDO.
DEF VAR s-codigo AS CHAR INIT "" NO-UNDO.
DEF VAR fInicio AS DATETIME NO-UNDO.

/* Buscamos parámetros */
FIND FIRST TabGener WHERE TabGener.CodCia = s-codcia
    AND TabGener.Clave = s-clave
    AND TabGener.Codigo = s-codigo
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE TabGener THEN QUIT.

DEF BUFFER B-MATE FOR Almmmate.
DEFINE BUFFER ix-almmmate FOR almmmate.

DISABLE TRIGGERS FOR LOAD OF TabGener.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-fResumen) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fResumen Procedure 
FUNCTION fResumen RETURNS CHARACTER
  ( INPUT pCodMat AS CHAR, INPUT pCodAlm AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fStock-disponible) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fStock-disponible Procedure 
FUNCTION fStock-disponible RETURNS DECIMAL
  ( INPUT pCodMat AS CHAR, INPUT pCodAlm AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fstock-maximo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fstock-maximo Procedure 
FUNCTION fstock-maximo RETURNS DECIMAL
  ( INPUT pCodMat AS CHAR, INPUT pCodAlm AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fstock-seguridad) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fstock-seguridad Procedure 
FUNCTION fstock-seguridad RETURNS DECIMAL
  ( INPUT pCodMat AS CHAR, INPUT pCodAlm AS CHAR)  FORWARD.

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
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 10.88
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
DEF VAR txtAlmacenes AS CHAR NO-UNDO.
DEF VAR cboProveedor AS CHAR NO-UNDO.
DEF VAR txtEvaluacion AS DEC NO-UNDO.

ASSIGN 
    txtAlmacenes = TabGener.Libre_c01 
    cboProveedor = TabGener.Libre_c04 
    txtEvaluacion = TabGener.Libre_d01
    fInicio = NOW.
IF TRIM(txtAlmacenes) = "" THEN QUIT.
IF txtEvaluacion <= 0 THEN QUIT.

PUT UNFORMATTED 'Inicio del proceso ' fInicio SKIP.
RUN Borra-Registros.
PUT 'Data borrada ' NOW SKIP.
RUN ue-procesar.
PUT UNFORMATTED 'Fin del proceso ' NOW SKIP.

QUIT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-Borra-Registros) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Borra-Registros Procedure 
PROCEDURE Borra-Registros :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FOR EACH Almacen_Quiebre EXCLUSIVE-LOCK:
    DELETE Almacen_Quiebre.
END.
RELEASE Almacen_Quiebre.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-graba-registro) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE graba-registro Procedure 
PROCEDURE graba-registro :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VAR lxStkReservado AS DEC.
DEFINE VAR lxTransfxrecep AS DEC.
DEFINE VAR lxLinea AS CHAR.
DEFINE VAR lxSubLinea AS CHAR.
DEFINE VAR lxMarca AS CHAR.
DEFINE VAR lxCmpTra AS DEC.

DEFINE VAR lxStkDisponible AS DEC.
DEFINE VAR lxStkevaluacion AS DEC.

DEFINE VAR lVtas15Dias AS DEC.
DEFINE VAR lVtas30Dias AS DEC.
DEFINE VAR lVtas45Dias AS DEC.
DEFINE VAR lVtas60Dias AS DEC.
DEFINE VAR lVtas90Dias AS DEC.

DEFINE VAR lFechaDesde AS DATE.
DEFINE VAR lFechaHasta AS DATE.

DEFINE VAR pResumen AS CHAR NO-UNDO.

FIND FIRST gn-prov WHERE gn-prov.codcia = pv-codcia
    AND gn-prov.codpro = almmmatg.codpr1 NO-LOCK NO-ERROR.

/*lxStkReservado = B-MATE.StkComprometido.*/
lxStkReservado = 0.                                                                      
RUN gn/stock-comprometido-v2 (INPUT B-MATE.codmat, INPUT B-MATE.codalm, NO, OUTPUT lxStkReservado). 

lxTransfxRecep = 0.
RUN pedidoreposicionentransito (INPUT B-MATE.codmat, INPUT B-MATE.codalm, OUTPUT lxTransfxRecep).

lxStkDisponible = B-MATE.stkact - lxStkReservado.
lxStkEvaluacion = (B-MATE.stkmin * txtEvaluacion ) / 100.

/* Ranking */
FIND FIRST factabla WHERE factabla.codcia = s-codcia 
    AND factabla.tabla = 'RANKVTA' 
    AND factabla.codigo = B-MATE.codmat
    NO-LOCK NO-ERROR.
/*
    23Ago2017, Max Ramos, correo del 21Ago2017
    IF B-MATE.stkmin > 0 AND lxStkDisponible <= lxStkEvaluacion THEN DO:
*/
/*IF B-MATE.VInMn1 > 0 OR B-MATE.stkact <> 0 THEN DO:*/
IF B-MATE.StockMax > 0 OR B-MATE.stkact <> 0 THEN DO:
CREATE Almacen_Quiebre.
    ASSIGN  
        Almacen_Quiebre.codalm    = B-MATE.codalm
        Almacen_Quiebre.desalm    = almacen.descripcion
        Almacen_Quiebre.proveedor = IF (AVAILABLE gn-prov) THEN gn-prov.nompro ELSE ""
        Almacen_Quiebre.linea     = almmmatg.codfam + " " + almtfam.desfam
        Almacen_Quiebre.sublinea  = almmmatg.subfam + " " + almsfam.dessub                     
        Almacen_Quiebre.codmat    = B-MATE.codmat
        Almacen_Quiebre.desmat    = almmmatg.desmat
        Almacen_Quiebre.marca     = almmmatg.desmar
        Almacen_Quiebre.umed      = almmmatg.undstk
        Almacen_Quiebre.costrepo  = almmmatg.ctolis * (IF(almmmatg.monvta = 2) THEN almmmatg.tpocmb ELSE 1)
        Almacen_Quiebre.stkmax    = B-MATE.StockMax
        Almacen_Quiebre.stkseg    = B-MATE.StockSeg
/*         Almacen_Quiebre.stkmax    = B-MATE.VinMn1 */
/*         Almacen_Quiebre.stkseg    = B-MATE.VinMn2 */
        Almacen_Quiebre.emprep    = B-MATE.stkmax
        Almacen_Quiebre.stkfisico = B-MATE.stkact
        Almacen_Quiebre.stkreser  = lxStkReservado
        Almacen_Quiebre.stkdispo  = lxStkDisponible
        Almacen_Quiebre.stktrftra = lxTransfxRecep        
        Almacen_Quiebre.vstkmax   = B-MATE.stkmin * Almacen_Quiebre.costrepo
        Almacen_Quiebre.vsfisico  = B-MATE.stkact * Almacen_Quiebre.costrepo
        Almacen_Quiebre.vsdispo   = lxStkDisponible * Almacen_Quiebre.costrepo
        Almacen_Quiebre.vstrans   = lxTransfxRecep * Almacen_Quiebre.costrepo.

        almacen_quiebre.cmptra = 0.

    /* 11Jul2017, Max pidio que saquen esta restriccion */
    /*IF Almacen_Quiebre.costfalta < 0 THEN Almacen_Quiebre.costfalta = 0.*/
    pResumen = fResumen(B-MATE.codmat,"11").
    Almacen_Quiebre.stkalm[1]     = DECIMAL(ENTRY(1,pResumen,'|')).
    Almacen_Quiebre.stkalmmax[1]  = DECIMAL(ENTRY(2,pResumen,'|')).
    Almacen_Quiebre.stkalmseg[1]  = DECIMAL(ENTRY(3,pResumen,'|')).
    pResumen = fResumen(B-MATE.codmat,"35").
    Almacen_Quiebre.stkalm[2]     = DECIMAL(ENTRY(1,pResumen,'|')).
    Almacen_Quiebre.stkalmmax[2]  = DECIMAL(ENTRY(2,pResumen,'|')).
    Almacen_Quiebre.stkalmseg[2]  = DECIMAL(ENTRY(3,pResumen,'|')).
    pResumen = fResumen(B-MATE.codmat,"14").
    Almacen_Quiebre.stkalm[3]     = DECIMAL(ENTRY(1,pResumen,'|')).
    Almacen_Quiebre.stkalmmax[3]  = DECIMAL(ENTRY(2,pResumen,'|')).
    Almacen_Quiebre.stkalmseg[3]  = DECIMAL(ENTRY(3,pResumen,'|')).
    pResumen = fResumen(B-MATE.codmat,"14f").
    Almacen_Quiebre.stkalm[4]     = DECIMAL(ENTRY(1,pResumen,'|')).
    Almacen_Quiebre.stkalmmax[4]  = DECIMAL(ENTRY(2,pResumen,'|')).
    Almacen_Quiebre.stkalmseg[4]  = DECIMAL(ENTRY(3,pResumen,'|')).
    pResumen = fResumen(B-MATE.codmat,"21").
    Almacen_Quiebre.stkalm[5]     = DECIMAL(ENTRY(1,pResumen,'|')).
    Almacen_Quiebre.stkalmmax[5]  = DECIMAL(ENTRY(2,pResumen,'|')).
    Almacen_Quiebre.stkalmseg[5]  = DECIMAL(ENTRY(3,pResumen,'|')).

    /* Compras x llegar */
    lxCmpTra = 0.
    RUN ue-compraspendientes(INPUT B-MATE.codalm, INPUT almmmatg.codmat,
                             OUTPUT lxCmpTra).

    almacen_quiebre.cmptra = lxCmpTra.
    Almacen_Quiebre.stkfalta  = Almacen_Quiebre.stkmax - Almacen_Quiebre.stkdispo -
                                Almacen_Quiebre.stktrftra - almacen_quiebre.cmptra.
    ASSIGN
        Almacen_Quiebre.costfalta = Almacen_Quiebre.stkfalta * Almacen_Quiebre.costrepo.

    /* Año ACTUAL */
    lFechaDesde = TODAY - 45.
    lFechaHasta = TODAY.
    RUN ue-ventas(INPUT B-MATE.codalm, INPUT almmmatg.codmat,
                  INPUT lFechaDesde, INPUT lFechaHasta,
                  OUTPUT lVtas15Dias, OUTPUT lVtas30Dias,
                  OUTPUT lVtas45Dias, OUTPUT lVtas60Dias, OUTPUT lVtas90Dias).
    ASSIGN
        Almacen_Quiebre.vta15dant = lVtas15Dias
        Almacen_Quiebre.vta30dant = lVtas30Dias
        Almacen_Quiebre.vta45dant = lVtas45Dias.

    /* Año ANTERIOR, hacia atras */
    lFechaHasta = DATE(MONTH(TODAY),DAY(TODAY),YEAR(TODAY) - 1).
    lFechaDesde = lFechaHasta - 30.
    RUN ue-ventas(INPUT B-MATE.codalm, INPUT almmmatg.codmat,
                  INPUT lFechaDesde, INPUT lFechaHasta,
                  OUTPUT lVtas15Dias, OUTPUT lVtas30Dias,
                  OUTPUT lVtas45Dias, OUTPUT lVtas60Dias, OUTPUT lVtas90Dias).
    ASSIGN Almacen_Quiebre.v30daante = lVtas30Dias.

    ASSIGN Almacen_Quiebre.fv30_max    = if(Almacen_Quiebre.stkmax > 0) THEN Almacen_Quiebre.vta30dant / Almacen_Quiebre.stkmax ELSE 0.
    ASSIGN Almacen_Quiebre.fv30p_max    = if(Almacen_Quiebre.stkmax > 0) THEN Almacen_Quiebre.v30daante / Almacen_Quiebre.stkmax ELSE 0.

    /* Año Anterior, hacia adelante */
    lFechaDesde = DATE(MONTH(TODAY),DAY(TODAY),YEAR(TODAY) - 1).
    lFechaHasta = lFechaDesde + 90.

    RUN ue-ventas(INPUT B-MATE.codalm, INPUT almmmatg.codmat,
                    INPUT lFechaDesde, INPUT lFechaHasta,
                    OUTPUT lVtas15Dias, OUTPUT lVtas30Dias,
                    OUTPUT lVtas45Dias, OUTPUT lVtas60Dias, OUTPUT lVtas90Dias).
    
    ASSIGN Almacen_Quiebre.vta30dap = lVtas30Dias
            Almacen_Quiebre.vta60dap = lVtas60Dias
            Almacen_Quiebre.vta90dap = lVtas90Dias.
    
    IF AVAILABLE factabla THEN DO:
        ASSIGN
            Almacen_Quiebre.cgrlc    = factabla.campo-c[1]
            Almacen_Quiebre.cmayc    = factabla.campo-c[3]
            Almacen_Quiebre.cutxc    = factabla.campo-c[2]
            Almacen_Quiebre.cgrlnc   = factabla.campo-c[4]
            Almacen_Quiebre.cmaync   = factabla.campo-c[6]
            Almacen_Quiebre.cutxnc   = factabla.campo-c[5].
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pedidoreposicionentransito) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pedidoreposicionentransito Procedure 
PROCEDURE pedidoreposicionentransito :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

{alm/i-pedidoreposicionentransito.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-stock-comprometido-v2) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE stock-comprometido-v2 Procedure 
PROCEDURE stock-comprometido-v2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:      Como se procesa en la noche se toma el valor como viene 
------------------------------------------------------------------------------*/

DEF INPUT PARAMETER pCodMat AS CHAR.
DEF INPUT PARAMETER pCodAlm AS CHAR.
DEF INPUT PARAMETER pContado AS LOG.
DEF OUTPUT PARAMETER pComprometido AS DEC.

pComprometido = 0.  /* Valor por defecto */

/* CALCULO DEL STOCK COMPROMETIDO */
FIND FacCfgGn WHERE faccfggn.codcia = s-codcia NO-LOCK NO-ERROR.
IF NOT AVAILABLE Faccfggn THEN RETURN.

/* Buffers de trabajo */
DEF BUFFER B-DPEDI FOR Facdpedi.
DEF BUFFER B-CPEDI FOR Faccpedi.
DEF BUFFER B-CREPO FOR Almcrepo.
DEF BUFFER B-DREPO FOR Almdrepo.

/* Stock comprometido por PEDIDOS ACTIVOS MOSTRADOR */
DEF VAR TimeOut AS INTEGER NO-UNDO.
DEF VAR TimeNow AS INTEGER NO-UNDO.

IF pContado = YES THEN DO:
    /* Tiempo por defecto fuera de campaña */
    TimeOut = (FacCfgGn.Dias-Res * 24 * 3600) +
              (FacCfgGn.Hora-Res * 3600) + 
              (FacCfgGn.Minu-Res * 60).
    FOR EACH B-DPEDI USE-INDEX Llave04 NO-LOCK WHERE B-DPEDI.codcia = s-CodCia
        AND B-DPEDI.almdes = pCodAlm
        AND B-DPEDI.codmat = pCodMat
        AND B-DPEDI.coddoc = 'P/M'
        AND B-DPEDI.flgest = 'P',
        FIRST B-CPEDI OF B-DPEDI NO-LOCK WHERE B-CPEDI.FlgEst = "P":
        TimeNow = (TODAY - B-CPEDI.FchPed) * 24 * 3600.
        TimeNow = TimeNow + TIME - ( (INTEGER(SUBSTRING(B-CPEDI.Hora, 1, 2)) * 3600) +
                  (INTEGER(SUBSTRING(B-CPEDI.Hora, 4, 2)) * 60) ).
        IF TimeOut > 0 THEN DO:
            IF TimeNow <= TimeOut   /* Dentro de la valides */
            THEN DO:
                /* cantidad en reservacion */
                pComprometido = pComprometido + B-DPEDI.Factor * B-DPEDI.CanPed.
            END.
        END.
    END.
END.
/**********   Barremos para los PEDIDOS AL CREDITO   ***********************/ 
FOR EACH B-DPEDI USE-INDEX Llave04 NO-LOCK WHERE B-DPEDI.codcia = s-CodCia
    AND B-DPEDI.almdes = pCodAlm
    AND B-DPEDI.codmat = pCodMat
    AND B-DPEDI.coddoc = 'PED'
    AND B-DPEDI.flgest = 'P',
    FIRST B-CPEDI OF B-DPEDI NO-LOCK WHERE LOOKUP(B-CPEDI.FlgEst, "G,X,P,W,WX,WL") > 0:
    pComprometido = pComprometido + B-DPEDI.Factor * (B-DPEDI.CanPed - B-DPEDI.canate).
END.
/* ORDENES DE DESPACHO CREDITO */
FOR EACH B-DPEDI USE-INDEX Llave04 NO-LOCK WHERE B-DPEDI.codcia = s-CodCia
    AND B-DPEDI.almdes = pCodAlm
    AND B-DPEDI.codmat = pCodMat
    AND B-DPEDI.coddoc = 'O/D'
    AND LOOKUP(B-DPEDI.flgest, 'WL,P') > 0, /* Aprobadas y por Aprobar */
    FIRST B-CPEDI OF B-DPEDI NO-LOCK WHERE B-CPEDI.flgest = 'P':
    pComprometido = pComprometido + B-DPEDI.Factor * (B-DPEDI.CanPed - B-DPEDI.canate).
END.
/* Stock Comprometido por Pedidos por Reposicion Automatica */
/* OJO ver tambien el programa vtamay/c-conped.w */
FOR EACH B-DREPO USE-INDEX Llave03 NO-LOCK WHERE B-DREPO.codcia = s-CodCia
    AND B-DREPO.codmat = pCodMat
    AND B-DREPO.CanApro > B-DREPO.CanAten,
    FIRST B-CREPO OF B-DREPO NO-LOCK WHERE B-CREPO.AlmPed = pCodAlm
    AND B-CREPO.FlgEst = 'P':
    pComprometido = pComprometido + (B-DREPO.CanApro - B-DREPO.CanAten).
END.
/* POR ORDENES DE TRANSFERENCIA */
FOR EACH B-DPEDI USE-INDEX Llave04 NO-LOCK WHERE B-DPEDI.codcia = s-CodCia
    AND B-DPEDI.almdes = pCodAlm
    AND B-DPEDI.codmat = pCodMat
    AND B-DPEDI.coddoc = 'OTR'
    AND B-DPEDI.flgest = 'P',
    FIRST B-CPEDI OF B-DPEDI NO-LOCK WHERE B-CPEDI.flgest = 'P':
    pComprometido = pComprometido + B-DPEDI.Factor * (B-DPEDI.CanPed - B-DPEDI.CanAte).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ue-compraspendientes) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ue-compraspendientes Procedure 
PROCEDURE ue-compraspendientes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER pCodAlm AS CHAR.
DEFINE INPUT PARAMETER pCodmat AS CHAR.
DEFINE OUTPUT PARAMETER pCantidad AS DEC.

pCantidad = 0.

FOR EACH OOComPend WHERE OOComPend.codalm = pCodAlm AND 
                        OOComPend.codmat = pCodmat NO-LOCK:
    pCantidad = pCantidad + (OOComPend.canped - OOComPend.canate).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ue-procesar) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ue-procesar Procedure 
PROCEDURE ue-procesar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE VAR lxProveedor AS CHAR NO-UNDO.
DEFINE VAR lxSec AS INT NO-UNDO.
DEFINE VAR lxAlmacen AS CHAR NO-UNDO.

DEFINE VAR lSeleRow AS INT.
DEFINE VAR lFams AS CHAR INIT "".
DEFINE VAR lMarcas AS CHAR INIT "".

lxProveedor = cboProveedor.
IF TRUE <> (lxProveedor > '') THEN lxProveedor = "Todos".
lFams = TabGener.Libre_c02.
lMarcas = TabGener.Libre_c03.

DO lxSec = 1 TO NUM-ENTRIES(txtAlmacenes,","):
    lxAlmacen = ENTRY(lxSec,txtAlmacenes,",").
    PUT UNFORMATTED 'Almacen ' lxAlmacen ' ' NOW SKIP.
    FOR EACH almmmatg WHERE almmmatg.codcia = s-codcia
        AND almmmatg.tpoart <> 'D' 
        AND (TRUE <> (lFams > '') OR LOOKUP(almmmatg.codfam,lFams) > 0 )
        AND (TRUE <> (lMarcas > '') OR  LOOKUP(almmmatg.codmar,lMarcas) > 0 )
        AND (lxProveedor = 'Todos' OR almmmatg.codpr1 = lxProveedor) NO-LOCK,
        FIRST B-MATE OF almmmatg WHERE B-MATE.codalm = lxAlmacen NO-LOCK,
        FIRST almtfam OF almmmatg NO-LOCK,
        FIRST almsfam OF almmmatg NO-LOCK,
        FIRST almacen OF B-MATE NO-LOCK :
        RUN graba-registro.
    END.
END.
DEF VAR iIntentos AS INT INIT 0 NO-UNDO.
REPEAT TRANSACTION:
    FIND CURRENT TabGener EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
    IF AVAILABLE TabGener THEN DO:
        ASSIGN
            TabGener.LlaveIni = STRING(fInicio)
            TabGener.LlaveFin = STRING(NOW).
        RELEASE TabGener.
        LEAVE.
    END.
    iIntentos = iIntentos + 1.
    IF iIntentos > 5 THEN DO:
        PUT UNFORMATTED 'NO se pudo grabar la fecha inicio y fin' SKIP.
        LEAVE.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ue-ventas) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ue-ventas Procedure 
PROCEDURE ue-ventas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER pCodAlm AS CHAR.
DEFINE INPUT PARAMETER pCodMat AS CHAR.
DEFINE INPUT PARAMETER pFechaDesde AS DATE.
DEFINE INPUT PARAMETER pFechaHasta AS DATE.
DEFINE OUTPUT PARAMETER pVtas15Dias AS DEC.
DEFINE OUTPUT PARAMETER pVtas30Dias AS DEC.
DEFINE OUTPUT PARAMETER pVtas45Dias AS DEC.
DEFINE OUTPUT PARAMETER pVtas60Dias AS DEC.
DEFINE OUTPUT PARAMETER pVtas90Dias AS DEC.

DEFINE VAR lContador AS INT INIT 0.

DEFINE VAR lFecha AS DATE.

pVtas15Dias = 0.
pVtas30Dias = 0.
pVtas45Dias = 0.
pVtas60Dias = 0.
pVtas90Dias = 0.

REPEAT lFecha = pFechaDesde TO pFechahasta :
    lContador = lContador + 1.
    FOR EACH almdmov USE-INDEX almd03
                        WHERE almdmov.codcia = s-codcia AND 
                                almdmov.codalm = pCodAlm AND 
                                almdmov.codmat = pCodmat AND 
                                almdmov.fchdoc = lFecha AND 
                                almdmov.tipmov = 'S' AND 
                                almdmov.codmov = 2 NO-LOCK:
        
        IF lContador <= 15 THEN pVtas15Dias = pVtas15Dias + (almdmov.candes * almdmov.factor).
        IF lContador <= 30 THEN pVtas30Dias = pVtas30Dias + (almdmov.candes * almdmov.factor).
        IF lContador <= 45 THEN pVtas45Dias = pVtas45Dias + (almdmov.candes * almdmov.factor).
        IF lContador <= 60 THEN pVtas60Dias = pVtas60Dias + (almdmov.candes * almdmov.factor).
        IF lContador <= 90 THEN pVtas90Dias = pVtas90Dias + (almdmov.candes * almdmov.factor).
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-fResumen) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fResumen Procedure 
FUNCTION fResumen RETURNS CHARACTER
  ( INPUT pCodMat AS CHAR, INPUT pCodAlm AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  formato "<stock disponible>|<stock maximo>|<stock de seguridad>"
------------------------------------------------------------------------------*/

  DEF VAR pReturn AS CHAR NO-UNDO.
  DEF VAR lStockComprometido AS DEC NO-UNDO.

  pReturn = "0|0|0".    /* Valor por defecto */

  FIND FIRST ix-almmmate WHERE ix-almmmate.codcia = s-codcia 
      AND ix-almmmate.codalm = pCodAlm 
      AND ix-almmmate.codmat = pCodMat NO-LOCK NO-ERROR.
  IF AVAILABLE ix-almmmate  THEN DO:
      RUN gn/stock-comprometido-v2 (ix-almmmate.codmat, ix-almmmate.codalm, NO, OUTPUT lStockComprometido).
      pReturn = STRING(ix-almmmate.stkact - lStockComprometido) + "|" +
                STRING(ix-almmmate.StockMax) + "|" +
                STRING(ix-almmmate.StockSeg).
  END.
  RETURN pReturn.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fStock-disponible) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fStock-disponible Procedure 
FUNCTION fStock-disponible RETURNS DECIMAL
  ( INPUT pCodMat AS CHAR, INPUT pCodAlm AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

DEFINE VAR lRetVal AS DEC.
DEFINE VAR lStkReservado AS DEC.

lRetVal = 0.
lStkReservado = 0.

FIND FIRST ix-almmmate WHERE ix-almmmate.codcia = s-codcia AND 
            ix-almmmate.codalm = pCodAlm AND 
            ix-almmmate.codmat = pCodMat NO-LOCK NO-ERROR.
IF AVAILABLE ix-almmmate  THEN DO:
    lRetVal = ix-almmmate.stkact.
END.
RUN gn/stock-comprometido-v2 (INPUT pCodMat, INPUT pCodAlm, NO, OUTPUT lStkReservado).

lRetVal = lRetVal - lStkReservado.

RETURN lRetVal.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fstock-maximo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fstock-maximo Procedure 
FUNCTION fstock-maximo RETURNS DECIMAL
  ( INPUT pCodMat AS CHAR, INPUT pCodAlm AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

DEFINE VAR lRetVal AS DEC.
DEFINE VAR lStkReservado AS DEC.

lRetVal = 0.

FIND FIRST ix-almmmate WHERE ix-almmmate.codcia = s-codcia AND 
            ix-almmmate.codalm = pCodAlm AND 
            ix-almmmate.codmat = pCodMat NO-LOCK NO-ERROR.
IF AVAILABLE ix-almmmate  THEN DO:
    lRetVal = ix-almmmate.StockMax.
/*     lRetVal = ix-almmmate.VInMn1. */
END.

RETURN lRetVal.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fstock-seguridad) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fstock-seguridad Procedure 
FUNCTION fstock-seguridad RETURNS DECIMAL
  ( INPUT pCodMat AS CHAR, INPUT pCodAlm AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

DEFINE VAR lRetVal AS DEC.
DEFINE VAR lStkReservado AS DEC.

lRetVal = 0.

FIND FIRST ix-almmmate WHERE ix-almmmate.codcia = s-codcia 
    AND ix-almmmate.codalm = pCodAlm 
    AND ix-almmmate.codmat = pCodMat NO-LOCK NO-ERROR.
IF AVAILABLE ix-almmmate  THEN DO:
    lRetVal = ix-almmmate.StockSeg.
/*     lRetVal = ix-almmmate.VInMn2. */
END.

RETURN lRetVal.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

