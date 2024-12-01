&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     : Cargar tabla de Stocks vs Transf y Compras en transito

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
DEF NEW SHARED VAR pv-codcia AS INT INIT 000.

DEF VAR s-User-Id AS CHAR INIT "SYSTEM" NO-UNDO.

/* 02/06/2022: Cambio de almac�n */
DEF VAR s-CodAlm  AS CHAR INIT '11w'    NO-UNDO.

DEF TEMP-TABLE t-Almacen_Stocks LIKE Almacen_Stocks
    FIELD Fecha AS CHAR FORMAT 'x(10)' LABEL 'Fecha'
    FIELD Hora  AS CHAR FORMAT 'x(5)'  LABEL 'Hora'
    .

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
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 17.62
         WIDTH              = 60.29.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

DEF VAR k AS INT NO-UNDO.
DEF VAR x-StockComprometido AS DEC NO-UNDO.

DEF VAR pRowid AS ROWID NO-UNDO.

RUN Log-Inicio (OUTPUT pRowid).

/* RHC 21/02/2020 Limpiamos las tablas relacionadas con la RAN
   Esta rutina corre a las 3:15 am y termina a las 4:20
   El proceso de RAN va a correr despu�s de este proceso, como a las 4:30am
*/
/* Anular RAN Pendientes */
RUN Rechaza-RAN.
IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
    /* El proceso debe continuar */
    PUT UNFORMATTED 'Error al ANULAR RAN: ' NOW SKIP.
    /*QUIT.*/
END.

/* Borramos detalle */
PUT UNFORMATTED "Borra detalle " DATETIME(TODAY, MTIME) SKIP.
RUN Borra-Detalle.
IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
    PUT UNFORMATTED 'NO SE PUDO ELIMINAR EL ARCHIVO ANTERIOR' SKIP.
    QUIT.
END.

/* Transferencias por recepcionar */
PUT UNFORMATTED "Carga transferencias " DATETIME(TODAY, MTIME) SKIP.
RUN Carga-Transferencias.

/* Ordenes de Compra en Transito */
PUT UNFORMATTED "Carga compras " DATETIME(TODAY, MTIME) SKIP.
RUN Carga-Compras.

/* Cargamos Productos */
PUT UNFORMATTED "Carga Productos " DATETIME(TODAY, MTIME) SKIP.
RUN Carga-Productos-v2.

/* Cargamos Stocks Comprometidos */
PUT UNFORMATTED "Carga comprometidos " DATETIME(TODAY, MTIME) SKIP.
RUN Carga-Comprometidos.

/* Revisa inconsistencias */
PUT UNFORMATTED "Inconsistencias " DATETIME(TODAY, MTIME) SKIP.
RUN Inconsistencias.

PUT UNFORMATTED "FIN " DATETIME(TODAY, MTIME) SKIP.

/* Iniciando almac�n 506 */
PUT UNFORMATTED "Inicio 506 " NOW SKIP.
RUN Borra-Detalle-506.
RUN Carga-Comprometidos-506.
PUT UNFORMATTED "Fin 506 " NOW SKIP.

PUT UNFORMATTED "Texto " NOW SKIP.
RUN Pasa-Texto.

RUN Log-Fin (INPUT pRowid).

/*IF CONNECTED("integral") THEN DISCONNECT "integral" NO-ERROR.*/

QUIT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-Borra-Detalle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Borra-Detalle Procedure 
PROCEDURE Borra-Detalle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FOR EACH estavtas.Almacen_Stocks EXCLUSIVE-LOCK ON ERROR UNDO, RETURN 'ADM-ERROR':
    DELETE estavtas.Almacen_Stocks.
END.
IF AVAILABLE(estavtas.Almacen_Stocks) THEN RELEASE estavtas.Almacen_Stocks.
RETURN 'OK'.

END PROCEDURE.

/* POSIBLE CODIGO 
DEFINE QUERY q-Almacen FOR Almacen_Stocks.
DEFINE VARIABLE icnt AS INT NO-UNDO.
OPEN QUERY q-Almacen FOR EACH Almacen_Stocks.
GET FIRST q-Almacen NO-LOCK.
tx-block:
REPEAT TRANSACTION:
    DO icnt = 1 TO 100:
        IF NOT AVAILABLE Almacen_Stocks THEN LEAVE tx-block.
        GET CURRENT q-Almacen EXCLUSIVE-LOCK.
        DELETE Almacen_Stocks.
        GET NEXT q-Almacen EXCLUSIVE-LOCK.
    END.
END.
*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Borra-Detalle-506) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Borra-Detalle-506 Procedure 
PROCEDURE Borra-Detalle-506 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FOR EACH estavtas.Almacen_Stocks WHERE estavtas.Almacen_Stocks.CodAlm = s-codalm
    AND estavtas.Almacen_Stocks.Reservado > 0
    EXCLUSIVE-LOCK ON ERROR UNDO, RETURN 'ADM-ERROR':
    estavtas.Almacen_Stocks.Reservado = 0.
END.
IF AVAILABLE(estavtas.Almacen_Stocks) THEN RELEASE estavtas.Almacen_Stocks.
RETURN 'OK'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Carga-Compras) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Carga-Compras Procedure 
PROCEDURE Carga-Compras :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    FOR EACH integral.OOComPend NO-LOCK WHERE (integral.OOComPend.CanPed - integral.OOComPend.CanAte) > 0:
        FIND FIRST estavtas.Almacen_Stocks WHERE estavtas.Almacen_Stocks.codalm = integral.OOComPend.CodAlm
            AND estavtas.Almacen_Stocks.codmat = integral.OOComPend.CodMat
            EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAILABLE estavtas.Almacen_Stocks THEN DO:
            CREATE estavtas.Almacen_Stocks.
            ASSIGN
                estavtas.Almacen_Stocks.codalm = TRIM(integral.OOComPend.CodAlm)
                estavtas.Almacen_Stocks.codmat = TRIM(integral.OOComPend.CodMat)
                estavtas.Almacen_Stocks.CmpTransito = 0.
        END.
        ASSIGN 
            estavtas.Almacen_Stocks.CmpTransito = estavtas.Almacen_Stocks.CmpTransito + (integral.OOComPend.CanPed - integral.OOComPend.CanAte).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Carga-Comprometidos) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Carga-Comprometidos Procedure 
PROCEDURE Carga-Comprometidos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FOR EACH estavtas.Almacen_Stocks EXCLUSIVE-LOCK WHERE estavtas.Almacen_Stocks.StkAct > 0:
    PUT UNFORMATTED "COMPROMETIDOS: " estavtas.Almacen_Stocks.codmat " " estavtas.Almacen_Stocks.codalm NOW SKIP.
    RUN ./gn/stock-comprometido-v2.p (estavtas.Almacen_Stocks.codmat, 
                                      estavtas.Almacen_Stocks.codalm, 
                                      NO,
                                      OUTPUT x-StockComprometido).
    ASSIGN
        estavtas.Almacen_Stocks.Reservado = x-StockComprometido.
END.
IF AVAILABLE estavtas.Almacen_Stocks THEN RELEASE estavtas.Almacen_Stocks.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Carga-Comprometidos-506) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Carga-Comprometidos-506 Procedure 
PROCEDURE Carga-Comprometidos-506 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FOR EACH estavtas.Almacen_Stocks WHERE estavtas.Almacen_Stocks.CodAlm = s-codalm
    AND estavtas.Almacen_Stocks.StkAct > 0
    EXCLUSIVE-LOCK :
    PUT UNFORMATTED "COMPROMETIDOS: " estavtas.Almacen_Stocks.codmat " " estavtas.Almacen_Stocks.codalm " " NOW SKIP.
    RUN ./gn/stock-comprometido-v2.p (estavtas.Almacen_Stocks.codmat, 
                                      estavtas.Almacen_Stocks.codalm, 
                                      NO,
                                      OUTPUT x-StockComprometido).
    ASSIGN
        estavtas.Almacen_Stocks.Reservado = x-StockComprometido.
END.
IF AVAILABLE estavtas.Almacen_Stocks THEN RELEASE estavtas.Almacen_Stocks.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Carga-Productos-v2) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Carga-Productos-v2 Procedure 
PROCEDURE Carga-Productos-v2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FOR EACH integral.Almacen NO-LOCK WHERE integral.Almacen.codcia = s-codcia
    AND LOOKUP(integral.Almacen.codalm, '999,998,997,996') = 0
    AND integral.Almacen.Campo-c[9] <> "I":     /* SOLO Almacenes Activos */
    PUT UNFORMATTED "Almacen: " integral.Almacen.codalm " " DATETIME(TODAY, MTIME) SKIP.
    FOR EACH integral.Almmmate NO-LOCK WHERE integral.Almmmate.CodCia = s-codcia
        AND integral.Almmmate.codalm = integral.Almacen.codalm
        AND (integral.Almmmate.Vctmn1 > 0           /* Stock Maximo Campa�a */
             OR integral.Almmmate.Vctmn2 > 0        /* Stock Maximo No Campa�a */
             OR integral.Almmmate.StkMax > 0        /* Empaque */
             OR integral.Almmmate.StkAct <> 0),      /* Stock */
        FIRST integral.Almmmatg OF integral.Almmmate NO-LOCK,
        FIRST integral.Almtfami OF integral.Almmmatg NO-LOCK,
        FIRST integral.Almsfami OF integral.Almmmatg NO-LOCK:
        RUN Graba-Registro.
    END.
END.
IF AVAILABLE estavtas.Almacen_Stocks THEN RELEASE estavtas.Almacen_Stocks.

DEF BUFFER B-DETALLE FOR estavtas.Almacen_Stocks.

FOR EACH integral.Almacen NO-LOCK WHERE integral.Almacen.codcia = s-codcia
    AND LOOKUP(integral.Almacen.codalm, '999,998,997,996') = 0
    AND integral.Almacen.Campo-c[9] <> "I":     /* SOLO Almacenes Activos */
    PUT UNFORMATTED "Almacen (*): " integral.Almacen.codalm " " DATETIME(TODAY, MTIME) SKIP.
    FOR EACH B-DETALLE NO-LOCK WHERE B-DETALLE.codalm = integral.Almacen.CodAlm
        AND TRUE <> (B-DETALLE.Producto > ''),
        FIRST integral.Almmmate NO-LOCK WHERE integral.Almmmate.codcia = s-codcia
        AND integral.Almmmate.codalm = B-DETALLE.CodAlm 
        AND integral.Almmmate.codmat = B-DETALLE.CodMat,
        FIRST integral.Almmmatg OF integral.Almmmate NO-LOCK,
        FIRST integral.Almtfami OF integral.Almmmatg NO-LOCK, 
        FIRST integral.Almsfami OF integral.Almmmatg NO-LOCK:
        RUN Graba-Registro.
    END.
END.
IF AVAILABLE B-DETALLE THEN RELEASE B-DETALLE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Carga-Transferencias) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Carga-Transferencias Procedure 
PROCEDURE Carga-Transferencias :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       Transferencias en tr�nsito
------------------------------------------------------------------------------*/

/* REPOSICIONES */
FOR EACH integral.Almacen NO-LOCK WHERE integral.Almacen.codcia = s-codcia
        AND LOOKUP(integral.Almacen.CodAlm, '997,998') = 0,
    EACH integral.Almcrepo NO-LOCK WHERE integral.Almcrepo.codcia = integral.Almacen.codcia
        AND integral.Almcrepo.CodAlm = integral.Almacen.CodAlm
        AND integral.Almcrepo.FlgEst = 'P',
    EACH integral.Almdrepo OF integral.Almcrepo NO-LOCK WHERE integral.almdrepo.CanApro > integral.almdrepo.CanAten:
    FIND FIRST estavtas.Almacen_Stocks WHERE estavtas.Almacen_Stocks.codalm = integral.Almcrepo.CodAlm
        AND estavtas.Almacen_Stocks.codmat = integral.Almdrepo.CodMat
        EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE estavtas.Almacen_Stocks THEN DO:
        CREATE estavtas.Almacen_Stocks.
        ASSIGN
            estavtas.Almacen_Stocks.codalm = TRIM(integral.Almcrepo.CodAlm)
            estavtas.Almacen_Stocks.codmat = TRIM(integral.Almdrepo.CodMat).
    END.
    ASSIGN 
        estavtas.Almacen_Stocks.TrfTransito = estavtas.Almacen_Stocks.TrfTransito + 
        (integral.Almdrepo.CanApro - integral.Almdrepo.CanAten).
END.
/* TRANSFERENCIAS */
DEF VAR cAlmDes AS CHAR NO-UNDO.
FOR EACH integral.Almacen NO-LOCK WHERE integral.Almacen.codcia = s-codcia,
    EACH integral.Almcmov NO-LOCK WHERE integral.Almcmov.codcia = integral.Almacen.codcia
        AND integral.Almcmov.codalm = integral.Almacen.codalm
        AND integral.Almcmov.tipmov = "S"
        AND integral.Almcmov.codmov = 03
        AND integral.Almcmov.flgest <> "A"
        AND integral.Almcmov.flgsit = "T",
    EACH integral.Almdmov OF integral.Almcmov NO-LOCK:
    /* RHC 05/02/2018 Dos casos: 
    1. Salida con Cross Docking
    2. Salida sin Cross Docking 
    */
    IF Almcmov.CrossDocking = YES THEN cAlmDes = INTEGRAL.Almcmov.AlmacenXD.     /* Destino Final */
    ELSE cAlmDes = INTEGRAL.Almcmov.AlmDes.  /* ALmac�n Destino */
    FIND FIRST estavtas.Almacen_Stocks WHERE estavtas.Almacen_Stocks.codalm = cAlmDes
        AND estavtas.Almacen_Stocks.codmat = integral.Almdmov.CodMat
        EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE estavtas.Almacen_Stocks THEN DO:
        CREATE estavtas.Almacen_Stocks.
        ASSIGN
            estavtas.Almacen_Stocks.codalm = TRIM(cAlmDes)
            estavtas.Almacen_Stocks.codmat = TRIM(integral.Almdmov.CodMat).
    END.
    ASSIGN 
        estavtas.Almacen_Stocks.TrfTransito = estavtas.Almacen_Stocks.TrfTransito + integral.Almdmov.candes.
END.
/* ORDENES DE TRANSFERENCIA */
FOR EACH integral.gn-divi NO-LOCK WHERE INTEGRAL.GN-DIVI.CodCia = s-codcia,
    EACH INTEGRAL.FacCPedi NO-LOCK WHERE INTEGRAL.FacCPedi.CodCia = INTEGRAL.GN-DIVI.CodCia
        AND INTEGRAL.FacCPedi.CodDiv = INTEGRAL.GN-DIVI.CodDiv
        AND INTEGRAL.FacCPedi.CodDoc = "OTR"
        AND INTEGRAL.FacCPedi.FlgEst = "P"
        AND INTEGRAL.FacCPedi.CodRef = "R/A",
    EACH INTEGRAL.Facdpedi OF INTEGRAL.FacCPedi NO-LOCK WHERE INTEGRAL.Facdpedi.flgest = 'P':
    /* RHC 05/02/2018 Dos casos: 
    1. Salida con Cross Docking
    2. Salida sin Cross Docking 
    */
    IF INTEGRAL.FacCPedi.CrossDocking = YES THEN cAlmDes = INTEGRAL.FacCPedi.AlmacenXD.     /* Destino Final */
    ELSE cAlmDes = INTEGRAL.FacCPedi.CodCli.  /* Almac�n Destino */
    FIND FIRST estavtas.Almacen_Stocks WHERE estavtas.Almacen_Stocks.codalm = cAlmDes
        AND estavtas.Almacen_Stocks.codmat = integral.Facdpedi.CodMat
        EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE estavtas.Almacen_Stocks THEN DO:
        CREATE estavtas.Almacen_Stocks.
        ASSIGN
            estavtas.Almacen_Stocks.codalm = cAlmDes
            estavtas.Almacen_Stocks.codmat = TRIM(integral.Facdpedi.CodMat).
    END.
    ASSIGN
        estavtas.Almacen_Stocks.TrfTransito = estavtas.Almacen_Stocks.TrfTransito +
        integral.Facdpedi.Factor * (integral.Facdpedi.CanPed - integral.Facdpedi.CanAte).
END.
/* ********************************************************************************************************* */
/* RHC 16/06/2021 Sloting */
/* ********************************************************************************************************* */
FOR EACH integral.OOMoviAlmacen NO-LOCK WHERE integral.OOMoviAlmacen.codcia = s-codcia
    AND integral.OOMoviAlmacen.FlagMigracion = "N" 
    AND integral.OOMoviAlmacen.TipMov = "I" 
    AND integral.OOMoviAlmacen.CodMov = 03:
    FIND FIRST estavtas.Almacen_Stocks WHERE estavtas.Almacen_Stocks.codalm = integral.OOMoviAlmacen.CodAlm
        AND estavtas.Almacen_Stocks.codmat = integral.OOMoviAlmacen.CodMat
        EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE estavtas.Almacen_Stocks THEN DO:
        CREATE estavtas.Almacen_Stocks.
        ASSIGN
            estavtas.Almacen_Stocks.codalm = integral.OOMoviAlmacen.CodAlm
            estavtas.Almacen_Stocks.codmat = integral.OOMoviAlmacen.CodMat.
    END.
    ASSIGN 
        estavtas.Almacen_Stocks.TrfTransito = estavtas.Almacen_Stocks.TrfTransito + 
                                                (integral.OOMoviAlmacen.CanDes * integral.OOMoviAlmacen.Factor).
END.
FOR EACH integral.OOMoviAlmacen NO-LOCK WHERE integral.OOMoviAlmacen.codcia = s-codcia
    AND integral.OOMoviAlmacen.FlagMigracion = "N" 
    AND integral.OOMoviAlmacen.FchDoc >= DATE(06,01,2019) 
    AND integral.OOMoviAlmacen.TipMov = "I" 
    AND integral.OOMoviAlmacen.CodMov = 90
    AND integral.OOMoviAlmacen.UseInDropShipment = "NO":
    FIND FIRST estavtas.Almacen_Stocks WHERE estavtas.Almacen_Stocks.codalm = integral.OOMoviAlmacen.CodAlm
        AND estavtas.Almacen_Stocks.codmat = integral.OOMoviAlmacen.CodMat
        EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE estavtas.Almacen_Stocks THEN DO:
        CREATE estavtas.Almacen_Stocks.
        ASSIGN
            estavtas.Almacen_Stocks.codalm = integral.OOMoviAlmacen.CodAlm
            estavtas.Almacen_Stocks.codmat = integral.OOMoviAlmacen.CodMat.
    END.
    ASSIGN 
        estavtas.Almacen_Stocks.TrfTransito = estavtas.Almacen_Stocks.TrfTransito + 
                                                (integral.OOMoviAlmacen.CanDes * integral.OOMoviAlmacen.Factor).
END.
FOR EACH integral.OOMoviAlmacen NO-LOCK WHERE integral.OOMoviAlmacen.codcia = s-codcia
    AND integral.OOMoviAlmacen.FlagMigracion = "N" 
    AND integral.OOMoviAlmacen.TipMov = "I" 
    AND (integral.OOMoviAlmacen.CodMov = 09 OR integral.OOMoviAlmacen.CodMov = 30)
    AND integral.OOMoviAlmacen.UseInDropShipment = "NO":
    FIND FIRST estavtas.Almacen_Stocks WHERE estavtas.Almacen_Stocks.codalm = integral.OOMoviAlmacen.CodAlm
        AND estavtas.Almacen_Stocks.codmat = integral.OOMoviAlmacen.CodMat
        EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE estavtas.Almacen_Stocks THEN DO:
        CREATE estavtas.Almacen_Stocks.
        ASSIGN
            estavtas.Almacen_Stocks.codalm = integral.OOMoviAlmacen.CodAlm
            estavtas.Almacen_Stocks.codmat = integral.OOMoviAlmacen.CodMat.
    END.
    ASSIGN 
        estavtas.Almacen_Stocks.TrfTransito = estavtas.Almacen_Stocks.TrfTransito + 
                                                (integral.OOMoviAlmacen.CanDes * integral.OOMoviAlmacen.Factor).
END.
/* ********************************************************************************************************* */
/* ********************************************************************************************************* */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Graba-Registro) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Graba-Registro Procedure 
PROCEDURE Graba-Registro :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR x-CtoLis AS DECI NO-UNDO.

FIND FIRST integral.Almtabla WHERE integral.Almtabla.tabla = 'MK' 
    AND integral.Almtabla.codigo = Almmmatg.codmar 
    NO-LOCK NO-ERROR.
FIND estavtas.Almacen_Stocks WHERE estavtas.Almacen_Stocks.CodAlm = integral.Almmmate.codalm
    AND estavtas.Almacen_Stocks.CodMat = integral.Almmmate.codmat
    EXCLUSIVE-LOCK NO-ERROR.
IF NOT AVAILABLE estavtas.Almacen_Stocks THEN CREATE estavtas.Almacen_Stocks.
ASSIGN
    estavtas.Almacen_Stocks.CodAlm = TRIM(integral.Almmmate.codalm)
    estavtas.Almacen_Stocks.CodMat = TRIM(integral.Almmmate.codmat)
    estavtas.Almacen_Stocks.Almacen = /*TRIM(integral.Almacen.CodAlm) + ' ' +*/ integral.Almacen.Descripcion
    estavtas.Almacen_Stocks.Producto = /*TRIM(integral.Almmmatg.codmat) + ' ' +*/ TRIM(integral.Almmmatg.DesMat)
    estavtas.Almacen_Stocks.Linea = integral.Almmmatg.codfam + ' ' + integral.Almtfami.DesFam
    estavtas.Almacen_Stocks.Sublinea = integral.Almmmatg.subfam + ' ' + INTEGRAL.AlmSFami.dessub 
    estavtas.Almacen_Stocks.Unidad = integral.Almmmatg.undstk
    estavtas.Almacen_Stocks.StkAct = integral.Almmmate.StkAct
    /* 23Jul2013 - Ic */
/*     estavtas.Almacen_Stocks.clasificacion = Almmmatg.tiprot[1] */
/*     estavtas.Almacen_Stocks.iranking = Almmmatg.ordtmp         */
/*     estavtas.Almacen_Stocks.clsfutlx = Almmmatg.undalt[3]      */
/*     estavtas.Almacen_Stocks.rnkgutlx = Almmmatg.libre_d04      */
/*     estavtas.Almacen_Stocks.clsfmayo = Almmmatg.undalt[4]      */
/*     estavtas.Almacen_Stocks.rnkgmayo = Almmmatg.libre_d05      */
    /* Fin 23Jul2013 Ic */
    /* 11Dic2014 Ic */
    estavtas.Almacen_Stocks.tpoart      = Almmmatg.tpoart
    estavtas.Almacen_Stocks.almacenes   = Almmmatg.tpomrg
    estavtas.Almacen_Stocks.catconta    = Almmmatg.catconta[1].
    /* Fin 11Dic2014 Ic */
/* Proveedor */
ASSIGN
    estavtas.Almacen_Stocks.Proveedor = TRIM(integral.Almmmatg.CodPr1).
FIND integral.gn-prov WHERE integral.gn-prov.codcia = pv-codcia
    AND integral.gn-prov.CodPro = estavtas.Almacen_Stocks.Proveedor
    NO-LOCK NO-ERROR.
IF AVAILABLE integral.gn-prov THEN estavtas.Almacen_Stocks.Proveedor = estavtas.Almacen_Stocks.Proveedor + ' ' + TRIM(integral.gn-prov.NomPro).
/* Costo Reposicion */
IF Almmmatg.monvta = 1 THEN ASSIGN estavtas.Almacen_Stocks.CostoMn = integral.Almmmatg.CtoLis.
ELSE ASSIGN estavtas.Almacen_Stocks.CostoMn = integral.Almmmatg.CtoLis * integral.Almmmatg.TpoCmb.
/* Costo Promedio */
FIND LAST integral.AlmStkge WHERE integral.AlmStkge.CodCia = s-codcia
    AND integral.AlmStkge.codmat = integral.Almmmatg.codmat
    AND integral.AlmStkge.Fecha <= TODAY 
    NO-LOCK NO-ERROR.
IF AVAILABLE integral.AlmStkge THEN estavtas.Almacen_Stocks.PromedioMn = integral.AlmStkge.CtoUni.
/* Informaci�n del Almac�n */
CASE integral.Almacen.campo-c[2]:
    WHEN '1' THEN estavtas.Almacen_Stocks.TpoAlm = "Mayorista".
    WHEN '2' THEN estavtas.Almacen_Stocks.TpoAlm = "Minorista".
    OTHERWISE estavtas.Almacen_Stocks.TpoAlm = "No definido".
END CASE.
CASE integral.Almacen.campo-c[3]:
    WHEN "Si" THEN estavtas.Almacen_Stocks.AlmRem = "Si".
    OTHERWISE estavtas.Almacen_Stocks.AlmRem = "No".
END CASE.
estavtas.Almacen_Stocks.AlmCom = integral.Almacen.campo-c[6].
estavtas.Almacen_Stocks.CodDiv = integral.Almacen.coddiv.
estavtas.Almacen_Stocks.StkMin = integral.Almmmate.StkMin.
estavtas.Almacen_Stocks.StkMax = integral.Almmmate.StkMax.
estavtas.Almacen_Stocks.Marca = integral.almmmatg.codmar + ' ' + (IF (AVAILABLE integral.Almtabla) THEN integral.Almtabla.nombre ELSE '').
estavtas.Almacen_Stocks.Chr__02 = integral.Almmmatg.CHR__02.
estavtas.Almacen_Stocks.StockMax = integral.Almmmate.StockMax.
estavtas.Almacen_Stocks.StockSeg = integral.Almmmate.StockSeg.
estavtas.Almacen_Stocks.VCtMn1 = integral.Almmmate.VCtMn1.
estavtas.Almacen_Stocks.VCtMn2 = integral.Almmmate.VCtMn2.
estavtas.Almacen_Stocks.StkRep = integral.Almmmatg.StkRep.
estavtas.Almacen_Stocks.CanEmp = integral.Almmmatg.CanEmp.
FIND INTEGRAL.FacTabla WHERE INTEGRAL.FacTabla.CodCia = s-codcia AND
    INTEGRAL.FacTabla.Tabla = 'RANKVTA' AND
    INTEGRAL.FacTabla.Codigo = integral.Almmmatg.codmat
    NO-LOCK NO-ERROR.
IF AVAILABLE INTEGRAL.FacTabla THEN
    ASSIGN
    estavtas.Almacen_Stocks.clasificacion = INTEGRAL.FacTabla.Campo-C[1] 
    estavtas.Almacen_Stocks.clsfutlx = INTEGRAL.FacTabla.Campo-C[2] 
    estavtas.Almacen_Stocks.clsfmayo = INTEGRAL.FacTabla.Campo-C[3] 
    estavtas.Almacen_Stocks.ClasificacionNC = INTEGRAL.FacTabla.Campo-C[4] 
    estavtas.Almacen_Stocks.ClsfUtlxNC = INTEGRAL.FacTabla.Campo-C[5] 
    estavtas.Almacen_Stocks.ClsfMayoNC = INTEGRAL.FacTabla.Campo-C[6]
    .
estavtas.Almacen_Stocks.IndiceCom = INTEGRAL.Almmmatg.FlgComercial.
FIND integral.almtabla WHERE INTEGRAL.almtabla.Tabla = "IN_CO" AND
    INTEGRAL.almtabla.Codigo = INTEGRAL.Almmmatg.FlgComercial
    NO-LOCK NO-ERROR.
IF AVAILABLE integral.almtabla THEN estavtas.Almacen_Stocks.IndiceCom = INTEGRAL.Almmmatg.FlgComercial + ' ' + 
    INTEGRAL.almtabla.Nombre.
estavtas.Almacen_Stocks.Volumen = INTEGRAL.Almmmatg.Libre_d02.
estavtas.Almacen_Stocks.Peso = INTEGRAL.Almmmatg.Pesmat.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Inconsistencias) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inconsistencias Procedure 
PROCEDURE Inconsistencias :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FOR EACH estavtas.Almacen_Stocks EXCLUSIVE-LOCK:
    IF TRUE <> (estavtas.Almacen_Stocks.Producto > '')
        OR TRUE <> (estavtas.Almacen_Stocks.Almacen > '')
        OR TRUE <> (estavtas.Almacen_Stocks.Linea > '')
        OR TRUE <> (estavtas.Almacen_Stocks.SubLinea > '')
        THEN DO:
        DELETE estavtas.Almacen_Stocks.
        NEXT.
    END.
END.
IF AVAILABLE estavtas.Almacen_Stocks THEN RELEASE estavtas.Almacen_Stocks.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Log-Fin) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Log-Fin Procedure 
PROCEDURE Log-Fin :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF INPUT PARAMETER pRowid AS ROWID.

FIND FIRST LogBacheros WHERE ROWID(LogBacheros) = pRowid EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
IF AVAILABLE LogBacheros  THEN DO:
    ASSIGN
        LogBacheros.FechaFin = TODAY
        LogBacheros.HoraFin = STRING(TIME,'HH:MM:SS')
        LogBacheros.Estado = 'CONCLUIDO'.
END.

RELEASE LogBacheros.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Log-Inicio) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Log-Inicio Procedure 
PROCEDURE Log-Inicio :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF OUTPUT PARAMETER pRowid AS ROWID.

CREATE LogBacheros.
ASSIGN
    LogBacheros.Estado = 'INICIADO'
    LogBacheros.Detalle = 'Actualiza el stock en la tabla Almacen_Stocks'
    LogBacheros.Grupo = 'Stocks'
    LogBacheros.FechaInicio = TODAY
    LogBacheros.HoraInicio = STRING(TIME,'HH:MM:SS')
    LogBacheros.Tabla = 'Almacen_Stocks'
    LogBacheros.Usuario = s-User-Id.
ASSIGN pRowid = ROWID(LogBacheros).

RELEASE LogBacheros.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Pasa-Texto) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Pasa-Texto Procedure 
PROCEDURE Pasa-Texto :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/* ********************************************************************************* */
/* ALMACEN_STOCKS */
/* ********************************************************************************* */
DEF VAR x-Archivo AS CHAR NO-UNDO.
DEF VAR x-CodFchI AS DATE NO-UNDO.
DEF VAR x-CodHora AS CHAR NO-UNDO.

x-CodFchI = TODAY.
x-CodHora = STRING(TIME, 'HH:MM').

/* ********************************************************************************* */
/* Cargamos el archivo texto */
/* 23/01/2023 Fecha y Hora solicitado por D.LL. para un control */
/* ********************************************************************************* */
EMPTY TEMP-TABLE t-Almacen_Stocks.
FOR EACH Almacen_Stocks NO-LOCK:
    CREATE t-Almacen_Stocks.
    BUFFER-COPY Almacen_Stocks TO t-Almacen_Stocks.
    /* 24/8/2023: Observaci�n B.Acu�a */
    IF t-Almacen_Stocks.CostoMN = ? THEN t-Almacen_Stocks.CostoMn = 0.
    /* ****************************** */
    ASSIGN
        t-Almacen_Stocks.Fecha = STRING(YEAR(x-CodFchI), '9999') + "-" + 
                                STRING(MONTH(x-CodFchI), '99') + "-" + 
                                STRING(DAY(x-CodFchI), '99')
        t-Almacen_Stocks.Hora = x-CodHora.
END.
/* ********************************************************************************* */
/* Borramos cualquier archivo generado la semana pasada */
/* ********************************************************************************* */
DEF VAR x-Comando AS CHAR NO-UNDO.
/*
IF WEEKDAY(TODAY) = 2 THEN DO:      /* Lunes */
    x-Comando = "rm -f /home/u/IN/log/almacen_stocks*.txt".
    OS-COMMAND VALUE(x-Comando) SILENT NO-CONSOLE.
END.
*/
/* ********************************************************************************* */
/* Pasamos el archivo texto */
/* ********************************************************************************* */
x-Archivo = "/home/v/IN/dbs/" + "almacen_stocks.txt".
OUTPUT TO VALUE(x-archivo) KEEP-MESSAGES.
FOR EACH t-Almacen_Stocks NO-LOCK:
    EXPORT delimiter "~029" t-Almacen_Stocks.
END.
OUTPUT CLOSE.
/* ******************************************************************************* */
/* COPIA DE CONTROL */
/* ******************************************************************************* */
DEF VAR x-Destino AS CHAR NO-UNDO.

x-Destino = "/home/u/IN/log/almacen_stocks" + ~
    STRING(YEAR(x-CodFchI), '9999') +  ~
    STRING(MONTH(x-CodFchI), '99') + ~
    STRING(DAY(x-CodFchI), '99') + ~
    REPLACE(x-CodHora,':','') +  ".txt".
OUTPUT TO VALUE(x-destino) KEEP-MESSAGES.
FOR EACH t-Almacen_Stocks NO-LOCK:
    EXPORT delimiter "~029" t-Almacen_Stocks.
END.
OUTPUT CLOSE.
/* x-Comando = "cp " + x-archivo + " " + x-destino. */
/* OS-COMMAND VALUE(x-Comando).                     */
/* ******************************************************************************* */
/* EXPORTA A OLAP */
/* ******************************************************************************* */
DEFINE VARIABLE comm-line AS CHARACTER FORMAT "x(70)".                                        

comm-line = "/usr/bin/qonvtaexport4".
OS-COMMAND VALUE(comm-line) SILENT NO-CONSOLE.

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
  Notes:       Solo las de ayer
------------------------------------------------------------------------------*/

DISABLE TRIGGERS FOR LOAD OF almcrepo.
DISABLE TRIGGERS FOR LOAD OF almdrepo.

DEF BUFFER B-CREPO FOR Almcrepo.


FOR EACH Almcrepo NO-LOCK WHERE Almcrepo.codcia = s-codcia AND
    Almcrepo.TipMov = "RAN" AND
    Almcrepo.FlgEst = "P" AND
    Almcrepo.FchDoc < TODAY:        /* <<< OJO <<< */
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

&IF DEFINED(EXCLUDE-stock-comprometido) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE stock-comprometido Procedure 
PROCEDURE stock-comprometido :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/*{vta2/stock-comprometido.i}*/

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
        /* Tiempo por defecto fuera de campa�a */
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

