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
/*DEF VAR s-CodAlm  AS CHAR INIT '506'    NO-UNDO.*/
/* 02/06/2022: Cambio de almac�n */
DEF VAR s-CodAlm  AS CHAR INIT '11w'    NO-UNDO.

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
         HEIGHT             = 8.38
         WIDTH              = 60.29.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

DEF VAR k AS INT NO-UNDO.
DEF VAR x-StockComprometido AS DEC NO-UNDO.

/* DEF VAR x-Salida AS CHAR NO-UNDO.                                                         */
/* x-Salida = "/home/u/IN/log/control" + STRING(DAY(TODAY),'99') + STRING(MONTH(TODAY),'99') */
/*     + STRING(YEAR(TODAY),'9999') + '.txt'.                                                */
/* OUTPUT TO VALUE(x-Salida).                                                                */

/* Borramos detalle */
PUT UNFORMATTED "Borra detalle " DATETIME(TODAY, MTIME) SKIP.
RUN Borra-Detalle-506.
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
RUN Carga-Comprometidos-506.

/* Revisa inconsistencias */
PUT UNFORMATTED "Inconsistencias " DATETIME(TODAY, MTIME) SKIP.
RUN Inconsistencias.

PUT UNFORMATTED "FIN " DATETIME(TODAY, MTIME) SKIP.

/* OUTPUT CLOSE. */

IF CONNECTED("integral") THEN DISCONNECT "integral" NO-ERROR.

QUIT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-Borra-Detalle-506) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Borra-Detalle-506 Procedure 
PROCEDURE Borra-Detalle-506 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FOR EACH estavtas.Almacen_Stocks WHERE estavtas.Almacen_Stocks.CodAlm = s-codalm
    EXCLUSIVE-LOCK ON ERROR UNDO, RETURN 'ADM-ERROR':
    DELETE estavtas.Almacen_Stocks.
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

FOR EACH estavtas.Almacen_Stocks EXCLUSIVE-LOCK WHERE estavtas.Almacen_Stocks.CodAlm = s-CodAlm:
    FOR EACH integral.OOComPend WHERE integral.OOComPend.CodAlm = estavtas.Almacen_Stocks.codalm
        AND integral.OOComPend.CodMat = estavtas.Almacen_Stocks.codmat NO-LOCK:
        estavtas.Almacen_Stocks.CmpTransito = estavtas.Almacen_Stocks.CmpTransito + (integral.OOComPend.CanPed - integral.OOComPend.CanAte).
    END.
END.

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

FOR EACH estavtas.Almacen_Stocks EXCLUSIVE-LOCK WHERE estavtas.Almacen_Stocks.CodAlm = s-CodAlm
    AND estavtas.Almacen_Stocks.StkAct > 0:
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
    AND integral.Almacen.codalm = s-CodAlm
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
    AND integral.Almacen.codalm = s-CodAlm
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
        AND integral.Almacen.CodAlm = s-CodAlm,
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
    ELSE cAlmDes = INTEGRAL.Almcmov.AlmDes.  /* Almac�n Destino */

    IF cAlmDes <> s-CodAlm THEN NEXT.

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

    IF cAlmDes <> s-CodAlm THEN NEXT.

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
    AND integral.OOMoviAlmacen.CodAlm = s-CodAlm
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
    AND integral.OOMoviAlmacen.CodAlm = s-CodAlm
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
    AND integral.OOMoviAlmacen.CodAlm = s-CodAlm
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
    estavtas.Almacen_Stocks.tpoart      = Almmmatg.tpoart
    estavtas.Almacen_Stocks.almacenes   = Almmmatg.tpomrg
    estavtas.Almacen_Stocks.catconta    = Almmmatg.catconta[1].
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

FOR EACH estavtas.Almacen_Stocks EXCLUSIVE-LOCK WHERE estavtas.Almacen_Stocks.CodAlm = s-CodAlm:
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

