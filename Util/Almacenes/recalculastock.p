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
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

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
         HEIGHT             = 5.58
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
DISABLE TRIGGERS FOR LOAD OF almcmov.
DISABLE TRIGGERS FOR LOAD OF almdmov.
DISABLE TRIGGERS FOR LOAD OF almmmate.

DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR DesdeC AS DATE NO-UNDO.
DEF VAR HastaC AS DATE NO-UNDO.

DEF BUFFER B-STKAL FOR AlmStkAl.
DEF BUFFER B-STKGE FOR AlmStkGe.
DEF BUFFER b-mov   FOR Almtmov.

DEF VAR x-signo  AS DECI NO-UNDO.  
DEF VAR x-ctomed AS DECI NO-UNDO.
DEF VAR x-stkgen AS DECI NO-UNDO.
DEF VAR x-cto    AS DECI NO-UNDO.
DEF VAR x-factor AS INTE NO-UNDO.

ASSIGN
    DesdeC = DATE(02,05,2018)
    HastaC = TODAY.

/* 1ro. cargamos productos por almacen */
DISPLAY 'INICIO:' NOW SKIP WITH STREAM-IO NO-BOX.
DEF TEMP-TABLE t-mate LIKE almmmate.
FOR EACH Almacen NO-LOCK WHERE Almacen.codcia = s-codcia AND Almacen.codalm = '14',
    FIRST gn-divi OF almacen NO-LOCK:
    DISPLAY 'ALMACEN:' almacen.codalm SKIP WITH STREAM-IO NO-BOX. PAUSE 0.
    FOR EACH almdmov USE-INDEX almd04 NO-LOCK WHERE almdmov.codcia = s-codcia
        AND almdmov.codalm = almacen.codalm
        AND almdmov.fchdoc >= desdec
        AND almdmov.fchdoc <= hastac:
        FIND FIRST t-mate WHERE t-mate.codcia = s-codcia
            AND t-mate.codalm = almdmov.codalm
            AND t-mate.codmat = almdmov.codmat
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE t-mate THEN DO:
            CREATE t-mate.
            ASSIGN
                t-mate.codcia = s-codcia
                t-mate.codalm = almdmov.codalm
                t-mate.codmat = almdmov.codmat.
        END.
    END.
END.

/* 2do actualizamos kardex */
FOR EACH t-mate NO-LOCK:
    RUN Actualiza-Producto (t-mate.codalm, t-mate.codmat).
    /* ************************************************************************ */
    /* RHC 27/01/2017 Stock comprometido */
    /* ************************************************************************ */
    RUN Stock-Comprometido (t-mate.CodAlm, t-mate.CodMat).
    /* ************************************************************************ */
    /* ************************************************************************ */
    RUN Replica.
END.
DISPLAY '   FIN:' NOW SKIP WITH STREAM-IO NO-BOX.

QUIT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-Actualiza-Producto) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Actualiza-Producto Procedure 
PROCEDURE Actualiza-Producto :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       solo se va a actualizar el kardex de ese almacén
------------------------------------------------------------------------------*/

DEF INPUT PARAMETER pCodAlm AS CHAR.                                  
DEF INPUT PARAMETER pCodMat AS CHAR.

DEF VAR f-candes AS DECI NO-UNDO.

FIND Almmmatg WHERE Almmmatg.codcia = s-codcia
    AND Almmmatg.codmat = pCodMat NO-LOCK NO-ERROR.
IF NOT AVAILABLE Almmmatg THEN RETURN.

/* Limpiamos stocks por almacen y producto */
FOR EACH AlmStkAl WHERE AlmStkAl.Codcia = S-CODCIA 
    AND AlmStkAl.CodAlm = pCodAlm
    AND AlmStkAl.CodMat = pCodMat 
    AND AlmStkAl.Fecha >= DesdeC
    EXCLUSIVE-LOCK:
    DELETE AlmStkAl.
END.                        
FOR EACH Almmmate WHERE Almmmate.Codcia = S-CODCIA 
    AND Almmmate.CodAlm = pCodAlm
    AND Almmmate.CodMat = pCodMat
    EXCLUSIVE-LOCK:
    Almmmate.StkAct = 0.
END.
/*************Ordenes Movimiento por Producto y Fecha *******************/     
FOR EACH AlmDmov USE-INDEX Almd03 WHERE Almdmov.Codcia = S-CODCIA 
    AND Almdmov.CodAlm = pCodAlm
    AND Almdmov.CodMat = pCodMat 
    AND Almdmov.FchDoc >= DesdeC
    EXCLUSIVE-LOCK:
    FIND FIRST almacen WHERE almacen.codcia = almdmov.codcia
        AND almacen.codalm = almdmov.codalm  
        NO-LOCK NO-ERROR.
    FIND Almtmovm WHERE almtmovm.codcia = almdmov.codcia
        AND almtmovm.tipmov = almdmov.tipmov
        AND almtmovm.codmov = almdmov.codmov
        NO-LOCK NO-ERROR.
    IF AVAILABLE almtmovm AND almtmovm.movtrf = YES 
        THEN x-Factor = 0.
        ELSE x-Factor = 1.
    IF AVAILABLE Almacen AND Almacen.FlgRep = NO THEN x-Factor = 0.
    IF AVAILABLE Almacen AND Almacen.AlmCsg = YES THEN x-Factor = 0.
    IF almdmov.tpocmb = 0 THEN DO:
        FIND LAST gn-tcmb WHERE  gn-tcmb.fecha <= almdmov.fchdoc
            USE-INDEX cmb01 NO-LOCK NO-ERROR.
        IF AVAIL gn-tcmb THEN DO:
            IF almdmov.tipmov = "i" THEN almdmov.tpocmb = gn-tcmb.venta.
            IF almdmov.tipmov = "s" THEN almdmov.tpocmb = gn-tcmb.compra. 
        END.
    END.
    f-candes = almdmov.candes * almdmov.factor.
    IF Almdmov.tipMov = "I" THEN x-signo = 1.
    IF Almdmov.tipMov = "S" THEN x-signo = -1.
    /***********Inicia Stock x Almacen********************/
    REPEAT:
        FIND LAST AlmStkAl WHERE AlmStkAl.Codcia = S-CODCIA 
            AND AlmStkAl.CodAlm = Almdmov.CodAlm 
            AND AlmStkAl.CodMat = Almdmov.CodMat 
            AND AlmstkAl.Fecha  = Almdmov.FchDoc
            EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
        IF NOT AVAILABLE AlmStkAl THEN DO:      
            IF NOT LOCKED AlmStkAl THEN DO:
                CREATE AlmStkAl.
                ASSIGN
                    AlmStkAl.Codcia = S-CODCIA 
                    AlmStkAl.CodAlm = Almdmov.CodAlm
                    AlmStkAl.CodMat = Almdmov.CodMat
                    AlmStkAl.Fecha  = Almdmov.FchDoc.
                FIND LAST B-STKAL WHERE B-STKAL.Codcia = S-CODCIA 
                    AND B-STKAL.CodAlm = Almdmov.CodAlm 
                    AND B-STKAL.CodMat = Almdmov.CodMat 
                    AND B-STKAL.Fecha  < Almdmov.FchDoc
                    NO-ERROR.
                IF AVAILABLE B-STKAL THEN AlmStkAl.StkAct = AlmStkAl.StkAct + B-STKAL.StkAct.                         
                LEAVE.
            END.
            ELSE UNDO, RETRY.
        END.
        LEAVE.
    END.
    ASSIGN
        AlmStkAl.StkAct = AlmStkAl.StkAct + f-candes * x-signo.
    ASSIGN
        Almdmov.StkSub  = AlmStkAl.StkAct.
    /***********Fin Stock x Almacen********************/
END.
/*************                            *******************/     
FIND LAST AlmStkAl WHERE AlmStkAl.Codcia = s-Codcia 
    AND AlmStkAl.CodAlm = pCodAlm 
    AND AlmStkAl.CodMat = pCodMat
    NO-LOCK NO-ERROR.
IF AVAILABLE AlmStkAl THEN DO:
    /* bloqueamos si o si Almmmate */
    REPEAT:
        FIND FIRST Almmmate WHERE Almmmate.Codcia = S-CODCIA 
            AND Almmmate.CodAlm = pCodAlm 
            AND Almmmate.CodMat = pCodMat
            EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
        IF NOT AVAILABLE Almmmate THEN DO:
            IF NOT LOCKED Almmmate THEN DO:
                CREATE Almmmate.
                ASSIGN
                    almmmate.codcia = S-CODCIA
                    almmmate.codalm = pCodAlm
                    almmmate.codmat = pCodMat
                    almmmate.desmat = almmmatg.desmat
                    almmmate.undvta = almmmatg.undstk
                    almmmate.facequ = 1.
                LEAVE.
            END.
            ELSE UNDO, RETRY.
        END.
        LEAVE.
    END.
    ASSIGN
        Almmmate.Stkact = AlmStkAl.StkAct.  
END.

IF AVAILABLE(AlmStkAl) THEN RELEASE AlmStkAl.
IF AVAILABLE(AlmStkGe) THEN RELEASE AlmStkGe.
IF AVAILABLE(Almmmate) THEN RELEASE Almmmate.
IF AVAILABLE(Almdmov)  THEN RELEASE Almdmov.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Replica) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Replica Procedure 
PROCEDURE Replica :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FIND Almmmate OF T-MATE NO-LOCK NO-ERROR.
IF NOT AVAILABLE Almmmate THEN RETURN.

    DEF VAR X-CanalVenta AS CHAR INIT "MIN" NO-UNDO.
    FIND FIRST almacen OF almmmate NO-LOCK NO-ERROR.
    IF AVAILABLE almacen THEN DO:
        FIND gn-divi WHERE gn-divi.codcia = almacen.codcia AND gn-divi.coddiv = almacen.coddiv NO-LOCK NO-ERROR.
        IF AVAILABLE gn-divi THEN x-CanalVenta = gn-divi.CanalVenta.
        {rpl/reptrig.i
        &Table  = almmmate
        &Key    =  "string(almmmate.codcia,'999') + string(almmmate.codalm,'x(5)') + string(almmmate.codmat,'x(6)')"
        &Prg    = r-almmmate
        &Event  = WRITE
        &FlgDB0 = TRUE
        &FlgDB1 = "NOT LOOKUP(almacen.coddiv, '00501') > 0"  /* TIENDA CONO NORTE */
        &FlgDB2 = "NOT LOOKUP(almacen.coddiv, '00023') > 0"  /* TIENDA SURQUILLO */
        &FlgDB3 = "NOT LOOKUP(almacen.coddiv, '00027') > 0"  /* TIENDA CHORRILLOS */
        &FlgDB4 = "NOT LOOKUP(almacen.coddiv, '00502') > 0"  /* TIENDA SAN BORJA */
        &FlgDB5 = "NOT LOOKUP(almacen.coddiv, '00503') > 0"  /* TIENDA LA MOLINA */
        &FlgDB6 = "NOT LOOKUP(almacen.coddiv, '00504') > 0"  /* TIENDA BENEFICENCIA */
        &FlgDB7 = "NOT LOOKUP(almacen.coddiv, '00505') > 0"  /* TIENDA PLAZA NORTE */
        &FlgDB8 = "NOT LOOKUP(almacen.coddiv, '00507') > 0"  /* TIENDA LA RAMBLA */
        &FlgDB9 = "NOT LOOKUP(almacen.coddiv, '00508') > 0"  /* TIENDA SAN ISIDRO */
        &FlgDB10 = "NOT LOOKUP(almacen.coddiv, '00065') > 0"  /* TIENDA CHICLAYO */
        &FlgDB11 = "NOT LOOKUP(almacen.coddiv, '00510') > 0"  /* TIENDA ATOCONGO */
        &FlgDB12 = "NOT LOOKUP(almacen.coddiv, '00511') > 0"  /* TIENDA ANGAMOS  */
        &FlgDB13 = "NOT LOOKUP(almacen.coddiv, '00512') > 0"  /* TIENDA SALAVERRY  */
        &FlgDB14 = "NOT LOOKUP(almacen.coddiv, '00513') > 0"  /* TIENDA CENTRO CIVICO  */
        &FlgDB15 = "NOT LOOKUP(almacen.coddiv, '00514') > 0"  /* TIENDA PRIMAVERA  */
        &FlgDB16 = "NOT LOOKUP(almacen.coddiv, '00516') > 0"  /* TIENDA BELLAVISTA  */
        &FlgDB17 = TRUE
        &FlgDB18 = "NOT LOOKUP(almacen.coddiv,'00060,00061,00062,00063,10060') > 0"     /* AREQUIPA */
        &FlgDB19 = "NOT LOOKUP(almacen.coddiv, '00069') > 0"  /* TRUJILLO */
        &FlgDB20 = "NOT x-CanalVenta = 'MIN'"   /*"NOT LOOKUP(almacen.coddiv, '00023,00027,00501,00502,00503,00504,00510') > 0"    /* SERVIDOR UTILEX */*/
        &FlgDB30 = NO
        }
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Stock-Comprometido) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Stock-Comprometido Procedure 
PROCEDURE Stock-Comprometido :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF INPUT PARAMETER pCodAlm AS CHAR.                                  
DEF INPUT PARAMETER pCodMat AS CHAR.

DEF VAR pComprometido AS DEC NO-UNDO.

DEF BUFFER B-DPEDI FOR Facdpedi.
DEF BUFFER B-CPEDI FOR Faccpedi.
DEF BUFFER B-MATE  FOR Almmmate.
DEF BUFFER B-CREPO FOR Almcrepo.
DEF BUFFER B-DREPO FOR Almdrepo.

LOOPGENERAL:
DO:
    FIND FacCfgGn WHERE faccfggn.codcia = s-CodCia NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Faccfggn THEN LEAVE.
    pComprometido = 0.
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
    /* Actualizamos Stock Comprometido */
    IF CAN-FIND(B-MATE WHERE B-MATE.codcia = s-CodCia AND B-MATE.codalm = pCodAlm AND B-MATE.codmat = pCodMat
                NO-LOCK) 
        THEN DO:
        {lib\lock-genericov21.i &Tabla="B-MATE" ~
            &Alcance="FIRST" ~
            &Intentos=10 ~
            &Condicion="B-MATE.codcia = s-CodCia AND B-MATE.codalm = pCodAlm AND B-MATE.codmat = pCodMat" ~
            &Bloqueo="EXCLUSIVE-LOCK NO-WAIT" ~
            &Accion="RETRY" ~
            &Mensaje="NO" ~
            &TipoError="RETURN" ~
            }
        ASSIGN
            B-MATE.StkComprometido = pComprometido.
        RELEASE B-MATE.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

