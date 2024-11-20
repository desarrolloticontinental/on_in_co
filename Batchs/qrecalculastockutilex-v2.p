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
         HEIGHT             = 7.69
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
DISABLE TRIGGERS FOR LOAD OF almcmov.
DISABLE TRIGGERS FOR LOAD OF almdmov.
DISABLE TRIGGERS FOR LOAD OF almmmate.

DEF NEW SHARED VAR s-codcia AS INT INIT 001.
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
DEF STREAM st-Errores.

/* 
RHC 16/10/2012 
SE RECALCULA EL STOCK CADA 5 MINUTOS YA QUE LOS MOVIMIENTOS 
DE VENTAS DE REPLICAN DE LAS TIENDAS AL SERVIDOR CENTRAL 
*/
ASSIGN
    /*DesdeC = TODAY - 1*/
    DesdeC = TODAY
    HastaC = TODAY.

/* 1ro. cargamos productos por almacen */
PUT UNFORMATTED 'INICIO:' NOW SKIP.
DEF TEMP-TABLE t-mate NO-UNDO LIKE almmmate.
/* ******************************************* */
/* Verificamos Ventas de tiendas UTILEX (S-02) */
/* ******************************************* */
FOR EACH Almacen NO-LOCK WHERE Almacen.codcia = s-codcia
    AND Almacen.campo-c[9] <> "I"
    AND Almacen.flgrep = YES
    AND Almacen.campo-c[6] = "Si":
    RUN Carga-Temporal.
END.
/* *********************** */
/* 2do actualizamos kardex */
/* *********************** */
/* OUTPUT STREAM st-Errores TO /u/backup/IN/ON_IN_CO/log/log-errores.txt KEEP-MESSAGES. */
/* OUTPUT TO /u/backup/IN/ON_IN_CO/log/qoninstockutilex-v2.txt KEEP-MESSAGES. */
DEF VAR pMensaje AS CHAR NO-UNDO.
FOR EACH t-mate NO-LOCK BREAK BY (t-mate.codmat):
    IF LAST-OF(t-mate.codmat) THEN DO:
        PUT UNFORMATTED T-Mate.CodMat ' ' NOW SKIP.
        RUN /v/IN/ON_IN_CO/prg/alm/calc-costo-promedio (t-mate.codmat, DesdeC, OUTPUT pMensaje).
        IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
            PUT UNFORMATTED
                'ERROR: ' pMensaje ' '
                'PRODUCTO: ' t-mate.codmat ' '
                'FECHA HORA: ' NOW 
                SKIP.
        END.
    END.
END.
/* RHC 30/10/2019 Bloqueado
FOR EACH t-mate NO-LOCK:
    /* ************************************************************************ */
    /* RHC 27/01/2017 Stock comprometido */
    /* ************************************************************************ */
    RUN stock-comprometido (t-mate.CodAlm, t-mate.CodMat).
    /* ************************************************************************ */
END.
*/
FOR EACH t-mate NO-LOCK:
    RUN Actualiza-Stock-Ecommerce (t-mate.codalm, t-mate.codmat).
END.
PUT UNFORMATTED 'FIN:' NOW SKIP.

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
DEF VAR x-Vueltas AS INT NO-UNDO.

FIND Almmmatg WHERE Almmmatg.codcia = s-codcia
    AND Almmmatg.codmat = pCodMat NO-LOCK NO-ERROR.
IF NOT AVAILABLE Almmmatg THEN RETURN 'OK'.

RLOOP:
DO TRANSACTION ON ERROR UNDO, RETURN 'ADM-ERROR' ON STOP UNDO, RETURN 'ADM-ERROR':
    x-Vueltas = 0.
    REPEAT:
        x-Vueltas = x-Vueltas + 1.
        IF x-Vueltas >= 100 THEN UNDO RLOOP, RETURN 'ADM-ERROR'.
        FIND FIRST Almmmate WHERE Almmmate.Codcia = S-CODCIA 
            AND Almmmate.CodAlm = pCodAlm
            AND Almmmate.CodMat = pCodMat 
            EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
        IF ERROR-STATUS:ERROR = YES THEN DO:
            IF LOCKED(Almmmate) THEN UNDO, RETRY.
            CREATE Almmmate.
            ASSIGN
                almmmate.codcia = S-CODCIA
                almmmate.codalm = pCodAlm
                almmmate.codmat = pCodMat
                almmmate.desmat = almmmatg.desmat
                almmmate.undvta = almmmatg.undstk
                almmmate.facequ = 1 NO-ERROR.
            IF ERROR-STATUS:ERROR = YES THEN UNDO RLOOP, RETURN 'ADM-ERROR'.
        END.
        LEAVE.
    END.

    /* Limpiamos stocks por almacen y producto */
    FOR EACH AlmStkAl EXCLUSIVE-LOCK WHERE AlmStkAl.Codcia = S-CODCIA 
        AND AlmStkAl.CodAlm = pCodAlm
        AND AlmStkAl.CodMat = pCodMat 
        AND AlmStkAl.Fecha >= DesdeC ON ERROR UNDO, THROW:
        DELETE AlmStkAl.
    END.          

    FOR EACH AlmDmov USE-INDEX Almd03 NO-LOCK /*EXCLUSIVE-LOCK*/ WHERE Almdmov.Codcia = S-CODCIA 
        AND Almdmov.CodAlm = pCodAlm
        AND Almdmov.CodMat = pCodMat 
        AND Almdmov.FchDoc >= DesdeC ON ERROR UNDO, THROW:
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
/*         IF almdmov.tpocmb = 0 THEN DO:                                        */
/*             FIND LAST gn-tcmb WHERE  gn-tcmb.fecha <= almdmov.fchdoc          */
/*                 USE-INDEX cmb01 NO-LOCK NO-ERROR.                             */
/*             IF AVAIL gn-tcmb THEN DO:                                         */
/*                 IF almdmov.tipmov = "i" THEN almdmov.tpocmb = gn-tcmb.venta.  */
/*                 IF almdmov.tipmov = "s" THEN almdmov.tpocmb = gn-tcmb.compra. */
/*             END.                                                              */
/*         END.                                                                  */
        f-candes = almdmov.candes * almdmov.factor.
        IF Almdmov.tipMov = "I" THEN x-signo = 1.
        IF Almdmov.tipMov = "S" THEN x-signo = -1.
        /***********Inicia Stock x Almacen********************/
        x-Vueltas = 0.
        REPEAT:
            x-Vueltas = x-Vueltas + 1.
            IF x-Vueltas >= 100 THEN UNDO RLOOP, RETURN 'ADM-ERROR'.
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
            AlmStkAl.StkAct = AlmStkAl.StkAct + (f-candes * x-signo).
/*         ASSIGN                                 */
/*             Almdmov.StkSub  = AlmStkAl.StkAct. */
        /***********Fin Stock x Almacen********************/
    END.
    FIND LAST AlmStkAl WHERE AlmStkAl.Codcia = s-Codcia 
        AND AlmStkAl.CodAlm = pCodAlm 
        AND AlmStkAl.CodMat = pCodMat
        AND AlmStkAl.Fecha <= TODAY
        NO-LOCK NO-ERROR.
    IF AVAILABLE AlmStkAl THEN DO:
        ASSIGN
            Almmmate.Stkact = AlmStkAl.StkAct.  
    END.
END.
IF AVAILABLE(AlmStkAl) THEN RELEASE AlmStkAl.
IF AVAILABLE(AlmStkGe) THEN RELEASE AlmStkGe.
IF AVAILABLE(Almmmate) THEN RELEASE Almmmate.
IF AVAILABLE(Almdmov)  THEN RELEASE Almdmov.
RETURN 'OK'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Actualiza-Stock-Ecommerce) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Actualiza-Stock-Ecommerce Procedure 
PROCEDURE Actualiza-Stock-Ecommerce :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF INPUT PARAMETER pCodAlm AS CHAR.                                  
DEF INPUT PARAMETER pCodMat AS CHAR.

IF pCodAlm <> "506" THEN RETURN.    /* Solo almacén 506 e-commerce (lista express) */
FIND Almmmate WHERE Almmmate.CodCia = s-CodCia AND
    Almmmate.CodAlm = pCodAlm AND
    Almmmate.codmat = pCodMat
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE Almmmate THEN RETURN.

REPEAT:
    FIND ecommerce.ec_stock_alm WHERE ecommerce.ec_stock_alm.CodCia = s-CodCia AND 
        ecommerce.ec_stock_alm.CodAlm = pCodAlm AND
        ecommerce.ec_stock_alm.CodMat = pCodMat
        EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
    IF NOT AVAILABLE ecommerce.ec_stock_alm THEN DO:
        IF NOT LOCKED ecommerce.ec_stock_alm THEN DO:
            CREATE ecommerce.ec_stock_alm.
            ASSIGN
                ecommerce.ec_stock_alm.CodAlm = pCodAlm
                ecommerce.ec_stock_alm.CodCia = s-CodCia
                ecommerce.ec_stock_alm.CodMat = pCodMat
                ecommerce.ec_stock_alm.LogDate = TODAY
                ecommerce.ec_stock_alm.LogTime = STRING(TIME,'HH:MM:SS')
                .
        END.
        /*ELSE UNDO, RETRY.*/
        ELSE UNDO, RETURN.
    END.
    ASSIGN
        ecommerce.ec_stock_alm.Reservado = Almmmate.StkComprometido
        ecommerce.ec_stock_alm.StkAct = Almmmate.StkAct.
    RELEASE ecommerce.ec_stock_alm.
    LEAVE.
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

    FOR EACH Almdmov USE-INDEX almd04 NO-LOCK WHERE almdmov.codcia = s-codcia
        AND almdmov.codalm = Almacen.codalm
        AND almdmov.fchdoc >= DesdeC
        AND almdmov.fchdoc <= HastaC,
        FIRST Almacen NO-LOCK WHERE Almacen.codcia = Almdmov.codcia AND
        Almacen.codalm = Almdmov.codalm,
        FIRST gn-divi NO-LOCK WHERE gn-divi.codcia = Almacen.codcia AND
        gn-divi.coddiv = Almacen.coddiv:
        IF Almdmov.TipMov = "I" THEN DO:
            IF NOT (Almdmov.codmov = 02 OR
                     Almdmov.codmov = 03 OR     /* Ic - 24Jun2019, reunion con Max y Daniel, debe coger tbm las transferencias */
                    Almdmov.codmov = 09 OR
                    Almdmov.codmov = 60 OR
                    Almdmov.codmov = 61 OR
                    Almdmov.codmov = 62 OR
                    Almdmov.codmov = 90 OR
                    Almdmov.codmov = 91 OR
                    Almdmov.codmov = 92 OR
                    Almdmov.codmov = 93 OR
                    Almdmov.codmov = 94 OR
                    Almdmov.codmov = 95)
                THEN NEXT.
        END.
        IF Almdmov.TipMov = "S" THEN DO:
            IF NOT (Almdmov.codmov = 02 OR
                     Almdmov.codmov = 03 OR     /* Ic - 24Jun2019, reunion con Max y Daniel, debe coger tbm las transferencias */
                    Almdmov.codmov = 60 OR
                    Almdmov.codmov = 61 OR
                    Almdmov.codmov = 62 OR
                    Almdmov.codmov = 90 OR
                    Almdmov.codmov = 91 OR
                    Almdmov.codmov = 92 OR
                    Almdmov.codmov = 93 OR
                    Almdmov.codmov = 94 OR
                    Almdmov.codmov = 95)
                THEN NEXT.
        END.
        IF GN-DIVI.CanalVenta <> "MIN" AND (Almdmov.CodMov = 02 OR Almdmov.codmov = 09) THEN NEXT.

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
        &FlgDB1 = TRUE
        &FlgDB2 = TRUE
        &FlgDB3 = TRUE
        &FlgDB4 = TRUE
        &FlgDB5 = TRUE
        &FlgDB6 = TRUE
        &FlgDB7 = TRUE
        &FlgDB8 = TRUE
        &FlgDB9 = TRUE
        &FlgDB10 = TRUE
        &FlgDB11 = TRUE
        &FlgDB12 = TRUE
        &FlgDB13 = TRUE
        &FlgDB14 = TRUE
        &FlgDB15 = TRUE
        &FlgDB16 = TRUE
        &FlgDB17 = "NOT x-CanalVenta = 'MIN'"
        &FlgDB18 = TRUE
        &FlgDB19 = TRUE
        &FlgDB20 = "NOT x-CanalVenta = 'MIN'"   /*"NOT LOOKUP(almacen.coddiv, '00023,00027,00501,00502,00503,00504,00510') > 0"    /* SERVIDOR UTILEX */*/
        &FlgDB30 = TRUE
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

IF pCodAlm <> "506" THEN RETURN.    /* Solo almacén 506 e-commerce (lista express) */

DEF VAR pComprometido AS DEC NO-UNDO.

DEF BUFFER B-MATE  FOR Almmmate.

LOOPGENERAL:
DO:
    pComprometido = 0.
    RUN ./gn/stock-comprometido-v2.p (pCodMat, pCodAlm, NO, OUTPUT pComprometido).
    /* Actualizamos Stock Comprometido */
    IF CAN-FIND(FIRST B-MATE WHERE B-MATE.codcia = s-CodCia AND 
                B-MATE.codalm = pCodAlm AND 
                B-MATE.codmat = pCodMat NO-LOCK) 
        THEN DO:
        {lib\lock-genericov3.i &Tabla="B-MATE" ~
            &Alcance="FIRST" ~
            &Condicion="B-MATE.codcia = s-CodCia AND B-MATE.codalm = pCodAlm AND B-MATE.codmat = pCodMat" ~
            &Bloqueo="EXCLUSIVE-LOCK NO-ERROR NO-WAIT" ~
            &Accion="RETRY" ~
            &Mensaje="NO" ~
            &TipoError="RETURN" ~
            &Intentos="10" ~
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

