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
         HEIGHT             = 4.73
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

/* 
RHC 16/10/2012 
SE RECALCULA EL STOCK CADA 5 MINUTOS YA QUE LOS MOVIMIENTOS 
DE VENTAS DE REPLICAN DE LAS TIENDAS AL SERVIDOR CENTRAL 
*/
ASSIGN
    DesdeC = TODAY
    HastaC = TODAY.

/* 1ro. cargamos productos por almacen */
DEF TEMP-TABLE t-matg LIKE almmmatg.

FOR EACH Almacen NO-LOCK WHERE Almacen.codcia = s-codcia
    AND LOOKUP(Almacen.coddiv, '00023,00027,00501,00502,00503,00504,00505,00506,00507,~
               00508,00509,00065') > 0:
    FOR EACH almdmov USE-INDEX almd04 NO-LOCK WHERE almdmov.codcia = s-codcia
        AND almdmov.codalm = almacen.codalm
        AND almdmov.fchdoc >= desdec
        AND almdmov.fchdoc <= hastac:
        FIND FIRST t-matg WHERE t-matg.codcia = s-codcia
            AND t-matg.codmat = almdmov.codmat
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE t-matg THEN DO:
            CREATE t-matg.
            ASSIGN
                t-matg.codcia = s-codcia
                t-matg.codmat = almdmov.codmat.
        END.
    END.
END.

/* 2do actualizamos kardex */
FOR EACH t-matg NO-LOCK:
    DISPLAY t-matg.codmat WITH STREAM-IO NO-BOX.
    RUN Actualiza-Producto (t-matg.codmat).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-Actualiza-Producto) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Actualiza-Producto Procedure 
PROCEDURE Actualiza-Producto :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF INPUT PARAMETER pCodMat AS CHAR.

DEF VAR f-candes AS DECI NO-UNDO.

FOR EACH AlmStkAl WHERE AlmStkAl.Codcia = S-CODCIA 
    AND AlmStkAl.CodMat = pCodMat 
    AND AlmStkAl.Fecha >= DesdeC:
    DELETE AlmStkAl.
END.                        
FOR EACH AlmStkGe WHERE AlmStkGe.Codcia = S-CODCIA 
    AND AlmStkGe.CodMat = pCodMat 
    AND AlmStkGe.Fecha >= DesdeC:
    DELETE AlmStkGe.
END.
FOR EACH Almmmate WHERE Almmmate.Codcia = S-CODCIA 
    AND Almmmate.CodMat = pCodMat 
    USE-INDEX Mate03:
    Almmmate.StkAct = 0.
END.
ASSIGN
    x-stkgen = 0
    x-ctomed = 0.
FIND LAST AlmStkGe WHERE AlmStkGe.Codcia = S-CODCIA 
    AND AlmStkGe.CodMat = pCodMat 
    AND AlmStkGe.Fecha  < DesdeC
    NO-LOCK NO-ERROR.
IF AVAILABLE AlmStkGe THEN DO:
    ASSIGN
        x-stkgen = AlmStkGe.StkAct
        x-ctomed = AlmStkGe.CtoUni.
END.
/*************Ordenes Movimiento por Producto y Fecha *******************/     
FOR EACH AlmDmov USE-INDEX Almd02 WHERE Almdmov.Codcia = S-CODCIA 
    AND Almdmov.CodMat = pCodMat 
    AND Almdmov.FchDoc >= DesdeC: 
    FIND FIRST almacen where almacen.codcia = almdmov.codcia
        AND almacen.codalm = almdmov.codalm  
        USE-INDEX alm01 NO-LOCK NO-ERROR.
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
    /***********Fin Stock x Almacen********************/
    FIND FIRST almtmov OF almdmov USE-INDEX mov01 NO-LOCK NO-ERROR.
    /***********Inicia Stock x Compañia********************/       
    REPEAT:
        FIND LAST AlmStkGe WHERE AlmStkGe.Codcia = S-CODCIA 
            AND AlmStkGe.CodMat = Almdmov.CodMat 
            AND AlmstkGe.Fecha  = Almdmov.FchDoc
            EXCLUSIVE NO-ERROR NO-WAIT.
        IF NOT AVAILABLE AlmStkGe THEN DO:      
            IF NOT LOCKED AlmStkGe THEN DO:
                CREATE AlmStkGe.
                ASSIGN
                    AlmStkGe.Codcia = S-CODCIA 
                    AlmStkGe.CodMat = Almdmov.CodMat
                    AlmStkGe.Fecha  = Almdmov.FchDoc.
                FIND LAST B-STKGE WHERE B-STKGE.Codcia = S-CODCIA 
                    AND B-STKGE.CodMat = Almdmov.CodMat 
                    AND B-STKGE.Fecha  < Almdmov.FchDoc
                    NO-ERROR.
                IF AVAILABLE B-STKGE THEN AlmStkGe.StkAct = AlmStkGe.StkAct + B-STKGE.StkAct.                         
                LEAVE.
            END.
            ELSE UNDO, RETRY.
        END.
        LEAVE.
    END.
    ASSIGN
        AlmStkGe.StkAct = AlmStkGe.StkAct + f-candes * x-signo * x-factor.
    /***********Fin Stock x Compañia********************/       
    /***********Inicia Calculo del Costo Promedio********************/       
    IF AVAILABLE AlmtMov THEN DO:
        IF Almtmov.TipMov = "I" AND x-Factor <> 0 THEN DO:
            x-cto = 0.
            IF Almtmov.TpoCto = 0 OR Almtmov.TpoCto = 1 THEN DO:
                IF almdmov.codmon = 1 THEN x-cto = Almdmov.Preuni.
                IF almdmov.codmon = 2 THEN x-cto = Almdmov.Preuni * Almdmov.TpoCmb.
                x-ctomed = ((x-cto * f-candes) + (x-ctomed * (AlmStkGe.StkAct - f-candes * x-signo))) / ( AlmStkGe.StkAct).
                IF x-ctomed < 0 THEN x-ctomed = x-cto.
                IF AlmStkGe.StkAct < f-candes AND AlmStkGe.StkAct >= 0 THEN x-ctomed = x-cto.
            END.
            ELSE DO:
            END.
        END.
        ASSIGN
            Almdmov.VctoMn1 = x-ctomed
            Almdmov.StkSub  = AlmStkAl.StkAct
            Almdmov.StkAct  = AlmStkGe.StkAct
            AlmStkGe.CtoUni = x-ctomed
            AlmStkAl.CtoUni = x-ctomed.
    END.
    ELSE DO:
        Almdmov.VctoMn1 = 0.
    END.
    /***********Fin    Calculo del Costo Promedio********************/       
END.
/*************                            *******************/     
FOR EACH Almacen NO-LOCK WHERE Almacen.Codcia = S-CODCIA:
    FIND LAST AlmStkAl WHERE AlmStkAl.Codcia = Almacen.Codcia 
        AND AlmStkAl.CodAlm = Almacen.CodAlm 
        AND AlmStkAl.CodMat = pCodMat
        NO-LOCK NO-ERROR.
    IF AVAILABLE AlmStkAl THEN DO:
        /* bloqueamos si o si Almmmate */
        REPEAT:
            FIND FIRST Almmmate WHERE Almmmate.Codcia = S-CODCIA 
                AND Almmmate.CodAlm = Almacen.CodAlm 
                AND Almmmate.CodMat = pCodMat
                EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
            IF NOT AVAILABLE Almmmate THEN DO:
                IF NOT LOCKED Almmmate THEN DO:
                    CREATE Almmmate.
                    ASSIGN
                        almmmate.codcia = S-CODCIA
                        almmmate.codalm = almacen.codalm
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
END.

IF AVAILABLE(AlmStkAl) THEN RELEASE AlmStkAl.
IF AVAILABLE(AlmStkGe) THEN RELEASE AlmStkGe.
IF AVAILABLE(Almmmate) THEN RELEASE Almmmate.
IF AVAILABLE(Almdmov)  THEN RELEASE Almdmov.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

