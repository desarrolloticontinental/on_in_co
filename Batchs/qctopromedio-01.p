&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : qctopromedio
    Purpose     : calculo del kardex por producto

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* NO Triggers */
disable triggers for load of almcmov.
disable triggers for load of almdmov.
disable triggers for load of almmmate.
disable triggers for load of almmmatg.
disable triggers for load of almstkge.
disable triggers for load of almstkal.
disable triggers for load of almcieal.
DISABLE TRIGGERS FOR LOAD OF Almtfami.
DISABLE TRIGGERS FOR LOAD OF lg-cocmp.
DISABLE TRIGGERS FOR LOAD OF lg-docmp.

def var s-codcia as inte init 001.

def buffer almdmov2 for almdmov.
def buffer almacen2 for almacen.
def buffer almcmov2 for almcmov.
def buffer ccbcdocu2 for ccbcdocu.

DEF TEMP-TABLE t-cocmp LIKE lg-cocmp 
    FIELD pRowid AS ROWID
    INDEX Llave01 AS PRIMARY codcia tpodoc nrodoc.

DEF TEMP-TABLE t-docmp LIKE lg-docmp 
    FIELD pRowid AS ROWID
    INDEX llave01 AS PRIMARY codcia tpodoc nrodoc codmat.


def var f-candes as deci no-undo.

def var desdec as char init "" format "x(6)".
def var hastac as char init "" format "x(6)".
                        
def var i-fchdoc as date format '99/99/9999' init ?.

def buffer B-STKAL FOR AlmStkAl.
def buffer B-STKGE FOR AlmStkGe.
def buffer b-mov   FOR Almtmov.
DEF BUFFER B-MATG  FOR Almmmatg.

def var x-signo  as deci no-undo.  
def var x-newcto as deci no-undo.
def var x-ctomed as deci no-undo.
def var x-stkgen as deci no-undo.
def var x-cto    as deci no-undo.
def var x-movtrf as deci no-undo.
def var x-factor as inte no-undo.
DEF VAR f-Factor AS DEC NO-UNDO.        /* Factor de Equivalencia */
DEF VAR x-SaldoActual AS DEC NO-UNDO.

IF desdec = '' then desdec = '000001'.
IF hastac = '' then hastac = '999999'.

/* Fecha de Cierre */
DEF VAR dFchCie AS DATE NO-UNDO.

IF DAY(TODAY) < 15 
THEN dFchCie = TODAY - DAY(TODAY).
ELSE dFchCie = TODAY.
dFchCie = dFchCie - DAY(dFchCie) + 1. 
i-FchDoc = dFchCie.        /* OJO */
i-FchDoc = ADD-INTERVAL(dFchCie, -1, "months").

/* BUSCAMOS FECHA DE ULTIMA MODIFICACION */
/* FIND Almcfggn WHERE Almcfggn.codcia = s-codcia NO-LOCK NO-ERROR. */
/* IF AVAILABLE Almcfggn AND AlmCfgGn.FchModMin <> ?                */
/*     THEN i-FchDoc = MINIMUM(i-FchDoc, AlmCfgGn.FchModMin).       */

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
         HEIGHT             = 11.81
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* SE VA A DIVIDIR EN DOS PARTE
LA 1ra. REVISARÁ SI TODOS LOS MOVIMIENTOS DE VENTAS ESTAN EN EL KARDEX
LA 2da. REGENERAR EL KARDEX */

/* 1ro. */
PUT UNFORMATTED 'Verifica Ventas ' STRING(DATETIME(TODAY, MTIME)) SKIP.
RUN Verifica-Ventas-Kardex.

/* 2do. */
PUT UNFORMATTED 'Regenera Kardex ' STRING(DATETIME(TODAY, MTIME)) SKIP.
RUN Regenera-Kardex.

/* 3ro. */
PUT UNFORMATTED 'Cierre Diario ' STRING(DATETIME(TODAY, MTIME)) SKIP.
RUN Cierre-Diario.

/* 4to */
PUT UNFORMATTED 'Valoriza por Linea ' STRING(DATETIME(TODAY, MTIME)) SKIP.
/*RUN Valorizado-por-linea.*/

/* 5to */
PUT UNFORMATTED 'Limpia Fecha ' STRING(DATETIME(TODAY, MTIME)) SKIP.
RUN Limpia-Fecha.

/* 6to */
/*RUN Minimos.*/
/* PUT UNFORMATTED 'Ajusta OC ' STRING(DATETIME(TODAY, MTIME)) SKIP. */
/* RUN Ajusta-OC.                                                    */

QUIT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-Act_Alm) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Act_Alm Procedure 
PROCEDURE Act_Alm :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR i AS INTEGER INITIAL 1 NO-UNDO.
PRINCIPAL:
DO TRANSACTION ON ERROR UNDO, RETURN "ADM-ERROR" ON STOP UNDO, RETURN 'ADM-ERROR':
    FOR EACH Ccbddocu OF Ccbcdocu NO-LOCK WHERE Ccbddocu.implin >= 0
        BREAK BY Ccbddocu.AlmDes:
        IF FIRST-OF(Ccbddocu.AlmDes) THEN DO:
            /* CABECERA */
/*             REPEAT:                                                      */
/*                 FIND Almacen WHERE Almacen.CodCia = Ccbddocu.codcia      */
/*                     AND Almacen.CodAlm = Ccbddocu.almdes       /* OJO */ */
/*                     EXCLUSIVE-LOCK NO-ERROR NO-WAIT.                     */
/*                 IF NOT AVAILABLE Almacen THEN DO:                        */
/*                     IF LOCKED Almacen THEN UNDO, RETRY.                  */
/*                     ELSE UNDO PRINCIPAL, RETURN 'ADM-ERROR'.             */
/*                 END.                                                     */
/*                 LEAVE.                                                   */
/*             END.                                                         */
            CREATE almcmov.
            ASSIGN Almcmov.CodCia  = CcbDDocu.CodCia 
                   Almcmov.CodAlm  = CcbDDocu.AlmDes
                   Almcmov.TipMov  = "S"
                   Almcmov.CodMov  = 02     /* CcbCDocu.CodMov */
                   /*Almcmov.NroSer  = 0 */
                   /*Almcmov.NroDoc  = Almacen.CorrSal */
                   /*Almacen.CorrSal = Almacen.CorrSal + 1*/
                   /* RHC 15/10/2012 para evitar duplicados en la llave */
                   Almcmov.NroSer  = (IF Ccbcdocu.coddoc = 'TCK' THEN INTEGER(SUBSTRING(Ccbcdocu.nrodoc,1,3))
                       ELSE IF Ccbcdocu.coddoc = 'FAC' THEN INTEGER(SUBSTRING(Ccbcdocu.nrodoc,1,3)) + 011
                           ELSE INTEGER(SUBSTRING(Ccbcdocu.nrodoc,1,3)) + 012)
                   Almcmov.NroDoc = INTEGER(SUBSTRING(Ccbcdocu.nrodoc,4))
                   Almcmov.FchDoc  = CcbCDocu.FchDoc
                   Almcmov.HorSal  = STRING(TIME, "HH:MM:SS")
                   Almcmov.CodVen  = ccbcdocu.CodVen
                   Almcmov.CodCli  = ccbcdocu.CodCli
                   Almcmov.Nomref  = ccbcdocu.NomCli
                   Almcmov.CodRef  = ccbcdocu.CodDoc
                   Almcmov.NroRef  = ccbcdocu.nrodoc
                   Almcmov.NroRf1  = STRING(CcbCDocu.CodDoc, 'x(3)') + CcbCDocu.NroDoc
                   Almcmov.NroRf2  = CcbCDocu.CodPed + CcbCDocu.NroPed
                   Almcmov.usuario = Ccbcdocu.usuario.
        END.
        /* DETALLE */
        CREATE Almdmov.
        ASSIGN Almdmov.CodCia = Almcmov.CodCia
               Almdmov.CodAlm = Almcmov.CodAlm
               Almdmov.CodMov = Almcmov.CodMov 
               Almdmov.NroSer = almcmov.nroser
               Almdmov.NroDoc = almcmov.nrodoc
               Almdmov.AftIgv = ccbddocu.aftigv
               Almdmov.AftIsc = ccbddocu.aftisc
               Almdmov.CanDes = ccbddocu.candes
               Almdmov.codmat = ccbddocu.codmat
               Almdmov.CodMon = ccbcdocu.codmon
               Almdmov.CodUnd = ccbddocu.undvta
               Almdmov.Factor = ccbddocu.factor
               Almdmov.FchDoc = CcbCDocu.FchDoc
               Almdmov.ImpDto = ccbddocu.impdto
               Almdmov.ImpIgv = ccbddocu.impigv
               Almdmov.ImpIsc = ccbddocu.impisc
               Almdmov.ImpLin = ccbddocu.implin
               Almdmov.NroItm = i
               Almdmov.PorDto = ccbddocu.pordto
               Almdmov.PreBas = ccbddocu.prebas
               Almdmov.PreUni = ccbddocu.preuni
               Almdmov.TipMov = "S"
               Almdmov.TpoCmb = ccbcdocu.tpocmb
               Almcmov.TotItm = i
               Almdmov.HraDoc = Almcmov.HorSal
               i = i + 1.
    END.
    IF AVAILABLE(Almacen) THEN RELEASE Almacen.
    IF AVAILABLE(Almcmov) THEN RELEASE Almcmov.
    IF AVAILABLE(Almdmov) THEN RELEASE Almdmov.
END.
RETURN 'OK'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Ajusta-OC) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Ajusta-OC Procedure 
PROCEDURE Ajusta-OC :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR x-Fecha AS DATE NO-UNDO.

x-Fecha = TODAY - 180.  /* Retrocedemos 6 meses */

EMPTY TEMP-TABLE t-cocmp.
FOR EACH lg-cocmp NO-LOCK WHERE lg-cocmp.codcia = s-codcia
    AND lg-cocmp.tpodoc = "N"       /* COMPRAS NACIONALES */
    AND LOOKUP(lg-cocmp.flgsit, 'P,T') > 0      /* SOLO Aprobadas y Atendidas */
    AND lg-cocmp.fchdoc >= x-Fecha:
    CREATE t-cocmp.
    BUFFER-COPY lg-cocmp
        TO t-cocmp
        ASSIGN t-cocmp.prowid = ROWID(lg-cocmp).
END.
EMPTY TEMP-TABLE t-docmp.
FOR EACH t-cocmp:
    FOR EACH lg-docmp NO-LOCK WHERE lg-docmp.codcia = s-codcia
        AND lg-docmp.coddiv = t-cocmp.coddiv
        AND lg-docmp.tpodoc = t-cocmp.tpodoc
        AND lg-docmp.nrodoc = t-cocmp.nrodoc:
        CREATE t-docmp.
        BUFFER-COPY lg-docmp
            TO t-docmp
            ASSIGN t-docmp.prowid = ROWID(lg-docmp).
    END.
END.

FOR EACH t-docmp:
    t-docmp.canaten = 0.
END.

FOR EACH almacen NO-LOCK WHERE almacen.codcia = s-codcia,
    EACH almcmov NO-LOCK WHERE almcmov.codcia = s-codcia
    AND almcmov.codalm = almacen.codalm
    AND almcmov.tipmov = 'I'
    AND almcmov.codmov = 02     /* COMPRAS NACIONALES */
    AND almcmov.flgest <> 'A'
    AND almcmov.fchdoc >= x-Fecha,
    EACH almdmov OF almcmov NO-LOCK:
    FIND t-cocmp WHERE t-cocmp.codcia = s-codcia
        AND t-cocmp.tpodoc = 'N'
        AND t-cocmp.nrodoc = INTEGER(almcmov.nrorf1)
        AND t-cocmp.codalm = almcmov.codalm
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE t-cocmp THEN NEXT.
    FIND FIRST t-docmp WHERE t-docmp.codcia = s-codcia
        AND t-docmp.tpodoc = 'N'
        AND t-docmp.nrodoc = INTEGER(almcmov.nrorf1)
        AND t-docmp.codmat = almdmov.codmat
        NO-ERROR.
    IF NOT AVAILABLE t-docmp THEN NEXT.
    ASSIGN
        t-docmp.canaten = t-docmp.canaten + almdmov.candes.
END.
/* buscamos errores */
FOR EACH t-cocmp WHERE t-cocmp.codcia = s-codcia:
    IF CAN-FIND(FIRST t-docmp WHERE t-docmp.codcia = s-codcia
                AND t-docmp.tpodoc = t-cocmp.tpodoc
                AND t-docmp.coddiv = t-cocmp.coddiv
                AND t-docmp.nrodoc = t-cocmp.nrodoc
                AND t-docmp.canpedi > t-docmp.canaten
                NO-LOCK)
        THEN t-cocmp.flgsit = "P".
END.
FOR EACH t-cocmp:
    FIND lg-cocmp WHERE ROWID(lg-cocmp) = t-cocmp.prowid.
    IF lg-cocmp.flgsit <> t-cocmp.flgsit THEN lg-cocmp.flgsit = t-cocmp.flgsit.
    FOR EACH t-docmp WHERE t-docmp.codcia = s-codcia
        AND t-docmp.coddiv = t-cocmp.coddiv
        AND t-docmp.tpodoc = t-cocmp.tpodoc
        AND t-docmp.nrodoc = t-cocmp.nrodoc:
        FIND lg-docmp WHERE ROWID(lg-docmp) = t-docmp.prowid.
        IF t-docmp.canaten <> lg-docmp.canate THEN lg-docmp.canaten = t-docmp.canate.
    END.
END.
IF AVAILABLE(lg-cocmp) THEN RELEASE lg-cocmp.
IF AVAILABLE(lg-docmp) THEN RELEASE lg-docmp.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Borra-Historicos) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Borra-Historicos Procedure 
PROCEDURE Borra-Historicos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DO TRANSACTION ON ERROR UNDO, RETURN 'ADM-ERROR' ON STOP UNDO, RETURN 'ADM-ERROR':
    /* limpia historico de stocks por almacen */
    FOR EACH AlmStkAl WHERE AlmStkAl.Codcia = S-CODCIA 
        AND AlmStkAl.CodMat = Almmmatg.CodMat 
        AND AlmStkAl.Fecha >= I-FchDoc:
        DELETE AlmStkAl.
    END.
    /* limpia historico de stock contable */
    FOR EACH AlmStkGe WHERE AlmStkGe.Codcia = S-CODCIA 
        AND AlmStkGe.CodMat = Almmmatg.CodMat 
        AND AlmStkGe.Fecha >= I-FchDoc:
        DELETE AlmStkGe.
    END.                        
    /* limpia stock por almacen */
    FOR EACH Almmmate WHERE Almmmate.Codcia = S-CODCIA 
        AND Almmmate.CodMat = Almmmatg.CodMat 
        AND (Almmmate.StkAct <> 0 OR Almmmate.StkComprometido <> 0)
        USE-INDEX Mate03:
        Almmmate.StkAct = 0.
        Almmmate.StkComprometido = 0.
        /* Stock Comprometido */
        {gn\comprometido-facdpedi.i &pCodCia=almmmate.codcia ~
            &pCodAlm=almmmate.codalm ~
            &pCodMat=almmmate.codmat}
    END.
END.

RETURN 'OK'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Cierre-Diario) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Cierre-Diario Procedure 
PROCEDURE Cierre-Diario :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FOR EACH Almacen NO-LOCK WHERE Almacen.codcia = s-codcia:
    FIND AlmCieAl WHERE Almcieal.codcia = s-codcia
        AND Almcieal.codalm = Almacen.codalm
        AND Almcieal.fchcie = (TODAY - 1)
        EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE Almcieal THEN DO:
        CREATE Almcieal.
        ASSIGN
            Almcieal.codcia = s-codcia
            Almcieal.codalm = Almacen.codalm
            Almcieal.fchcie = TODAY - 1.
    END.
    ASSIGN
        Almcieal.flgcie = Yes
        Almcieal.usucierr = 'AUTOMATICO'.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Kardex) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Kardex Procedure 
PROCEDURE Kardex :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    ASSIGN
        x-stkgen = 0
        x-ctomed = 0.
        
    FIND LAST AlmStkGe WHERE AlmStkGe.Codcia = S-CODCIA 
        AND AlmStkGe.CodMat = Almmmatg.CodMat 
        AND AlmStkGe.Fecha  < I-Fchdoc
        NO-LOCK NO-ERROR.
    IF AVAILABLE AlmStkGe 
        THEN ASSIGN
                x-stkgen = AlmStkGe.StkAct
                x-ctomed = AlmStkGe.CtoUni.
    /*************Ordenes Movimiento por Producto y Fecha *******************/     
    FOR EACH AlmDmov USE-INDEX Almd02 EXCLUSIVE-LOCK WHERE Almdmov.Codcia = S-CODCIA 
        AND Almdmov.CodMat = Almmmatg.CodMat 
        AND Almdmov.FchDoc >= I-FchDoc
        ON ERROR UNDO, RETURN 'ADM-ERROR' ON STOP UNDO, RETURN 'ADM-ERROR':
        /* VERIFICAR SI TIENE CABECERA DE MOVIMIENTO */
        FIND FIRST Almcmov OF Almdmov NO-LOCK NO-ERROR.
        IF NOT AVAILABLE Almcmov THEN DO:
            DELETE Almdmov.
            NEXT.
        END.
        /* ***************************************** */
        /* RHC 11.01.10 ARREGLAMOS EL FACTOR */
        FIND Almtconv WHERE Almtconv.CodUnid = Almmmatg.UndBas 
            AND Almtconv.Codalter = Almdmov.codund
            NO-LOCK NO-ERROR.
        f-Factor = 1.
        IF AVAILABLE Almtconv AND Almmmatg.FacEqu <> 0 THEN f-Factor = Almtconv.Equival / Almmmatg.FacEqu.
        IF f-Factor = ? THEN f-Factor = 1.
        Almdmov.Factor = f-Factor.
        /* ********************************** */

        FIND FIRST almacen WHERE almacen.codcia = almdmov.codcia
            AND almacen.codalm = almdmov.codalm  
            NO-LOCK NO-ERROR.
       /* RHC 06.05.04 FILTRAMOS POR CAMPO MOVTRF */
       FIND Almtmovm WHERE almtmovm.codcia = almdmov.codcia
           AND almtmovm.tipmov = almdmov.tipmov
           AND almtmovm.codmov = almdmov.codmov
           NO-LOCK NO-ERROR.
       IF AVAILABLE almtmovm AND almtmovm.movtrf = YES
       THEN x-Factor = 0.
       ELSE x-Factor = 1.

       /* Almacenes que NO son propios */
       IF AVAILABLE Almacen AND Almacen.FlgRep = NO THEN x-Factor = 0.

       /* RHC 09.09.04 EL ALMACEN DE CONSIGN. NO TIENE MOVIMIENTO CONTABLE  */
       IF AVAILABLE Almacen AND Almacen.AlmCsg = YES THEN x-Factor = 0.

       IF almdmov.tpocmb = 0 then do:
            find last gn-tcmb where  gn-tcmb.fecha <= almdmov.fchdoc
                              use-index cmb01 no-lock no-error.
            if avail gn-tcmb then do:
                if almdmov.tipmov = "i" then almdmov.tpocmb = gn-tcmb.venta.
                if almdmov.tipmov = "s" then almdmov.tpocmb = gn-tcmb.compra. 
            end.
       end.

       f-candes = almdmov.candes * almdmov.factor.
       IF Almdmov.tipMov = "I" THEN x-signo = 1.
       IF Almdmov.tipMov = "S" THEN x-signo = -1.
       
       /***********Inicia Stock x Almacen********************/
       FIND AlmStkAl WHERE AlmStkAl.Codcia = S-CODCIA 
           AND AlmStkAl.CodAlm = Almdmov.CodAlm 
           AND AlmStkAl.CodMat = Almdmov.CodMat 
           AND AlmstkAl.Fecha  = Almdmov.FchDoc
           NO-LOCK NO-ERROR.
       IF NOT AVAILABLE AlmStkAl THEN DO:      
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
              NO-LOCK NO-ERROR.
          IF AVAILABLE B-STKAL THEN AlmStkAl.StkAct = AlmStkAl.StkAct + B-STKAL.StkAct.                         
       END.
       FIND CURRENT AlmStkAl EXCLUSIVE-LOCK NO-ERROR.
       ASSIGN
           AlmStkAl.StkAct = AlmStkAl.StkAct + f-candes * x-signo.
       /***********Fin Stock x Almacen********************/

       FIND FIRST almtmov OF almdmov USE-INDEX mov01 NO-LOCK NO-ERROR.
       /***********Inicia Stock x Compañia********************/       
       FIND AlmStkGe WHERE AlmStkGe.Codcia = S-CODCIA 
           AND AlmStkGe.CodMat = Almdmov.CodMat 
           AND AlmstkGe.Fecha  = Almdmov.FchDoc
           NO-LOCK NO-ERROR.
       IF NOT AVAILABLE AlmStkGe THEN DO:      
          CREATE AlmStkGe.
          ASSIGN
              AlmStkGe.Codcia = S-CODCIA 
              AlmStkGe.CodMat = Almdmov.CodMat
              AlmStkGe.Fecha  = Almdmov.FchDoc.
          FIND LAST B-STKGE WHERE B-STKGE.Codcia = S-CODCIA 
              AND B-STKGE.CodMat = Almdmov.CodMat 
              AND B-STKGE.Fecha  < Almdmov.FchDoc
              NO-LOCK NO-ERROR.
          IF AVAILABLE B-STKGE THEN AlmStkGe.StkAct = AlmStkGe.StkAct + B-STKGE.StkAct.                         
       END.
       FIND CURRENT AlmStkGe EXCLUSIVE-LOCK NO-ERROR.
       ASSIGN
           AlmStkGe.StkAct = AlmStkGe.StkAct + (f-candes * x-signo * x-factor).
       /***********Fin Stock x Compañia********************/       
       /***********Inicia Calculo del Costo Promedio********************/       
       x-SaldoActual = AlmStkGe.StkAct - (f-candes * x-signo * x-factor).  /* Saldo en el Kardex en ese momento */
       IF AVAILABLE AlmtMov THEN DO:
          IF almtmov.TipMov = "I" AND x-Factor <> 0 THEN DO:
             x-cto = 0.
             IF Almtmov.TpoCto = 0 OR Almtmov.TpoCto = 1 THEN DO:
                IF almdmov.codmon = 1 THEN x-cto = Almdmov.Preuni / Almdmov.factor.
                IF almdmov.codmon = 2 THEN x-cto = Almdmov.Preuni / Almdmov.factor * Almdmov.TpoCmb.
                /* RHC  22/01/2013 CASO DE "SOLO VALORES" */
                IF Almtmovm.MovVal = NO THEN DO:
                   /* RHC 05/11/2015 */
                   IF x-SaldoActual > 0 AND AlmStkGe.StkAct > 0 THEN
                       x-ctomed = ((x-cto * f-candes) + (x-ctomed * x-SaldoActual)) / ( AlmStkGe.StkAct).
                   ELSE
                       x-ctomed = x-cto.
/*                    x-ctomed = ((x-cto * f-candes) + (x-ctomed * (AlmStkGe.StkAct - f-candes * x-signo))) / ( AlmStkGe.StkAct). */
                   /* PARCHE 12.01.10 */
                   IF Almdmov.ImpCto = 0 THEN Almdmov.ImpCto = Almdmov.CanDes * Almdmov.PreUni.
                END.
                ELSE DO:
                    x-ctomed = ( x-cto + (x-ctomed * AlmStkGe.StkAct) ) / AlmStkGe.StkAct.
                    /* PARCHE 12.01.10 */
                    IF Almdmov.ImpCto = 0 THEN Almdmov.ImpCto = Almdmov.PreUni.
                    /* ****** */
                END.
                IF x-ctomed < 0 THEN x-ctomed = x-cto.
                /*IF AlmStkGe.StkAct < f-candes AND AlmStkGe.StkAct >= 0 THEN x-ctomed = x-cto.*/
             END.
          END.
       /* RHC 08.02.2013 SALIDA por Devolución de Compras a precio de la O/C */
          IF Almtmov.TipMov = "S" AND x-Factor <> 0 AND Almdmov.FchDoc >= 12/01/2012
              THEN DO:
              IF ( Almtmov.MovCmp = YES AND Almtmov.CodMov = 09 )
                  OR ( Almtmov.MovCmp = YES AND Almtmov.CodMov <> 09 AND Almdmov.FchDoc > DATE(03,01,2016) )
                  THEN DO:
                  x-cto = 0.
                  IF almdmov.codmon = 1 THEN x-cto = Almdmov.Preuni / Almdmov.factor.
                  IF almdmov.codmon = 2 THEN x-cto = Almdmov.Preuni / Almdmov.factor * Almdmov.TpoCmb.
                  /* RHC  22/01/2013 CASO DE "SOLO VALORES" */
                  IF Almtmovm.MovVal = NO THEN DO:
                      IF x-SaldoActual > 0 AND AlmStkGe.StkAct > 0 THEN
                          x-ctomed = ( (x-ctomed * x-SaldoActual) - (x-cto * f-candes) ) / ( AlmStkGe.StkAct).
                      ELSE
                          x-ctomed = x-cto.

                      /* RHC 17/11/2015 Solicitado por Roxana Elera */
/*                       x-ctomed = ( (x-ctomed * (AlmStkGe.StkAct - f-candes * x-signo)) - (x-cto * f-candes) ) / ( AlmStkGe.StkAct). */
                      IF Almdmov.ImpCto = 0 THEN Almdmov.ImpCto = Almdmov.CanDes * Almdmov.PreUni.
                  END.
                  ELSE DO:
                      x-ctomed = ( (x-ctomed * AlmStkGe.StkAct * x-signo) - x-cto ) / AlmStkGe.StkAct.
                      IF Almdmov.ImpCto = 0 THEN Almdmov.ImpCto = Almdmov.PreUni.
                  END.
                  IF x-ctomed < 0 THEN x-ctomed = x-cto.
                  /*IF AlmStkGe.StkAct < f-candes AND AlmStkGe.StkAct >= 0 THEN x-ctomed = x-cto.*/
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
     RETURN 'OK'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Limpia-Fecha) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Limpia-Fecha Procedure 
PROCEDURE Limpia-Fecha :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FIND Almcfggn WHERE Almcfggn.codcia = s-codcia EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
IF AVAILABLE Almcfggn THEN  AlmCfgGn.FchModMin = ?.
RELEASE Almcfggn.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Margenes) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Margenes Procedure 
PROCEDURE Margenes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE VARIABLE F-PreVta-A AS DECIMAL NO-UNDO.
  DEFINE VARIABLE F-PreVta-B AS DECIMAL NO-UNDO.
  DEFINE VARIABLE F-PreVta-C AS DECIMAL NO-UNDO.
  DEFINE VARIABLE X-CTOUND AS DECIMAL NO-UNDO.
  DEFINE VARIABLE F-MrgUti-A AS DECIMAL NO-UNDO.
  DEFINE VARIABLE F-MrgUti-B AS DECIMAL NO-UNDO.
  DEFINE VARIABLE F-MrgUti-C AS DECIMAL NO-UNDO.
  DEFINE VARIABLE f-MonVta LIKE Almmmatg.MonVta NO-UNDO.
  DEFINE VARIABLE f-MrgLis AS DECIMAL NO-UNDO.

  IF Almmmatg.CtoTot <= 0 THEN RETURN.

  /**** MARGEN PRECIO DE LISTA ****/
  /*ASSIGN Almmmatg.MrgUti = ((Almmmatg.Prevta[1] / Almmmatg.Ctotot) - 1 ) * 100. */
  ASSIGN f-MrgLis = ((Almmmatg.Prevta[1] / Almmmatg.Ctotot) - 1 ) * 100.
  /****   MARGEN A   ****/
  ASSIGN
      F-PreVta-A = Almmmatg.Prevta[2]
      X-CTOUND = Almmmatg.CtoTot
      F-FACTOR = 1
      F-MrgUti-A = 0.    
  IF Almmmatg.UndA <> "" THEN DO:
      FIND Almtconv WHERE Almtconv.CodUnid = Almmmatg.UndBas
          AND  Almtconv.Codalter = Almmmatg.UndA
          NO-LOCK NO-ERROR.
      IF AVAILABLE Almtconv THEN DO:
          F-FACTOR = Almtconv.Equival.
          F-MrgUti-A = ROUND(((((F-PreVta-A / F-FACTOR) / X-CTOUND) - 1) * 100), 6).
      END.
  END.
  ASSIGN
      F-PreVta-B = Almmmatg.Prevta[3]
      X-CTOUND = Almmmatg.CtoTot
      F-FACTOR = 1
      F-MrgUti-B = 0.   
  /****   MARGEN B   ****/
  IF Almmmatg.UndB <> "" THEN DO:
      FIND Almtconv WHERE Almtconv.CodUnid = Almmmatg.UndBas
          AND  Almtconv.Codalter = Almmmatg.UndB
          NO-LOCK NO-ERROR.
      IF AVAILABLE Almtconv THEN DO:
          F-FACTOR = Almtconv.Equival.
          F-MrgUti-B = ROUND(((((F-PreVta-B / F-FACTOR) / X-CTOUND) - 1) * 100), 6).
      END.
  END.
  /****   MARGEN C   ****/
  ASSIGN
      F-PreVta-C = Almmmatg.Prevta[4]
      X-CTOUND = Almmmatg.CtoTot
      F-FACTOR = 1
      F-MrgUti-C = 0.
  IF Almmmatg.UndC <> "" THEN DO:
      FIND Almtconv WHERE Almtconv.CodUnid = Almmmatg.UndBas
          AND  Almtconv.Codalter = Almmmatg.UndC
          NO-LOCK NO-ERROR.
      IF AVAILABLE Almtconv THEN DO:
          F-FACTOR = Almtconv.Equival.
          F-MrgUti-C = ROUND(((((F-PreVta-C / F-FACTOR) / X-CTOUND) - 1) * 100), 6).
      END.
  END.

  /**** MARGEN PRECIO DE OFICINA ****/
  DEFINE VARIABLE fMot LIKE Almmmatg.PreOfi.
  DEFINE VARIABLE MrgOfi LIKE Almmmatg.MrgUti-A.

  ASSIGN
      fMot   = 0
      MrgOfi = 0
      F-FACTOR = 1
      X-CTOUND = Almmmatg.CtoTot.
  CASE Almmmatg.Chr__02 :
      WHEN "T" THEN DO:        
          IF Almmmatg.Chr__01 <> "" THEN DO:
              FIND Almtconv WHERE Almtconv.CodUnid = Almmmatg.UndBas
                  AND  Almtconv.Codalter = Almmmatg.Chr__01
                  NO-LOCK NO-ERROR.
              IF AVAILABLE Almtconv THEN DO:
                  F-FACTOR = Almtconv.Equival.
              END.
          END.
          fMot = Almmmatg.PreOfi / X-CTOUND / F-FACTOR.
          MrgOfi = ROUND((fMot - 1) * 100, 6).
      END.
      WHEN "P" THEN DO:
          MrgOfi = ((Almmmatg.Prevta[1] / Almmmatg.Ctotot) - 1 ) * 100. 
      END. 
  END.    

  /* Grabamos */
  FIND B-MATG WHERE ROWID(B-MATG) = ROWID(Almmmatg) EXCLUSIVE-LOCK NO-ERROR.
  IF AVAILABLE B-MATG THEN
      ASSIGN
      B-MATG.MrgUti   = f-MrgLis
      B-MATG.MrgUti-A = f-MrgUti-A
      B-MATG.MrgUti-B = f-MrgUti-B
      B-MATG.MrgUti-C = f-MrgUti-C
      B-MATG.DEC__01  = MrgOfi.
  IF AVAILABLE(B-MATG) THEN RELEASE B-MATG.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Minimos) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Minimos Procedure 
PROCEDURE Minimos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/* DEF VAR EsCampana AS LOG NO-UNDO.                                                      */
/*                                                                                        */
/* /* Buscamos si es o no campaña */                                                      */
/* EsCampana = NO.                                                                        */
/* rloop:                                                                                 */
/* FOR EACH VtaTabla WHERE VtaTabla.CodCia = s-codcia                                     */
/*     AND VtaTabla.Tabla = "CAMPAÑAS" NO-LOCK:                                           */
/*     IF TODAY >= VtaTabla.Rango_fecha[1] AND TODAY <= VtaTabla.Rango_fecha[2] THEN DO:  */
/*         EsCampana = YES.                                                               */
/*         LEAVE rloop.                                                                   */
/*     END.                                                                               */
/* END.                                                                                   */
/*                                                                                        */
/* FOR EACH almmmate WHERE almmmate.codcia = s-codcia                                     */
/*     AND (almmmate.vctmn1 <> 0 OR almmmate.vctmn2 <> 0):                                */
/*     IF EsCampana = NO AND Almmmate.VCtMn2 > 0 THEN Almmmate.StkMin = Almmmate.VCtMn2.  */
/*     IF EsCampana = YES AND Almmmate.VCtMn1 > 0 THEN Almmmate.StkMin = Almmmate.VCtMn1. */
/* END.                                                                                   */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Regenera-Kardex) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Regenera-Kardex Procedure 
PROCEDURE Regenera-Kardex :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

CATALOGO:
FOR EACH Almmmatg NO-LOCK WHERE Almmmatg.codcia = S-CODCIA 
    AND Almmmatg.codmat >= desdec 
    AND Almmmatg.codmat <= hastac
    USE-INDEX matg01 TRANSACTION:
    DISPLAY almmmatg.codmat i-fchdoc STRING(TIME, 'hh:mm') TODAY.                     
    PAUSE 0.

    RUN Borra-Historicos.
    IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
        DISPLAY 'ERROR: borrado de historicos' almmmatg.codmat.
        PAUSE 0.
        UNDO CATALOGO.
        NEXT CATALOGO.
    END.

    RUN Kardex.
    IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
        DISPLAY 'ERROR: calculo del kardex' almmmatg.codmat.
        PAUSE 0.
        UNDO CATALOGO.
        NEXT CATALOGO.
    END.

    RUN Stocks.
    IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
        DISPLAY 'ERROR: calculo de stocks' almmmatg.codmat.
        PAUSE 0.
        UNDO CATALOGO.
        NEXT CATALOGO.
    END.

    /* RHC 24/07/2015 Recalcula Margenes de Utilidad */
    RUN Margenes.
END.
IF AVAILABLE(almcmov)  THEN RELEASE almcmov.
IF AVAILABLE(almdmov)  THEN RELEASE almdmov.
IF AVAILABLE(almmmate) THEN RELEASE almmmate.
IF AVAILABLE(almstkge) THEN RELEASE almstkge.
IF AVAILABLE(almstkal) THEN RELEASE almstkal.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Stocks) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Stocks Procedure 
PROCEDURE Stocks :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

     /* Stocks por Almacen */
     FOR EACH Almacen NO-LOCK WHERE Almacen.Codcia = S-CODCIA
         ON ERROR UNDO, RETURN 'ADM-ERROR' ON STOP UNDO, RETURN 'ADM-ERROR':
         FIND LAST AlmStkAl WHERE AlmStkAl.Codcia = Almacen.Codcia 
             AND AlmStkAl.CodAlm = Almacen.CodAlm 
             AND AlmStkAl.CodMat = Almmmatg.CodMat
             NO-ERROR. 
         IF AVAILABLE AlmStkAl THEN DO:
             FIND FIRST Almmmate WHERE Almmmate.Codcia = S-CODCIA 
                 AND Almmmate.CodAlm = Almacen.CodAlm 
                 AND Almmmate.CodMat = Almmmatg.CodMat
                 NO-LOCK NO-ERROR.
             IF NOT AVAILABLE Almmmate THEN DO:
                 CREATE Almmmate.
                 ASSIGN
                     almmmate.codcia = S-CODCIA
                     almmmate.codalm = almacen.codalm
                     almmmate.codmat = almmmatg.codmat
                     almmmate.desmat = almmmatg.desmat
                     almmmate.undvta = almmmatg.undstk
                     almmmate.facequ = 1.
             END.
             FIND CURRENT Almmmate EXCLUSIVE-LOCK NO-ERROR.
             Almmmate.Stkact = AlmStkAl.StkAct.  
             FIND CURRENT Almmmate NO-LOCK NO-ERROR.
         END.
     END.
     RETURN 'OK'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Valorizado-por-linea) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Valorizado-por-linea Procedure 
PROCEDURE Valorizado-por-linea :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR x-almacenes AS CHAR NO-UNDO.
DEF VAR x-codalm AS CHAR NO-UNDO.
DEF VAR k AS INT NO-UNDO.
DEF VAR x-imptot AS DEC NO-UNDO.

x-almacenes = "03,04,05,10,10A,11,11e,18,21,21s,21T,22,27,28,32,34,34e,35,35e,38,39,500,501,502,503,504,505,60,61".
DISPLAY DATETIME(TODAY, MTIME).
FOR EACH almtfami WHERE codcia = s-codcia:
    DISPLAY 'Linea:' Almtfami.codfam STRING(TIME, 'hh:mm') TODAY.                     
    PAUSE 0.
    ASSIGN
         Almtfami.Libre_d01 = 0.
    FOR EACH almmmatg OF almtfami NO-LOCK:
        DO k = 1 TO NUM-ENTRIES(x-almacenes):
            x-codalm = ENTRY(k, x-almacenes).
            FOR EACH almmmate OF almmmatg NO-LOCK WHERE codalm = x-codalm
                AND almmmate.stkact <> 0:
                x-imptot = Almmmate.stkact * Almmmatg.CtoTot * (IF Almmmatg.MonVta = 1 THEN 1 ELSE Almmmatg.TpoCmb).
                IF x-imptot <> ? THEN Almtfami.Libre_d01 = Almtfami.Libre_d01 + x-imptot.
                ELSE DO:
                    DISPLAY 'Linea: ERROR almacen' almmmate.codalm 'articulo' almmmatg.codmat.
                    PAUSE 0.
                END.
            END.
        END.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Verifica-Ventas-Kardex) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Verifica-Ventas-Kardex Procedure 
PROCEDURE Verifica-Ventas-Kardex :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

RETURN.

DEF VAR x-Comprobantes AS CHAR INIT 'FAC,BOL,TCK' NO-UNDO.
DEF VAR x-Error AS CHAR FORMAT 'x(50)' NO-UNDO.
DEF VAR k AS INT NO-UNDO.
DEF VAR x-FchDoc AS DATE NO-UNDO.

x-FchDoc = i-FchDoc.
IF x-FchDoc < 11/01/2012 THEN x-FchDoc = 11/01/2012.

/* 1RO VERIFICAMOS LOS MOVIMIENTOS DE ALMACEN */
FOR EACH gn-divi NO-LOCK WHERE gn-divi.codcia = s-codcia,
    EACH almacen NO-LOCK WHERE almacen.codcia = s-codcia
    AND almacen.coddiv = gn-divi.coddiv,
    EACH almcmov WHERE almcmov.codcia = s-codcia
    AND almcmov.codalm = almacen.codalm
    AND almcmov.tipmov = "S"    /* Salidas */
    AND almcmov.codmov = 02     /* Por Ventas */
    AND almcmov.fchdoc >= x-FchDoc
    AND almcmov.fchdoc < TODAY - 1:
    /* COnsistencia de Anulados */
    k = 0.
    FOR EACH almdmov OF almcmov NO-LOCK:
        k = k + 1.
    END.
    IF almcmov.flgest = "A" AND k > 0 THEN DO:
        FOR EACH almdmov OF almcmov:
            DELETE almdmov.
        END.
        /* buscamos si el comprobante está anulado */
        FIND ccbcdocu WHERE ccbcdocu.codcia = s-codcia
            AND ccbcdocu.coddoc = almcmov.codref
            AND ccbcdocu.nrodoc = almcmov.nroref
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE ccbcdocu OR ccbcdocu.flgest <> "A" THEN DELETE almcmov.
        NEXT.   /* Siguiente Registro */
    END.
    /* Consistencia del comprobante */
    FIND ccbcdocu WHERE ccbcdocu.codcia = s-codcia
        AND ccbcdocu.coddoc = almcmov.codref
        AND ccbcdocu.nrodoc = almcmov.nroref
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE ccbcdocu THEN DO:
        FOR EACH almdmov OF almcmov:
            DELETE almdmov.
        END.
        DELETE almcmov.
        NEXT.   /* Siguiente Registro */
    END.
    /* Consistencia de Fechas */
    /* RHC 28/04/2015 NO para la 00500 WEB UTILEX */
    IF Ccbcdocu.CodDiv <> "00500" AND almcmov.fchdoc <> ccbcdocu.fchdoc THEN DO:
        almcmov.fchdoc = ccbcdocu.fchdoc.
        FOR EACH almdmov OF almcmov:
            almdmov.fchdoc = ccbcdocu.fchdoc.
        END.
    END.
    /* Consistencia de detalles */
    FOR EACH almdmov OF almcmov:
        FIND ccbddocu OF ccbcdocu WHERE ccbddocu.codmat = almdmov.codmat NO-LOCK NO-ERROR.
        IF NOT AVAILABLE ccbddocu THEN DO:
            DELETE almdmov.
            NEXT.   /* Siguiente Registro */
        END.
        IF (almdmov.candes * almdmov.factor) <> (ccbddocu.candes * ccbddocu.factor) THEN DO:
            DELETE almdmov.
            NEXT.   /* Siguiente Registro */
        END.
    END.
END.

/*  2DO VERIFICAMOS LOS COMPROBANTES */
PORCOMPROBANTES:
FOR EACH gn-divi NO-LOCK WHERE gn-divi.codcia = s-codcia,
    EACH ccbcdocu NO-LOCK WHERE ccbcdocu.codcia = s-codcia
        AND ccbcdocu.coddiv = gn-divi.coddiv
        AND LOOKUP(ccbcdocu.coddoc, x-comprobantes) > 0
        AND LOOKUP(CcbCdocu.TpoFac, 'A,S,B,F,M') = 0    /* NO Adelanto, Servicio, Canje Sunat, x FAI ni x Mercaderia*/
        AND ccbcdocu.fchdoc >= x-FchDoc
        AND ccbcdocu.fchdoc < TODAY - 1:
    /* POR FAI */
    IF LOOKUP(Ccbcdocu.CodDoc, "FAC,BOL") > 0 AND Ccbcdocu.CodRef = "FAI" THEN NEXT.
    /* ANULADOS */
    IF ccbcdocu.flgest = "A" THEN DO:
        FOR EACH almcmov WHERE almcmov.codcia = s-codcia
            AND almcmov.tipmov = "S"
            AND almcmov.codmov = 02
            AND almcmov.codref = ccbcdocu.coddoc
            AND almcmov.nroref = ccbcdocu.nrodoc:
            FOR EACH almdmov OF almcmov:
                DELETE almdmov.
            END.
            almcmov.flgest = "A".
        END.
        NEXT PORCOMPROBANTES.   /* Siguiente Registro */
    END.
    /* CABECERAS */
    FIND almcmov WHERE almcmov.codcia = s-codcia
        AND almcmov.tipmov = "S"
        AND almcmov.codmov = 02
        AND almcmov.codref = ccbcdocu.coddoc
        AND almcmov.nroref = ccbcdocu.nrodoc
        AND almcmov.flgest <> 'A'
        NO-ERROR.
    IF NOT AVAILABLE almcmov THEN DO:
        IF AMBIGUOUS almcmov THEN DO:
            x-Error = "COMPROBANTE REPETIDO EN ALMACEN".
            DISPLAY
                x-Error '|'
                ccbcdocu.coddiv '|'
                ccbcdocu.coddoc '|'
                ccbcdocu.nrodoc '|'
                ccbcdocu.fchdoc
                WITH STREAM-IO NO-BOX NO-LABELS WIDTH 200.
            PAUSE 0.
            FOR EACH almcmov WHERE almcmov.codcia = s-codcia
                AND almcmov.tipmov = "S"
                AND almcmov.codmov = 02
                AND almcmov.codref = ccbcdocu.coddoc
                AND almcmov.nroref = ccbcdocu.nrodoc:
                FOR EACH almdmov OF almcmov:
                    DELETE almdmov.
                END.
                DELETE almcmov.
            END.
            /* CORREGIR ERROR */
            RUN Act_Alm.
            NEXT PORCOMPROBANTES.
        END.
        x-Error = "COMPROBANTE NO REGISTRADO EN ALMACEN".
        DISPLAY
            x-Error '|'
            ccbcdocu.coddiv '|'
            ccbcdocu.coddoc '|'
            ccbcdocu.nrodoc '|'
            ccbcdocu.fchdoc
            WITH STREAM-IO NO-BOX NO-LABELS WIDTH 200.
        PAUSE 0.
        /* CORREGIR ERROR */
        RUN Act_Alm.
        NEXT PORCOMPROBANTES.
    END.
    /* DETALLLE */
    x-Error = "".
    FOR EACH ccbddocu OF ccbcdocu NO-LOCK:
        FIND almdmov OF almcmov WHERE almdmov.codmat = ccbddocu.codmat NO-LOCK NO-ERROR.
        IF NOT AVAILABLE almdmov OR ccbddocu.candes <> almdmov.candes THEN DO:
            IF AMBIGUOUS almdmov THEN x-Error = "PRODUCTO: " + ccbddocu.codmat + " REPETIDO EN MOV ALMACEN".
            ELSE x-Error = "PRODUCTO: " + ccbddocu.codmat + " NO REGISTRADO EN MOV ALMACEN".
        END.
    END.
    IF x-Error <> "" THEN DO:
        DISPLAY
            x-Error '|'
            ccbcdocu.coddiv '|'
            ccbcdocu.coddoc '|'
            ccbcdocu.nrodoc '|'
            ccbcdocu.fchdoc 
            WITH STREAM-IO NO-BOX NO-LABELS WIDTH 200.
        PAUSE 0.
        FOR EACH almdmov OF almcmov:
            DELETE almdmov.
        END.
        DELETE almcmov.
        /* CORREGIR ERROR */
        RUN Act_Alm.
    END.
/*     FOR EACH ccbddocu OF ccbcdocu NO-LOCK:                                               */
/*         FIND almdmov OF almcmov WHERE almdmov.codmat = ccbddocu.codmat NO-LOCK NO-ERROR. */
/*         IF NOT AVAILABLE almdmov OR ccbddocu.candes <> almdmov.candes THEN DO:           */
/*             IF AMBIGUOUS almdmov THEN x-Error = "PRODUCTO REPETIDO EN MOV ALMACEN".      */
/*             ELSE x-Error = "PRODUCTO NO REGISTRADO EN MOV ALMACEN".                      */
/*             DISPLAY                                                                      */
/*                 x-Error '|'                                                              */
/*                 ccbcdocu.coddiv '|'                                                      */
/*                 ccbcdocu.coddoc '|'                                                      */
/*                 ccbcdocu.nrodoc '|'                                                      */
/*                 ccbcdocu.fchdoc '|'                                                      */
/*                 ccbddocu.codmat                                                          */
/*                 WITH STREAM-IO NO-BOX NO-LABELS WIDTH 200.                               */
/*             PAUSE 0.                                                                     */
/*             IF AMBIGUOUS Almdmov THEN NEXT.                                              */
/*             /* CORREGIR ERROR */                                                         */
/*             IF AVAILABLE Almdmov THEN DO:                                                */
/*                 FIND CURRENT Almdmov EXCLUSIVE-LOCK NO-ERROR NO-WAIT.                    */
/*                 IF NOT AVAILABLE Almdmov THEN NEXT.                                      */
/*                 DELETE Almdmov.                                                          */
/*             END.                                                                         */
/*             CREATE Almdmov.                                                              */
/*             ASSIGN Almdmov.CodCia = Almcmov.CodCia                                       */
/*                    Almdmov.CodAlm = Almcmov.CodAlm                                       */
/*                    Almdmov.TipMov = Almcmov.TipMov                                       */
/*                    Almdmov.CodMov = Almcmov.CodMov                                       */
/*                    Almdmov.NroSer = almcmov.nroser                                       */
/*                    Almdmov.NroDoc = almcmov.nrodoc                                       */
/*                    Almdmov.codmat = ccbddocu.codmat                                      */
/*                    Almdmov.AftIgv = ccbddocu.aftigv                                      */
/*                    Almdmov.AftIsc = ccbddocu.aftisc                                      */
/*                    Almdmov.CanDes = ccbddocu.candes                                      */
/*                    Almdmov.CodMon = ccbcdocu.codmon                                      */
/*                    Almdmov.CodUnd = ccbddocu.undvta                                      */
/*                    Almdmov.Factor = ccbddocu.factor                                      */
/*                    Almdmov.FchDoc = CcbCDocu.FchDoc                                      */
/*                    Almdmov.ImpDto = ccbddocu.impdto                                      */
/*                    Almdmov.ImpIgv = ccbddocu.impigv                                      */
/*                    Almdmov.ImpIsc = ccbddocu.impisc                                      */
/*                    Almdmov.ImpLin = ccbddocu.implin                                      */
/*                    Almdmov.NroItm = ccbddocu.nroitm                                      */
/*                    Almdmov.PorDto = ccbddocu.pordto                                      */
/*                    Almdmov.PreBas = ccbddocu.prebas                                      */
/*                    Almdmov.PreUni = ccbddocu.preuni                                      */
/*                    Almdmov.TpoCmb = ccbcdocu.tpocmb                                      */
/*                    Almdmov.HraDoc = Almcmov.HorSal.                                      */
/*             RELEASE Almdmov.                                                             */
/*         END.                                                                             */
/*     END.    /* FOR EACH DETALLE */ */
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

