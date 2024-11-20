disable triggers for load of almcmov.
disable triggers for load of almdmov.
disable triggers for load of almmmate.
disable triggers for load of almmmatg.
disable triggers for load of almstkge.
disable triggers for load of almstkal.
disable triggers for load of almcieal.

def var s-codcia as inte init 1.

def buffer almdmov2 for almdmov.
def buffer almacen2 for almacen.
def buffer almcmov2 for almcmov.
def buffer ccbcdocu2 for ccbcdocu.

def var f-candes as deci no-undo.

def var desdec as char init "" format "x(6)".
def var hastac as char init "" format "x(6)".
                        
def var i-fchdoc as date format '99/99/9999' init ?.

def buffer B-STKAL FOR AlmStkAl.
def buffer B-STKGE FOR AlmStkGe.
def buffer b-mov   FOR Almtmov.

def var x-signo  as deci no-undo.  
def var x-newcto as deci no-undo.
def var x-ctomed as deci no-undo.
def var x-stkgen as deci no-undo.
def var x-cto    as deci no-undo.
def var x-movtrf as deci no-undo.
def var x-factor as inte no-undo.
DEF VAR f-Factor AS DEC NO-UNDO.        /* Factor de Equivalencia */

/*update desdec hastac.*/
IF desdec = '' then desdec = '000001'.
IF hastac = '' then hastac = '999999'.
/* Fecha de Cierre */
DEF VAR dFchCie AS DATE NO-UNDO.

IF DAY(TODAY) < 15 
THEN dFchCie = TODAY - DAY(TODAY).
ELSE dFchCie = TODAY.
dFchCie = dFchCie - DAY(dFchCie) + 1. 
i-FchDoc = dFchCie.        /* OJO */
/* RHC Habilitar cuando lo necesite */
/*i-FchDoc = 12/01/2010.*/
/* ******************************** */

FOR EACH Almmmatg NO-LOCK WHERE Almmmatg.codcia = S-CODCIA 
    AND Almmmatg.codmat >= desdec 
    AND Almmmatg.codmat <= hastac
    USE-INDEX matg01 TRANSACTION ON ERROR UNDO, RETRY ON STOP UNDO, RETRY:
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
        USE-INDEX Mate03:
        Almmmate.StkAct = 0.
    END.
    
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
    FOR EACH AlmDmov WHERE Almdmov.Codcia = S-CODCIA 
        AND Almdmov.CodMat = Almmmatg.CodMat 
        AND Almdmov.FchDoc >= I-FchDoc
        USE-INDEX Almd02:
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
        IF AVAILABLE Almtconv THEN f-Factor = Almtconv.Equival / Almmmatg.FacEqu.
        IF Almdmov.factor <> f-Factor THEN Almdmov.Factor = f-Factor.
        /* ********************************** */

        FIND FIRST almacen WHERE almacen.codcia = almdmov.codcia
            AND almacen.codalm = almdmov.codalm  
            USE-INDEX alm01 NO-LOCK NO-ERROR.
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
           AlmStkGe.StkAct = AlmStkGe.StkAct + f-candes * x-signo * x-factor .
       /***********Fin Stock x Compañia********************/       
       /***********Inicia Calculo del Costo Promedio********************/       
       IF AVAILABLE AlmtMov THEN DO:
          IF almtmov.TipMov = "I" AND x-Factor <> 0 THEN DO:
             x-cto = 0.
             IF Almtmov.TpoCto = 0 OR Almtmov.TpoCto = 1 THEN DO:
                IF almdmov.codmon = 1 THEN x-cto = Almdmov.Preuni.
                IF almdmov.codmon = 2 THEN x-cto = Almdmov.Preuni * Almdmov.TpoCmb.
                x-ctomed = ((x-cto * f-candes) + (x-ctomed * (AlmStkGe.StkAct - f-candes * x-signo))) / ( AlmStkGe.StkAct).
                IF x-ctomed < 0 THEN x-ctomed = x-cto.
                IF AlmStkGe.StkAct < f-candes AND AlmStkGe.StkAct >= 0 THEN x-ctomed = x-cto.
                /* PARCHE 12.01.10 */
                IF Almdmov.ImpCto = 0 THEN Almdmov.ImpCto = Almdmov.CanDes * Almdmov.PreUni.
                /* ****** */
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
     /* Stocks por Almacen */
     FOR EACH Almacen WHERE Almacen.Codcia = S-CODCIA:
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
                                      
            DISPLAY almacen.codalm almmmatg.codmat i-fchdoc 
                STRING(TIME, 'hh:mm') TODAY.                     
            PAUSE 0.
          END.
     END.
END.

RUN Cierre-Diario.

PROCEDURE Cierre-Diario:

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
    RELEASE Almcieal.
END.

END PROCEDURE.

