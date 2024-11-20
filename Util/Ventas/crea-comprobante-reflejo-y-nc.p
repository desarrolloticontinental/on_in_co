/* CREA FACTURAS, DEVOLUCION DE MERCADERIA Y NOTAS DE CREDITO POR DEVOLUCION */
DEF NEW SHARED VAR s-codcia AS INT INIT 001.
DEF NEW SHARED VAR cl-codcia AS INT INIT 000.
DEF NEW SHARED VAR cb-codcia AS INT INIT 000.
DEF NEW SHARED VAR s-user-id AS CHAR.

DEF VAR s-coddoc AS CHAR INIT "FAC" NO-UNDO.
DEF VAR x-nrodoc AS CHAR FORMAT 'x(11)' NO-UNDO.
DEF VAR s-coddiv AS CHAR NO-UNDO.
DEF VAR s-nroser AS INT NO-UNDO.
DEF VAR s-codalm AS CHAR NO-UNDO.
DEF VAR I-NroSer AS INT NO-UNDO.
DEF VAR I-NroDoc AS INT NO-UNDO.
DEF VAR s-codmov AS INT INIT 09 NO-UNDO.
DEF VAR R-ROWID  AS ROWID NO-UNDO. 

DEF BUFFER B-CDOCU FOR Ccbcdocu.
DEF BUFFER FACTURA FOR Ccbcdocu.
DEF BUFFER B-DDOCU FOR Ccbddocu.
DEF BUFFER B-ADocu FOR CcbADocu.

REPEAT :
    ASSIGN
        s-coddoc = "FAC".
    PROMPT-FOR x-nrodoc LABEL "Número de FAC"    
        WITH ROW 2 SIDE-LABELS.
    FIND B-CDOCU WHERE B-CDOCU.codcia = s-codcia
        AND B-CDOCU.coddoc = s-coddoc
        AND B-CDOCU.nrodoc = INPUT x-nrodoc
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE B-CDOCU THEN DO:
        MESSAGE 'Documento NO registrado' VIEW-AS ALERT-BOX ERROR.
        NEXT.
    END.
    ASSIGN
        s-coddiv = B-CDOCU.coddiv
        s-nroser = INTEGER(SUBSTRING(B-CDOCU.nrodoc,1,3))
        s-codalm = B-CDOCU.codalm
        s-user-id = B-CDOCU.usuario.
    RUN Proceso-Principal.
END.

PROCEDURE Proceso-Principal:
/* ************************ */

    DO TRANSACTION ON ERROR UNDO, RETURN 'ADM-ERROR' ON STOP UNDO, RETURN 'ADM-ERROR':
        RUN Crea-Comprobante.
        IF RETURN-VALUE = "ADM-ERROR" THEN DO:
            MESSAGE 'NO se pudo crear el comprobante' VIEW-AS ALERT-BOX ERROR.
            UNDO, RETURN.
        END.
        RUN Devolucion-Mercaderia.
        IF RETURN-VALUE = "ADM-ERROR" THEN DO:
            MESSAGE 'NO se pudo crear la nota de crédito' VIEW-AS ALERT-BOX ERROR.
            UNDO, RETURN.
        END.
    END.

END PROCEDURE.


PROCEDURE Crea-Comprobante:
/* *********************** */

trloop:
DO TRANSACTION ON ERROR UNDO trloop, RETURN 'ADM-ERROR' ON STOP UNDO trloop, RETURN 'ADM-ERROR':
    ASSIGN
        s-coddoc = "FAC".
    /* COPIA LA FACTURA */
    FIND FacCorre WHERE FacCorre.CodCia = s-CodCia AND
        FacCorre.CodDiv = s-CodDiv AND
        FacCorre.CodDoc = s-CodDoc AND
        FacCorre.NroSer = s-NroSer
        EXCLUSIVE-LOCK NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        MESSAGE 'Error en el control de correlativo' s-CodDoc s-CodDiv s-NroSer
            VIEW-AS ALERT-BOX ERROR.
        UNDO trloop, RETURN 'ADM-ERROR'.
    END.

    /* Crea Cabecera */
    CREATE CcbCDocu.
    BUFFER-COPY B-CDOCU
        TO CcbCDocu
        ASSIGN
        CcbCDocu.NroDoc =  STRING(FacCorre.NroSer,"999") + STRING(FacCorre.Correlativo,"999999") 
        CcbCDocu.FchDoc = TODAY
        CcbCDocu.SdoAct = CcbCDocu.ImpTot
        CcbCDocu.FlgEst = "P"
        CcbCDocu.CodRef = ""
        CcbCDocu.NroRef = ""
        CcbCDocu.Glosa  = "".
    ASSIGN
        FacCorre.Correlativo = FacCorre.Correlativo + 1.
    FIND gn-clie WHERE gn-clie.CodCia = cl-codcia 
        AND gn-clie.CodCli = CcbCDocu.CodCli 
        NO-LOCK NO-ERROR.
    IF AVAILABLE gn-clie  THEN DO:
        ASSIGN
            CcbCDocu.NomCli  = gn-clie.NomCli
            CcbCDocu.DirCli  = gn-clie.DirCli
            CcbCDocu.CodDpto = gn-clie.CodDept 
            CcbCDocu.CodProv = gn-clie.CodProv 
            CcbCDocu.CodDist = gn-clie.CodDist.
    END.
    /* Crea Detalle */
    FOR EACH B-DDOCU OF B-CDOCU NO-LOCK:
        CREATE Ccbddocu.
        BUFFER-COPY B-DDOCU
            TO Ccbddocu
            ASSIGN
            Ccbddocu.nrodoc = Ccbcdocu.nrodoc
            Ccbddocu.fchdoc = Ccbcdocu.fchdoc
            Ccbddocu.candev = 0.
    END.
    /* GENERACION DE CONTROL DE PERCEPCIONES */
    RUN vta2/control-percepcion-cargos (ROWID(Ccbcdocu)) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        MESSAGE 'ERROR Percepciones' ccbcdocu.coddoc ccbcdocu.nrodoc
            VIEW-AS ALERT-BOX ERROR.
        UNDO trloop, RETURN 'ADM-ERROR'.
    END.
    /* Descarga de Almacen */
    RUN vta2\act_alm (ROWID(CcbCDocu)).
    IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
        MESSAGE 'ERROR Actualizando Almacén' ccbcdocu.coddoc ccbcdocu.nrodoc
            VIEW-AS ALERT-BOX ERROR.
        UNDO trloop, RETURN 'ADM-ERROR'.
    END.
    /* RHC 25-06-2012 ASIENTO DE TRANSFERENCIA PARA SPEED */           
    RUN aplic/sypsa/registroventas (INPUT ROWID(ccbcdocu), INPUT "I", YES). 

    /* *************************************************************** */
    /* **************** 2DA PARTE: GENERACION DE GUIAS *************** */
    /* *************************************************************** */
    /* Correlativo */
    FIND FIRST FacCorre WHERE FacCorre.CodCia = s-CodCia AND
        FacCorre.CodDoc = "G/R" AND
        FacCorre.CodDiv = s-CodDiv AND
        FacCorre.CodAlm = s-CodAlm AND
        FacCorre.Codmov <> 03 AND
        FacCorre.FlgEst = YES
        EXCLUSIVE-LOCK NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        MESSAGE 'Error en el control de correlativo G/R' s-CodDiv 
            VIEW-AS ALERT-BOX ERROR.
        UNDO trloop, RETURN 'ADM-ERROR'.
    END.
    FIND FACTURA OF Ccbcdocu.
    CREATE CcbCDocu.
    BUFFER-COPY FACTURA
        TO CcbCDocu
        ASSIGN
        CcbCDocu.CodDiv = s-CodDiv
        CcbCDocu.CodDoc = "G/R"
        CcbCDocu.NroDoc =  STRING(FacCorre.NroSer,"999") + STRING(FacCorre.Correlativo,"999999") 
        CcbCDocu.FchDoc = TODAY
        CcbCDocu.CodRef = FACTURA.CodDoc
        CcbCDocu.NroRef = FACTURA.NroDoc
        CcbCDocu.FlgEst = "F"   /* FACTURADO */
        CcbCDocu.usuario = S-USER-ID
        CcbCDocu.TpoFac = "A".    /* AUTOMATICA (No descarga stock) */
    ASSIGN
        FacCorre.Correlativo = FacCorre.Correlativo + 1.
    ASSIGN
        FACTURA.CodRef = Ccbcdocu.coddoc
        FACTURA.NroRef = Ccbcdocu.nrodoc.
    /* COPIAMOS DATOS DEL TRANSPORTISTA */
    FIND Ccbadocu WHERE Ccbadocu.codcia = FACTURA.codcia
        AND Ccbadocu.coddiv = FACTURA.coddiv
        AND Ccbadocu.coddoc = FACTURA.coddoc
        AND Ccbadocu.nrodoc = FACTURA.nrodoc
        NO-LOCK NO-ERROR.
    IF AVAILABLE Ccbadocu THEN DO:
        CREATE B-ADOCU.
        BUFFER-COPY Ccbadocu TO B-ADOCU
            ASSIGN
            B-ADOCU.CodDiv = Ccbcdocu.CodDiv
            B-ADOCU.CodDoc = Ccbcdocu.CodDoc
            B-ADOCU.NroDoc = Ccbcdocu.NroDoc.
    END.
    FOR EACH B-DDOCU OF FACTURA NO-LOCK:
        CREATE Ccbddocu.
        BUFFER-COPY B-DDOCU 
            TO Ccbddocu
            ASSIGN
            Ccbddocu.coddiv = Ccbcdocu.coddiv
            Ccbddocu.coddoc = Ccbcdocu.coddoc
            Ccbddocu.nrodoc = Ccbcdocu.nrodoc.
    END.
END.

RETURN "OK".

END PROCEDURE.

PROCEDURE Devolucion-Mercaderia:
/* ***************************** */

trloop:
DO TRANSACTION ON ERROR UNDO trloop, RETURN 'ADM-ERROR' ON STOP UNDO trloop, RETURN 'ADM-ERROR':
    ASSIGN
        s-coddoc = "D/F".
    /* ****************** INGRESO POR DEVOLUCION ****************** */
    FIND Almacen WHERE Almacen.CodCia = S-CODCIA 
        AND Almacen.CodAlm = s-CodAlm
        EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE Almacen THEN DO:
        MESSAGE "ERROR No se pudo bloquear almacén" s-codalm VIEW-AS ALERT-BOX ERROR.
        UNDO trloop, RETURN 'ADM-ERROR'.
    END.
    ASSIGN 
        I-NroDoc = Almacen.CorrIng
        Almacen.CorrIng = Almacen.CorrIng + 1.
    CREATE Almcmov.
    ASSIGN 
        Almcmov.CodCia = S-CodCia 
        Almcmov.CodAlm = s-CodAlm
        Almcmov.TipMov = "I"
        Almcmov.CodMov = S-CodMov 
        Almcmov.NroSer = 000
        Almcmov.NroDoc = I-NRODOC
        Almcmov.FchDoc = TODAY
        Almcmov.FlgEst = "C"        /* OJO */
        Almcmov.HorRcp = STRING(TIME,"HH:MM:SS")
        Almcmov.usuario = B-CDOCU.Usuario
        Almcmov.CodRef = B-CDOCU.CodDoc
        Almcmov.NroRf1 = B-CDOCU.NroDoc
        Almcmov.NroRef = B-CDOCU.NroDoc
        Almcmov.CodMon = B-CDOCU.CodMon
        Almcmov.TpoCmb = B-CDOCU.TpoCmb.
    FIND FacCorre WHERE FacCorre.CodCia = S-CODCIA 
        AND FacCorre.CodDoc = S-CODDOC 
        AND FacCorre.CodDiv = S-CODDIV 
        AND FacCorre.CodAlm = S-CODALM
        EXCLUSIVE-LOCK NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        MESSAGE "ERROR No se pudo bloquear correlativo" s-coddoc s-coddiv s-codalm
            VIEW-AS ALERT-BOX ERROR.
        UNDO trloop, RETURN 'ADM-ERROR'.
    END.
    ASSIGN 
        I-NroSer = FacCorre.NroSer
        I-NroDoc = FacCorre.Correlativo
        FacCorre.Correlativo = FacCorre.Correlativo + 1.
    ASSIGN 
        Almcmov.NroRf2 = STRING(I-NroSer,"999") + STRING(I-NroDoc,"999999").
    ASSIGN 
        Almcmov.CodCli = B-CDOCU.CodCli
        Almcmov.NomRef = B-CDOCU.NomCli
        Almcmov.CodMon = B-CDOCU.CodMon
        Almcmov.CodVen = B-CDOCU.CodVen.
    FOR EACH B-DDOCU OF B-CDOCU NO-LOCK:
        CREATE almdmov.
        ASSIGN 
            Almdmov.CodCia = Almcmov.CodCia 
            Almdmov.CodAlm = Almcmov.CodAlm 
            Almdmov.TipMov = Almcmov.TipMov 
            Almdmov.CodMov = Almcmov.CodMov 
            Almdmov.NroSer = Almcmov.NroSer
            Almdmov.NroDoc = Almcmov.NroDoc 
            Almdmov.CodMon = Almcmov.CodMon 
            Almdmov.FchDoc = Almcmov.FchDoc 
            Almdmov.TpoCmb = Almcmov.TpoCmb 
            Almdmov.codmat = B-DDOCU.codmat
            Almdmov.CanDes = B-DDOCU.CanDes
            Almdmov.CodUnd = B-DDOCU.UndVta
            Almdmov.Factor = B-DDOCU.Factor
            Almdmov.ImpCto = B-DDOCU.ImpCto
            Almdmov.PreUni = B-DDOCU.PreUni
            Almdmov.CodAjt = '' 
            Almdmov.PreBas = B-DDOCU.PreBas 
            Almdmov.PorDto = B-DDOCU.PorDto 
            Almdmov.ImpLin = B-DDOCU.ImpLin 
            Almdmov.ImpIsc = B-DDOCU.ImpIsc 
            Almdmov.ImpIgv = B-DDOCU.ImpIgv 
            Almdmov.ImpDto = B-DDOCU.ImpDto 
            Almdmov.AftIsc = B-DDOCU.AftIsc 
            Almdmov.AftIgv = B-DDOCU.AftIgv 
            Almdmov.Por_Dsctos[1] = B-DDOCU.Por_Dsctos[1]
            Almdmov.Por_Dsctos[2] = B-DDOCU.Por_Dsctos[2]
            Almdmov.Por_Dsctos[3] = B-DDOCU.Por_Dsctos[3]
            Almdmov.Flg_factor = B-DDOCU.Flg_factor
            Almdmov.HraDoc     = Almcmov.HorRcp
            R-ROWID = ROWID(Almdmov).
       RUN ALM\ALMACSTK (R-ROWID).
       IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
           MESSAGE 'ERROR almcstk' Almdmov.codmat.
           UNDO trloop, RETURN 'ADM-ERROR'.
       END.
       /* RHC 31.03.04 REACTIVAMOS KARDEX POR ALMACEN */
       RUN alm/almacpr1 (R-ROWID, 'U').
       IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
           MESSAGE 'ERROR almacpr1' Almdmov.codmat.
           UNDO trloop, RETURN 'ADM-ERROR'.
       END.
    END.
    RUN Actualiza-Factura(1).
    IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
        MESSAGE 'ERROR NO se pudo actualizar factura' VIEW-AS ALERT-BOX ERROR.
        UNDO trloop, RETURN 'ADM-ERROR'.
    END.
    /* ************************************************************ */
    /* ************ NOTA DE CREDITO POR DEVOLUCION **************** */
    /* *** SE VA A FORZAR A LA SERIE 001 DE LA DIVISION 00000 ***** */
    ASSIGN
        s-coddoc = "N/C".
    ASSIGN s-NroSer = 001.
    FIND FIRST FacCorre WHERE FacCorre.CodCia = S-CODCIA 
        AND  FacCorre.CodDoc = S-CODDOC 
        AND  FacCorre.CodDiv = "00000"
        AND  FacCorre.NroSer = s-NroSer
        EXCLUSIVE-LOCK NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        MESSAGE 'ERROR correlativo' "00000" s-coddoc s-nroser
            VIEW-AS ALERT-BOX ERROR.
        UNDO trloop, RETURN 'ADM-ERROR'.
    END.
    ASSIGN i-NroDoc = FacCorre.Correlativo.
    ASSIGN FacCorre.Correlativo = FacCorre.Correlativo + 1.
    CREATE CcbCDocu.
    ASSIGN 
        CcbCDocu.CodCia = S-CODCIA
        CcbCDocu.FchDoc = TODAY
        CcbCDocu.FchVto = TODAY
        CcbCDocu.FlgEst = "P"
        CcbCDocu.PorIgv = B-CDOCU.PorIgv      /* FacCfgGn.PorIgv*/
        CcbCDocu.PorDto = B-CDOCU.PORDTO
        CcbCDocu.CodDoc = S-CODDOC
        CcbCDocu.NroDoc = STRING(s-NroSer,"999") + STRING(I-NroDoc,"999999")
        CcbCDocu.CodRef = B-CDOCU.CodDoc
        CcbCDocu.Nroref = B-CDOCU.NroDoc
        CcbCDocu.CodDiv = "00000"
        CcbCDocu.DivOri = s-CodDiv
        CcbCDocu.CodMon = B-CDOCU.CodMon
        CcbCDocu.TpoCmb = B-CDOCU.TpoCmb
        CcbCDocu.CndCre = 'D'
        CcbCDocu.Tipo   = "OFICINA"
        CcbCDocu.usuario = B-CDOCU.usuario
        CcbCDocu.CodCli = B-CDOCU.CodCli 
        CcbCDocu.NomCli = B-CDOCU.NomCli
        CcbCDocu.DirCli = B-CDOCU.DirCli
        CcbCDocu.RucCli = B-CDOCU.RucCli
        CcbCDocu.CodAlm = B-CDOCU.CodAlm
        CcbCDocu.CodVen = B-CDOCU.CodVen
        CcbCDocu.NroPed = STRING(Almcmov.NroDoc)
        CcbCDocu.NroOrd = Almcmov.NroRf2.
    FIND gn-clie WHERE gn-clie.CodCia = cl-codcia 
        AND gn-clie.CodCli = CcbCDocu.CodCli 
        NO-LOCK NO-ERROR.
    IF AVAILABLE gn-clie 
        THEN ASSIGN
        CcbCDocu.NomCli  = gn-clie.NomCli
        CcbCDocu.DirCli  = gn-clie.DirCli
        CcbCDocu.CodDpto = gn-clie.CodDept 
        CcbCDocu.CodProv = gn-clie.CodProv 
        CcbCDocu.CodDist = gn-clie.CodDist.
    FIND GN-VEN WHERE gn-ven.codcia = s-codcia
        AND gn-ven.codven = CcbCDocu.codven
        NO-LOCK NO-ERROR.
    IF AVAILABLE GN-VEN THEN ccbcdocu.cco = gn-ven.cco.
    FOR EACH B-DDOCU OF B-CDOCU NO-LOCK:
        CREATE CcbDDocu.
        ASSIGN 
            CcbDDocu.CodCia = CcbCDocu.CodCia 
            CcbDDocu.Coddiv = CcbCDocu.Coddiv 
            CcbDDocu.CodDoc = CcbCDocu.CodDoc 
            CcbDDocu.NroDoc = CcbCDocu.NroDoc
            CcbDDocu.codmat = B-DDOCU.codmat 
            CcbDDocu.PreUni = B-DDOCU.PreUni 
            CcbDDocu.CanDes = B-DDOCU.CanDes 
            CcbDDocu.Factor = B-DDOCU.Factor 
            CcbDDocu.ImpIsc = B-DDOCU.ImpIsc
            CcbDDocu.ImpIgv = B-DDOCU.ImpIgv 
            CcbDDocu.ImpLin = B-DDOCU.ImpLin
            CcbDDocu.PorDto = B-DDOCU.PorDto 
            CcbDDocu.PreBas = B-DDOCU.PreBas 
            CcbDDocu.ImpDto = B-DDOCU.ImpDto
            CcbDDocu.AftIgv = B-DDOCU.AftIgv
            CcbDDocu.AftIsc = B-DDOCU.AftIsc
            CcbDDocu.UndVta = B-DDOCU.UndVta
            CcbDDocu.Por_Dsctos[1] = B-DDOCU.Por_Dsctos[1]
            CcbDDocu.Por_Dsctos[2] = B-DDOCU.Por_Dsctos[2]
            CcbDDocu.Por_Dsctos[3] = B-DDOCU.Por_Dsctos[3]
            CcbDDocu.Flg_factor = B-DDOCU.Flg_factor
            CcbDDocu.ImpCto     = B-DDOCU.ImpCto.
    END.

    RUN Graba-Totales.

    /* GENERACION DE CONTROL DE PERCEPCIONES */
    RUN vta2/control-percepcion-abonos (ROWID(Ccbcdocu)) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        MESSAGE 'ERROR control de percepción por abono' VIEW-AS ALERT-BOX ERROR.
        UNDO trloop, RETURN 'ADM-ERROR'.
    END.
    /* RHC 02-07-2012 ASIENTO DE TRANSFERENCIA PARA SPEED */
    RUN aplic/sypsa/registroventas (INPUT ROWID(ccbcdocu), INPUT "I", YES).
    /* ************************************************** */
END.

END PROCEDURE.

PROCEDURE Graba-Totales:
/* ******************** */

    {vta/graba-totales-abono.i}

END PROCEDURE.

PROCEDURE Actualiza-Factura:
/* ************************ */

    DEFINE INPUT PARAMETER FACTOR AS INTEGER.

    DEFINE VARIABLE F-Des AS DECIMAL INIT 0 NO-UNDO.
    DEFINE VARIABLE F-Dev AS DECIMAL INIT 0 NO-UNDO.
    DEFINE VARIABLE C-SIT AS CHARACTER INIT "" NO-UNDO.

    DO TRANSACTION ON ERROR UNDO, RETURN "ADM-ERROR" ON STOP UNDO, RETURN "ADM-ERROR":
        FIND CcbCDocu WHERE CcbCDocu.CodCia = S-CODCIA 
                       AND  CcbCDocu.CodDoc = Almcmov.CodRef 
                       AND  CcbCDocu.NroDoc = Almcmov.NroRf1 
                      EXCLUSIVE-LOCK NO-ERROR.
        IF ERROR-STATUS:ERROR THEN UNDO, RETURN 'ADM-ERROR'.                  
        FOR EACH Almdmov OF Almcmov NO-LOCK:
            FIND FIRST CcbDDocu WHERE CcbDDocu.CodCia = Almdmov.CodCia 
                                 AND  CcbDDocu.CodDoc = Almcmov.CodRef 
                                 AND  CcbDDocu.NroDoc = Almcmov.NroRf1 
                                 AND  CcbDDocu.CodMat = Almdmov.CodMat 
                                EXCLUSIVE-LOCK NO-ERROR.
            ASSIGN CcbDDocu.CanDev = CcbDDocu.CanDev + (FACTOR * Almdmov.CanDes).
            RELEASE CcbDDocu.
        END.
        IF Ccbcdocu.FlgEst <> 'A' THEN DO:
          FOR EACH CcbDDocu OF CcbCDocu NO-LOCK:
              F-Des = F-Des + CcbDDocu.CanDes.
              F-Dev = F-Dev + CcbDDocu.CanDev. 
          END.
          IF F-Dev > 0 THEN C-SIT = "P".
          IF F-Des = F-Dev THEN C-SIT = "D".
          ASSIGN CcbCDocu.FlgCon = C-SIT.
      END.
  END.  


END PROCEDURE.
