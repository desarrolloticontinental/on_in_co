/* EJECUTARLO DESDE CISSAC */
DEF VAR x-linea AS CHAR NO-UNDO.
DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR s-codalm AS CHAR INIT "21" NO-UNDO.
DEF VAR s-coddiv AS CHAR INIT "00021" NO-UNDO.
DEF VAR s-nroser AS INT INIT 228 NO-UNDO.
DEF VAR pRowid AS ROWID NO-UNDO.
DEF VAR r-Rowid AS ROWID NO-UNDO.

DEF VAR x-Item AS INT INIT 0 NO-UNDO.
DEF VAR S-CODMOV AS INTEGER NO-UNDO.
DEF VAR S-CODDOC AS CHAR INITIAL "D/F" NO-UNDO.
DEF VAR s-user-id AS CHAR INIT "ADMIN" NO-UNDO.
DEF VAR I-NROSER AS INT NO-UNDO.
DEF VAR I-NRODOC AS INTEGER NO-UNDO.

FIND FacDocum WHERE FacDocum.CodCia = S-CODCIA 
    AND FacDocum.CodDoc = S-CODDOC 
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE FacDocum OR FacDocum.CodMov = 0 THEN DO:
   MESSAGE "Codigo de Documento no configurado" VIEW-AS ALERT-BOX ERROR.
   RETURN.
END.
S-CODMOV = FacDocum.CodMov.

FIND FacCorre WHERE FacCorre.CodCia = S-CODCIA 
    AND FacCorre.CodDoc = S-CODDOC 
    AND FacCorre.CodDiv = S-CODDIV 
    AND FacCorre.CodAlm = S-CODALM
    NO-LOCK NO-ERROR.
IF AVAILABLE FacCorre THEN ASSIGN I-NroSer = FacCorre.NroSer.

DEFINE TEMP-TABLE DMOV LIKE CcbDDocu.
    
FIND FacCfgGn WHERE FAccfggn.codcia = s-codcia NO-LOCK.

/* CARGAMOS LOS MOVIMIENTOS */
RUN Carga-Temporal.

/* GENERAMOS LOS MOVIMIENTOS CADA 13 ITEMS */
FOR EACH DMOV BREAK BY DMOV.coddoc BY DMOV.nrodoc BY DMOV.codmat TRANSACTION:
    IF FIRST-OF(DMOV.coddoc) OR FIRST-OF(DMOV.nrodoc) THEN DO:
        RUN Crea-Cabecera (OUTPUT pRowid).
        IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.
        x-Item = 1.
    END.
    RUN Crea-Detalle (INPUT pRowid).
    IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.
END.

PROCEDURE Carga-Temporal:
/* ********************* */

    INPUT FROM c:\tmp\ing-dev-conti-cissac.prn.
    REPEAT :
        IMPORT UNFORMATTED x-linea.
        IF x-linea = '' THEN LEAVE.
        FIND LAST ccbddocu WHERE ccbddocu.codcia = s-codcia
            AND ccbddocu.coddoc = SUBSTRING(x-linea,1,3)
            AND ccbddocu.nrodoc = SUBSTRING(x-linea,4,9)
            AND ccbddocu.codmat = SUBSTRING(x-linea,13,6)
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE ccbddocu THEN DO:
            MESSAGE 'ERROR:' x-linea.
            NEXT.
        END.
        CREATE DMOV.
        ASSIGN 
            DMOV.CodCia = CcbDDocu.CodCia
            DMOV.coddoc = CcbDDocu.CodDoc
            DMOV.nrodoc = CcbDDocu.NroDoc
            DMOV.almdes = s-CodAlm
            DMOV.codmat = CcbDDocu.CodMat
            DMOV.CanDes = DECIMAL(SUBSTRING(x-linea,19))
            DMOV.CanDev = DECIMAL(SUBSTRING(x-linea,19))
            DMOV.UndVta = CcbDDocu.UndVta 
            DMOV.Factor = CcbDDocu.Factor 
            DMOV.PreUni = CcbDDocu.PreUni 
            DMOV.PreBas = CcbDDocu.PreBas 
            DMOV.ImpLin = CcbDDocu.ImpLin 
            DMOV.ImpIsc = CcbDDocu.ImpIsc 
            DMOV.ImpIgv = CcbDDocu.ImpIgv 
            DMOV.AftIsc = CcbDDocu.AftIsc 
            DMOV.AftIgv = CcbDDocu.AftIgv
            DMOV.Flg_factor = CcbDDocu.Flg_factor.
        ASSIGN
            DMOV.ImpLin = ROUND( DMOV.PreUni * DMOV.CanDes , 2 ).
/*         IF DMOV.AftIsc                                                                       */
/*             THEN DMOV.ImpIsc = ROUND(DMOV.PreBas * DMOV.CanDes * (Almmmatg.PorIsc / 100),4). */
        IF DMOV.AftIgv 
            THEN DMOV.ImpIgv = DMOV.ImpLin - ROUND(DMOV.ImpLin  / (1 + (FacCfgGn.PorIgv / 100)),4).
    END.
    INPUT CLOSE.

END PROCEDURE.


PROCEDURE Crea-Cabecera:
/* ******************** */

DEF OUTPUT PARAMETER pRowid AS ROWID.

FIND Almacen WHERE Almacen.CodCia = S-CODCIA 
    AND Almacen.CodAlm = s-CodAlm
    EXCLUSIVE-LOCK NO-ERROR.
IF ERROR-STATUS:ERROR THEN DO:
    MESSAGE 'No se pudo bloquear el correlativo'.
    RETURN 'ADM-ERROR'.
END.
ASSIGN 
    I-NroDoc = Almacen.CorrIng
    Almacen.CorrIng = Almacen.CorrIng + 1.
CREATE Almcmov.
ASSIGN 
    Almcmov.CodCia = s-CodCia 
    Almcmov.CodAlm = s-CodAlm
    Almcmov.TipMov = "I"
    Almcmov.CodMov = S-CodMov 
    Almcmov.NroSer = 000
    Almcmov.NroDoc = I-NRODOC
    Almcmov.FchDoc = 12/31/2012
    Almcmov.CodCli = "20100038146"
    Almcmov.NomRef = "CONTINENTAL S.A.C."
    Almcmov.CodRef = DMOV.coddoc
    Almcmov.NroRef = DMOV.nrodoc
    Almcmov.NroRf1 = DMOV.nrodoc
    Almcmov.TpoCmb = FacCfgGn.Tpocmb[1]
    Almcmov.FlgEst = "P"
    Almcmov.HorRcp = "00:00:00"
    Almcmov.usuario = S-USER-ID.
FIND FacCorre WHERE FacCorre.CodCia = S-CODCIA 
    AND FacCorre.CodDoc = S-CODDOC 
    AND FacCorre.CodDiv = S-CODDIV 
    AND FacCorre.CodAlm = S-CODALM
    EXCLUSIVE-LOCK NO-ERROR.
IF ERROR-STATUS:ERROR THEN DO:
    MESSAGE 'No se pudo bloquear el correlativo'.
    RETURN 'ADM-ERROR'.
END.
ASSIGN 
    I-NroDoc = FacCorre.Correlativo
    FacCorre.Correlativo = FacCorre.Correlativo + 1.
ASSIGN 
    Almcmov.NroRf2 = STRING(I-NroSer,"999") + STRING(I-NroDoc,"999999").
FIND CcbCDocu WHERE CcbCDocu.CodCia = S-CODCIA 
    AND  CcbCDocu.CodDoc = Almcmov.CodRef 
    AND  CcbCDocu.NroDoc = Almcmov.NroRef 
    NO-LOCK NO-ERROR.
IF AVAILABLE CcbCDocu THEN DO:
    ASSIGN 
        Almcmov.CodCli = CcbCDocu.CodCli
        Almcmov.CodMon = CcbCDocu.CodMon
        Almcmov.CodVen = CcbCDocu.CodVen
        Almcmov.NomRef  = CcbCDocu.nomcli.
END.
pRowid = ROWID(Almcmov).
RETURN 'OK'.

END PROCEDURE.

PROCEDURE Crea-Detalle:
/* ******************* */

DEF INPUT PARAMETER pRowid AS ROWID.

FIND Almcmov WHERE ROWID(Almcmov) = pRowid.
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
    Almdmov.codmat = DMOV.codmat
    Almdmov.CanDes = DMOV.CanDes
    Almdmov.CodUnd = DMOV.UndVta
    Almdmov.Factor = DMOV.Factor
    Almdmov.ImpCto = DMOV.ImpCto
    Almdmov.PreUni = DMOV.PreUni
    Almdmov.CodAjt = '' 
    Almdmov.PreBas = DMOV.PreBas 
    Almdmov.PorDto = DMOV.PorDto 
    Almdmov.ImpLin = DMOV.ImpLin 
    Almdmov.ImpIsc = DMOV.ImpIsc 
    Almdmov.ImpIgv = DMOV.ImpIgv 
    Almdmov.ImpDto = DMOV.ImpDto 
    Almdmov.AftIsc = DMOV.AftIsc 
    Almdmov.AftIgv = DMOV.AftIgv 
    /*Almdmov.CodAnt = DMOV.CodAnt*/
    Almdmov.Por_Dsctos[1] = DMOV.Por_Dsctos[1]
    Almdmov.Por_Dsctos[2] = DMOV.Por_Dsctos[2]
    Almdmov.Por_Dsctos[3] = DMOV.Por_Dsctos[3]
    Almdmov.Flg_factor = DMOV.Flg_factor
    Almdmov.HraDoc     = Almcmov.HorRcp
    Almdmov.NroItm = x-Item
    R-ROWID = ROWID(Almdmov).
x-Item = x-Item + 1.

/* RUN ALM\ALMACSTK (R-ROWID).                                  */
/* IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'. */
/* /* RHC 31.03.04 REACTIVAMOS KARDEX POR ALMACEN */            */
/* RUN alm/almacpr1 (R-ROWID, 'U').                             */
/* IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'. */

RETURN 'OK'.

END PROCEDURE.

