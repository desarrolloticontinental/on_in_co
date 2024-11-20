DEF VAR x-linea AS CHAR NO-UNDO.
DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR pRowid AS ROWID NO-UNDO.
DEF VAR r-Rowid AS ROWID NO-UNDO.
DEF VAR pComprometido AS DEC NO-UNDO.
DEF VAR x-Item AS INT INIT 0 NO-UNDO.

DEFINE TEMP-TABLE ITEM LIKE Almdmov.
    
FIND FacCfgGn WHERE FAccfggn.codcia = s-codcia NO-LOCK.

/* CARGAMOS LOS MOVIMIENTOS DE TRANSFERENCIA */
RUN Carga-Temporal.
/* GENERAMOS LOS MOVIMIENTOS DE TRANSFERENCIA */
FOR EACH ITEM BREAK BY ITEM.codalm BY ITEM.codmat TRANSACTION:
    IF FIRST-OF(ITEM.codalm) THEN DO:
        RUN Crea-Cabecera (OUTPUT pRowid).
        IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.
        x-Item = 1.
    END.
    RUN Crea-Detalle (INPUT pRowid).
    IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.
END.

PROCEDURE Carga-Temporal:
/* ********************* */
    INPUT FROM c:\tmp\salidas-por-transf.prn.
    REPEAT :
        IMPORT UNFORMATTED x-linea.
        IF x-linea = '' THEN LEAVE.
        FIND almmmatg WHERE codcia = s-codcia
            AND codmat = SUBSTRING(x-linea,1,6)
            NO-LOCK.
        IF DECIMAL(SUBSTRING(x-linea,11,10)) > 0 THEN DO:
            CREATE ITEM.
            ASSIGN
                ITEM.codcia = s-codcia
                ITEM.tipmov = "S"
                ITEM.codmov = 03
                ITEM.codmon = 1
                ITEM.codmat = SUBSTRING(x-linea,1,6)
                ITEM.candes = DECIMAL(SUBSTRING(x-linea,11,10))
                ITEM.CodUnd = Almmmatg.UndBas
                ITEM.factor = 1
                ITEM.codalm = '11'
                ITEM.almori = '21'.
        END.
        IF DECIMAL(SUBSTRING(x-linea,21,10)) > 0 THEN DO:
            CREATE ITEM.
            ASSIGN
                ITEM.codcia = s-codcia
                ITEM.tipmov = "S"
                ITEM.codmov = 03
                ITEM.codmon = 1
                ITEM.codmat = SUBSTRING(x-linea,1,6)
                ITEM.candes = DECIMAL(SUBSTRING(x-linea,21,10))
                ITEM.CodUnd = Almmmatg.UndBas
                ITEM.factor = 1
                ITEM.codalm = '21s'
                ITEM.almori = '21'.
        END.
    END.
    INPUT CLOSE.

END PROCEDURE.


PROCEDURE Crea-Cabecera:
/* ******************** */

DEF OUTPUT PARAMETER pRowid AS ROWID.

DEF VAR x-NroDoc LIKE Almcmov.nrodoc INIT 0 NO-UNDO.

FIND Almacen WHERE Almacen.CodCia = S-CODCIA 
    AND Almacen.CodAlm = ITEM.CodAlm 
    EXCLUSIVE-LOCK NO-ERROR.
IF NOT AVAILABLE Almacen THEN DO: 
    MESSAGE 'NO se pudo bloquer el correlativo por almacen' VIEW-AS ALERT-BOX ERROR.
    RETURN 'ADM-ERROR'.
END.
CREATE Almcmov.
ASSIGN 
    x-Nrodoc  = Almacen.CorrSal
    Almacen.CorrSal = Almacen.CorrSal + 1
    Almcmov.CodCia = s-CodCia 
    Almcmov.CodAlm = ITEM.CodAlm 
    Almcmov.AlmDes = ITEM.AlmOri
    Almcmov.TipMov = ITEM.TipMov
    Almcmov.CodMov = ITEM.CodMov
    Almcmov.FlgSit = "T"      /* Transferido pero no Recepcionado */
    Almcmov.FchDoc = 12/31/2012
    Almcmov.HorSal = "00:00"
    Almcmov.HraDoc = "00:00"
    Almcmov.NroSer = 000
    Almcmov.NroDoc = x-NroDoc
    Almcmov.CodRef = ""
    Almcmov.NomRef = ""
    Almcmov.NroRf1 = ""
    Almcmov.NroRf2 = ""
    Almcmov.NroRf3 = ""
    Almcmov.usuario = "ADMIN".
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
    Almdmov.HraDoc = Almcmov.HraDoc
    Almdmov.TpoCmb = Almcmov.TpoCmb
    Almdmov.codmat = ITEM.codmat
    Almdmov.CanDes = ITEM.CanDes
    Almdmov.CodUnd = ITEM.CodUnd
    Almdmov.Factor = ITEM.Factor
    Almdmov.ImpCto = ITEM.ImpCto
    Almdmov.PreUni = ITEM.PreUni
    Almdmov.AlmOri = Almcmov.AlmDes 
    Almdmov.CodAjt = ''
    Almdmov.HraDoc = HorSal
    Almdmov.NroItm = x-Item
    R-ROWID = ROWID(Almdmov).
x-Item = x-Item + 1.
RUN alm/almdcstk (R-ROWID).
IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.
RUN alm/almacpr1 (R-ROWID, "U").
IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.
RETURN 'OK'.

END PROCEDURE.
