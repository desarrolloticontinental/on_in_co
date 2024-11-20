DEF VAR x-linea AS CHAR NO-UNDO.
DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR pv-codcia AS INT INIT 000 NO-UNDO.
DEF VAR s-coddiv AS CHAR init "00021" NO-UNDO.
DEF VAR s-nroser AS INT INIT 229 NO-UNDO.
DEF VAR pRowid AS ROWID NO-UNDO.
DEF VAR r-Rowid AS ROWID NO-UNDO.
DEF VAR pComprometido AS DEC NO-UNDO.
DEF VAR x-Item AS INT INIT 0 NO-UNDO.

DEFINE TEMP-TABLE ITEM LIKE Almdmov.
    
FIND FacCfgGn WHERE FAccfggn.codcia = s-codcia NO-LOCK.

/* CARGAMOS LOS MOVIMIENTOS */
RUN Carga-Temporal.
/* GENERAMOS LOS MOVIMIENTOS CADA 13 ITEMS */
FOR EACH ITEM BREAK BY ITEM.codalm BY ITEM.codmat TRANSACTION:
    IF FIRST-OF(ITEM.codalm) OR x-Item > 13 THEN DO:
        RUN Crea-Cabecera (OUTPUT pRowid).
        IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.
        x-Item = 1.
    END.
    RUN Crea-Detalle (INPUT pRowid).
    IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.
END.

PROCEDURE Carga-Temporal:
/* ********************* */
    INPUT FROM c:\tmp\dev-conti-a-cissac.prn.
    REPEAT :
        IMPORT UNFORMATTED x-linea.
        IF x-linea = '' THEN LEAVE.
        FIND almmmatg WHERE codcia = s-codcia
            AND codmat = SUBSTRING(x-linea,1,6)
            NO-LOCK.
        CREATE ITEM.
        ASSIGN
            ITEM.codcia = s-codcia
            ITEM.tipmov = "S"
            ITEM.codmov = 09
            ITEM.codmon = 1
            ITEM.codmat = SUBSTRING(x-linea,1,6)
            ITEM.candes = DECIMAL(SUBSTRING(x-linea,7))
            ITEM.CodUnd = Almmmatg.UndBas
            ITEM.factor = 1
            ITEM.codalm = '21'.
    END.
    INPUT CLOSE.

END PROCEDURE.


PROCEDURE Crea-Cabecera:
/* ******************** */

DEF OUTPUT PARAMETER pRowid AS ROWID.

FIND FIRST FacCorre WHERE FacCorre.CodCia = S-CODCIA 
    AND FacCorre.CodDoc = "G/R" 
    AND FacCorre.CodDiv = S-CODDIV 
    AND FacCorre.NroSer = S-NROSER EXCLUSIVE-LOCK NO-ERROR.
IF ERROR-STATUS:ERROR THEN DO:
    MESSAGE 'No se pudo bloquear el correlativo'.
    RETURN 'ADM-ERROR'.
END.
CREATE Almcmov.
ASSIGN 
    Almcmov.CodCia = s-CodCia 
    Almcmov.CodAlm = ITEM.CodAlm 
    Almcmov.TipMov = ITEM.TipMov 
    Almcmov.CodMov = ITEM.CodMov 
    Almcmov.NroSer = S-NROSER
    Almcmov.FchDoc = 12/31/2012
    Almcmov.HorRcp = "00:00:00"
    Almcmov.CodPro = "51135890"
    Almcmov.CodMon = 1.
FIND LAST gn-tcmb WHERE gn-tcmb.fecha <= Almcmov.FchDoc NO-LOCK NO-ERROR.
IF AVAILABLE gn-tcmb THEN Almcmov.TpoCmb = gn-tcmb.venta.
FIND gn-prov WHERE gn-prov.CodCia = PV-CODCIA 
    AND gn-prov.CodPro = Almcmov.CodPro NO-LOCK NO-ERROR.
ASSIGN 
    Almcmov.NroDoc = FacCorre.Correlativo
    FacCorre.Correlativo = FacCorre.Correlativo + 1
    Almcmov.NomRef = gn-prov.nompro.
ASSIGN 
    Almcmov.usuario = "ADMIN"
    Almcmov.ImpIgv  = 0
    Almcmov.ImpMn1  = 0
    Almcmov.ImpMn2  = 0.

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
    Almdmov.TpoCmb = Almcmov.TpoCmb
    Almdmov.FchDoc = Almcmov.FchDoc 
    Almdmov.HraDoc = Almcmov.HraDoc
    Almdmov.TpoCmb = Almcmov.TpoCmb
    Almdmov.codmat = ITEM.codmat
    Almdmov.CanDes = ITEM.CanDes
    Almdmov.CodUnd = ITEM.CodUnd
    Almdmov.Factor = ITEM.Factor
    Almdmov.ImpCto = ITEM.ImpCto
    Almdmov.PreUni = ITEM.PreUni
    Almdmov.IgvMat = 18.00
    Almdmov.CodAjt = ''
    Almdmov.HraDoc = Almcmov.HorRcp
    Almdmov.NroItm = x-Item
    R-ROWID = ROWID(Almdmov).
x-Item = x-Item + 1.

RUN alm/almdcstk (R-ROWID).
IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.
RUN ALM\ALMACPR1 (R-ROWID,"U").
IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.
/* IF AlmDMov.CodMon = 1 THEN DO:                                                                                */
/*     Almcmov.ImpIgv  = Almcmov.ImpIgv  + ROUND(AlmDMov.ImpCto * Almdmov.IgvMat / 100 ,2).                      */
/*     ASSIGN Almcmov.ImpMn1  = Almcmov.ImpMn1 + AlmDMov.ImpCto.                                                 */
/*     IF AlmDMov.TpoCmb > 0 THEN                                                                                */
/*         ASSIGN  Almcmov.ImpMn2  = Almcmov.ImpMn2 + ROUND(AlmDMov.ImpCto / AlmDMov.TpoCmb,2).                  */
/* END.                                                                                                          */
/* ELSE  DO:                                                                                                     */
/*     ASSIGN Almcmov.ImpMn1  = Almcmov.ImpMn1 + ROUND(AlmDMov.ImpCto * AlmDMov.TpoCmb,2)                        */
/*         Almcmov.ImpMn2  = Almcmov.ImpMn2 + AlmDMov.ImpCto                                                     */
/*         Almcmov.ImpIgv  = Almcmov.ImpIgv  + ROUND(AlmDMov.ImpCto * AlmDMov.TpoCmb * Almdmov.IgvMat / 100 ,2). */
/* END.                                                                                                          */

RETURN 'OK'.

END PROCEDURE.
