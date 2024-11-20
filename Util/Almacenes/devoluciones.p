DEFINE TEMP-TABLE ITEM LIKE almdmov.

DEF TEMP-TABLE detalle
    FIELD ordcmp AS CHAR
    FIELD nrogui AS CHAR
    FIELD codalm AS CHAR
    FIELD codmat AS CHAR
    FIELD candes AS DEC
    FIELD preuni AS DEC DECIMALS 4.

DEF VAR x-linea AS CHAR.

INPUT FROM d:\tmp\devoluciones.prn.
REPEAT:
    IMPORT UNFORMATTED x-linea.
    IF x-linea = '' THEN LEAVE.
    CREATE detalle.
    ASSIGN
        detalle.ordcmp = SUBSTRING(x-linea,1,20)
        detalle.nrogui = SUBSTRING(x-linea,21,20)
        detalle.codalm = SUBSTRING(x-linea,41,10)
        detalle.codmat = SUBSTRING(x-linea,51,10)
        detalle.candes = DECIMAL(SUBSTRING(x-linea,61,20))
        detalle.preuni = DECIMAL(SUBSTRING(x-linea,81)).
END.
INPUT CLOSE.


DEF VAR s-codcia AS INT INIT 001.
DEF VAR s-codalm AS CHAR.
DEF VAR s-tpomov AS CHAR INIT 'S'.
DEF VAR s-codmov AS INT INIT 09.
DEF VAR s-nroser AS INT INIT 228.
DEF VAR s-coddiv AS CHAR INIT '00021'.
DEF VAR s-user-id AS CHAR INIT 'ADMIN'.

/* Acumulamos por ciertos criterios */
FOR EACH detalle, FIRST Almmmatg NO-LOCK WHERE Almmmatg.codcia = s-codcia
    AND Almmmatg.codmat = detalle.codmat
    BREAK BY detalle.nrogui BY detalle.codalm BY detalle.codmat:
    IF FIRST-OF(detalle.nrogui) OR FIRST-OF(detalle.codalm) 
        THEN EMPTY TEMP-TABLE ITEM.
    FIND ITEM WHERE ITEM.codmat = detalle.codmat NO-ERROR.
    IF NOT AVAILABLE ITEM THEN CREATE ITEM.
    ASSIGN
        ITEM.codmat = detalle.codmat
        ITEM.candes = ITEM.candes + detalle.candes
        ITEM.preuni = detalle.preuni
        ITEM.prelis = detalle.preuni.
    ASSIGN 
        ITEM.CodCia    = s-CodCia 
        ITEM.CodAlm    = detalle.CodAlm 
        ITEM.CanDev    = ITEM.CanDes 
        ITEM.Factor    = 1
        ITEM.CodUnd    = Almmmatg.undstk
        ITEM.IgvMat    = 18.

    IF LAST-OF(detalle.nrogui) OR LAST-OF(detalle.codalm) THEN DO:
        s-codalm = detalle.codalm.
        RUN Crea-Registro.
    END.
END.



PROCEDURE Crea-Registro:

FIND Almtdocm WHERE Almtdocm.CodCia = S-CODCIA
    AND Almtdocm.CodAlm = S-CODALM
    AND Almtdocm.TipMov = S-TPOMOV
    AND Almtdocm.CodMov = S-CODMOV NO-LOCK NO-ERROR.
IF NOT AVAILABLE Almtdocm THEN DO:
    MESSAGE 'NO configurado mov' s-tpomov s-codmov 'del almacén' s-codalm
        VIEW-AS ALERT-BOX ERROR.
    RETURN 'ADM-ERROR'.
END.

CREATE Almcmov.
ASSIGN 
    Almcmov.CodCia  = Almtdocm.CodCia 
    Almcmov.CodAlm  = Almtdocm.CodAlm 
    Almcmov.TipMov  = Almtdocm.TipMov 
    Almcmov.CodMov  = Almtdocm.CodMov 
    Almcmov.NroSer  = S-NROSER
    Almcmov.FchDoc  = DATE(01,31,2016)
    Almcmov.HorRcp  = STRING(TIME,"HH:MM:SS")
    Almcmov.nrorf1  = detalle.ordcmp
    Almcmov.nrorf2  = detalle.nrogui
    Almcmov.codpro  = "51135890"
    Almcmov.NomRef  = "CORPORACION DE INDUSTRIAS STANDFORD S.A.C."
    Almcmov.codmon  = 1.
DISPLAY almcmov.codalm almcmov.nroser almcmov.nrodoc.
PAUSE 0.

FIND LAST gn-tcmb WHERE gn-tcmb.fecha <= Almcmov.FchDoc NO-LOCK NO-ERROR.
IF AVAILABLE gn-tcmb THEN Almcmov.TpoCmb = gn-tcmb.venta.
FIND FIRST FacCorre WHERE 
    FacCorre.CodCia = S-CODCIA AND
    FacCorre.CodDoc = "G/R" AND
    FacCorre.CodDiv = S-CODDIV AND
    FacCorre.NroSer = S-NROSER EXCLUSIVE-LOCK NO-ERROR.
IF ERROR-STATUS:ERROR THEN UNDO, RETURN 'ADM-ERROR'.
ASSIGN 
    Almcmov.NroDoc = FacCorre.Correlativo.
FacCorre.Correlativo = FacCorre.Correlativo + 1.

ASSIGN 
    Almcmov.usuario = S-USER-ID
    Almcmov.ImpIgv  = 0
    Almcmov.ImpMn1  = 0
    Almcmov.ImpMn2  = 0.
/* GENERAMOS NUEVO DETALLE */
RUN Genera-Detalle.
IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.

RETURN 'OK'.

              
END PROCEDURE.


PROCEDURE Genera-Detalle:

DEF VAR R-ROWID AS ROWID.

FOR EACH ITEM WHERE ON ERROR UNDO, RETURN "ADM-ERROR":
    CREATE almdmov.
    ASSIGN Almdmov.CodCia = Almcmov.CodCia 
           Almdmov.CodAlm = Almcmov.CodAlm 
           Almdmov.TipMov = Almcmov.TipMov 
           Almdmov.CodMov = Almcmov.CodMov 
           Almdmov.NroSer = Almcmov.NroSer 
           Almdmov.NroDoc = Almcmov.NroDoc 
           Almdmov.CodMon = Almcmov.CodMon 
           Almdmov.FchDoc = Almcmov.FchDoc 
           Almdmov.TpoCmb = Almcmov.TpoCmb
           Almdmov.codmat = ITEM.codmat
           Almdmov.CanDes = ITEM.CanDes
           Almdmov.CodUnd = ITEM.CodUnd
           Almdmov.Factor = ITEM.Factor
           Almdmov.PreUni = ITEM.PreUni
           Almdmov.ImpCto = ROUND(ITEM.CanDes * ITEM.PreUni,2)
           Almdmov.PreLis = ITEM.PreLis
           Almdmov.Dsctos[1] = ITEM.Dsctos[1]
           Almdmov.Dsctos[2] = ITEM.Dsctos[2]
           Almdmov.Dsctos[3] = ITEM.Dsctos[3]
           Almdmov.IgvMat = ITEM.IgvMat
           Almdmov.CodAjt = ' '
           Almdmov.HraDoc = Almcmov.HorRcp
                  R-ROWID = ROWID(Almdmov).

    RUN alm/almdcstk (R-ROWID).
    IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.

    RUN ALM\ALMACPR1 (R-ROWID,"U").
    IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.

    IF AlmDMov.CodMon = 1 THEN DO:
         Almcmov.ImpIgv  = Almcmov.ImpIgv  + ROUND(AlmDMov.ImpCto * Almdmov.IgvMat / 100 ,2).
         ASSIGN Almcmov.ImpMn1  = Almcmov.ImpMn1 + AlmDMov.ImpCto.
         IF AlmDMov.TpoCmb > 0 THEN 
            ASSIGN  Almcmov.ImpMn2  = Almcmov.ImpMn2 + ROUND(AlmDMov.ImpCto / AlmDMov.TpoCmb,2).
      END.
    ELSE  DO:
      ASSIGN Almcmov.ImpMn1  = Almcmov.ImpMn1 + ROUND(AlmDMov.ImpCto * AlmDMov.TpoCmb,2)
             Almcmov.ImpMn2  = Almcmov.ImpMn2 + AlmDMov.ImpCto
             Almcmov.ImpIgv  = Almcmov.ImpIgv  + ROUND(AlmDMov.ImpCto * AlmDMov.TpoCmb * Almdmov.IgvMat / 100 ,2).
    END.
END.


END PROCEDURE.
