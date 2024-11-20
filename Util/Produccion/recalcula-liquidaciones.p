DEF VAR s-codcia AS INT INIT 001.
DEF VAR x-linea AS CHAR FORMAT 'x(100)'.
DISABLE TRIGGERS FOR LOAD OF pr-liqc.
DISABLE TRIGGERS FOR LOAD OF almcmov.
DISABLE TRIGGERS FOR LOAD OF almdmov.

FIND PR-CFGPRO WHERE PR-CFGPRO.Codcia = S-CODCIA 
                     NO-LOCK NO-ERROR.

IF NOT AVAILABLE PR-CFGPRO THEN DO:
   MESSAGE "Registro de Configuracion de Produccion no existe"
           VIEW-AS ALERT-BOX ERROR.
           RETURN.
END.

INPUT FROM c:\tmp\cissac.prn.
REPEAT:
    IMPORT UNFORMATTED x-linea.
    IF x-linea <> '' THEN DO:
        DISPLAY x-linea FORMA 'x(50)' WITH STREAM-IO.
        PAUSE 0.
        FIND pr-liqc WHERE codcia = s-codcia
            AND numliq = SUBSTRING(x-linea,1,6).
        ASSIGN
            PR-LIQC.ctogas = DEC(SUBSTRING(x-linea,7))
            PR-LIQC.CtoTot = PR-LIQC.CtoMat + PR-LIQC.CtoHor + PR-LIQC.CtoGas + PR-LIQC.CtoFab 
            PR-LIQC.PreUni = PR-LIQC.CtoTot / PR-LIQC.CanFin.
        RUN Actualizar-Costos.
    END.
END.
INPUT CLOSE.


PROCEDURE Actualizar-Costos:
/* ************************ */

DEFINE VAR F-INGRESO AS CHAR INIT "50".
DEFINE VAR I AS INTEGER.

IF NOT AVAILABLE PR-LIQC OR PR-LIQC.FlgEst = "A" THEN RETURN.

FIND PR-ODPC WHERE PR-ODPC.Codcia = PR-LIQC.Codcia AND
                   PR-ODPC.NumOrd = PR-LIQC.NumOrd
                   NO-LOCK NO-ERROR.
IF AVAILABLE PR-ODPC THEN DO:                   
    FOR EACH Almcmov EXCLUSIVE-LOCK WHERE Almcmov.Codcia = S-CODCIA AND
        Almcmov.CodAlm = PR-ODPC.CodAlm AND
        Almcmov.TipMov = PR-CFGPRO.TipMov[2] AND
        Almcmov.Codmov = PR-CFGPRO.CodMov[2] AND
        Almcmov.CodRef = "OP" AND
        Almcmov.Nroref = PR-ODPC.NumOrd AND
        Almcmov.FchDoc >= PR-LIQC.FecIni AND
        Almcmov.FchDoc <= PR-LIQC.FecFin:
        ASSIGN
            Almcmov.ImpMn1 = 0
            Almcmov.ImpMn2 = 0
            Almcmov.CodMon = 1.
        FOR EACH Almdmov OF Almcmov EXCLUSIVE-LOCK:
            ASSIGN
                Almdmov.Codmon = Almcmov.CodMon
                Almdmov.ImpCto = PR-LIQC.PreUni * Almdmov.CanDes /*PR-LIQCX.PreUni * Almdmov.CanDes*/
                Almdmov.PreLis = PR-LIQC.PreUni /*PR-LIQCX.PreUni*/
                Almdmov.PreUni = PR-LIQC.PreUni /*PR-LIQCX.PreUni*/
                Almdmov.Dsctos[1] = 0
                Almdmov.Dsctos[2] = 0
                Almdmov.Dsctos[3] = 0           
                Almdmov.ImpMn1    = PR-LIQC.PreUni * Almdmov.CanDes  /* PR-LIQCX.PreUni * Almdmov.CanDes*/
                Almdmov.ImpMn2    = PR-LIQC.PreUni * Almdmov.CanDes / Almcmov.TpoCmb /*PR-LIQCX.PreUni * Almdmov.CanDes / Almcmov.TpoCmb */.              
            IF Almcmov.codmon = 1 THEN Almcmov.ImpMn1 = Almcmov.ImpMn1 + Almdmov.ImpMn1.
            ELSE Almcmov.ImpMn2 = Almcmov.ImpMn2 + Almdmov.ImpMn2.
        END.
    END.
END.

END PROCEDURE.
