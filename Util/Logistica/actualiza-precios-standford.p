DEF VAR x-linea AS CHAR.
DEFINE VAR F-CTOLIS AS DECIMAL NO-UNDO.
DEFINE VAR F-CTOTOT AS DECIMAL NO-UNDO.

FIND lg-cmatpr WHERE codcia = 1
    AND LG-cmatpr.nrolis = 000640.
INPUT FROM c:\tmp\costos.prn.
REPEAT :
    IMPORT UNFORMATTED x-linea.
    IF x-linea = '' THEN LEAVE.
    FIND lg-dmatpr WHERE codcia = 1
        AND LG-dmatpr.nrolis = 000640
        AND codmat = SUBSTRING(x-linea,1,6)
        NO-ERROR.
    IF NOT AVAILABLE lg-dmatpr THEN CREATE lg-dmatpr.
    ASSIGN 
        LG-dmatpr.CodCia = LG-cmatpr.CodCia
        LG-dmatpr.nrolis = LG-cmatpr.nrolis
        LG-dmatpr.codpro = LG-cmatpr.codpro
        LG-dmatpr.FchEmi = LG-cmatpr.FchEmi
        LG-dmatpr.FchVto = LG-cmatpr.FchVto
        LG-dmatpr.tpobien = 1
        LG-dmatpr.FlgEst  = LG-cmatpr.FlgEst
        LG-dmatpr.codmat = SUBSTRING(x-linea,1,6)
        LG-dmatpr.preact = DECIMAL(SUBSTRING(x-linea,7))
        LG-dmatpr.CodMon = 2
        LG-dmatpr.IgvMat = 18.
    FIND Almmmatg WHERE Almmmatg.CodCia = LG-dmatpr.CodCia 
        AND  Almmmatg.codmat = LG-dmatpr.codmat 
        NO-LOCK NO-ERROR.
    IF AVAILABLE Almmmatg THEN DO:
       F-CtoLis = ROUND(LG-dmatpr.PreAct * 
                  (1 - (LG-dmatpr.Dsctos[1] / 100)) * 
                  (1 - (LG-dmatpr.Dsctos[2] / 100)) * 
                  (1 - (LG-dmatpr.Dsctos[3] / 100)) ,4).
       F-CtoTot = ROUND(LG-dmatpr.PreAct * 
                  (1 - (LG-dmatpr.Dsctos[1] / 100)) *
                  (1 - (LG-dmatpr.Dsctos[2] / 100)) *
                  (1 - (LG-dmatpr.Dsctos[3] / 100)) *
                  (1 + (LG-dmatpr.IgvMat / 100)) , 4).

       ASSIGN 
           LG-dmatpr.desmat = Almmmatg.DesMat
           LG-dmatpr.PreAnt = Almmmatg.preant
           LG-dmatpr.PreCos = F-CtoLis           
           LG-dmatpr.CtoLis = F-CtoLis
           LG-dmatpr.CtoTot = F-CtoTot. 
    END.

END.
