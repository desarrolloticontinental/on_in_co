def var f-mrguti-a as dec.
def var f-prevta-a as dec.
def var x-ctotot as dec.
def var f-factor as dec.

FIND FacCfgGn WHERE FacCfgGn.CodCia = 001 NO-LOCK NO-ERROR.

for each almmmatg where codcia = 1 and prealt[1] <> 0:
  /* calculo del margen de utilidad */
  ASSIGN
       F-MrgUti-A = Almmmatg.MrgAlt[1]
       F-PreVta-A = Almmmatg.PreAlt[1]
       x-CtoTot   = Almmmatg.ctotot.
    IF Almmmatg.AftIgv THEN X-CTOTOT = X-CTOTOT / (1 + FacCfgGn.PorIgv / 100).
    F-FACTOR = 1.
    /****   Busca el Factor de conversion   ****/
    IF UndAlt[1] <> "" 
    THEN DO:
        FIND Almtconv WHERE Almtconv.CodUnid = Almmmatg.UndBas
            AND  Almtconv.Codalter = UndAlt[1] 
            NO-LOCK NO-ERROR.
        IF AVAILABLE Almtconv THEN F-FACTOR = Almtconv.Equival.
        F-MrgUti-A = ROUND(((((((F-PreVta-A / F-FACTOR) / (1 + FacCfgGn.PorIgv / 100)) ) / x-ctotot) - 1) * 100), 6).
    END.
    /*******************************************/
    IF F-PreVta-A = 0 THEN F-MrgUti-A = 0.
    mrgalt[1] = F-MrgUti-A.
end.
