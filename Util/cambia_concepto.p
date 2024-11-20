
OUTPUT TO personal_502.txt.
FOR EACH PL-MOV-MES WHERE 
    PL-MOV-MES.CodCia = 1 AND
    PL-MOV-MES.Periodo = 2009 AND
    PL-MOV-MES.NroMes >= 1 AND
    PL-MOV-MES.CodPln = 1 AND
    PL-MOV-MES.CodCal >= 0 AND
    PL-MOV-MES.CodPer >= "" AND
    PL-MOV-MES.CODMOV = 502 NO-LOCK,
    FIRST pl-pers OF PL-MOV-MES NO-LOCK
    BREAK BY PL-MOV-MES.CodPer BY PL-MOV-MES.NroMes BY PL-MOV-MES.CodCal:
    DISPLAY
        PL-MOV-MES.CodPer
        pl-pers.nomper
        pl-pers.patper
        pl-pers.matper
        PL-MOV-MES.NroMes
        PL-MOV-MES.CodCal
        PL-MOV-MES.ValCal-Mes
        WITH WIDTH 300 STREAM-IO.
END.
OUTPUT CLOSE.
