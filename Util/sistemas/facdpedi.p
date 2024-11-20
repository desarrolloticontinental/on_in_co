OUTPUT TO d:\faccpedi2018.txt.
FOR EACH faccpedi NO-LOCK WHERE codcia = 1 and 
    lookup(coddoc, 'OTR,O/D,O/M') > 0 
    and fchped >= 01/01/2018 AND fchped <= DATE(12,31,2018):
    DISPLAY faccpedi WITH STREAM-IO NO-BOX WIDTH 320.
END.
OUTPUT CLOSE.
OUTPUT TO d:\facdpedi2018.txt.
FOR EACH faccpedi NO-LOCK WHERE codcia = 1 and 
    lookup(coddoc, 'OTR,O/D,O/M') > 0 
    and fchped >= 01/01/2018 AND fchped <= DATE(12,31,2018),
    EACH facdpedi OF faccpedi NO-LOCK:
    DISPLAY
        FacDPedi.AftIgv 
        FacDPedi.AlmDes 
        FacDPedi.AlmTrf 
        FacDPedi.CanApr 
        FacDPedi.canate 
        FacDPedi.CanPed 
        FacDPedi.CodAux 
        FacDPedi.CodCia 
        FacDPedi.CodDiv 
        FacDPedi.CodDoc 
        FacDPedi.codmat 
        FacDPedi.Factor 
        FacDPedi.FchPed 
        FacDPedi.FlgEst 
        FacDPedi.ImpLin 
        FacDPedi.NroItm 
        FacDPedi.NroPed 
        FacDPedi.Pesmat 
        FacDPedi.TipVta 
        FacDPedi.UndVta
        WITH STREAM-IO NO-BOX WIDTH 320.
END.
OUTPUT CLOSE.
