for each LG-COCmp where
    LG-COCmp.CodCia = 1 and
    LG-COCmp.CodDiv = "00000" and
    LG-COCmp.NroDoc = 1223:
    
    display
        LG-COCmp.nrodoc
        LG-COCmp.fchdoc
        CodPro
        FchVto
        ImpBrt
        FlgSit
        codmon
        NomPro.
    
        update FlgSit.
        
/*
display LG-COCmp.CndCmp LG-COCmp.CodAlm LG-COCmp.CodCia LG-COCmp.CodDiv LG-COCmp.CodMaq LG-COCmp.CodMar LG-COCmp.Codmon LG-COCmp.CodPro LG-COCmp.FchAte LG-COCmp.Fchdoc LG-COCmp.FchEmb LG-COCmp.FchEnt LG-COCmp.FchVto LG-COCmp.FlgSit LG-COCmp.ImpBrt LG-COCmp.ImpDto LG-COCmp.ImpExo LG-COCmp.ImpFle LG-COCmp.ImpIgv LG-COCmp.ImpNet LG-COCmp.ImpTot LG-COCmp.Linea LG-COCmp.ModAdq LG-COCmp.MonPro LG-COCmp.NomPro LG-COCmp.NroCot LG-COCmp.NroDoc LG-COCmp.NroRef LG-COCmp.NroReq LG-COCmp.Observaciones LG-COCmp.SerReq LG-COCmp.TpoCmb LG-COCmp.TpoCmbP LG-COCmp.TpoDoc LG-COCmp.Userid-com LG-COCmp.UsoInt with stream-io 2 col. 
*/

      for  EACH LG-DOCmp WHERE
      LG-DOCmp.CodCia = LG-COCmp.CodCia
  AND LG-DOCmp.TpoDoc = LG-COCmp.TpoDoc
  AND LG-DOCmp.NroDoc = LG-COCmp.NroDoc
  AND LG-DOCmp.CodDiv = LG-COCmp.CodDiv:
                update FlgSit.
    display LG-DOCmp with 1 col.
 end.

 end.
