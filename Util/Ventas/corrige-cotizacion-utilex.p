FIND FIRST faccpedi WHERE codcia = 1 AND coddoc = 'cot' AND nroped = '000119611' AND coddiv = '00017'.
IF AVAILABLE faccpedi THEN DO:
    DISPLAY coddiv nomcli.
    UPDATE Faccpedi.ImpDto2 WITH 1 COL.
    /*
    {vta2/graba-totales-pedido-utilex.i}
    */
      DEFINE VARIABLE F-IGV AS DECIMAL NO-UNDO.
      DEFINE VARIABLE F-ISC AS DECIMAL NO-UNDO.
      DEFINE VARIABLE x-ImpDto2 AS DEC NO-UNDO.     /* OJO: DEscuentos por Encartes */
      DEFINE VARIABLE x-Dto2xExonerados AS DEC NO-UNDO.
      DEFINE VARIABLE x-Dto2xAfectosIgv AS DEC NO-UNDO.
      
      ASSIGN
          FacCPedi.ImpDto = 0
          /*FacCPedi.ImpDto2 = 0*/
          FacCPedi.ImpIgv = 0
          FacCPedi.ImpIsc = 0
          FacCPedi.ImpTot = 0
          FacCPedi.ImpExo = 0
          FacCPedi.Importe[3] = 0
          F-IGV = 0
          F-ISC = 0
          x-ImpDto2 = 0
          x-Dto2xExonerados = 0
          x-Dto2xAfectosIgv = 0.
        
      FOR EACH FacDPedi OF FacCPedi NO-LOCK: 
          F-Igv = F-Igv + FacDPedi.ImpIgv.
          F-Isc = F-Isc + FacDPedi.ImpIsc.
          x-ImpDto2 = x-ImpDto2 + FacDPedi.ImpDto2.
    
          FacCPedi.ImpTot = FacCPedi.ImpTot + FacDPedi.ImpLin.
          FacCPedi.ImpDto2 = FacCPedi.ImpDto2 + FacDPedi.ImpDto2.
    
          IF NOT FacDPedi.AftIgv THEN FacCPedi.ImpExo = FacCPedi.ImpExo + FacDPedi.ImpLin.
          IF FacDPedi.AftIgv = YES
          THEN FacCPedi.ImpDto = FacCPedi.ImpDto + ROUND(FacDPedi.ImpDto / (1 + FacCPedi.PorIgv / 100), 2).
          ELSE FacCPedi.ImpDto = FacCPedi.ImpDto + FacDPedi.ImpDto.
    
          IF NOT FacDPedi.AftIgv THEN x-Dto2xExonerados = x-Dto2xExonerados + FacDPedi.ImpDto2.
          ELSE x-Dto2xAfectosIgv = x-Dto2xAfectosIgv + FacDPedi.ImpDto2.
      END.
    /*   IF Faccpedi.ImpDto2 > 0 THEN                                                                                               */
    /*       ASSIGN                                                                                                                 */
    /*       Faccpedi.ImpTot = Faccpedi.ImpTot - Faccpedi.ImpDto2                                                                   */
    /*       Faccpedi.ImpIgv = Faccpedi.ImpIgv - ROUND( Faccpedi.ImpDto2 / (1 + Faccpedi.PorIgv / 100) * Faccpedi.PorIgv / 100, 2). */
      ASSIGN
        FacCPedi.ImpIgv = ROUND(F-IGV,2)
        FacCPedi.ImpIsc = ROUND(F-ISC,2).
      ASSIGN
          FacCPedi.ImpVta = FacCPedi.ImpTot - FacCPedi.ImpExo - FacCPedi.ImpIgv.
      ASSIGN
          FacCPedi.ImpBrt = FacCPedi.ImpVta + FacCPedi.ImpDto.
    
      /* RHC 06/05/2014 En caso tenga descuento por Encarte */
      IF Faccpedi.ImpDto2 > 0 THEN DO:
          ASSIGN
              Faccpedi.ImpTot = Faccpedi.ImpTot - Faccpedi.ImpDto2
              Faccpedi.ImpIgv = Faccpedi.ImpIgv -  ~
              ROUND(x-Dto2xAfectosIgv / ( 1 + Faccpedi.PorIgv / 100) * Faccpedi.PorIgv / 100, 2)
              Faccpedi.ImpExo = Faccpedi.ImpExo - x-Dto2xExonerados
              FacCPedi.ImpVta = FacCPedi.ImpTot - FacCPedi.ImpExo - FacCPedi.ImpIgv
              FacCPedi.ImpBrt = FacCPedi.ImpVta + FacCPedi.ImpDto.
      END.
END.
ELSE DO:
    MESSAGE "No existe".
END.
