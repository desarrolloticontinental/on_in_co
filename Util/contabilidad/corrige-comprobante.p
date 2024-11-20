  DEFINE VARIABLE F-IGV AS DEC NO-UNDO.
  DEFINE VARIABLE F-ISC AS DEC NO-UNDO.
  DEFINE VARIABLE F-ImpDtoAdelanto AS DECIMAL NO-UNDO.
  DEFINE VARIABLE F-IgvDtoAdelanto AS DECIMAL NO-UNDO.
  DEFINE VARIABLE F-ImpLin AS DEC NO-UNDO.
  DEFINE VARIABLE x-Dto2xExonerados AS DEC NO-UNDO.
  DEFINE VARIABLE x-Dto2xAfectosIgv AS DEC NO-UNDO.


FIND ccbcdocu WHERE ccbcdocu.codcia = 1
    AND ccbcdocu.coddoc = 'BOL'
    AND ccbcdocu.nrodoc = '22300000695'.
FOR EACH ccbddocu OF ccbcdocu EXCLUSIVE-LOCK:
    DISPLAY codmat candes.
    MESSAGE 'anulamos?' VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
        UPDATE rpta AS LOG.
    IF rpta = YES THEN DELETE ccbddocu.
END.

  ASSIGN
      ccbcdocu.ImpDto = 0
      ccbcdocu.ImpDto2 = 0
      ccbcdocu.ImpIgv = 0
      ccbcdocu.ImpIsc = 0
      ccbcdocu.ImpTot = 0
      ccbcdocu.ImpExo = 0
      ccbcdocu.ImpTot2 = 0
      F-IGV = 0
      F-ISC = 0
      x-Dto2xExonerados = 0
      x-Dto2xAfectosIgv = 0
      F-ImpDtoAdelanto = 0
      F-IgvDtoAdelanto = 0
      F-ImpLin = 0
      /*ccbcdocu.Libre_d01 = 0*/    /* DESCUENTO LISTA EXPRESS WEB */
      ccbcdocu.Libre_d02 = 0.
  /* RHC 14/03/2013 Nuevo cálculo */
  FOR EACH ccbddocu OF ccbcdocu EXCLUSIVE-LOCK:
      F-Igv = F-Igv + ccbddocu.ImpIgv.
      F-Isc = F-Isc + ccbddocu.ImpIsc.

      ccbcdocu.ImpTot = ccbcdocu.ImpTot + ccbddocu.ImpLin.
      ccbcdocu.ImpDto2 = ccbcdocu.ImpDto2 + ccbddocu.ImpDto2.

      IF NOT ccbddocu.AftIgv THEN ccbcdocu.ImpExo = ccbcdocu.ImpExo + ccbddocu.ImpLin.
      IF ccbddocu.AftIgv = YES
      THEN ccbcdocu.ImpDto = ccbcdocu.ImpDto + ROUND(ccbddocu.ImpDto / (1 + ccbcdocu.PorIgv / 100), 2).
      ELSE ccbcdocu.ImpDto = ccbcdocu.ImpDto + ccbddocu.ImpDto.

      IF NOT ccbddocu.AftIgv THEN x-Dto2xExonerados = x-Dto2xExonerados + ccbddocu.ImpDto2.
      ELSE x-Dto2xAfectosIgv = x-Dto2xAfectosIgv + ccbddocu.ImpDto2.
  END.
  ASSIGN
      ccbcdocu.ImpIgv = ROUND(F-IGV,2)
      ccbcdocu.ImpIsc = ROUND(F-ISC,2).
  ASSIGN
      ccbcdocu.ImpVta = ccbcdocu.ImpTot - ccbcdocu.ImpExo - ccbcdocu.ImpIgv.
  ASSIGN
      ccbcdocu.ImpBrt = ccbcdocu.ImpVta + ccbcdocu.ImpDto
      ccbcdocu.SdoAct = ccbcdocu.ImpTot.
  /* RHC 06/05/2014 En caso tenga descuento por Encarte */
  IF ccbcdocu.ImpDto2 > 0 THEN DO:
      ASSIGN
          ccbcdocu.ImpTot = ccbcdocu.ImpTot - ccbcdocu.ImpDto2
          ccbcdocu.ImpIgv = ccbcdocu.ImpIgv -  ~
                ROUND(x-Dto2xAfectosIgv / ( 1 + ccbcdocu.PorIgv / 100) * ccbcdocu.PorIgv / 100, 2)
          ccbcdocu.ImpExo = ccbcdocu.ImpExo - x-Dto2xExonerados
          ccbcdocu.ImpVta = ccbcdocu.ImpTot - ccbcdocu.ImpExo - ccbcdocu.ImpIgv
          ccbcdocu.ImpBrt = ccbcdocu.ImpVta + ccbcdocu.ImpDto.
  END.
  IF ccbcdocu.PorIgv = 0.00     /* VENTA INAFECTA */
      THEN ASSIGN
            ccbcdocu.ImpIgv = 0
            ccbcdocu.ImpVta = ccbcdocu.ImpExo
            ccbcdocu.ImpBrt = ccbcdocu.ImpExo.

