DEF VAR s-codcia AS INT INIT 001.
DEF VAR f-Factor AS DEC NO-UNDO.

FOR EACH vtalistamay NO-LOCK WHERE codcia = 1
    AND coddiv = '00000'
    AND LOOKUP(codfam, '010,012') > 0:
    DISPLAY codmat codfam.
    PAUSE 0.
    RUN Actualiza-precio.
END.


PROCEDURE Actualiza-Precio:

    /* Actualiza Tabla de Materiales  SOLO para list ade precios activas */
    DEFINE VARIABLE F-PreVta-A AS DECIMAL NO-UNDO.
    DEFINE VARIABLE F-PreVta-B AS DECIMAL NO-UNDO.
    DEFINE VARIABLE F-PreVta-C AS DECIMAL NO-UNDO.
    DEFINE VARIABLE X-CTOUND AS DECIMAL NO-UNDO.
    DEFINE VARIABLE F-MrgUti-A AS DECIMAL NO-UNDO.
    DEFINE VARIABLE F-MrgUti-B AS DECIMAL NO-UNDO.
    DEFINE VARIABLE F-MrgUti-C AS DECIMAL NO-UNDO.
    DEFINE VARIABLE F-MrgDto-A AS DECIMAL NO-UNDO.
    DEFINE VARIABLE F-MrgDto-B AS DECIMAL NO-UNDO.
    DEFINE VARIABLE F-MrgDto-C AS DECIMAL NO-UNDO.
    DEFINE VARIABLE f-MonVta LIKE Almmmatg.MonVta NO-UNDO.

    /* 15.09.10 SOLO PARA LISTAS ACTIVAS */
    FIND FacCfgGn WHERE Faccfggn.codcia = s-codcia NO-LOCK.
    FIND almmmatg OF vtalistamay EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE Almmmatg THEN RETURN "ADM-ERROR".
    ASSIGN 
        Almmmatg.PreVta[1] = VtaListaMay.PreOfi
        Almmmatg.PreOfi    = VtaListaMay.PreOfi
        f-MrgDto-A = 8
        f-MrgDto-B = 9
        f-MrgDto-C = 9.
    IF Almmmatg.MonVta <> VtaListaMay.MonVta THEN DO:
        IF Almmmatg.MonVta = 1 THEN DO:
            ASSIGN
                Almmmatg.PreVta[1] = VtaListaMay.PreOfi * Almmmatg.TpoCmb
                Almmmatg.PreOfi    = VtaListaMay.PreOfi * Almmmatg.TpoCmb.
        END.
        IF Almmmatg.MonVta = 2 THEN DO:
            ASSIGN
                Almmmatg.PreVta[1] = VtaListaMay.PreOfi / Almmmatg.TpoCmb
                Almmmatg.PreOfi    = VtaListaMay.PreOfi / Almmmatg.TpoCmb.
        END.
    END.

    /**** MARGEN PRECIO DE LISTA ****/
    ASSIGN
        Almmmatg.MrgUti = ((Almmmatg.Prevta[1] / Almmmatg.Ctotot) - 1 ) * 100. 
    /****   PRECIO A   ****/
    ASSIGN
        F-PreVta-A = Almmmatg.Prevta[2]
        X-CTOUND = Almmmatg.CtoTot
        F-FACTOR = 1.
    IF Almmmatg.UndA <> "" THEN DO:
        FIND Almtconv WHERE Almtconv.CodUnid = Almmmatg.UndBas
            AND  Almtconv.Codalter = Almmmatg.UndA
            NO-LOCK NO-ERROR.
        IF AVAILABLE Almtconv THEN DO:
            F-FACTOR = Almtconv.Equival.
            F-PreVta-A = Almmmatg.PreOfi * ( 1 - F-MrgDto-A / 100 ) * f-Factor.
            F-MrgUti-A = ROUND((((((F-PreVta-A / F-FACTOR) ) / X-CTOUND) - 1) * 100), 6).
        END.
    END.
    /****   PRECIO B   ****/
    ASSIGN
        F-PreVta-B = Almmmatg.Prevta[3]
        X-CTOUND = Almmmatg.CtoTot
        F-FACTOR = 1.
    IF Almmmatg.UndB <> "" THEN DO:
        FIND Almtconv WHERE Almtconv.CodUnid = Almmmatg.UndBas
            AND  Almtconv.Codalter = Almmmatg.UndB
            NO-LOCK NO-ERROR.
        IF AVAILABLE Almtconv THEN DO:
            F-FACTOR = Almtconv.Equival.
            F-PreVta-B = Almmmatg.PreOfi * ( 1 - F-MrgDto-B / 100 ) * f-Factor.
            F-MrgUti-B = ROUND((((((F-PreVta-B / F-FACTOR) ) / X-CTOUND) - 1) * 100), 6).
        END.
    END.
    /****   PRECIO C   ****/
    ASSIGN
        F-PreVta-C = Almmmatg.Prevta[4]
        X-CTOUND = Almmmatg.CtoTot
        F-FACTOR = 1.
    IF Almmmatg.UndC <> "" THEN DO:
        FIND Almtconv WHERE Almtconv.CodUnid = Almmmatg.UndBas
            AND  Almtconv.Codalter = Almmmatg.UndC
            NO-LOCK NO-ERROR.
        IF AVAILABLE Almtconv THEN DO:
            F-FACTOR = Almtconv.Equival.
            F-PreVta-C = Almmmatg.PreOfi * ( 1 - F-MrgDto-C / 100 ) * f-Factor.
            F-MrgUti-C = ROUND((((((F-PreVta-C / F-FACTOR) ) / X-CTOUND) - 1) * 100), 6).
        END.
    END.

    ASSIGN
        Almmmatg.PreVta[2] = f-PreVta-A
        Almmmatg.PreVta[3] = f-PreVta-B
        Almmmatg.PreVta[4] = f-PreVta-C
        Almmmatg.MrgUti-A  = f-MrgUti-A
        Almmmatg.MrgUti-B  = f-MrgUti-B
        Almmmatg.MrgUti-C  = f-MrgUti-C.

      /**** MARGEN PRECIO DE OFICINA ****/
  DEFINE VARIABLE fMot LIKE Almmmatg.PreOfi.
  DEFINE VARIABLE MrgOfi LIKE Almmmatg.MrgUti-A.

  ASSIGN
      fMot   = 0
      MrgOfi = 0
      F-FACTOR = 1
      X-CTOUND = Almmmatg.CtoTot.
  CASE Almmmatg.Chr__02 :
      WHEN "T" THEN DO:        
          IF Almmmatg.Chr__01 <> "" THEN DO:
              FIND Almtconv WHERE Almtconv.CodUnid = Almmmatg.UndBas
                  AND  Almtconv.Codalter = Almmmatg.Chr__01
                  NO-LOCK NO-ERROR.
              IF AVAILABLE Almtconv THEN DO:
                  F-FACTOR = Almtconv.Equival.
              END.
          END.
          fMot = Almmmatg.PreOfi / X-CTOUND / F-FACTOR.
          MrgOfi = ROUND((fMot - 1) * 100, 6).
      END.
      WHEN "P" THEN DO:
          MrgOfi = ((Almmmatg.Prevta[1] / Almmmatg.Ctotot) - 1 ) * 100. 
      END. 
  END.    
  ASSIGN
      Almmmatg.DEC__01 = MrgOfi.

END PROCEDURE.

