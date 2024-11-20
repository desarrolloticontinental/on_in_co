/* ******************************************************* */
/* CALCULA LOS MARGENES DE UTILIDAD Y EL PRECIO DE OFICINA */
/* ******************************************************* */

DEF VAR f-factor AS DEC.
DEF VAR x-CtoUnd AS DEC.
DEF VAR f-MrgUti-A AS DEC.
DEF VAR f-MrgUti-B AS DEC.
DEF VAR f-MrgUti-C AS DEC.
DEF VAR f-PreVta-A AS DEC.
DEF VAR f-PreVta-B AS DEC.
DEF VAR f-PreVta-C AS DEC.

FOR EACH Almmmatg WHERE codcia = 001 /*AND TpoArt <> 'D'*/:
   display almmmatg.codmat.
   pause 0.
   X-CTOUND = Almmmatg.CtoTot.
   /*******************************************/
   ASSIGN
       F-MrgUti-A = Almmmatg.MrgUti-A
       F-MrgUti-B = Almmmatg.MrgUti-B
       F-MrgUti-C = Almmmatg.MrgUti-C
       F-PreVta-A = Almmmatg.Prevta[2]
       F-PreVta-B = Almmmatg.Prevta[3]
       F-PreVta-C = Almmmatg.Prevta[4].
    RUN Precio-de-Oficina.
END.

PROCEDURE Precio-de-Oficina:

    DEFINE VARIABLE fmot LIKE Almmmatg.PreOfi.
    DEFINE VARIABLE pre-ofi LIKE Almmmatg.PreOfi.
    DEFINE VARIABLE MrgMin LIKE Almmmatg.MrgUti-A.
    DEFINE VARIABLE MrgOfi LIKE Almmmatg.MrgUti-A.
    DEFINE VARIABLE MaxCat AS DEC.
    DEFINE VARIABLE MaxVta AS DEC.
    
    MaxCat = 0.
    MaxVta = 0.
    fmot   = 0.
    MrgMin = 100.
    MrgOfi = 0.
    F-FACTOR = 1.
    MaxCat = 4.
    MaxVta = 3.
    
    /****   Busca el Factor de conversion   ****/
    IF Almmmatg.Chr__01 <> "" THEN DO:
        FIND Almtconv WHERE Almtconv.CodUnid = Almmmatg.UndBas
                       AND  Almtconv.Codalter = Almmmatg.Chr__01
                      NO-LOCK NO-ERROR.
        IF NOT AVAILABLE Almtconv THEN DO:
           RETURN.
        END.
        F-FACTOR = Almtconv.Equival.
    END.
    /*******************************************/
    CASE Almmmatg.Chr__02 :
        WHEN "T" THEN DO:        
            
            IF F-MrgUti-A < MrgMin AND F-MrgUti-A <> 0 THEN MrgMin = F-MrgUti-A.
            IF F-MrgUti-B < MrgMin AND F-MrgUti-B <> 0 THEN MrgMin = F-MrgUti-B.
            IF F-MrgUti-C < MrgMin AND F-MrgUti-C <> 0 THEN MrgMin = F-MrgUti-C.
            
            fmot = (1 + MrgMin / 100) / ((1 - MaxCat / 100) * (1 - MaxVta / 100)).
            
            pre-ofi = X-CTOUND * fmot * F-FACTOR .        
           
            MrgOfi = ROUND((fmot - 1) * 100, 6).
        END.
        WHEN "P" THEN DO:
           pre-ofi = Almmmatg.Prevta[1] * F-FACTOR.
           MrgOfi = ((Almmmatg.Prevta[1] / Almmmatg.Ctotot) - 1 ) * 100. 
        END. 
    END.    
    
    ASSIGN
        Almmmatg.Dec__01 = MrgOfi
        Almmmatg.PreOfi  = Pre-Ofi.

END PROCEDURE.
