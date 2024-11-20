
DEF VAR x-codmat LIKE almmmatg.codmat.
DEF VAR x-ctolis LIKE almmmatg.ctolis.
DEF VAR x-prevta LIKE almmmatg.prevta.
DEF VAR x-linea AS CHAR FORMAT 'x(100)'.

DEF VAR s-codcia AS INT INIT 001.

DEFINE VARIABLE X-CTOUND AS DECIMAL NO-UNDO.
DEFINE VARIABLE F-PRENET AS DECIMAL NO-UNDO.
DEFINE VARIABLE F-CTOPRM AS DECIMAL NO-UNDO.
DEFINE VARIABLE F-PreVta LIKE Almmmatg.PreBas NO-UNDO.
DEFINE VARIABLE F-MrgUti AS DECIMAL NO-UNDO.
DEFINE VARIABLE F-MrgCom AS DECIMAL NO-UNDO.
DEFINE VARIABLE F-PorImp AS DECIMAL NO-UNDO.
DEFINE VARIABLE F-PreSol AS DECIMAL NO-UNDO.
DEFINE VARIABLE X-CTOTOT AS DECIMAL FORMAT "->>>>>>>>>9.999999" NO-UNDO.
DEFINE VARIABLE F-PorMax AS DECIMAL NO-UNDO.
DEFINE VARIABLE x-clase  AS CHAR.
DEFINE VARIABLE F-PreVta-A AS DECIMAL NO-UNDO.
DEFINE VARIABLE F-PreVta-B AS DECIMAL NO-UNDO.
DEFINE VARIABLE F-PreVta-C AS DECIMAL NO-UNDO.
DEFINE VARIABLE F-FACTOR     AS DECIMAL   NO-UNDO.
DEFINE VARIABLE F-MrgUti-A AS DECIMAL NO-UNDO.
DEFINE VARIABLE F-MrgUti-B AS DECIMAL NO-UNDO.
DEFINE VARIABLE F-MrgUti-C AS DECIMAL NO-UNDO.
DEFINE VARIABLE MaxCat LIKE ClfClie.PorDsc.
DEFINE VARIABLE MaxVta LIKE Dsctos.PorDto.

FIND FacCfgGn WHERE FacCfgGn.CodCia = S-CODCIA NO-LOCK NO-ERROR.

DEFINE VARIABLE x-Factor AS DEC.
x-Factor = 1.0989.
FOR EACH almmmatg WHERE almmmatg.codcia = s-codcia
    AND almmmatg.codmat = '015724':
    DISPLAY almmmatg.codmat.
    PAUSE 0.
    ASSIGN
        /*almmmatg.prevta[1] = x-prevta[1] * x-factor */
        almmmatg.prevta[2] = almmmatg.prevta[2] * x-factor
        almmmatg.prevta[3] = almmmatg.prevta[3] * x-factor
        almmmatg.prevta[4] = almmmatg.prevta[4] * x-factor.
    /* CALCULOS */
    ASSIGN
        X-CTOTOT = Almmmatg.CtoLis
        X-CTOUND = Almmmatg.CtoTot.
    ASSIGN
        F-MrgUti-A = Almmmatg.MrgUti-A
        F-PreVta-A = Almmmatg.Prevta[2]
        F-MrgUti-B = Almmmatg.MrgUti-B
        F-PreVta-B = Almmmatg.Prevta[3]
        F-MrgUti-C = Almmmatg.MrgUti-C
        F-PreVta-C = Almmmatg.Prevta[4].
     IF Almmmatg.Prevta[2] > 0 THEN DO:
         F-FACTOR = 1.
         /****   Busca el Factor de conversion   ****/
         IF Almmmatg.UndA <> "" THEN DO:
             FIND Almtconv WHERE Almtconv.CodUnid = Almmmatg.UndBas
                 AND  Almtconv.Codalter = Almmmatg.UndA
                 NO-LOCK NO-ERROR.
             IF NOT AVAILABLE Almtconv THEN NEXT.
             F-FACTOR = Almtconv.Equival.
             F-MrgUti-A = ROUND((((((F-PreVta-A / F-FACTOR) ) / X-CTOUND) - 1) * 100), 6).
         END.
         /*******************************************/
     END.
     IF Almmmatg.Prevta[3] > 0 THEN DO:
         F-FACTOR = 1.
         /****   Busca el Factor de conversion   ****/
         IF Almmmatg.UndB <> "" THEN DO:
             FIND Almtconv WHERE Almtconv.CodUnid = Almmmatg.UndBas
                 AND  Almtconv.Codalter = Almmmatg.UndB
                 NO-LOCK NO-ERROR.
             IF NOT AVAILABLE Almtconv THEN NEXT.
             F-FACTOR = Almtconv.Equival.
             F-MrgUti-B = ROUND((((((F-PreVta-B / F-FACTOR) ) / X-CTOUND) - 1) * 100), 6).
         END.
         /*******************************************/
     END.
     IF Almmmatg.Prevta[4] > 0 THEN DO:
         F-FACTOR = 1.
         /****   Busca el Factor de conversion   ****/
         IF Almmmatg.UndC <> "" THEN DO:
             FIND Almtconv WHERE Almtconv.CodUnid = Almmmatg.UndBas
                            AND  Almtconv.Codalter = Almmmatg.UndC
                           NO-LOCK NO-ERROR.
             IF NOT AVAILABLE Almtconv THEN NEXT.
             F-FACTOR = Almtconv.Equival.
             F-MrgUti-C = ROUND((((((F-PreVta-C / F-FACTOR) ) / X-CTOUND) - 1) * 100), 6).
         END.
         /*******************************************/
     END.
     ASSIGN
         Almmmatg.MrgUti-A = F-MrgUti-A
         Almmmatg.MrgUti-B = F-MrgUti-B
         Almmmatg.MrgUti-C = F-MrgUti-C.

     RUN Precio-de-Oficina.
END.

PROCEDURE Precio-de-Oficina:
/* ************************ */

DEFINE VARIABLE fmot LIKE Almmmatg.PreOfi.
DEFINE VARIABLE pre-ofi LIKE Almmmatg.PreOfi.
DEFINE VARIABLE MrgMin LIKE Almmmatg.MrgUti-A.
DEFINE VARIABLE MrgOfi LIKE Almmmatg.MrgUti-A.

MaxCat = 0.
MaxVta = 0.
fmot   = 0.
MrgMin = 5000.
MrgOfi = 0.
F-FACTOR = 1.
MaxCat = 4.
MaxVta = 3.

ASSIGN
    F-MrgUti-A = Almmmatg.MrgUti-A
    F-PreVta-A = Almmmatg.Prevta[2]
    F-MrgUti-B = Almmmatg.MrgUti-B
    F-PreVta-B = Almmmatg.Prevta[3]
    F-MrgUti-C = Almmmatg.MrgUti-C
    F-PreVta-C = Almmmatg.Prevta[4].

X-CTOUND = Almmmatg.CtoTot.

/****   Busca el Factor de conversion   ****/
IF Almmmatg.Chr__01 <> "" THEN DO:
    FIND Almtconv WHERE Almtconv.CodUnid = Almmmatg.UndBas
                   AND  Almtconv.Codalter = Almmmatg.Chr__01
                  NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Almtconv THEN RETURN.
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
    Almmmatg.PreOfi = pre-ofi.

END PROCEDURE.


