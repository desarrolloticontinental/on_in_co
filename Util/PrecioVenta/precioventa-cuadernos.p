DEF VAR x-codmat LIKE almmmatg.codmat.
DEF VAR x-ctolis LIKE almmmatg.ctolis.
DEF VAR x-prevta LIKE almmmatg.prevta.
DEF VAR x-linea AS CHAR FORMAT 'x(200)'.

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


INPUT FROM c:\tmp\cuadernos.prn.
REPEAT :
    IMPORT UNFORMATTED x-linea.
    ASSIGN
        x-codmat = SUBSTRING(x-linea,1,6).
    FIND almmmatg WHERE codcia = s-codcia
        AND codmat = x-codmat
        EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE almmmatg THEN NEXT.
    DISPLAY x-codmat.
    PAUSE 0.
    IF Almmmatg.MonVta = 0 THEN Almmmatg.MonVta = 2.
    IF Almmmatg.MonVta = 1 THEN
        ASSIGN
        x-prevta[2] = dec(SUBSTRING(x-linea,12,7))
        x-prevta[3] = dec(SUBSTRING(x-linea,41,8))
        x-prevta[4] = dec(SUBSTRING(x-linea,70,9)).
    IF Almmmatg.MonVta = 2 THEN
        ASSIGN
        x-prevta[2] = dec(SUBSTRING(x-linea,22,7))
        x-prevta[3] = dec(SUBSTRING(x-linea,51,8))
        x-prevta[4] = dec(SUBSTRING(x-linea,80,9)).
    ASSIGN
        almmmatg.tpocmb = 2.75
        almmmatg.prevta[2] = x-prevta[2] 
        almmmatg.prevta[3] = x-prevta[3] 
        almmmatg.prevta[4] = x-prevta[4].
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

END.
INPUT CLOSE.





