DEF VAR x-linea AS CHAR NO-UNDO.
DEF VAR s-codcia AS INT INIT 001 NO-UNDO.

DEF TEMP-TABLE detalle
    FIELD codcli AS CHAR FORMAT 'x(12)'
    FIELD nomcli AS CHAR FORMAT 'x(100)'
    FIELD ruccli AS CHAR FORMAT 'x(20)'
    FIELD credis AS DEC FORMAT '->>>,>>>,>>9.99'.
INPUT FROM c:\tmp\invitados.prn.
REPEAT :
    IMPORT UNFORMATTED x-linea.
    IF x-linea = '' THEN LEAVE.
    CREATE detalle.
    ASSIGN
        detalle.codcli = SUBSTRING(x-linea,1,12)
        detalle.nomcli = SUBSTRING(x-linea,13,100)
        detalle.ruccli = SUBSTRING(x-linea,113).
END.
INPUT CLOSE.
DEF VAR dMonLC AS INT.
DEFINE VARIABLE dImpLCred LIKE Gn-ClieL.ImpLC NO-UNDO.
DEF VAR F-CreUsa AS DEC  FORMAT "-Z,ZZZ,ZZ9.99".
DEF VAR F-TotDol AS DEC  FORMAT "-Z,ZZZ,ZZ9.99".
DEF VAR F-TotSol AS DEC  FORMAT "-Z,ZZZ,ZZ9.99".
DEFINE VAR w-cambio AS DEC NO-UNDO.
DEF VAR F-CreDis AS DEC  FORMAT "-Z,ZZZ,ZZ9.99".
DEF VAR F-LinCre AS DEC  FORMAT "-Z,ZZZ,ZZ9.99".
DEF VAR f-MonLC AS CHAR.

FOR EACH detalle, FIRST gn-clie NO-LOCK WHERE gn-clie.codcia = 000 AND gn-clie.codcli = detalle.codcli:
    DISPLAY detalle.codcli.
    PAUSE 0.
    RUN Calcula-Linea-Credito.
    ASSIGN detalle.credis = f-credis.
    
END.
OUTPUT TO c:\tmp\xxx.txt.
FOR EACH detalle:
    PUT UNFORMATTED
        detalle.codcli '|'
        detalle.nomcli '|'
        detalle.ruccli '|'
        detalle.credis SKIP.
END.
OUTPUT CLOSE.

PROCEDURE Calcula-Linea-Credito:
/* **************************** */

    F-CreDis = 0.
    RUN ccb/p-implc (gn-clie.codcia, gn-clie.codcli, OUTPUT dMonLC, OUTPUT dImpLCred).
    IF dImpLCred = 0 THEN RETURN.

DEFINE VAR w-cambio AS DEC NO-UNDO.
DEFINE VAR x-Signo  AS INT NO-UNDO.
    
ASSIGN 
       F-TotDol = 0 
       F-TotSol = 0.
     
FIND FIRST FacCfgGn NO-LOCK NO-ERROR.
      IF AVAILABLE FacCfgGn THEN 
            w-cambio = FacCfgGn.Tpocmb[1].
      ELSE  w-cambio = 0.     

FOR EACH CcbCDocu NO-LOCK WHERE CcbCDocu.CodCia = s-CodCia
        AND CcbCDocu.CodCli = gn-clie.CodCli
        AND CcbCDocu.FlgEst = 'P'
        AND LOOKUP(Ccbcdocu.coddoc, 'FAC,BOL,LET,CHQ,N/D,N/C,A/R,BD,A/C') > 0,
    FIRST FacDocum OF CcbCDocu NO-LOCK WHERE FacDocum.TpoDoc <> ?:
    /* DOCUMENTOS QUE NO DEBEN APARECER EN LA LINEA DE CREDITO */
    IF LOOKUP(Ccbcdocu.coddoc, 'A/R,BD,A/C') > 0 THEN NEXT.
    /* ******************************************************* */
    x-Signo = IF FacDocum.TpoDoc = YES THEN 1 ELSE -1.
    CASE CcbCDocu.CodMon :           
         WHEN 1 THEN DO :
                F-TotSol = F-TotSol + CcbCDocu.SdoAct * x-Signo.
           END.     
         WHEN 2 THEN DO :
                F-TotDol = F-TotDol + CcbCDocu.SdoAct * x-Signo.
           END.
    END CASE .
END.
/* POR PEDIDOS PENDIENTES DE ATENCION */
DEF VAR f-Total  AS DEC NO-UNDO.
DEF VAR x-ImpLin AS DEC NO-UNDO.
FOR EACH FacCPedi NO-LOCK WHERE FacCPedi.CodCia = S-CODCIA 
    AND FacCPedi.CodDoc = "PED" 
    AND FacCPedi.CodCli = gn-clie.CodCli 
    AND LOOKUP(FacCPedi.FlgEst, "G,X,P,W,WX,WL") > 0:
    FIND FIRST gn-convt WHERE gn-convt.codig = Faccpedi.FmaPgo NO-LOCK.
    f-Total = 0.
    FOR EACH Facdpedi OF Faccpedi NO-LOCK:
        x-ImpLin = (FacDPedi.CanPed - FacDPedi.CanAte) * FacDPedi.ImpLin / FacDPedi.CanPed.
        IF x-ImpLin > 0 THEN f-Total = f-Total + x-ImpLin.
    END.
    IF gn-convt.tipvta = "1" THEN NEXT.     /* NO CONTADOS */
    IF Faccpedi.codmon = 1
    THEN F-TotSol = F-TotSol + f-Total.
    ELSE F-TotDol = F-TotDol + f-Total.
END.
/* ********************************** */

/* POR ORDENES DE DESPACHO DE ATENCION */
FOR EACH FacCPedi NO-LOCK WHERE FacCPedi.CodCia = S-CODCIA 
    AND FacCPedi.CodDoc = "O/D" 
    AND FacCPedi.CodCli = gn-clie.CodCli 
    AND FacCPedi.FlgEst = "P":
    FIND FIRST gn-convt WHERE gn-convt.codig = Faccpedi.FmaPgo NO-LOCK.
    IF gn-convt.tipvta = "1" THEN NEXT.     /* NO CONTADOS */
    f-Total = 0.
    FOR EACH Facdpedi OF Faccpedi NO-LOCK:
        x-ImpLin = (FacDPedi.CanPed - FacDPedi.CanAte) * FacDPedi.ImpLin / FacDPedi.CanPed.
        IF x-ImpLin > 0 THEN f-Total = f-Total + x-ImpLin.
    END.
    IF Faccpedi.codmon = 1
    THEN F-TotSol = F-TotSol + f-Total.
    ELSE F-TotDol = F-TotDol + f-Total.
END.
/* ********************************** */



CASE dMonLC:  
     WHEN 1 THEN 
          F-CreUsa = ( F-TotDol * w-cambio ) + F-TotSol.
     WHEN 2 THEN 
          F-CreUsa = ( F-TotSol / w-cambio ) + F-TotDol.
     WHEN 0 THEN 
          F-CreUsa = ( F-TotSol / w-cambio ) + F-TotDol.
END CASE.           

F-CreDis = dImpLCred - F-CreUsa.
F-LinCre = dImpLCred.  
f-MonLC = IF dMonLc = 1 THEN 'S/.' ELSE IF dMonLC = 2 THEN 'US$' ELSE ''.

END PROCEDURE.


