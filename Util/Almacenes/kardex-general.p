DEF VAR DesdeF AS DATE.
DEF VAR HastaF AS DATE.
DEF VAR DesdeC AS CHAR.
DEF VAR HastaC AS CHAR.
DEF VAR s-codcia AS INT INIT 001.
DEF VAR nCodMon AS INT INIT 1.
DEF STREAM REPORT.

DEFINE VAR F-Ingreso AS DECIMAL FORMAT "(>>>,>>9.99)" NO-UNDO.
DEFINE VAR F-PreIng  AS DECIMAL FORMAT "(>>>,>>9.9999)" NO-UNDO.
DEFINE VAR F-TotIng  AS DECIMAL FORMAT "(>>>,>>9.99)" NO-UNDO.
DEFINE VAR F-Salida  AS DECIMAL FORMAT "(>>>,>>9.99)" NO-UNDO.
DEFINE VAR F-PreSal  AS DECIMAL FORMAT "(>>>,>>9.9999)" NO-UNDO.
DEFINE VAR F-TotSal  AS DECIMAL FORMAT "(>>>,>>9.99)" NO-UNDO.
DEFINE VAR F-Saldo   AS DECIMAL FORMAT "(>>,>>>,>>9.99)" NO-UNDO.
DEFINE VAR F-STKGEN  AS DECIMAL FORMAT "(>>>,>>9.99)" NO-UNDO.
DEFINE VAR F-PRECIO  AS DECIMAL FORMAT "(>>>,>>9.9999)" NO-UNDO.

DEFINE VAR F-VALCTO  AS DECIMAL FORMAT "(>>,>>>,>>9.99)" NO-UNDO.
DEFINE VAR S-SUBTIT  AS CHARACTER FORMAT "X(40)" NO-UNDO.
DEFINE VAR F-SPeso   AS DECIMAL FORMAT "(>,>>>,>>9.99)" NO-UNDO.

DEF VAR x-Archivo AS CHAR NO-UNDO INIT 'c:\tmp'.

DEF VAR x-CodMat AS CHAR FORMAT 'x(6)' NO-UNDO.

ASSIGN
    DesdeF = 03/01/2009
    HastaF = 02/29/2012.

/* ACUMULAMOS EN EL TEMPORAL */
DEF TEMP-TABLE Detalle LIKE Almdmov.
INPUT FROM c:\tmp\auditoria.prn.
REPEAT:
    IMPORT UNFORMATTED x-CodMat.
    IF x-CodMat <> '' THEN DO:
        ASSIGN
            DesdeC = x-CodMat
            HastaC = x-CodMat.
        RUN Carga-Temporal.
    END.
END.
INPUT CLOSE.
/* GENERAMOS EL KARDEX */
x-Archivo = "c:\tmp\kardex.txt".
OUTPUT STREAM REPORT TO VALUE(x-Archivo).
PUT STREAM REPORT
  "CODIGO|"
  "DESCRIPCION|"
  "CAT CONTABLE|"
  "MARCA|"
  "UM|"
  "ALMACEN|"
  "CODMOV|"
  "NUMERO|"
  "ALM ORIGEN|"
  "PROVEEDOR|"
  "CLIENTE|"
  "NRO DOCUMENTO|"
  "REFERENCIA|"
  "FECHA|"
  "INGRESO|"
  "SALIDA|"
  "CTO INGRESO|"
  "CTO PROMEDIO|"
  "SALDO|"
  "CTO TOTAL|"
  "GUIA REMISION|"
  SKIP.
RUN Texto.
OUTPUT STREAM REPORT CLOSE.

RETURN.

PROCEDURE Carga-Temporal:
/* ********************* */

FOR EACH Almdmov NO-LOCK USE-INDEX ALMD02 WHERE Almdmov.CodCia = s-CodCia 
    AND  Almdmov.codmat = x-CodMat 
    AND  Almdmov.FchDoc >= DesdeF 
    AND  Almdmov.FchDoc <= HastaF:
    CREATE Detalle.
    BUFFER-COPY Almdmov TO Detalle.
END.

END PROCEDURE.


PROCEDURE Texto:
/* ************* */

DEFINE VARIABLE S-CODMOV AS CHAR NO-UNDO. 
DEFINE VARIABLE x-codpro LIKE Almcmov.codpro NO-UNDO.
DEFINE VARIABLE x-codcli LIKE Almcmov.codcli NO-UNDO.
DEFINE VARIABLE x-nrorf1 LIKE Almcmov.nrorf1 NO-UNDO.
DEFINE VARIABLE x-nrorf2 LIKE Almcmov.nrorf2 NO-UNDO.
DEFINE VARIABLE x-codmov LIKE Almcmov.codmov NO-UNDO.
DEFINE VARIABLE x-codref AS CHAR FORMAT 'x(12)' NO-UNDO.

DEFINE VARIABLE x-inggen LIKE Almdmov.candes NO-UNDO.
DEFINE VARIABLE x-salgen LIKE Almdmov.candes NO-UNDO.
DEFINE VARIABLE x-totgen LIKE Almdmov.candes NO-UNDO.
DEFINE VARIABLE x-codmon LIKE Almdmov.candes NO-UNDO.
DEFINE VARIABLE x-tpocmb LIKE Almdmov.candes NO-UNDO.

DEFINE VARIABLE x-total AS DECIMAL.

FIND LAST gn-tcmb WHERE gn-tcmb.fecha <= DesdeF NO-LOCK NO-ERROR.


ASSIGN
   x-inggen = 0
   x-salgen = 0
   x-totgen = 0  
   x-total  = 0.
FIND LAST gn-tcmb WHERE gn-tcmb.fecha <= DesdeF NO-LOCK NO-ERROR.
FOR EACH Detalle NO-LOCK USE-INDEX ALMD02,
    FIRST Almmmatg OF Detalle NO-LOCK,
    FIRST Almtmov NO-LOCK WHERE Almtmovm.Codcia = Detalle.Codcia 
                          AND Almtmovm.TipMov = Detalle.TipMov 
                          AND Almtmovm.Codmov = Detalle.Codmov
                          AND Almtmovm.Movtrf = No,
    FIRST Almacen OF Detalle NO-LOCK WHERE Almacen.FlgRep = Yes
                          AND Almacen.AlmCsg = No
    BREAK BY Detalle.CodCia BY Detalle.CodMat BY Detalle.FchDoc:

    IF FIRST-OF(Detalle.CodMat) THEN DO:
       /* BUSCAMOS SI TIENE MOVIMIENTOS ANTERIORES A DesdeF */
       FIND LAST AlmStkGe WHERE AlmstkGe.Codcia = Almmmatg.Codcia AND
                              AlmstkGe.CodMat = Almmmatg.CodMat AND
                              AlmstkGe.Fecha < DesdeF
                              NO-LOCK NO-ERROR.
       F-STKGEN = 0.
       F-SALDO  = 0.
       F-PRECIO = 0.
       F-VALCTO = 0.

       IF AVAILABLE AlmStkGe THEN DO:
          F-STKGEN = AlmStkGe.StkAct.
          F-SALDO  = AlmStkGe.StkAct.
          F-PRECIO = AlmStkGe.CtoUni.
          F-VALCTO = F-STKGEN * F-PRECIO.
       END.

    END.

    x-codpro = "".
    x-codcli = "".
    x-nrorf1 = "".
    x-nrorf2 = "".

    FIND Almcmov WHERE Almcmov.CodCia = Detalle.codcia 
                  AND  Almcmov.CodAlm = Detalle.codalm 
                  AND  Almcmov.TipMov = Detalle.tipmov 
                  AND  Almcmov.CodMov = Detalle.codmov 
                  AND  Almcmov.NroDoc = Detalle.nrodoc 
                 NO-LOCK NO-ERROR.

    IF AVAILABLE Almcmov THEN DO:
       ASSIGN
          x-codpro = Almcmov.codpro
          x-codcli = Almcmov.codcli
          x-nrorf1 = Almcmov.nrorf1
          x-nrorf2 = Almcmov.nrorf2
          x-codmon = Almcmov.codmon
          x-tpocmb = Almcmov.tpocmb
          x-codref = almcmov.codref + almcmov.nroref.
       IF almcmov.codref <> "G/R" THEN DO:
           FIND ccbcdocu WHERE ccbcdocu.codcia = s-codcia
               AND ccbcdocu.coddoc = almcmov.codref
               AND ccbcdocu.nrodoc = almcmov.nroref
               AND ccbcdocu.flgest <> "A"
               NO-LOCK NO-ERROR.
           IF AVAILABLE ccbcdocu AND ccbcdocu.codref = "G/R" AND ccbcdocu.nroref <> ""
               THEN x-codref = ccbcdocu.codref + ccbcdocu.nroref.
       END.
    END.

    S-CODMOV  = Detalle.TipMov + STRING(Detalle.CodMov,"99"). 

    F-Ingreso = IF LOOKUP(Detalle.TipMov,"I,U,R") > 0 THEN (Detalle.CanDes * Detalle.Factor) ELSE 0.
    F-PreIng  = 0.
    F-TotIng  = 0.

    IF nCodmon = x-Codmon THEN DO:
       IF Detalle.Tipmov = 'I' THEN DO:
          F-PreIng  = Detalle.PreUni / Detalle.Factor.
          F-TotIng  = Detalle.ImpCto.
       END.
       ELSE DO:
          F-PreIng  = 0.
          F-TotIng  = F-PreIng * F-Ingreso.
       END.
       END.
    ELSE DO:
       IF nCodmon = 1 THEN DO:
          IF Detalle.Tipmov = 'I' THEN DO:
             F-PreIng  = ROUND(Detalle.PreUni * Detalle.Tpocmb / Detalle.Factor, 4).
             F-TotIng  = (Detalle.ImpCto * Detalle.TpoCmb ).
          END.
          IF LOOKUP(Detalle.Tipmov, 'U,R') > 0 THEN DO:
             F-PreIng  = 0.
             F-TotIng  = F-PreIng * F-Ingreso.
          END.
          END.
       ELSE DO:
          IF Detalle.Tipmov = 'I' THEN DO:
             F-PreIng  = ROUND(Detalle.PreUni / Detalle.TpoCmb / Detalle.Factor, 4).
             F-TotIng  = ROUND(Detalle.ImpCto / Detalle.TpoCmb, 2).
          END.
          IF LOOKUP(Detalle.Tipmov, 'U,R') > 0 THEN DO:
             F-PreIng  = 0.
             F-TotIng  = F-PreIng * F-Ingreso.
          END.
       END.
    END.

    F-Salida  = IF LOOKUP(Detalle.TipMov,"S,T") > 0 THEN (Detalle.CanDes * Detalle.Factor) ELSE 0.
    F-Saldo   = Detalle.StkAct.
    F-VALCTO = Detalle.StkAct * Detalle.VctoMn1.
    F-PRECIO = Detalle.VctoMn1.
    ACCUMULATE F-Ingreso (TOTAL BY Detalle.CodMat).
    ACCUMULATE F-Salida  (TOTAL BY Detalle.CodMat).

    PUT STREAM REPORT
        Almmmatg.CodMat "|"
        Almmmatg.DesMat "|"
        Almmmatg.catconta[1] "|"
        Almmmatg.DesMar "|"
        Almmmatg.UndStk "|"
        Detalle.CodAlm  "|"
        S-CODMOV "|"
        Detalle.NroDoc FORMAT '9999999' "|".
    IF Detalle.Codmov = 03 THEN PUT STREAM REPORT Detalle.Almori "|".
    ELSE PUT STREAM REPORT "|".
    PUT STREAM REPORT
        x-CodPro "|"
        x-CodCli "|"
        x-NroRf1 "|"
        x-NroRf2 "|"
        Detalle.FchDoc "|"
        F-Ingreso "|"
        F-Salida "|".
    IF Detalle.TipMov = "I" AND (ALmtmovm.TpoCto = 0 OR ALmtmovm.TpoCto = 1) THEN
        PUT STREAM REPORT F-PreIng "|".
    ELSE PUT STREAM REPORT "|".
    PUT STREAM REPORT
        F-PRECIO "|"
        F-SALDO "|"
        F-VALCTO "|"
        x-codref "|"
        SKIP.

    IF LAST-OF(Detalle.CodMat) THEN DO:
      x-total = x-total + F-VALCTO.
    END.
END.    

END.