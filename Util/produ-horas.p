def var s-codcia as int init 001 .
def var x-fecini as date.
def var x-fecfin as date.
def var x-tothor as dec.
def var x-totimp as dec.

assign
    x-fecini = 09/01/2006
    x-fecfin = 03/30/2007.

DEFINE TEMP-TABLE T-Horas 
       FIELD CodPer LIKE Pl-PERS.CodPer
       FIELD DesPer AS CHAR FORMAT "X(45)"
       FIELD TotMin AS DECI FORMAT "->>>,>>9.99"
       FIELD TotHor AS DECI FORMAT "->>>,>>9.99"
       FIELD Factor AS DECI EXTENT 10 FORMAT "->>9.99" .

output to c:\tmp\tot-horas-val.txt.
for each pr-odpc no-lock where codcia = s-codcia and flgest <> 'a'
        and fchord >= x-fecini and fchord <= x-fecfin:
    run crea-tempo-horas.
    assign
        x-tothor = 0
        x-totimp = 0.
    FOR EACH T-Horas:
        x-tothor = x-tothor + TRUNCATE(T-Horas.Totmin / 60 ,0 ) + (( T-Horas.TotMin MOD 60 ) / 100 ).
        x-totimp = x-totimp + t-horas.tothor.
    END.
    display
        pr-odpc.numord
        pr-odpc.fchord
        pr-odpc.observ[1]
        x-tothor
        x-totimp
        with stream-io no-box width 320.
end.
output close.
        


procedure crea-tempo-horas:

  FOR EACH T-Horas:
      DELETE T-Horas.
  END.
  DEFINE VAR X-DIAS AS INTEGER INIT 208.
  DEFINE VAR I AS INTEGER.
  DEFINE VAR F-STKGEN AS DECI .
  DEFINE VAR F-VALCTO AS DECI .
  DEFINE VAR F-PRECIO AS DECI.
  DEFINE VAR F-TOTAL AS DECI.
  DEFINE VAR X-DESPER  AS CHAR FORMAT "X(35)".
  DEFINE VAR X-HORAI AS DECI .
  DEFINE VAR X-SEGI AS DECI .
  DEFINE VAR X-SEGF AS DECI .
  DEFINE VAR X-IMPHOR AS DECI FORMAT "->>9.99".
  DEFINE VAR X-BASE   AS DECI .
  DEFINE VAR X-HORMEN AS DECI .
  DEFINE VAR X-FACTOR AS DECI .
  
  DEFINE VAR X-HORA AS DECI EXTENT 10 FORMAT "->>9.99".
  DEFINE VAR X-TOTA AS DECI EXTENT 10 FORMAT "->>>>>9.99".
  
  DEFINE VAR X-TOT1 AS DECI.
  DEFINE VAR X-TOT2 AS DECI.
  DEFINE VAR X-TOT3 AS DECI.
  DEFINE VAR X-TOT4 AS DECI.
  DEFINE VAR X-TOT5 AS DECI.
  DEFINE VAR X-TOT6 AS DECI.
  DEFINE VAR X-TOT10 AS DECI.
  



       FOR EACH PR-MOV-MES NO-LOCK WHERE PR-MOV-MES.CodCia = S-CODCIA AND  
                                         PR-MOV-MES.NumOrd = PR-ODPC.NumOrd  AND
                                         PR-MOV-MES.FchReg >= X-FECINI  AND  
                                         PR-MOV-MES.FchReg <= X-FECFIN
                                         BREAK BY PR-MOV-MES.NumOrd
                                               BY PR-MOV-MES.FchReg
                                               BY PR-MOV-MES.CodPer :
      
 

          FIND PL-PERS WHERE Pl-PERS.Codper = PR-MOV-MES.CodPer
                             NO-LOCK NO-ERROR.
          X-DESPER = "".
          IF AVAILABLE Pl-PERS THEN 
          X-DESPER = TRIM(PL-PERS.PatPer) + " " + TRIM(PL-PERS.MatPer) + " " + TRIM(PL-PERS.NomPer).
          X-HORAI  = PR-MOV-MES.HoraI.
          X-HORA   = 0.
          X-IMPHOR = 0.
          X-TOTA   = 0.
          X-BASE   = 0.
          X-HORMEN = 0.
          X-FACTOR = 0.
                    
          FOR EACH PL-MOV-MES NO-LOCK WHERE PL-MOV-MES.Codcia = PR-MOV-MES.Codcia AND
                                   PL-MOV-MES.Periodo = PR-MOV-MES.Periodo AND
                                   PL-MOV-MES.Nromes  = PR-MOV-MES.NroMes AND
                                   PL-MOV-MES.CodPln  = 01 AND
                                   PL-MOV-MES.Codcal  = 0 AND
                                   PL-MOV-MES.CodPer = PR-MOV-MES.CodPer AND
                                   (PL-MOV-MES.CodMov = 101 OR
                                    PL-MOV-MES.CodMov = 103)  :
          
            X-BASE = X-BASE + PL-MOV-MES.VALCAL-MES .                     

              
          END.

        FIND LAST PL-VAR-MES WHERE
                  PL-VAR-MES.Periodo = PR-MOV-MES.Periodo AND
                  PL-VAR-MES.NroMes  = PR-MOV-MES.NroMes 
                  NO-ERROR.

        IF AVAILABLE PL-VAR-MES THEN 
           ASSIGN
           X-HORMEN = PL-VAR-MES.ValVar-MES[11]
           X-FACTOR = PL-VAR-MES.ValVar-MES[12] + PL-VAR-MES.ValVar-MES[13] + PL-VAR-MES.ValVar-MES[3] + PL-VAR-MES.ValVar-MES[5] + PL-VAR-MES.ValVar-MES[10].

           X-IMPHOR = (X-BASE / X-HORMEN) * ( 1 + (X-FACTOR / 100) ) / 60.
         
          
          FOR EACH PR-CFGPL NO-LOCK WHERE PR-CFGPL.Codcia = S-CODCIA /*AND
                                          PR-CFGPL.Periodo = PR-MOV-MES.Periodo AND
                                          PR-CFGPL.NroMes  = PR-MOV-MES.NroMes*/:
              IF X-HORAI >= PR-CFGPL.HoraI AND X-HORAI < PR-CFGPL.HoraF THEN DO:
                 IF PR-MOV-MES.HoraF <= PR-CFGPL.HoraF THEN DO:
                    X-SEGF = TRUNCATE(PR-MOV-MES.HoraF,0) * 60 + (PR-MOV-MES.HoraF - TRUNCATE(PR-MOV-MES.HoraF,0)) * 100 . 
                    X-SEGI = TRUNCATE(X-HORAI,0) * 60 + (X-HORAI - TRUNCATE(X-HORAI,0)) * 100 . 
                    X-HORA[PR-CFGPL.Nroitm] = X-HORA[PR-CFGPL.Nroitm] + (X-SEGF - X-SEGI) .                 
                    X-TOTA[PR-CFGPL.Nroitm] = X-TOTA[PR-CFGPL.Nroitm] + (X-SEGF - X-SEGI) * PR-CFGPL.Factor * X-IMPHOR .                 
                    X-TOTA[10] = X-TOTA[10] + (X-SEGF - X-SEGI) * PR-CFGPL.Factor * X-IMPHOR .                 

                    LEAVE.
                 END.
                 IF PR-MOV-MES.HoraF > PR-CFGPL.HoraF THEN DO:
                    X-SEGF = TRUNCATE(PR-CFGPL.HoraF,0) * 60 + (PR-CFGPL.HoraF - TRUNCATE(PR-CFGPL.HoraF,0)) * 100 . 
                    X-SEGI = TRUNCATE(X-HORAI,0) * 60 + (X-HORAI - TRUNCATE(X-HORAI,0)) * 100 . 
                    X-HORA[PR-CFGPL.Nroitm] = X-HORA[PR-CFGPL.Nroitm] + (X-SEGF - X-SEGI) .                 
                    X-TOTA[PR-CFGPL.Nroitm] = X-TOTA[PR-CFGPL.Nroitm] + (X-SEGF - X-SEGI) * PR-CFGPL.Factor * X-IMPHOR .                 
                    X-TOTA[10] = X-TOTA[10] + (X-SEGF - X-SEGI) * PR-CFGPL.Factor * X-IMPHOR .                 
                    X-HORAI = PR-CFGPL.HoraF.
                 END.
                 
              END.
                             
                                          
          END.               

          X-IMPHOR = X-IMPHOR * 60 .         

          X-SEGF = TRUNCATE(PR-MOV-MES.HoraF,0) * 60 + (PR-MOV-MES.HoraF - TRUNCATE(PR-MOV-MES.HoraF,0)) * 100 . 
          X-SEGI = TRUNCATE(PR-MOV-MES.HoraI,0) * 60 + (PR-MOV-MES.HoraI - TRUNCATE(PR-MOV-MES.HoraI,0)) * 100 . 

          FIND T-Horas WHERE T-Horas.CodPer = PR-MOV-MES.CodPer 
                             NO-ERROR.
          IF NOT AVAILABLE T-Horas THEN DO:
             CREATE T-Horas.
             ASSIGN 
             T-Horas.CodPer = PR-MOV-MES.CodPer
             T-Horas.DesPer = X-DESPER .
          END.
          ASSIGN
          T-Horas.TotMin = T-Horas.TotMin +  X-SEGF - X-SEGI .
          T-Horas.TotHor = T-Horas.TotHor + X-TOTA[10] .
           
                             
                     

  END.  
end procedure.
