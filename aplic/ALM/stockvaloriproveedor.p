define var s-codcia as integer init 1.
define var x-fecha as date init today.
define var x-despro as char.
define var f-stkgen as deci.
define var f-precto as deci.
define var f-valcto1 as deci.
define var f-valcto2 as deci.
define var i-codmon as integer init 2.
define var f-costo as deci.
define var f-tot1 as deci format "->>,>>>,>>>,>>>,>>9.99".
define var f-tot2 as deci format "->>,>>>,>>>,>>>,>>9.99".

output to c:\tmp\stock01.txt.
x-fecha = date(06,30,2005).

define temp-table tempo
  field codpro like gn-prov.codpro
  field nompro like gn-prov.nompro
  field imptot1 like almdmov.preuni
  field imptot2 like almdmov.preuni.

  FOR EACH Almmmatg NO-LOCK WHERE Almmmatg.CodCia = s-codcia:
                                  
     
      FIND LAST gn-tcmb WHERE gn-tcmb.fecha <= x-fecha
           NO-LOCK NO-ERROR.
      
      
      FIND LAST AlmStkge WHERE AlmStkge.Codcia = S-CODCIA AND
                        AlmStkge.Codmat = Almmmatg.Codmat AND
                        AlmStkge.Fecha <= x-fecha
                        NO-LOCK NO-ERROR.
                        
      IF NOT AVAILABLE AlmStkge THEN NEXT. 
      
      IF ALmStkge.StkAct = 0 THEN NEXT.
      
      FIND Tempo WHERE Tempo.Codpro = almmmatg.codpr1 
                       EXCLUSIVE-LOCK NO-ERROR.

      IF NOT AVAILABLE Tempo THEN DO:
         X-DESpro = "".
         FIND gn-prov WHERE gn-prov.codcia = 0
                       AND  gn-prov.codpro = Almmmatg.Codpr1
                       NO-LOCK NO-ERROR.
         IF AVAILABLE gn-prov THEN X-DESpro = gn-prov.nompro.
         
         CREATE Tempo.
         ASSIGN 
         Tempo.Codpro = Almmmatg.Codpr1 
         Tempo.Nompro = x-despro.
      END.        
      
      F-STKGEN = AlmStkge.Stkact.
      F-PRECTO = AlmStkGe.CtoUni.
      
      IF I-CodMon = 1 THEN F-VALCTO1 = F-STKGEN * F-PRECTO.
      IF I-CodMon = 2 THEN F-VALCTO1 = F-STKGEN * F-PRECTO / Gn-Tcmb.Compra. 
      
 
      /***************Costo Ultima Reposicion********MAGM***********/
         F-COSTO = Almmmatg.Ctolis.
         IF I-CodMon <> Almmmatg.MonVta THEN DO:
            IF I-CodMon = 1 THEN F-costo = F-costo * gn-tcmb.venta. 
            IF I-CodMon = 2 THEN F-costo = F-costo / gn-tcmb.venta. 
         END.            

      F-VALCTO2 = F-COSTO * F-STKGEN.
      
      /***************************************************************/

      tempo.imptot1 = tempo.imptot1 + f-valcto1.
      tempo.imptot2 = tempo.imptot2 + f-valcto2.
      f-tot1 = f-tot1 + f-valcto1.
      f-tot2 = f-tot2 + f-valcto2.
      
  END.

for each tempo break by imptot1 descending:
    display tempo.codpro format "x(11)" column-label "Codigo "
            tempo.nompro format "x(45)" column-label "Nombre o Razon Social"
            imptot1 column-label "Importe/Promedio!US$!Sin IGV"
            imptot2 column-label "Importe/Reposicion!US$!Sin IGV"
            with width 350.
end.
display "" skip.
display f-tot1 f-tot2 no-label .
output close.