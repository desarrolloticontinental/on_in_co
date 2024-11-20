for each almcmov where
     CodCia = 1 and
     CodAlm = "12" and
     TipMov = "i" and
     CodMov = 50 and
     /*
     NroSer = "" */
     NroDoc = 041961 :
     display nroref.
     for each almdmov of almcmov no-lock:
     display almdmov with 2 col stream-io.
     end.
 end.
