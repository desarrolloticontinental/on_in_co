def var total as decimal extent 2.

  output to "D:\lll.txt".
    FOR EACH CCBCDOCU NO-LOCK WHERE CcbCDocu.CodCia = 1
        AND CcbCDocu.CodDiv = "00000" 
        AND CcbCDocu.CodDoc = "FAC" 
        AND CcbCDocu.FchDoc >= 08/15/08
        AND CcbCDocu.FchDoc <= 08/31/08
        break by fchdoc:

       accumulate imptot(sub-total by fchdoc).

        display
            fchdoc
            nrodoc
            codcli
            nomcli format "x(35)"
            codmon
            imptot
            imptot2
            TpoCmb
            with width 320.
            
         if last-of (fchdoc) then do:
           underline imptot.
           display
            "subtotal:" @ nomcli
            accum sub-total by fchdoc imptot @ imptot.
         end.   
     end.
  output to close.
