for each ccbccaja where
    codcia = 1 and
    coddoc = "i/c" and
    nrodoc = "011074340" : /* 37, 38, 40*/
    display
        usuario
        nomcli
        flgest
        flgcie
        fchcie
        fchdoc
        IMPNAC[2].
       
     update
         fchdoc
         . /*
        fchcie
        flgest
        flgcie
        */
        def var X-NUMDOC as char.
        def buffer B-CCBC for ccbcdocu.
        /*
    /* Verifica Cheque Aceptado */
    IF ((CcbCCaja.Voucher[2] <> "" ) AND
        (CcbCCaja.ImpNac[2] + CcbCCaja.ImpUsa[2]) > 0 ) OR
        ((CcbCCaja.Voucher[3] <> "" ) AND
        (CcbCCaja.ImpNac[3] + CcbCCaja.ImpUsa[3]) > 0) THEN DO:

        IF CcbCCaja.Voucher[2] <> "" THEN X-NUMDOC = CcbCCaja.Voucher[2].
        IF CcbCCaja.Voucher[3] <> "" THEN X-NUMDOC = CcbCCaja.Voucher[3].       

        FIND FIRST CcbCDocu WHERE 
            CcbCDocu.CodCia = ccbccaja.CodCia AND
            CcbCDocu.CodDoc = "CHC" AND
            CcbCDocu.NroDoc = X-NUMDOC
            NO-ERROR.
        IF AVAILABLE CcbCDocu THEN DO:
        message "Hola" view-as alert-box.
        
          ASSIGN
           CcbCDocu.FlgEst = "P"
           CcbCDocu.FchCan = ?
           CcbCDocu.SdoAct = CcbCDocu.imptot.

           FIND B-CCBC EXCLUSIVE-LOCK where
           B-CCBC.CodCia = CcbCDocu.CodCia and
           B-CCBC.CodDiv = CcbCDocu.CodDiv and
           B-CCBC.CodDoc = "CHD" and
           B-CCBC.NroDoc = CcbCDocu.NroDoc no-error.
            display B-CCBC.imptot.
            delete B-CCBC.
        END.

    END.
    */
end.
