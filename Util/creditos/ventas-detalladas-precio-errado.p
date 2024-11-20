DEF BUFFER cdocu FOR ccbcdocu.
DEF BUFFER cpedi FOR faccpedi.
DEF VAR cl-codcia AS INT INIT 0.
DEF VAR f-porimp AS DEC.
DEF VAR s-codcli AS CHAR INIT '20109072177'.
DEF VAR x-clfcli AS CHAR.
DEF VAR s-codmon AS INT.
DEF VAR f-prebas AS DEC.
DEF VAR f-factor AS DEC INIT 1.
DEF VAR maxcat AS DEC.
DEF VAR maxvta AS DEC.
DEF VAR s-cndvta AS CHAR.
DEF VAR f-dsctos AS DEC.
DEF VAR f-prevta LIKE ccbddocu.preuni.
DEF VAR x-nrodec AS DEC INIT 4.
DEF VAR x-nrocot LIKE faccpedi.nroped.

OUTPUT TO c:\tmp\diferencias.txt.
FOR EACH ccbcdocu NO-LOCK WHERE codcia = 1
    AND codcli = s-codcli
    AND coddoc = 'fac'
    AND fchdoc >= 01/01/09
    AND coddiv = '00000'
    AND flgest <> 'a',
    EACH ccbddocu OF ccbcdocu NO-LOCK,
    FIRST almmmatg OF ccbddocu NO-LOCK
    BY ccbcdocu.nrodoc:
    FIND faccpedi WHERE faccpedi.codcia = ccbcdocu.codcia
        AND faccpedi.coddoc = ccbcdocu.codped
        AND faccpedi.nroped = ccbcdocu.nroped
        NO-LOCK NO-ERROR.
    IF AVAILABLE faccpedi THEN x-nrocot = faccpedi.nroref.
    ELSE x-nrocot = ''.
    s-codmon = ccbcdocu.codmon.
    s-cndvta = ccbcdocu.fmapgo.
    RUN calculo-precio.
    DISPLAY 
        ccbcdocu.coddoc COLUMN-LABEL 'Doc'
        ccbcdocu.nrodoc COLUMN-LABEL 'Numero'
        ccbcdocu.fchdoc COLUMN-LABEL 'Emision'
        ccbcdocu.nroped COLUMN-LABEL 'Pedido'
        x-nrocot COLUMN-LABEL 'Cotizacion'
        ccbddocu.codmat COLUMN-LABEL 'Material'
        desmat COLUMN-LABEL 'Descripcion'
        desmar COLUMN-LABEL 'Marca'
        candes  COLUMN-LABEL 'Cantidad'
        ccbcdocu.codmon COLUMN-LABEL 'Moneda'
        ccbddocu.undvta COLUMN-LABEL 'Unidad'
        preuni COLUMN-LABEL 'Unitario Incorrecto (¿?)'
        f-prevta COLUMN-LABEL 'Unitario Correcto (¿?)'
        almmmatg.tpocmb COLUMN-LABEL 'Tipo de Cambio'
        WITH STREAM-IO NO-BOX WIDTH 320.
END.
OUTPUT CLOSE.

PROCEDURE calculo-precio:
/* ******************** */
   F-PorImp = 1.

   /*********/
   FIND gn-clie WHERE 
        gn-clie.CodCia = cl-codcia AND  
        gn-clie.CodCli = S-CODCLI NO-LOCK NO-ERROR.
   IF AVAIL gn-clie THEN X-CLFCLI = gn-clie.clfCli.
   ELSE X-CLFCLI = "".
   
   /* RHC 12.06.08 al tipo de cambio de la familia */
   IF S-CODMON = 1 THEN DO:
      IF Almmmatg.MonVta = 1 THEN
          ASSIGN F-PREBAS = (Almmmatg.PreOfi * F-PorImp) * F-FACTOR.
      ELSE
          ASSIGN F-PREBAS = (Almmmatg.PreOfi * F-PorImp) * Almmmatg.TpoCmb * F-FACTOR.
   END.
   IF S-CODMON = 2 THEN DO:
      IF Almmmatg.MonVta = 2 THEN
         ASSIGN F-PREBAS = (Almmmatg.PreOfi * F-PorImp) * F-FACTOR.
      ELSE
         ASSIGN F-PREBAS = ((Almmmatg.PreOfi * F-PorImp) / Almmmatg.TpoCmb) * F-FACTOR.
   END.

   MaxCat = 0.
   MaxVta = 0.
   
   FIND ClfClie WHERE ClfClie.Categoria = X-CLFCLI /*gn-clie.clfCli*/
                NO-LOCK NO-ERROR.
   IF AVAIL ClfClie THEN DO:
      IF Almmmatg.Chr__02 = "P" THEN 
          MaxCat = ClfClie.PorDsc.
      ELSE 
          MaxCat = ClfClie.PorDsc1.
   END.
   
    FIND Dsctos WHERE 
         Dsctos.CndVta = S-CNDVTA AND  
         Dsctos.clfCli = Almmmatg.Chr__02
          NO-LOCK NO-ERROR.
    IF AVAIL Dsctos THEN MaxVta = Dsctos.PorDto.
    
    /***************************************************/
    
    IF NOT AVAIL ClfClie THEN
        F-DSCTOS = (1 - (1 - MaxVta / 100)) * 100.
    ELSE
        F-DSCTOS = (1 - (1 - MaxCat / 100) * (1 - MaxVta / 100)) * 100.
    F-PREVTA = F-PREBAS * (1 - F-DSCTOS / 100).

    RUN BIN/_ROUND1(F-PREVTA,X-NRODEC,OUTPUT F-PREVTA).



END PROCEDURE.
