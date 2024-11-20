DEF VAR x-preuni AS DEC.
DEF VAR x-margen AS DEC.
DEF VAR x-limite AS DEC.
DEF VAR perror AS CHAR.
DEF NEW SHARED VAR s-coddiv AS CHAR.
s-coddiv = '00027'.
FOR EACH vtalistamingn NO-LOCK WHERE codcia = 1, FIRST almmmatg OF vtalistamingn NO-LOCK:
    x-PreUni = VtaListaMinGn.PreOfi.
    IF almmmatg.monvta = 2 THEN x-preuni = x-preuni * almmmatg.tpocmb.
    RUN vtagn/p-margen-utilidad (
        vtalistamingn.codmat,
        x-PreUni,  /* Precio de venta unitario */
        VtaListaMinGn.Chr__01,
        1,
        almmmatg.tpocmb,
        NO,
        "",
        OUTPUT x-Margen,        /* Margen de utilidad */
        OUTPUT x-Limite,        /* Margen mínimo de utilidad */
        OUTPUT pError           /* Control de errores: "OK" "ADM-ERROR" */
        ).
  IF pError = "ADM-ERROR" THEN DO:
      DISPLAY vtalistamingn.codmat x-margen x-limite.
  END.

END.
