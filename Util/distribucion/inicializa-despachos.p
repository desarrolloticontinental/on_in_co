DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR s-coddiv AS CHAR INIT '00038' NO-UNDO.

FOR EACH Vtatabla WHERE VtaTabla.CodCia = s-codcia
    AND VtaTabla.Tabla = "CPXDIA"
    AND VtaTabla.Llave_c1 = s-coddiv:
    DELETE Vtatabla.
END.
DEF VAR k AS INT NO-UNDO.
FOR EACH Almtabla NO-LOCK WHERE almtabla.Tabla = "CP":
  DO k = 2 TO 7:
      CREATE VtaTabla.
      ASSIGN
          VtaTabla.CodCia = s-codcia
          VtaTabla.Tabla  = "CPXDIA"
          VtaTabla.Llave_c1 = s-coddiv 
          VtaTabla.Llave_c2 = almtabla.Codigo
          VtaTabla.Valor[1] = k.
  END.
END.

