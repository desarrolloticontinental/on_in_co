DISABLE TRIGGERS FOR LOAD OF vtalistamay.

DEF VAR x-Divisiones AS CHAR INIT '00060,00061' NO-UNDO.
DEF VAR x-CodDiv AS CHAR NO-UNDO.
DEF VAR k AS INT NO-UNDO.


DO k = 1 TO NUM-ENTRIES(x-Divisiones):
    x-CodDiv = ENTRY(k, x-Divisiones).
    RUN Carga-Lista.
END.

PROCEDURE Carga-Lista:

FOR EACH vtalistamay WHERE vtalistamay.codcia = 001
    AND vtalistamay.coddiv = x-coddiv:
    DELETE vtalistamay.
END.

DEF VAR J AS INT NO-UNDO.

FOR EACH Almmmatg NO-LOCK WHERE tpoart <> "D":
    CREATE vtalistamay.
    BUFFER-COPY almmmatg
        EXCEPT promfchd promfchh promdto
        TO vtalistamay
        ASSIGN vtalistamay.coddiv = x-coddiv.
    DO J = 1 TO 10:
        IF Almmmatg.PromDivi[J] = x-CODDIV THEN DO:
            ASSIGN
                VtaListaMay.PromDto = Almmmatg.PromDto[J]
                VtaListaMay.PromFchD = Almmmatg.PromFchD[J]
                VtaListaMay.PromFchH = Almmmatg.PromFchH[J].
            LEAVE.
        END.
    END.
    /* RHC 19/11/2013 INCREMENTO POR DIVISION Y FAMILIA */
    FIND VtaTabla WHERE VtaTabla.CodCia = s-codcia
        AND VtaTabla.Llave_c1 = s-CodDiv
        AND VtaTabla.Llave_c2 = Almmmatg.codfam
        AND VtaTabla.Tabla = "DIVFACXLIN"
        NO-LOCK NO-ERROR.
    IF AVAILABLE VtaTabla THEN DO:
        f-PreVta = f-Prevta * (1 + VtaTabla.Valor[1] / 100).
        f-PreBas = f-PreBas * (1 + VtaTabla.Valor[1] / 100).
    END.

END.

END PROCEDURE.
